module Eval.ResolvedExpression exposing
    ( HigherOrderDispatcher
    , REnv
    , buildHigherOrderRegistry
    , emptyREnv
    , evalR
    )

{-| Resolved-IR evaluator (Phase 3). Consumes `Eval.ResolvedIR.RExpr`
produced by `Eval.Resolver` and the `buildProjectEnvFromParsed` pipeline.

**Iteration 3b1 — simple expressions.** This iteration adds:

  - `RLocal i` — indexed lookup into `env.locals` (the cons-list head is
    the innermost binding).
  - `RIf` — strict conditional with `Bool` check.
  - `RAnd`/`ROr` — short-circuiting boolean operators.
  - `RRecord`, `RRecordAccess`, `RRecordUpdate` — record construction,
    field access, and slot-targeted update.
  - `RLet` — sequential let evaluation for value bindings (arity 0
    including destructuring). Function bindings still return
    `Unsupported` because mutual-recursion + closure capture arrives
    with lambdas in a later iteration.

Still `Unsupported` for now: `RGlobal`, `RLambda`, `RApply`, `RCase`,
`RCtor`, `RRecordAccessFunction`.

3b1 also skips trace/yield/memo threading through its helper functions.
Those execution modes require wiring to `Eval.Module` entry points,
which is a later-iteration concern. An incoming `EvOkTrace` / `EvYield` /
`EvMemoLookup` is currently treated as an error in the helpers that
chain through `EvalResult` values — acceptable because the expressions
supported here never produce those.

-}

import Elm.Syntax.Expression as Expression
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Environment
import Eval.Expression
import Eval.NativeDispatch as NativeDispatch
import Eval.ResolvedIR as IR exposing (RExpr(..))
import FastDict
import MemoSpec
import Set
import Syntax
import Types
    exposing
        ( Config
        , Env
        , EvalErrorData
        , EvalErrorKind(..)
        , EvalResult(..)
        , Implementation(..)
        , Intercept(..)
        , Value(..)
        )
import Value


{-| Evaluator context for the resolved IR.

`locals` is a cons-list of already-bound values; `RLocal i` evaluates to
`List.drop i env.locals |> List.head`. The head is the innermost binding.

`globals` holds pre-computed top-level values. Phase 3 iteration 3b3
populates this lazily — the first time an `RGlobal` is encountered, its
body is evaluated and the result is cached for subsequent lookups.
(Returned via the EvalResult stream so the caller can thread the update.)

`resolvedBodies` is the map from `GlobalId` to the resolved `RExpr` body
the resolver produced at `buildProjectEnv` time. The evaluator reaches
into this when an `RGlobal id` reference needs to evaluate an unresolved
top-level that isn't cached yet.

`currentModule` and `callStack` are bookkeeping for error messages and
eventual tracing integration, matching the fields on the existing
`Env`. Iteration 3a doesn't walk into calls, so they're mostly inert.

-}
type alias REnv =
    { locals : List Value
    , globals : FastDict.Dict IR.GlobalId Value
    , resolvedBodies : FastDict.Dict IR.GlobalId RExpr
    , globalIdToName : FastDict.Dict IR.GlobalId ( ModuleName, String )
    , nativeDispatchers : FastDict.Dict IR.GlobalId NativeDispatch.NativeDispatcher
    , higherOrderDispatchers : FastDict.Dict IR.GlobalId HigherOrderDispatcher
    , kernelDispatchers :
        FastDict.Dict IR.GlobalId
            { arity : Int
            , kernelFn : List Value -> Config -> Env -> EvalResult Value
            }
    , interceptsByGlobal : FastDict.Dict IR.GlobalId ( String, Intercept )
    , fallbackEnv : Env
    , fallbackConfig : Config
    , currentModule : ModuleName
    , callStack : List QualifiedNameRef
    , callDepth : Int
    }


{-| A higher-order kernel dispatcher: a `REnv -> List Value -> Maybe
(EvalResult Value)` that takes the current `REnv` and a list of
already-evaluated arguments, and either returns `Just evalResult` if
it handles that arg shape, or `Nothing` to fall through to the normal
delegation path.

These exist as a sibling to `NativeDispatch.NativeDispatcher` (which
is pure, for scalar operators like `+` and `==`) to cover core
functions like `List.foldl` that take a callback Value as one of
their args. The dispatcher invokes the callback via `applyClosure`
directly — bypassing the `Kernel.function` marshaling path in
`Eval.Expression` that was designed for `AstImpl` closures and
mishandles `RExprImpl` closures (which carry `patterns = []`).

Yield / memo / error propagation is handled automatically by threading
`applyClosure`'s result through `andThenValue` in the helper loop.

Wrapped in a one-constructor `type` (not `type alias`) because Elm
forbids recursive type aliases, and this type is (indirectly)
self-referential via `REnv`.
-}
type HigherOrderDispatcher
    = HigherOrderDispatcher (REnv -> List Value -> Maybe (EvalResult Value))


{-| A completely empty REnv useful for literal-only tests. Real callers
build one from a `ProjectEnv` via `Eval.Module.evalWithResolvedIR`.

The `fallbackEnv` is a completely empty `Env` with no core or user
functions — that's fine for the literal/closure tests this is used
for, because they never hit the core-dispatch path. Tests that need
core dispatch build their own `REnv` with a real project env.

-}
emptyREnv : REnv
emptyREnv =
    { locals = []
    , globals = FastDict.empty
    , resolvedBodies = FastDict.empty
    , globalIdToName = FastDict.empty
    , nativeDispatchers = FastDict.empty
    , higherOrderDispatchers = FastDict.empty
    , kernelDispatchers = FastDict.empty
    , interceptsByGlobal = FastDict.empty
    , fallbackEnv = Environment.empty []
    , fallbackConfig = emptyConfig
    , currentModule = []
    , callStack = []
    , callDepth = 0
    }


{-| Custom trampoline type, replacement for `Recursion.Rec`. The
difference: `RTail` is a pure tail-recursive call that the runner
handles WITHOUT pushing anything onto the continuation stack.

`rTail` was implemented as `Recurse r base`, which pushes
a `base` continuation even for pure tail calls. For iterative tail-call
chains (like the hot RApply → RLambda shortcut used by parser
combinators and review-rule visitors), that leaks the continuation
stack — the heap profiler at a 512 MB OOM showed 10.5 million live
`base`-continuation entries retaining ~480 MB. `RTail` fixes that by
making pure tail calls truly iterative: `go (project r) stack` with no
stack modification.

`RecThen` carries a continuation for non-tail calls, matching the
behavior of `rRecThen`.
-}
type Rec r t a
    = RBase a
    | RTail r
    | RecThen r (t -> Rec r t a)


{-| Construct a base (done) result. Equivalent to `rBase`.
-}
rBase : a -> Rec r t a
rBase =
    RBase


{-| Pure tail call — recurse without pushing a continuation. Use this
wherever a computation ends with a recursive call whose result is the
computation's direct return value. For `RTail r`, the runner does
`go (project r) stack`, so consecutive tail calls iterate in constant
continuation-stack space.
-}
rTail : r -> Rec r t a
rTail =
    RTail


{-| Non-tail recursion: recurse on `r`, then run `after` with the
result. Equivalent to `rRecThen`. The continuation is
pushed onto the runner's stack and popped when the recursion returns.
-}
rRecThen : r -> (t -> Rec r t a) -> Rec r t a
rRecThen =
    RecThen


{-| Run a recursion given a step function and an initial value. Local
`go` closes over `project` (matching `Recursion.runRecursion`'s
structure — fewer arguments per iteration), but adds a fast path for
`RTail` that skips the continuation push.

`Rec` is restricted to `t = a` here (mirroring `runRecursion`'s
`Rec r t t`), which matches every use site in the trampolined
evaluator.
-}
runRec : (r -> Rec r t t) -> r -> t
runRec project init =
    let
        go : Rec r t t -> List (t -> Rec r t t) -> t
        go step stack =
            case step of
                RBase t ->
                    case stack of
                        [] ->
                            t

                        next :: rest ->
                            go (next t) rest

                RTail r ->
                    go (project r) stack

                RecThen r after ->
                    go (project r) (after :: stack)
    in
    go (project init) []


emptyConfig : Config
emptyConfig =
    { trace = False
    , coverage = False
    , coverageProbeLines = Set.empty
    , maxSteps = Nothing
    , tcoTarget = Nothing
    , callCounts = Nothing
    , intercepts = FastDict.empty
    , memoizedFunctions = MemoSpec.emptyRegistry
    , collectMemoStats = False
    , useResolvedIR = False
    }


{-| Evaluate a resolved expression to a `Value`.

Stack-safe via `runRec`. The trampoline's state is
`(REnv, RExpr)` — whenever we need to enter a new body (via
`evalGlobal`'s resolved-body branch, or `applyClosure`'s `RExprImpl`
branch), we `rTail` into that body instead of calling
`evalR` recursively. That keeps cross-body recursion iterative —
the JS stack doesn't grow per Elm function call, which matters for
deeply recursive user code like parser combinators in `elm/parser`
or non-tail-recursive traversals.

Within a single body, sub-expression recursion (e.g. evaluating
arguments of an `RApply`) still uses direct calls because body
expression trees are bounded in depth.

-}
evalR : REnv -> RExpr -> EvalResult Value
evalR initEnv initExpr =
    {- Trampoline step state is `(List Value, RExpr)`, not
       `(REnv, RExpr)`. The static REnv fields (globals,
       resolvedBodies, dispatchers, fallbackEnv, fallbackConfig,
       currentModule, callStack) are closed over here; only the
       dynamic `locals` field varies per iteration. This avoids
       allocating a new REnv record per cross-body transition on
       the hot recursion path — each iteration creates only a
       tuple and a new `locals` cons list. For workloads with
       millions of iterations (elm-spa-parity under site 2a's
       flip), this is the difference between linear memory
       growth and OOM.

       When a Rec-aware helper needs to call a direct-style
       function that takes a full REnv, it constructs one on
       the spot via `envWithLocals staticEnv locals`. Those
       fallbacks are less frequent than the hot recurse path,
       so their record-construction cost doesn't dominate.
    -}
    runRec
        (\( locals, expr ) -> evalRStep initEnv locals expr)
        ( initEnv.locals, initExpr )


{-| Rebuild a full REnv from the static-env closure + the current
locals. Used only when calling direct-style helpers that require the
whole REnv shape; the hot trampoline path never needs this.
-}
envWithLocals : REnv -> List Value -> REnv
envWithLocals staticEnv locals =
    { staticEnv | locals = locals }


{-| One step of the trampolined evaluator. Takes the static REnv
(closed over, never modified here) and the current locals cons
list separately, so the trampoline step state can be just
`(List Value, RExpr)`. Returns a `Rec` computation that the
outer `runRecursion` drives to completion.

Most cases wrap the direct evaluator's result in `rBase`;
only the cases that may lead to cross-body recursion route through
`rTail` / `recurseThen` so that cross-body transitions
don't accumulate JS stack frames.
-}
evalRStep : REnv -> List Value -> RExpr -> Rec ( List Value, RExpr ) (EvalResult Value) (EvalResult Value)
evalRStep staticEnv locals expr =
    case expr of
        RIf cond t f ->
            rRecThen ( locals, cond )
                (\condResult ->
                    case condResult of
                        EvOk (Bool True) ->
                            rTail ( locals, t )

                        EvOk (Bool False) ->
                            rTail ( locals, f )

                        EvOk other ->
                            rBase
                                (evErr (envWithLocals staticEnv locals)
                                    (TypeError
                                        ("if condition not Bool: "
                                            ++ Value.toString other
                                        )
                                    )
                                )

                        _ ->
                            -- EvErr / EvYield / EvMemo / coverage / trace:
                            -- propagate as-is. Trampolined evalR doesn't
                            -- try to thread yields/memos through the
                            -- continuation stack — the higher-order
                            -- helpers that care about those work on
                            -- EvalResult directly.
                            rBase condResult
                )

        RApply (RGlobal id) argExprs ->
            -- Evaluate arg list through the trampoline so each arg's
            -- evaluation stays in the same `runRecursion` as the outer
            -- call. Walking the arg list uses direct recursion in Elm
            -- but it's bounded by arg count (typically 1–6), not by
            -- recursion depth — so no stack issue from that.
            evalArgsStep staticEnv locals argExprs []
                (\argValues ->
                    dispatchGlobalApplyStep staticEnv locals id argValues
                )

        RApply headExpr argExprs ->
            -- Non-global head: a let-bound function, a lambda, or a
            -- parameter-passed callback. Evaluate head, evaluate args,
            -- then apply — all through the trampoline so cross-body
            -- invocations into the callback's body stay iterative.
            rRecThen ( locals, headExpr )
                (\headResult ->
                    case headResult of
                        EvOk headValue ->
                            evalArgsStep staticEnv locals argExprs []
                                (\argValues ->
                                    applyClosureStep staticEnv locals headValue argValues
                                )

                        _ ->
                            rBase headResult
                )

        RGlobal id ->
            evalGlobalStep staticEnv locals id

        RCase scrutineeExpr branches ->
            -- Evaluate scrutinee through the trampoline, then match
            -- branches against it. The matching branch body is
            -- evaluated via `recurse` so cross-body calls inside the
            -- branch stay in this trampoline.
            rRecThen ( locals, scrutineeExpr )
                (\scrutineeResult ->
                    case scrutineeResult of
                        EvOk scrutinee ->
                            matchCaseBranchesStep staticEnv locals scrutinee branches

                        _ ->
                            rBase scrutineeResult
                )

        RLet bindings letBody ->
            -- Evaluate bindings sequentially through the trampoline,
            -- then evaluate the body with the extended locals.
            evalLetBindingsStep staticEnv locals bindings
                (\newLocals ->
                    rTail ( newLocals, letBody )
                )

        -- Trivial cases handled inline to avoid the `envWithLocals`
        -- record copy that the fallback path would pay. These are
        -- the most frequent RExpr constructors after RApply/RIf/RCase.
        RInt i ->
            rBase (EvOk (Int i))

        RFloat f ->
            rBase (EvOk (Float f))

        RString s ->
            rBase (EvOk (String s))

        RChar c ->
            rBase (EvOk (Char c))

        RUnit ->
            rBase (EvOk Unit)

        RLocal i ->
            case List.drop i locals of
                value :: _ ->
                    rBase (EvOk value)

                [] ->
                    rBase
                        (evErr (envWithLocals staticEnv locals)
                            (TypeError
                                ("RLocal "
                                    ++ String.fromInt i
                                    ++ " out of bounds (locals has "
                                    ++ String.fromInt (List.length locals)
                                    ++ " slots)"
                                )
                            )
                        )

        RLambda lambda ->
            rBase (EvOk (makeClosure (envWithLocals staticEnv locals) lambda.arity lambda.body 0 Nothing))

        RCtor ref ->
            rBase (evalConstructor ref)

        RRecordAccessFunction fieldName ->
            rBase
                (EvOk
                    (makeClosure
                        { staticEnv | locals = [] }
                        1
                        (RRecordAccess (RLocal 0) fieldName)
                        0
                        Nothing
                    )
                )

        _ ->
            -- Remaining complex cases: RNegate, RList, RTuple2/3,
            -- RRecord, RRecordAccess, RRecordUpdate, RAnd, ROr, RGLSL.
            -- These are bounded by body structure, not recursion
            -- depth, so the envWithLocals copy is acceptable.
            rBase (evalRDirect (envWithLocals staticEnv locals) expr)


{-| Rec-aware case branch matcher. Walks branches in order; on the first
match, evaluates the branch body through the trampoline so recursive
calls inside the branch share the outer `runRecursion`. Returns a
`TypeError` if no branch matches — Elm's type checker enforces totality
so this should be unreachable at runtime.
-}
matchCaseBranchesStep :
    REnv
    -> List Value
    -> Value
    -> List ( IR.RPattern, RExpr )
    -> Rec ( List Value, RExpr ) (EvalResult Value) (EvalResult Value)
matchCaseBranchesStep staticEnv locals scrutinee branches =
    case branches of
        [] ->
            rBase
                (evErr (envWithLocals staticEnv locals)
                    (TypeError
                        ("case expression failed to match any branch for value: "
                            ++ Value.toString scrutinee
                        )
                    )
                )

        ( pattern, branchBody ) :: rest ->
            case matchPattern pattern scrutinee locals of
                Just newLocals ->
                    rTail ( newLocals, branchBody )

                Nothing ->
                    matchCaseBranchesStep staticEnv locals scrutinee rest


{-| Rec-aware let binding evaluator. Threads each binding's RHS through
`recurseThen` so cross-body calls from inside a let binding stay in the
outer trampoline.

Function bindings (arity > 0) build their closure directly without
needing to evaluate the body (it's an `RLambda` that only constructs a
`PartiallyApplied`), so they bypass the trampoline.
-}
evalLetBindingsStep :
    REnv
    -> List Value
    -> List IR.RLetBinding
    -> (List Value -> Rec ( List Value, RExpr ) (EvalResult Value) (EvalResult Value))
    -> Rec ( List Value, RExpr ) (EvalResult Value) (EvalResult Value)
evalLetBindingsStep staticEnv locals bindings k =
    case bindings of
        [] ->
            k locals

        binding :: rest ->
            if binding.arity > 0 then
                case binding.body of
                    RLambda lambda ->
                        let
                            closureValue : Value
                            closureValue =
                                -- Build a closure that captures the current
                                -- locals. `makeClosure` wants a REnv, so we
                                -- synthesize one with the right locals here
                                -- — this path is uncommon (only fires for
                                -- let-bound recursive functions).
                                makeClosure
                                    (envWithLocals staticEnv locals)
                                    lambda.arity
                                    lambda.body
                                    1
                                    Nothing
                        in
                        evalLetBindingsStep staticEnv (closureValue :: locals) rest k

                    _ ->
                        rBase
                            (evErr (envWithLocals staticEnv locals)
                                (TypeError
                                    ("let function binding '"
                                        ++ binding.debugName
                                        ++ "' body is not an RLambda"
                                    )
                                )
                            )

            else
                rRecThen ( locals, binding.body )
                    (\bindingResult ->
                        case bindingResult of
                            EvOk value ->
                                case matchPattern binding.pattern value locals of
                                    Just newLocals ->
                                        evalLetBindingsStep staticEnv newLocals rest k

                                    Nothing ->
                                        rBase
                                            (evErr (envWithLocals staticEnv locals)
                                                (TypeError
                                                    ("pattern match failed in let binding '"
                                                        ++ binding.debugName
                                                        ++ "'"
                                                    )
                                                )
                                            )

                            _ ->
                                rBase bindingResult
                    )


{-| Rec-aware closure application. For an `RExprImpl` closure at exact
arity, shortcuts directly to `recurse` on the closure body so the
cross-body transition stays in the outer trampoline. For partial or
over-application of `RExprImpl`, or for non-`RExprImpl` closures
(`AstImpl`, `KernelImpl`, `Custom`), falls through to the direct
`applyClosure` — those paths either create a fresh `PartiallyApplied`
Value (no recursion) or go through the old evaluator's trampolined
`evalExpression` (stack-safe).
-}
applyClosureStep :
    REnv
    -> List Value
    -> Value
    -> List Value
    -> Rec ( List Value, RExpr ) (EvalResult Value) (EvalResult Value)
applyClosureStep staticEnv locals head newArgs =
    if List.isEmpty newArgs then
        rBase (EvOk head)

    else
        case head of
            PartiallyApplied _ appliedArgs _ debugName impl arity ->
                case impl of
                    RExprImpl implBody ->
                        let
                            totalArgs : List Value
                            totalArgs =
                                appliedArgs ++ newArgs

                            totalCount : Int
                            totalCount =
                                List.length totalArgs
                        in
                        if totalCount < arity then
                            -- Partial application: construct a new PA. No recursion.
                            rBase
                                (EvOk
                                    (PartiallyApplied
                                        (Environment.empty [])
                                        totalArgs
                                        []
                                        debugName
                                        impl
                                        arity
                                    )
                                )

                        else if totalCount == arity then
                            -- Exact application: shortcut directly into the
                            -- body via `recurse`, setting up locals the same
                            -- way `runRExprClosure` would. This is the key
                            -- stack-safety hop — without it, every callback
                            -- invocation starts a fresh `runRecursion`.
                            let
                                selfClosure : Value
                                selfClosure =
                                    PartiallyApplied
                                        (Environment.empty [])
                                        []
                                        []
                                        debugName
                                        impl
                                        arity

                                withSelf : List Value
                                withSelf =
                                    prependRepeated implBody.selfSlots selfClosure implBody.capturedLocals

                                bodyLocals : List Value
                                bodyLocals =
                                    List.foldl (::) withSelf totalArgs
                            in
                            rTail ( bodyLocals, implBody.body )

                        else
                            -- Over-application: run the body with `arity` args,
                            -- then apply the extras to the result. The "run the
                            -- body" part is trampolined; the "apply extras" part
                            -- becomes another `applyClosureStep` via the
                            -- continuation.
                            let
                                bodyArgs : List Value
                                bodyArgs =
                                    List.take arity totalArgs

                                extraArgs : List Value
                                extraArgs =
                                    List.drop arity totalArgs

                                selfClosure : Value
                                selfClosure =
                                    PartiallyApplied
                                        (Environment.empty [])
                                        []
                                        []
                                        debugName
                                        impl
                                        arity

                                withSelf : List Value
                                withSelf =
                                    prependRepeated implBody.selfSlots selfClosure implBody.capturedLocals

                                bodyLocals : List Value
                                bodyLocals =
                                    List.foldl (::) withSelf bodyArgs
                            in
                            rRecThen ( bodyLocals, implBody.body )
                                (\bodyResult ->
                                    case bodyResult of
                                        EvOk resultValue ->
                                            applyClosureStep staticEnv locals resultValue extraArgs

                                        _ ->
                                            rBase bodyResult
                                )

                    _ ->
                        -- AstImpl / KernelImpl: delegate to the direct path.
                        -- `Eval.Expression.evalFunction` uses its own trampoline
                        -- (`runRec` on the expression tree), so
                        -- this doesn't accumulate JS frames.
                        rBase (applyClosure (envWithLocals staticEnv locals) head newArgs)

            _ ->
                -- Custom constructor or other non-callable: delegate.
                rBase (applyClosure (envWithLocals staticEnv locals) head newArgs)


{-| Rec-aware arg list evaluator. Threads each expression evaluation
through `recurseThen` so the args share a single trampoline with the
outer `evalRStep` call — critical for stack safety, since otherwise
each arg evaluation would start a fresh `runRecursion` and accumulate
JS frames across sibling calls.

The `k` continuation receives the fully-evaluated arg list and returns
the next `Rec` step. Short-circuits on non-`EvOk` intermediate results.
-}
evalArgsStep :
    REnv
    -> List Value
    -> List RExpr
    -> List Value
    -> (List Value -> Rec ( List Value, RExpr ) (EvalResult Value) (EvalResult Value))
    -> Rec ( List Value, RExpr ) (EvalResult Value) (EvalResult Value)
evalArgsStep staticEnv locals remaining accRev k =
    case remaining of
        [] ->
            k (List.reverse accRev)

        head :: rest ->
            rRecThen ( locals, head )
                (\headResult ->
                    case headResult of
                        EvOk value ->
                            evalArgsStep staticEnv locals rest (value :: accRev) k

                        _ ->
                            rBase headResult
                )


{-| Rec-aware dispatch for `RApply (RGlobal id) args`. Mirrors
`dispatchGlobalApplyNoIntercept`, but when the dispatch resolves to
a user-declaration body this returns a `rTail` on the
body so the trampoline tail-calls into it instead of growing the JS
stack.

Intercepts still go through the direct path (they're uncommon enough
that the one-level stack frame doesn't matter, and their callbacks
are arbitrary framework code that isn't ready to return `Rec`).
-}
dispatchGlobalApplyStep :
    REnv
    -> List Value
    -> IR.GlobalId
    -> List Value
    -> Rec ( List Value, RExpr ) (EvalResult Value) (EvalResult Value)
dispatchGlobalApplyStep staticEnv locals id argValues =
    case FastDict.get id staticEnv.interceptsByGlobal of
        Just _ ->
            -- Intercept path — defer to the direct dispatch.
            rBase (dispatchGlobalApply (envWithLocals staticEnv locals) id argValues)

        Nothing ->
            case FastDict.get id staticEnv.globals of
                Just cached ->
                    rBase (applyClosure (envWithLocals staticEnv locals) cached argValues)

                Nothing ->
                    case FastDict.get id staticEnv.resolvedBodies of
                        Just (RLambda lambda) ->
                            {- Shortcut: the resolved body is an RLambda,
                               and we have the args in hand. The normal
                               path would be `evalR body` → PA →
                               `applyClosure env PA argValues` →
                               `runRExprClosure` → `evalR inner.body` with
                               locals = args. That chain has two fresh
                               `evalR` calls, each starting its own
                               `runRecursion` and blowing the JS stack on
                               deep recursion.

                               Instead, when arg count matches arity, we
                               skip the PA round-trip and `recurse`
                               directly into `lambda.body` with the args
                               bound as locals. This keeps the trampoline
                               iterative across cross-body calls, which
                               is exactly the point of the refactor.
                            -}
                            let
                                argCount : Int
                                argCount =
                                    List.length argValues
                            in
                            if argCount == lambda.arity then
                                let
                                    bodyLocals : List Value
                                    bodyLocals =
                                        List.foldl (::) [] argValues
                                in
                                rTail ( bodyLocals, lambda.body )

                            else
                                -- Partial / over-application: fall back
                                -- to the direct path, which handles both
                                -- cases correctly via `applyClosure`.
                                rBase (dispatchGlobalApplyNoIntercept (envWithLocals staticEnv locals) id argValues)

                        Just body ->
                            rRecThen ( [], body )
                                (\headResult ->
                                    case headResult of
                                        EvOk headValue ->
                                            rBase
                                                (applyClosure (envWithLocals staticEnv locals) headValue argValues)

                                        _ ->
                                            rBase headResult
                                )

                        Nothing ->
                            rBase (fallbackDispatch (envWithLocals staticEnv locals) id argValues)


{-| Rec-aware `evalGlobal`. Zero-arg top-level references dispatch
into another body; that's the cross-body transition we want to
trampoline.
-}
evalGlobalStep :
    REnv
    -> List Value
    -> IR.GlobalId
    -> Rec ( List Value, RExpr ) (EvalResult Value) (EvalResult Value)
evalGlobalStep staticEnv locals id =
    case FastDict.get id staticEnv.globals of
        Just cached ->
            rBase (EvOk cached)

        Nothing ->
            case FastDict.get id staticEnv.resolvedBodies of
                Just body ->
                    rTail ( [], body )

                Nothing ->
                    rBase (delegateCoreApply (envWithLocals staticEnv locals) id [])


{-| Fallback dispatch — native, higher-order, or old-evaluator delegation.
Called when a global has no resolved body AND no cache hit. These paths
are all direct (no cross-body recursion into evalR itself), so they can
stay on the direct-result path.
-}
fallbackDispatch : REnv -> IR.GlobalId -> List Value -> EvalResult Value
fallbackDispatch env id argValues =
    case NativeDispatch.tryDispatch env.nativeDispatchers id argValues of
        Just result ->
            EvOk result

        Nothing ->
            case tryHigherOrderDispatch env id argValues of
                Just result ->
                    result

                Nothing ->
                    delegateCoreApply env id argValues


{-| Helper: lift a non-`EvOk` `EvalResult (List Value)` to `EvalResult Value`
for the fall-through propagation in the trampolined `RApply` branch.
-}
mapListResultToValue : EvalResult (List Value) -> EvalResult Value
mapListResultToValue result =
    case result of
        EvOk _ ->
            -- Shouldn't happen — the caller checks for EvOk before
            -- dispatching to this helper.
            evErrBlank "mapListResultToValue: unexpected EvOk"

        EvErr e ->
            EvErr e

        EvYield tag payload resume ->
            EvYield tag payload (\value -> mapListResultToValue (resume value))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> mapListResultToValue (resume maybeValue))

        EvMemoStore payload inner ->
            EvMemoStore payload (mapListResultToValue inner)

        EvOkTrace _ _ _ ->
            evErrBlank "mapListResultToValue: EvOkTrace not supported in trampolined evalR"

        EvErrTrace e tree logs ->
            EvErrTrace e tree logs

        EvOkCoverage _ _ ->
            evErrBlank "mapListResultToValue: EvOkCoverage not supported in trampolined evalR"

        EvErrCoverage _ _ ->
            evErrBlank "mapListResultToValue: EvErrCoverage not supported in trampolined evalR"


{-| Direct-style step evaluator — the pre-trampolining implementation,
still used as the fallback for cases `evalRStep` hasn't been
converted to handle via `Recursion`. Each case either returns a
leaf value directly or calls `evalR` (the trampolined wrapper)
recursively; within-body recursion stays direct because body
expression trees are bounded.
-}
evalRDirect : REnv -> RExpr -> EvalResult Value
evalRDirect env expr =
    case expr of
        RInt i ->
            EvOk (Int i)

        RFloat f ->
            EvOk (Float f)

        RString s ->
            EvOk (String s)

        RChar c ->
            EvOk (Char c)

        RUnit ->
            EvOk Unit

        RNegate inner ->
            case evalR env inner of
                EvOk (Int i) ->
                    EvOk (Int -i)

                EvOk (Float f) ->
                    EvOk (Float -f)

                EvOk other ->
                    evErr env (TypeError ("negate applied to non-numeric " ++ Value.toString other))

                other ->
                    other

        RList items ->
            evalExprList env items
                |> mapResult (\vs -> List vs)

        RTuple2 a b ->
            case evalR env a of
                EvOk va ->
                    case evalR env b of
                        EvOk vb ->
                            EvOk (Tuple va vb)

                        bErr ->
                            bErr

                aErr ->
                    aErr

        RTuple3 a b c ->
            case evalR env a of
                EvOk va ->
                    case evalR env b of
                        EvOk vb ->
                            case evalR env c of
                                EvOk vc ->
                                    EvOk (Triple va vb vc)

                                cErr ->
                                    cErr

                        bErr ->
                            bErr

                aErr ->
                    aErr

        RLocal i ->
            case List.drop i env.locals of
                value :: _ ->
                    EvOk value

                [] ->
                    evErr env
                        (TypeError
                            ("RLocal "
                                ++ String.fromInt i
                                ++ " out of bounds (locals has "
                                ++ String.fromInt (List.length env.locals)
                                ++ " slots)"
                            )
                        )

        RIf cond t f ->
            evalR env cond
                |> andThenValue
                    (\condValue ->
                        case condValue of
                            Bool True ->
                                evalR env t

                            Bool False ->
                                evalR env f

                            other ->
                                evErr env
                                    (TypeError
                                        ("if condition not Bool: "
                                            ++ Value.toString other
                                        )
                                    )
                    )

        RAnd l r ->
            evalR env l
                |> andThenValue
                    (\lValue ->
                        case lValue of
                            Bool False ->
                                EvOk (Bool False)

                            Bool True ->
                                evalR env r
                                    |> andThenValue
                                        (\rValue ->
                                            case rValue of
                                                Bool _ ->
                                                    EvOk rValue

                                                other ->
                                                    evErr env
                                                        (TypeError
                                                            ("&& right not Bool: "
                                                                ++ Value.toString other
                                                            )
                                                        )
                                        )

                            other ->
                                evErr env
                                    (TypeError
                                        ("&& left not Bool: " ++ Value.toString other)
                                    )
                    )

        ROr l r ->
            evalR env l
                |> andThenValue
                    (\lValue ->
                        case lValue of
                            Bool True ->
                                EvOk (Bool True)

                            Bool False ->
                                evalR env r
                                    |> andThenValue
                                        (\rValue ->
                                            case rValue of
                                                Bool _ ->
                                                    EvOk rValue

                                                other ->
                                                    evErr env
                                                        (TypeError
                                                            ("|| right not Bool: "
                                                                ++ Value.toString other
                                                            )
                                                        )
                                        )

                            other ->
                                evErr env
                                    (TypeError
                                        ("|| left not Bool: " ++ Value.toString other)
                                    )
                    )

        RRecord fields ->
            evalRecordFields env fields
                |> mapResult (FastDict.fromList >> Record)

        RRecordAccess recordExpr fieldName ->
            evalR env recordExpr
                |> andThenValue
                    (\recordValue ->
                        case recordValue of
                            Record dict ->
                                case FastDict.get fieldName dict of
                                    Just v ->
                                        EvOk v

                                    Nothing ->
                                        evErr env
                                            (TypeError
                                                ("record has no field ." ++ fieldName)
                                            )

                            other ->
                                evErr env
                                    (TypeError
                                        (".field access on non-record: "
                                            ++ Value.toString other
                                        )
                                    )
                    )

        RRecordUpdate slotIdx setters ->
            case List.drop slotIdx env.locals of
                (Record originalDict) :: _ ->
                    evalRecordFields env setters
                        |> mapResult
                            (\newFields ->
                                Record
                                    (List.foldl
                                        (\( fieldName, value ) acc ->
                                            FastDict.insert fieldName value acc
                                        )
                                        originalDict
                                        newFields
                                    )
                            )

                other :: _ ->
                    evErr env
                        (TypeError
                            ("record update target (slot "
                                ++ String.fromInt slotIdx
                                ++ ") is not a record: "
                                ++ Value.toString other
                            )
                        )

                [] ->
                    evErr env
                        (TypeError
                            ("record update slot "
                                ++ String.fromInt slotIdx
                                ++ " out of bounds"
                            )
                        )

        RLet bindings letBody ->
            evalLetBindings env bindings
                |> andThenList
                    (\newLocals ->
                        evalR { env | locals = newLocals } letBody
                    )

        RLambda lambda ->
            -- Creating a closure snapshots the current locals. The body
            -- will be run with its arguments prepended to this snapshot.
            -- Regular lambdas don't need a self-reference slot.
            EvOk (makeClosure env lambda.arity lambda.body 0 Nothing)

        RApply headExpr argExprs ->
            -- Fast path for core-backed globals: evaluate the args, then
            -- either dispatch natively (via the NativeDispatch registry,
            -- which covers the ~15 hottest core functions) or delegate
            -- the whole call to the old evaluator via a synthesized
            -- `Application` AST.
            case headExpr of
                RGlobal id ->
                    evalExprList env argExprs
                        |> andThenList
                            (\argValues ->
                                dispatchGlobalApply env id argValues
                            )

                _ ->
                    evalR env headExpr
                        |> andThenValue
                            (\headValue ->
                                evalExprList env argExprs
                                    |> andThenList
                                        (\argValues ->
                                            applyClosure env headValue argValues
                                        )
                            )

        RCtor ref ->
            evalConstructor ref

        RCase scrutineeExpr branches ->
            evalR env scrutineeExpr
                |> andThenValue
                    (\scrutineeValue ->
                        evalCaseBranches env scrutineeValue branches
                    )

        RRecordAccessFunction fieldName ->
            -- `.field` is a function `\r -> r.field`. Build it as a
            -- one-arg closure whose body is `RRecordAccess (RLocal 0) field`.
            -- No captures needed since the body only references the argument.
            EvOk
                (makeClosure
                    { env | locals = [] }
                    1
                    (RRecordAccess (RLocal 0) fieldName)
                    0
                    Nothing
                )

        RGlobal id ->
            evalGlobal env id

        RGLSL _ ->
            evErr env (Unsupported "RGLSL (not supported)")



-- CLOSURES AND APPLICATION


{-| Build a `PartiallyApplied` Value carrying a resolved-IR body. The Env
field is a dummy (the new evaluator never reads it) and the patterns list
is empty (the new evaluator uses the cached-arity Int directly instead of
counting patterns).

`selfSlots` is the number of self-reference slots to prepend to the
captured locals at call time. Use `0` for regular lambda closures and `1`
for let-bound recursive functions — see the `Implementation` docstring in
`Types.elm` for why.

-}
makeClosure : REnv -> Int -> RExpr -> Int -> Maybe QualifiedNameRef -> Value
makeClosure env arity body selfSlots debugName =
    PartiallyApplied
        {- Save the current `fallbackEnv` in the `PartiallyApplied`
           instead of an empty placeholder. The new evaluator itself
           never reads this field, but the kernel marshalling layer
           (`Kernel.function` in `src/Kernel.elm`) uses it as
           `localEnv` when it converts a `PartiallyApplied` Value to a
           pure-Elm higher-order function and invokes `evalFunction`
           on each call. Without the real `fallbackEnv`, `localEnv`
           has `shared.resolveBridge = noResolveBridge` and the
           bridge fires into a stub error the moment a resolved-IR
           closure reaches a core higher-order function (`List.foldl`,
           `List.map`, `Dict.foldl`, etc.).
        -}
        env.fallbackEnv
        []
        []
        debugName
        (RExprImpl
            { body = body
            , capturedLocals = env.locals
            , selfSlots = selfSlots
            }
        )
        arity


{-| Apply a list of new arguments to a callable value. Handles three cases:

  - **Partial application**: fewer args total than arity → return an
    updated closure with more `appliedArgs`.

  - **Exact application**: total args == arity → run the body with the
    captured locals plus the bound arguments.

  - **Over-application**: total args > arity → run with exactly `arity`
    args, then recursively apply the extra args to the result. Common in
    Elm code like `List.map (\x -> \y -> ...) xs ys`.

Also dispatches `Custom` constructor values (which behave like
partially-applied N-arg constructors — applying args just extends the
arg list) and passes kernel-backed `PartiallyApplied` values through to
a simple argument accumulator. Full kernel dispatch from the new
evaluator is deferred until Phase 3 wire-up (`Config.useResolvedIR`
flag); for now, applying to a kernel closure returns `Unsupported`.

-}
applyClosure : REnv -> Value -> List Value -> EvalResult Value
applyClosure env head newArgs =
    if List.isEmpty newArgs then
        EvOk head

    else
        case head of
            PartiallyApplied paLocalEnv appliedArgs patterns debugName impl arity ->
                case impl of
                    RExprImpl implBody ->
                        let
                            totalArgs : List Value
                            totalArgs =
                                appliedArgs ++ newArgs

                            totalCount : Int
                            totalCount =
                                List.length totalArgs

                            selfClosure : Value
                            selfClosure =
                                -- The "fresh" self-reference used for
                                -- recursive calls has no applied args — it's
                                -- what the user wrote as `f`, not what they
                                -- wrote as `f 5` part-way through a call.
                                PartiallyApplied
                                    (Environment.empty [])
                                    []
                                    []
                                    debugName
                                    impl
                                    arity
                        in
                        if totalCount < arity then
                            EvOk
                                (PartiallyApplied
                                    (Environment.empty [])
                                    totalArgs
                                    []
                                    debugName
                                    impl
                                    arity
                                )

                        else if totalCount == arity then
                            runRExprClosure env implBody selfClosure totalArgs

                        else
                            let
                                bodyArgs : List Value
                                bodyArgs =
                                    List.take arity totalArgs

                                extraArgs : List Value
                                extraArgs =
                                    List.drop arity totalArgs
                            in
                            runRExprClosure env implBody selfClosure bodyArgs
                                |> andThenValue
                                    (\result ->
                                        applyClosure env result extraArgs
                                    )

                    _ ->
                        {- AstImpl / KernelImpl: delegate to the old
                           evaluator's `evalFunction`, which already handles
                           pattern binding for AstImpl and calls the kernel
                           function directly for KernelImpl. This is the
                           path that lets higher-order core callbacks like
                           `List.foldl (+) 0 xs` work — `(+)` is a KernelImpl
                           PA, and the dispatcher in `NativeDispatchHO`
                           calls this with the fully-applied args list.

                           For partial application (total < arity) we could
                           return a PA here too, but the old evaluator's
                           `evalFunction` already handles that case (it
                           returns `Value.mkPartiallyApplied` when
                           `oldArgsLength < patternsLength`).
                        -}
                        let
                            totalArgs : List Value
                            totalArgs =
                                appliedArgs ++ newArgs
                        in
                        Eval.Expression.evalFunction
                            totalArgs
                            patterns
                            arity
                            debugName
                            impl
                            env.fallbackConfig
                            paLocalEnv

            Custom qualRef existingArgs ->
                -- Constructor application: treat as arg accumulation.
                -- Elm's type checker ensures arity is correct upstream, so
                -- we don't validate here.
                EvOk (Custom qualRef (existingArgs ++ newArgs))

            other ->
                evErr env
                    (TypeError
                        ("apply on non-callable: " ++ Value.toString other)
                    )


{-| Run a resolved-IR closure body with the captured locals plus newly-bound
arguments. The args are prepended in application order, so the first-bound
parameter ends up at the deepest index.

`selfSlots` copies of the closure itself are prepended to the captured
locals before the args are added. This implements single-binding
self-recursion without needing cyclic data structures — see `makeClosure`
and the `Implementation.RExprImpl` docstring for the full story.

-}
runRExprClosure :
    REnv
    ->
        { body : RExpr
        , capturedLocals : List Value
        , selfSlots : Int
        }
    -> Value
    -> List Value
    -> EvalResult Value
runRExprClosure env implBody selfClosure args =
    let
        withSelf : List Value
        withSelf =
            prependRepeated implBody.selfSlots selfClosure implBody.capturedLocals

        bodyLocals : List Value
        bodyLocals =
            List.foldl (::) withSelf args

        bodyEnv : REnv
        bodyEnv =
            { env | locals = bodyLocals, callDepth = env.callDepth + 1 }
    in
    evalR bodyEnv implBody.body


prependRepeated : Int -> a -> List a -> List a
prependRepeated n value list =
    if n <= 0 then
        list

    else
        prependRepeated (n - 1) value (value :: list)



-- GLOBALS


{-| Evaluate a top-level reference.

Iteration 3b3 supports **user declarations only** — GlobalIds that the
resolver produced for declarations in the project's user modules and
stored in `env.resolvedBodies`. Core declarations (`Basics.add`,
`List.map`, etc.) are deliberately left as `Unsupported` so tests and
benchmarks are forced to notice when they hit the boundary. A follow-up
iteration (3b4 or later) will add a delegation path that dispatches core
calls through the existing string-keyed evaluator for interoperation.

The evaluation happens against a **fresh locals stack** (`[]`) because
top-level declarations don't close over any runtime locals — everything
they reference is either another global or a binder inside their own
body. This matches how the string-keyed evaluator populates
`currentModuleFunctions` / `letFunctions` for calls into top-level
declarations.

Memoized top-levels (`env.globals`) are checked before the body is
evaluated; a hit returns immediately. Iteration 3b3 does NOT write to
`env.globals` after a miss — threading cache updates through the
`EvalResult` stream requires the same plumbing the memoization system
already uses, and that's deferred until we wire the `useResolvedIR`
flag into `Eval.Module`'s actual entry points in a later iteration.

-}
evalGlobal : REnv -> IR.GlobalId -> EvalResult Value
evalGlobal env id =
    case FastDict.get id env.globals of
        Just cached ->
            EvOk cached

        Nothing ->
            case FastDict.get id env.resolvedBodies of
                Just body ->
                    if env.callDepth >= evalRCallDepthBudget then
                        -- See `dispatchGlobalApplyNoIntercept` for why.
                        delegateCoreApply env id []

                    else
                        let
                            topLevelEnv : REnv
                            topLevelEnv =
                                { env | locals = [] }
                        in
                        evalR topLevelEnv body

                Nothing ->
                    -- Core declaration — delegate a zero-arg reference to
                    -- the old evaluator. Used for things like `Basics.pi`
                    -- (a value, not a function) and for taking a core
                    -- function reference without immediately applying it.
                    delegateCoreApply env id []


{-| Dispatch an RGlobal being called with the given (already-evaluated)
argument Values. Order of precedence:

1. **Intercepts.** If the GlobalId has a registered intercept, call its
   callback with the args + fallback Config/Env. Matches the old
   evaluator's intercept check in `Eval.Expression` around line 1892.
2. **Cached globals.** If the global value is memoized in `env.globals`
   (e.g., a top-level arity-0 value), apply args to that value.
3. **User declarations.** If the global has an `RExpr` body in
   `env.resolvedBodies`, evaluate it to a Value (which produces a
   closure for arity > 0) and apply args.
4. **Native dispatch.** Hot operators like `+`, `-`, `==` go directly
   through `NativeDispatch.tryDispatch`.
5. **Delegation fallback.** Synthesize a Core call and hand it to the
   old evaluator.

The intercept check happens **first** because intercepts are how the
framework short-circuits dispatch at specific qualified names — they
must beat every other path.

-}
dispatchGlobalApply : REnv -> IR.GlobalId -> List Value -> EvalResult Value
dispatchGlobalApply env id argValues =
    case FastDict.get id env.interceptsByGlobal of
        Just ( qualifiedName, Intercept interceptFn ) ->
            let
                evaluateOriginal : () -> EvalResult Value
                evaluateOriginal () =
                    dispatchGlobalApplyNoIntercept env id argValues
            in
            interceptFn
                { qualifiedName = qualifiedName
                , evaluateOriginal = evaluateOriginal
                }
                argValues
                env.fallbackConfig
                env.fallbackEnv

        Nothing ->
            dispatchGlobalApplyNoIntercept env id argValues


{-| Same as `dispatchGlobalApply` but skips the intercept check. Used as
the `evaluateOriginal` continuation for intercept callbacks that want to
fall back to normal dispatch, and for calls that have already been
intercept-checked.
-}
dispatchGlobalApplyNoIntercept : REnv -> IR.GlobalId -> List Value -> EvalResult Value
dispatchGlobalApplyNoIntercept env id argValues =
    case FastDict.get id env.globals of
        Just cached ->
            applyClosure env cached argValues

        Nothing ->
            case FastDict.get id env.resolvedBodies of
                Just body ->
                    {- Depth-budget fallback: `evalR` is direct-style and
                       blows the JS stack on deeply-nested real Elm code.
                       Before we run a resolved body, check how deep the
                       call chain is; if it's past the budget, hand off
                       to the old evaluator whose trampolined `call`
                       machinery handles arbitrary depth. The old
                       evaluator's `ResolveBridge` closes the loop in
                       the other direction.
                    -}
                    if env.callDepth >= evalRCallDepthBudget then
                        case NativeDispatch.tryDispatch env.nativeDispatchers id argValues of
                            Just result ->
                                EvOk result

                            Nothing ->
                                case tryHigherOrderDispatch env id argValues of
                                    Just result ->
                                        result

                                    Nothing ->
                                        delegateCoreApply env id argValues

                    else
                        let
                            topLevelEnv : REnv
                            topLevelEnv =
                                { env | locals = [] }
                        in
                        evalR topLevelEnv body
                            |> andThenValue
                                (\headValue ->
                                    applyClosure env headValue argValues
                                )

                Nothing ->
                    case NativeDispatch.tryDispatch env.nativeDispatchers id argValues of
                        Just result ->
                            EvOk result

                        Nothing ->
                            case tryHigherOrderDispatch env id argValues of
                                Just result ->
                                    result

                                Nothing ->
                                    delegateCoreApply env id argValues


{-| Soft stack budget for `evalR`'s direct-style recursion. When a
resolved body is about to be dispatched and `env.callDepth` is past
this threshold, we fall through to `delegateCoreApply` so the old
evaluator's trampolined machinery handles the rest of the chain. The
old evaluator's `ResolveBridge` will loop back into `evalR` for any
resolved closures it encounters, so correctness is preserved — we
just trade new-evaluator speed for stack safety on deep chains.

Chosen empirically so that `small-12` / `elm-spa-parity` workloads
stay under the budget on their normal call depth while still tripping
on the pathological chains that were crashing the resolver-widened
configuration.
-}
evalRCallDepthBudget : Int
evalRCallDepthBudget =
    150


{-| Delegate a core declaration call to the old evaluator by synthesizing
an `Application` AST (or a bare `FunctionOrValue` for zero-arg) and
calling `Eval.Expression.evalExpression`.

The args are the already-evaluated Values from the new evaluator; they
get converted back to Expressions via `Value.toExpression` for the
old evaluator to consume. This is lossy for closures and some other
compound values, but fine for literals, constructors, records, and
lists of those — the common arguments to core functions.

The fallback `Env` already has every core function in
`shared.functions`, so the old evaluator's dispatch just works. The
`currentModule` on the fallback env is set per-call to the module
containing the target declaration, so trace output matches the
expected call site.

-}
delegateCoreApply : REnv -> IR.GlobalId -> List Value -> EvalResult Value
delegateCoreApply env id args =
    -- Kernel fast path: if this GlobalId is a pre-resolved direct kernel
    -- wrapper, call the kernel function directly with the args, skipping
    -- the Value.toExpression + synthesized Application + evalExpression
    -- round-trip entirely. This covers most of Basics.*, String.*,
    -- Char.*, etc. — wherever the Core module wrapper is just a
    -- pass-through to `Elm.Kernel.*`.
    case FastDict.get id env.kernelDispatchers of
        Just dispatcher ->
            if List.length args == dispatcher.arity then
                dispatcher.kernelFn args env.fallbackConfig env.fallbackEnv

            else
                -- Partial or over-application of a kernel function:
                -- fall back to the general delegation path, which
                -- constructs a PartiallyApplied value via the old
                -- evaluator's usual machinery. Uncommon for hot
                -- operators since they're arity-2.
                delegateViaAst env id args

        Nothing ->
            delegateViaAst env id args


delegateViaAst : REnv -> IR.GlobalId -> List Value -> EvalResult Value
delegateViaAst env id args =
    case FastDict.get id env.globalIdToName of
        Nothing ->
            evErr env
                (Unsupported
                    ("RGlobal " ++ String.fromInt id ++ " has no name metadata")
                )

        Just ( moduleName, name ) ->
            delegateByName env moduleName name args


{-| Synthesize an `Application` AST calling `moduleName.name` with the
given arg Values and dispatch it through the old evaluator. Used both by
`delegateViaAst` (which resolves a `GlobalId` to its name first) and by
`applyClosure`'s Custom branch (which detects a record-alias constructor
mis-routed through `RCtor` and needs to run its synthesized function body
to produce the correct `Record` value).

Each arg is injected into the dispatched env's `values` under a unique
synthetic name and referenced from the synthesized AST instead of being
round-tripped through `Value.toExpression`. That avoids the
`<resolved-closure>` placeholder `Value.toExpression` emits for
`RExprImpl` closures, which previously crashed the old evaluator inside
kernel callbacks. Literal args go through injection too — it's cheaper
than a `Value.toExpression` round trip.
-}
delegateByName : REnv -> ModuleName -> String -> List Value -> EvalResult Value
delegateByName env moduleName name args =
    let
        argBindings : List ( String, Value )
        argBindings =
            List.indexedMap
                (\i v -> ( "__re_arg_" ++ String.fromInt i ++ "__", v ))
                args

        argReferences : List (Node Expression.Expression)
        argReferences =
            argBindings
                |> List.map
                    (\( argName, _ ) ->
                        Syntax.fakeNode (Expression.FunctionOrValue [] argName)
                    )

        headExpr : Node Expression.Expression
        headExpr =
            Syntax.fakeNode (Expression.FunctionOrValue moduleName name)

        fullExpr : Node Expression.Expression
        fullExpr =
            if List.isEmpty args then
                headExpr

            else
                Syntax.fakeNode
                    (Expression.Application (headExpr :: argReferences))

        baseEnv : Env
        baseEnv =
            env.fallbackEnv

        dispatchModuleKey : String
        dispatchModuleKey =
            Environment.moduleKey moduleName

        dispatchValues : FastDict.Dict String Value
        dispatchValues =
            List.foldl
                (\( argName, argValue ) acc ->
                    FastDict.insert argName argValue acc
                )
                baseEnv.values
                argBindings

        dispatchEnv : Env
        dispatchEnv =
            { baseEnv
                | currentModule = moduleName
                , currentModuleKey = dispatchModuleKey
                , currentModuleFunctions =
                    FastDict.get dispatchModuleKey baseEnv.shared.functions
                        |> Maybe.withDefault FastDict.empty
                , imports =
                    FastDict.get dispatchModuleKey baseEnv.shared.moduleImports
                        |> Maybe.withDefault baseEnv.imports
                , values = dispatchValues
            }
    in
    Eval.Expression.evalExpression fullExpr env.fallbackConfig dispatchEnv



-- CONSTRUCTORS


{-| Evaluate a constructor reference. Special-cases `True`/`False` (Elm's
`Basics.Bool` constructors map to the dedicated `Bool` Value variant) and
treats every other constructor as an initially-empty `Custom` value that
`applyClosure` grows as arguments are applied.

Name matching ignores the module name to match the existing evaluator's
behavior — Elm's type checker has already enforced unique constructor
names within a scope, and the resolver preserves source-level qualification
which may be empty or qualified depending on how the user imported the
type.

-}
evalConstructor : { moduleName : ModuleName, name : String } -> EvalResult Value
evalConstructor { moduleName, name } =
    case name of
        "True" ->
            EvOk (Bool True)

        "False" ->
            EvOk (Bool False)

        _ ->
            EvOk (Custom { moduleName = moduleName, name = name } [])



-- CASE EXPRESSIONS


evalCaseBranches :
    REnv
    -> Value
    -> List ( IR.RPattern, RExpr )
    -> EvalResult Value
evalCaseBranches env scrutinee branches =
    case branches of
        [] ->
            evErr env
                (TypeError
                    ("case expression failed to match any branch for value: "
                        ++ Value.toString scrutinee
                    )
                )

        ( pattern, branchBody ) :: rest ->
            case matchPattern pattern scrutinee env.locals of
                Just newLocals ->
                    evalR { env | locals = newLocals } branchBody

                Nothing ->
                    evalCaseBranches env scrutinee rest



-- HELPERS


{-| Evaluate a list of RExprs to a list of Values in order. Propagates
errors from any element; short-circuits on the first failure. Properly
threads `EvYield` / `EvMemoLookup` / `EvMemoStore` continuations so that
any intercept or memo hook encountered while evaluating a sub-expression
resumes at the right position in the list.
-}
evalExprList : REnv -> List RExpr -> EvalResult (List Value)
evalExprList env exprs =
    case exprs of
        [] ->
            EvOk []

        expr :: rest ->
            bindValueResult (evalR env expr)
                (\v ->
                    mapListResult (\vs -> v :: vs) (evalExprList env rest)
                )


{-| Evaluate a record's field list in source order, returning the list of
`(name, Value)` pairs. Callers fold this into a `FastDict` to produce the
final `Record` value. Threads yield/memo continuations through field
evaluation, matching `evalExprList`.
-}
evalRecordFields :
    REnv
    -> List ( String, RExpr )
    -> EvalResult (List ( String, Value ))
evalRecordFields env fields =
    case fields of
        [] ->
            EvOk []

        ( fieldName, fieldExpr ) :: rest ->
            bindValueResultToField (evalR env fieldExpr)
                (\v ->
                    mapFieldResult (\vs -> ( fieldName, v ) :: vs)
                        (evalRecordFields env rest)
                )


{-| Sequential let evaluation.

Each binding's RHS is evaluated against the current locals (which contain
all previously-evaluated sibling bindings but not yet the current one).
The result is pattern-matched against the binding's pattern; successful
matches prepend the pattern's bound values to the locals list in walk
order. Pattern match failures become `TypeError` — Elm's type checker
already rejects total-match failures, so if we reach one at runtime,
something upstream is inconsistent.

Function bindings (arity > 0) return `Unsupported` because they need
closure support that hasn't landed yet. The resolver only produces those
for top-level `let f x = ...` declarations, which run through the normal
string-keyed evaluator today anyway.

-}
evalLetBindings :
    REnv
    -> List IR.RLetBinding
    -> EvalResult (List Value)
evalLetBindings env bindings =
    evalLetBindingsHelp env env.locals bindings


evalLetBindingsHelp :
    REnv
    -> List Value
    -> List IR.RLetBinding
    -> EvalResult (List Value)
evalLetBindingsHelp env locals bindings =
    case bindings of
        [] ->
            EvOk locals

        binding :: rest ->
            let
                bodyEnv : REnv
                bodyEnv =
                    { env | locals = locals }
            in
            if binding.arity > 0 then
                case binding.body of
                    RLambda lambda ->
                        let
                            closureValue : Value
                            closureValue =
                                makeClosure
                                    bodyEnv
                                    lambda.arity
                                    lambda.body
                                    1
                                    Nothing
                        in
                        evalLetBindingsHelp env (closureValue :: locals) rest

                    _ ->
                        evErr env
                            (TypeError
                                ("let function binding '"
                                    ++ binding.debugName
                                    ++ "' body is not an RLambda"
                                )
                            )

            else
                bindValueResult (evalR bodyEnv binding.body)
                    (\value ->
                        case matchPattern binding.pattern value locals of
                            Just newLocals ->
                                evalLetBindingsHelp env newLocals rest

                            Nothing ->
                                evErr env
                                    (TypeError
                                        ("pattern match failed in let binding '"
                                            ++ binding.debugName
                                            ++ "'"
                                        )
                                    )
                    )


{-| Match a value against a pattern, extending the locals stack with the
pattern's bindings if the match succeeds. Returns `Nothing` if the match
fails (for let destructuring this is a runtime error; for case expressions
it means try the next branch).

Iteration 3b1 handles `RPVar`, `RPWildcard`, `RPUnit`, literal patterns,
2- and 3-tuples, and `RPAs`. Constructor, record, and list patterns
return `Nothing` for now (they land with `RCase` in iteration 3b2) —
Phase 3b2 expands this helper to cover every `RPattern` constructor.

-}
matchPattern : IR.RPattern -> Value -> List Value -> Maybe (List Value)
matchPattern pat value locals =
    case pat of
        IR.RPVar ->
            Just (value :: locals)

        IR.RPWildcard ->
            Just locals

        IR.RPUnit ->
            case value of
                Unit ->
                    Just locals

                _ ->
                    Nothing

        IR.RPInt i ->
            case value of
                Int j ->
                    if i == j then
                        Just locals

                    else
                        Nothing

                _ ->
                    Nothing

        IR.RPFloat f ->
            case value of
                Float g ->
                    if f == g then
                        Just locals

                    else
                        Nothing

                _ ->
                    Nothing

        IR.RPChar c ->
            case value of
                Char d ->
                    if c == d then
                        Just locals

                    else
                        Nothing

                _ ->
                    Nothing

        IR.RPString s ->
            case value of
                String t ->
                    if s == t then
                        Just locals

                    else
                        Nothing

                _ ->
                    Nothing

        IR.RPTuple2 a b ->
            case value of
                Tuple va vb ->
                    case matchPattern a va locals of
                        Just afterA ->
                            matchPattern b vb afterA

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        IR.RPTuple3 a b c ->
            case value of
                Triple va vb vc ->
                    case matchPattern a va locals of
                        Just afterA ->
                            case matchPattern b vb afterA of
                                Just afterB ->
                                    matchPattern c vc afterB

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        IR.RPAs inner ->
            -- Match the inner pattern first, then add the whole value as
            -- an additional slot. The slot order matches the resolver's
            -- walk: inner bindings first (deeper indices), then the `as`
            -- binding on top (slot 0).
            case matchPattern inner value locals of
                Just afterInner ->
                    Just (value :: afterInner)

                Nothing ->
                    Nothing

        IR.RPRecord fieldNames ->
            -- Record destructuring: each field name becomes one bound slot
            -- with the record's value for that field. fieldNames is stored
            -- sorted by the resolver, so the slot order here matches the
            -- alphabetical resolver layout.
            case value of
                Record fieldDict ->
                    bindRecordFields fieldNames fieldDict locals

                _ ->
                    Nothing

        IR.RPCons headPat tailPat ->
            case value of
                List (h :: t) ->
                    case matchPattern headPat h locals of
                        Just afterHead ->
                            matchPattern tailPat (List t) afterHead

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        IR.RPList elemPats ->
            case value of
                List items ->
                    if List.length items == List.length elemPats then
                        matchPatternsAgainstValues elemPats items locals

                    else
                        Nothing

                _ ->
                    Nothing

        IR.RPCtor ref argPats ->
            -- Constructor pattern. Name matching ignores moduleName to
            -- stay compatible with the existing evaluator (which also
            -- matches by name only — see the `match` function in
            -- Eval.Expression around line 4002 for the Bool special case).
            --
            -- Special-cases `True`/`False` because the evaluator
            -- represents Bool as its own Value variant, not as `Custom`.
            case ( ref.name, value ) of
                ( "True", Bool True ) ->
                    if List.isEmpty argPats then
                        Just locals

                    else
                        Nothing

                ( "False", Bool False ) ->
                    if List.isEmpty argPats then
                        Just locals

                    else
                        Nothing

                ( _, Custom valueRef valueArgs ) ->
                    if ref.name == valueRef.name then
                        matchPatternsAgainstValues argPats valueArgs locals

                    else
                        Nothing

                _ ->
                    Nothing


{-| Match a list of patterns against a list of values in lock-step. Returns
`Nothing` if the lists have different lengths or any pattern fails to match.
Used for tuple elements, list elements, and constructor arguments.
-}
matchPatternsAgainstValues :
    List IR.RPattern
    -> List Value
    -> List Value
    -> Maybe (List Value)
matchPatternsAgainstValues patterns values locals =
    case ( patterns, values ) of
        ( [], [] ) ->
            Just locals

        ( p :: ps, v :: vs ) ->
            case matchPattern p v locals of
                Just afterP ->
                    matchPatternsAgainstValues ps vs afterP

                Nothing ->
                    Nothing

        _ ->
            Nothing


{-| Walk a sorted list of record field names, looking up each field in the
record's dict and prepending the resolved value to the locals stack.
Returns `Nothing` if any field is missing from the record.
-}
bindRecordFields :
    List String
    -> FastDict.Dict String Value
    -> List Value
    -> Maybe (List Value)
bindRecordFields fieldNames fieldDict locals =
    case fieldNames of
        [] ->
            Just locals

        name :: rest ->
            case FastDict.get name fieldDict of
                Just fieldValue ->
                    bindRecordFields rest fieldDict (fieldValue :: locals)

                Nothing ->
                    Nothing


{-| Bind the result of an `EvalResult Value` to a continuation, correctly
propagating `EvYield` / `EvMemoLookup` / `EvMemoStore` by wrapping their
resume callbacks. Used across every caller that needs to sequence work
after evaluating a sub-expression.

`EvOkTrace` / `EvErrTrace` aren't actively used by the review runner
benchmark (trace is off), so they short-circuit to `EvErr` if
encountered — matches previous behavior.

-}
andThenValue :
    (Value -> EvalResult Value)
    -> EvalResult Value
    -> EvalResult Value
andThenValue f result =
    case result of
        EvOk v ->
            f v

        EvErr e ->
            EvErr e

        EvYield tag payload resume ->
            EvYield tag payload (\value -> andThenValue f (resume value))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> andThenValue f (resume maybeValue))

        EvMemoStore payload inner ->
            EvMemoStore payload (andThenValue f inner)

        EvOkTrace _ _ _ ->
            evErrBlank "andThenValue: EvOkTrace not supported in new evaluator"

        EvErrTrace e tree logs ->
            EvErrTrace e tree logs

        EvOkCoverage _ _ ->
            evErrBlank "andThenValue: EvOkCoverage not supported in new evaluator"

        EvErrCoverage _ _ ->
            evErrBlank "andThenValue: EvErrCoverage not supported in new evaluator"


andThenList :
    (List Value -> EvalResult Value)
    -> EvalResult (List Value)
    -> EvalResult Value
andThenList f result =
    case result of
        EvOk vs ->
            f vs

        EvErr e ->
            EvErr e

        EvYield tag payload resume ->
            EvYield tag payload (\value -> andThenList f (resume value))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> andThenList f (resume maybeValue))

        EvMemoStore payload inner ->
            EvMemoStore payload (andThenList f inner)

        EvOkTrace _ _ _ ->
            evErrBlank "andThenList: EvOkTrace not supported in new evaluator"

        EvErrTrace e tree logs ->
            EvErrTrace e tree logs

        EvOkCoverage _ _ ->
            evErrBlank "andThenList: EvOkCoverage not supported in new evaluator"

        EvErrCoverage _ _ ->
            evErrBlank "andThenList: EvErrCoverage not supported in new evaluator"


{-| Bind an `EvalResult Value` to a continuation that itself returns an
`EvalResult (List Value)`. Separate from `andThenValue` because the
return type differs. Propagates yield/memo.
-}
bindValueResult :
    EvalResult Value
    -> (Value -> EvalResult (List Value))
    -> EvalResult (List Value)
bindValueResult result f =
    case result of
        EvOk v ->
            f v

        EvErr e ->
            EvErr e

        EvYield tag payload resume ->
            EvYield tag payload (\value -> bindValueResult (resume value) f)

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> bindValueResult (resume maybeValue) f)

        EvMemoStore payload inner ->
            EvMemoStore payload (bindValueResult inner f)

        EvOkTrace _ _ _ ->
            evErrBlank "bindValueResult: EvOkTrace not supported in new evaluator"

        EvErrTrace e tree logs ->
            EvErrTrace e tree logs

        EvOkCoverage _ _ ->
            evErrBlank "bindValueResult: EvOkCoverage not supported in new evaluator"

        EvErrCoverage _ _ ->
            evErrBlank "bindValueResult: EvErrCoverage not supported in new evaluator"


bindValueResultToField :
    EvalResult Value
    -> (Value -> EvalResult (List ( String, Value )))
    -> EvalResult (List ( String, Value ))
bindValueResultToField result f =
    case result of
        EvOk v ->
            f v

        EvErr e ->
            EvErr e

        EvYield tag payload resume ->
            EvYield tag payload (\value -> bindValueResultToField (resume value) f)

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> bindValueResultToField (resume maybeValue) f)

        EvMemoStore payload inner ->
            EvMemoStore payload (bindValueResultToField inner f)

        EvOkTrace _ _ _ ->
            evErrBlank "bindValueResultToField: EvOkTrace not supported in new evaluator"

        EvErrTrace e tree logs ->
            EvErrTrace e tree logs

        EvOkCoverage _ _ ->
            evErrBlank "bindValueResultToField: EvOkCoverage not supported in new evaluator"

        EvErrCoverage _ _ ->
            evErrBlank "bindValueResultToField: EvErrCoverage not supported in new evaluator"


mapListResult :
    (List Value -> List Value)
    -> EvalResult (List Value)
    -> EvalResult (List Value)
mapListResult f result =
    case result of
        EvOk vs ->
            EvOk (f vs)

        EvErr e ->
            EvErr e

        EvYield tag payload resume ->
            EvYield tag payload (\value -> mapListResult f (resume value))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> mapListResult f (resume maybeValue))

        EvMemoStore payload inner ->
            EvMemoStore payload (mapListResult f inner)

        EvOkTrace _ _ _ ->
            evErrBlank "mapListResult: EvOkTrace not supported in new evaluator"

        EvErrTrace e tree logs ->
            EvErrTrace e tree logs

        EvOkCoverage _ _ ->
            evErrBlank "mapListResult: EvOkCoverage not supported in new evaluator"

        EvErrCoverage _ _ ->
            evErrBlank "mapListResult: EvErrCoverage not supported in new evaluator"


mapFieldResult :
    (List ( String, Value ) -> List ( String, Value ))
    -> EvalResult (List ( String, Value ))
    -> EvalResult (List ( String, Value ))
mapFieldResult f result =
    case result of
        EvOk vs ->
            EvOk (f vs)

        EvErr e ->
            EvErr e

        EvYield tag payload resume ->
            EvYield tag payload (\value -> mapFieldResult f (resume value))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> mapFieldResult f (resume maybeValue))

        EvMemoStore payload inner ->
            EvMemoStore payload (mapFieldResult f inner)

        EvOkTrace _ _ _ ->
            evErrBlank "mapFieldResult: EvOkTrace not supported in new evaluator"

        EvErrTrace e tree logs ->
            EvErrTrace e tree logs

        EvOkCoverage _ _ ->
            evErrBlank "mapFieldResult: EvOkCoverage not supported in new evaluator"

        EvErrCoverage _ _ ->
            evErrBlank "mapFieldResult: EvErrCoverage not supported in new evaluator"


mapResult : (a -> Value) -> EvalResult a -> EvalResult Value
mapResult f result =
    case result of
        EvOk v ->
            EvOk (f v)

        EvErr e ->
            EvErr e

        EvYield tag payload resume ->
            EvYield tag payload (\value -> mapResult f (resume value))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> mapResult f (resume maybeValue))

        EvMemoStore payload inner ->
            EvMemoStore payload (mapResult f inner)

        EvOkTrace v tree logs ->
            EvOkTrace (f v) tree logs

        EvErrTrace e tree logs ->
            EvErrTrace e tree logs

        EvOkCoverage v s ->
            EvOkCoverage (f v) s

        EvErrCoverage e s ->
            EvErrCoverage e s


evErrBlank : String -> EvalResult a
evErrBlank msg =
    EvErr
        { currentModule = []
        , callStack = []
        , error = Unsupported msg
        }


evErr : REnv -> EvalErrorKind -> EvalResult a
evErr env kind =
    EvErr
        { currentModule = env.currentModule
        , callStack = env.callStack
        , error = kind
        }



-- HIGHER-ORDER KERNEL DISPATCHERS
--
-- Parallel to `Eval.NativeDispatch` (scalar operators), but for
-- core functions that take a callback Value as one of their args.
-- Each dispatcher here invokes user callbacks via `applyClosure` so
-- resolved closures (`RExprImpl` with `patterns = []`) are dispatched
-- natively rather than round-tripping through the old evaluator's
-- `Kernel.function` marshaling layer, which was built for named
-- patterns and gets arg binding wrong for RExprImpl.


{-| Try to dispatch a core call as a higher-order kernel. Mirrors
`NativeDispatch.tryDispatch` but takes the REnv (needed to invoke
callbacks via `applyClosure`).
-}
tryHigherOrderDispatch : REnv -> IR.GlobalId -> List Value -> Maybe (EvalResult Value)
tryHigherOrderDispatch env id argValues =
    case FastDict.get id env.higherOrderDispatchers of
        Just (HigherOrderDispatcher dispatcher) ->
            dispatcher env argValues

        Nothing ->
            Nothing


{-| Build the registry at `buildProjectEnv` time. Takes the same
`( ModuleName, String ) -> Maybe GlobalId` lookup function as
`NativeDispatch.buildRegistry`, returns a Dict keyed by resolved
`GlobalId`s.
-}
buildHigherOrderRegistry :
    (( ModuleName, String ) -> Maybe IR.GlobalId)
    -> FastDict.Dict IR.GlobalId HigherOrderDispatcher
buildHigherOrderRegistry lookupId =
    higherOrderDispatcherList
        |> List.filterMap
            (\( moduleName, name, dispatcher ) ->
                lookupId ( moduleName, name )
                    |> Maybe.map (\id -> ( id, dispatcher ))
            )
        |> FastDict.fromList


higherOrderDispatcherList : List ( ModuleName, String, HigherOrderDispatcher )
higherOrderDispatcherList =
    [ ( [ "List" ], "foldl", HigherOrderDispatcher foldlDispatcher )
    , ( [ "List" ], "foldr", HigherOrderDispatcher foldrDispatcher )
    , ( [ "List" ], "map", HigherOrderDispatcher listMapDispatcher )
    , ( [ "List" ], "filter", HigherOrderDispatcher listFilterDispatcher )
    , ( [ "List" ], "filterMap", HigherOrderDispatcher listFilterMapDispatcher )
    , ( [ "List" ], "concatMap", HigherOrderDispatcher listConcatMapDispatcher )
    , ( [ "List" ], "any", HigherOrderDispatcher listAnyDispatcher )
    , ( [ "List" ], "all", HigherOrderDispatcher listAllDispatcher )
    , ( [ "Dict" ], "foldl", HigherOrderDispatcher dictFoldlDispatcher )
    , ( [ "Dict" ], "foldr", HigherOrderDispatcher dictFoldrDispatcher )
    , ( [ "Dict" ], "map", HigherOrderDispatcher dictMapDispatcher )
    ]



-- INDIVIDUAL DISPATCHERS


{-| `List.foldl : (a -> b -> b) -> b -> List a -> b`

The resolved callback is invoked via `applyClosure env f [ x, acc ]`.
`andThenValue` threads `EvYield` / `EvMemoLookup` / `EvMemoStore`
continuations automatically — the loop only stays hot on the `EvOk`
path, which Elm's compiler emits as a host-Elm `while` loop due to
tail-recursion on `foldlHelper`.
-}
foldlDispatcher : REnv -> List Value -> Maybe (EvalResult Value)
foldlDispatcher env args =
    case args of
        [ f, init, List xs ] ->
            Just (foldlHelper env f init xs)

        _ ->
            Nothing


foldlHelper : REnv -> Value -> Value -> List Value -> EvalResult Value
foldlHelper env f acc remaining =
    case remaining of
        [] ->
            EvOk acc

        x :: rest ->
            case applyClosure env f [ x, acc ] of
                EvOk newAcc ->
                    -- Hot path: synchronous, tail-recursive in Elm → JS while loop.
                    foldlHelper env f newAcc rest

                EvErr e ->
                    EvErr e

                otherResult ->
                    -- Yield / memo / trace: thread the continuation.
                    andThenValue
                        (\newAcc -> foldlHelper env f newAcc rest)
                        otherResult


{-| `Dict.foldl : (k -> v -> b -> b) -> b -> Dict k v -> b`

Dicts in the interpreter are represented as `Custom` red-black tree
nodes — `RBNode_elm_builtin color key value left right` and
`RBEmpty_elm_builtin`. We walk the tree in-order (left, node, right)
and invoke the callback via `applyClosure env f [ k, v, acc ]` for
each node. Same callback dispatch path as `foldlDispatcher`, which is
specifically what the old kernel's `Kernel.function3` marshaling
layer mis-handles for `RExprImpl` closures.
-}
dictFoldlDispatcher : REnv -> List Value -> Maybe (EvalResult Value)
dictFoldlDispatcher env args =
    case args of
        [ f, init, dict ] ->
            Just (dictFoldlHelper env f init dict)

        _ ->
            Nothing


dictFoldlHelper : REnv -> Value -> Value -> Value -> EvalResult Value
dictFoldlHelper env f acc node =
    case node of
        Custom { name } nodeArgs ->
            if name == "RBEmpty_elm_builtin" then
                EvOk acc

            else
                case nodeArgs of
                    [ _, key, value, left, right ] ->
                        case dictFoldlHelper env f acc left of
                            EvOk leftAcc ->
                                case applyClosure env f [ key, value, leftAcc ] of
                                    EvOk midAcc ->
                                        dictFoldlHelper env f midAcc right

                                    EvErr e ->
                                        EvErr e

                                    otherResult ->
                                        andThenValue
                                            (\midAcc -> dictFoldlHelper env f midAcc right)
                                            otherResult

                            EvErr e ->
                                EvErr e

                            otherResult ->
                                andThenValue
                                    (\leftAcc ->
                                        case applyClosure env f [ key, value, leftAcc ] of
                                            EvOk midAcc ->
                                                dictFoldlHelper env f midAcc right

                                            other ->
                                                andThenValue
                                                    (\midAcc -> dictFoldlHelper env f midAcc right)
                                                    other
                                    )
                                    otherResult

                    _ ->
                        EvOk acc

        _ ->
            EvOk acc


{-| `List.foldr : (a -> b -> b) -> b -> List a -> b`
Reverse the list, then foldl.
-}
foldrDispatcher : REnv -> List Value -> Maybe (EvalResult Value)
foldrDispatcher env args =
    case args of
        [ f, init, List xs ] ->
            Just (foldlHelper env f init (List.reverse xs))

        _ ->
            Nothing


{-| `List.map : (a -> b) -> List a -> List b` -}
listMapDispatcher : REnv -> List Value -> Maybe (EvalResult Value)
listMapDispatcher env args =
    case args of
        [ f, List xs ] ->
            Just (listMapHelper env f xs [])

        _ ->
            Nothing


listMapHelper : REnv -> Value -> List Value -> List Value -> EvalResult Value
listMapHelper env f remaining accRev =
    case remaining of
        [] ->
            EvOk (List (List.reverse accRev))

        x :: rest ->
            case applyClosure env f [ x ] of
                EvOk mapped ->
                    listMapHelper env f rest (mapped :: accRev)

                EvErr e ->
                    EvErr e

                otherResult ->
                    andThenValue
                        (\mapped -> listMapHelper env f rest (mapped :: accRev))
                        otherResult


{-| `List.filter : (a -> Bool) -> List a -> List a` -}
listFilterDispatcher : REnv -> List Value -> Maybe (EvalResult Value)
listFilterDispatcher env args =
    case args of
        [ pred, List xs ] ->
            Just (listFilterHelper env pred xs [])

        _ ->
            Nothing


listFilterHelper : REnv -> Value -> List Value -> List Value -> EvalResult Value
listFilterHelper env pred remaining accRev =
    case remaining of
        [] ->
            EvOk (List (List.reverse accRev))

        x :: rest ->
            case applyClosure env pred [ x ] of
                EvOk (Bool True) ->
                    listFilterHelper env pred rest (x :: accRev)

                EvOk (Bool False) ->
                    listFilterHelper env pred rest accRev

                EvOk _ ->
                    listFilterHelper env pred rest accRev

                EvErr e ->
                    EvErr e

                otherResult ->
                    andThenValue
                        (\result ->
                            case result of
                                Bool True ->
                                    listFilterHelper env pred rest (x :: accRev)

                                _ ->
                                    listFilterHelper env pred rest accRev
                        )
                        otherResult


{-| `List.filterMap : (a -> Maybe b) -> List a -> List b` -}
listFilterMapDispatcher : REnv -> List Value -> Maybe (EvalResult Value)
listFilterMapDispatcher env args =
    case args of
        [ f, List xs ] ->
            Just (listFilterMapHelper env f xs [])

        _ ->
            Nothing


listFilterMapHelper : REnv -> Value -> List Value -> List Value -> EvalResult Value
listFilterMapHelper env f remaining accRev =
    case remaining of
        [] ->
            EvOk (List (List.reverse accRev))

        x :: rest ->
            case applyClosure env f [ x ] of
                EvOk (Custom { name } [ value ]) ->
                    if name == "Just" then
                        listFilterMapHelper env f rest (value :: accRev)

                    else
                        listFilterMapHelper env f rest accRev

                EvOk _ ->
                    listFilterMapHelper env f rest accRev

                EvErr e ->
                    EvErr e

                otherResult ->
                    andThenValue
                        (\result ->
                            case result of
                                Custom { name } [ value ] ->
                                    if name == "Just" then
                                        listFilterMapHelper env f rest (value :: accRev)

                                    else
                                        listFilterMapHelper env f rest accRev

                                _ ->
                                    listFilterMapHelper env f rest accRev
                        )
                        otherResult


{-| `List.concatMap : (a -> List b) -> List a -> List b` -}
listConcatMapDispatcher : REnv -> List Value -> Maybe (EvalResult Value)
listConcatMapDispatcher env args =
    case args of
        [ f, List xs ] ->
            Just (listConcatMapHelper env f xs [])

        _ ->
            Nothing


listConcatMapHelper : REnv -> Value -> List Value -> List Value -> EvalResult Value
listConcatMapHelper env f remaining accRev =
    case remaining of
        [] ->
            EvOk (List (List.reverse accRev))

        x :: rest ->
            case applyClosure env f [ x ] of
                EvOk (List mapped) ->
                    listConcatMapHelper env f rest (List.foldl (::) accRev mapped)

                EvOk _ ->
                    listConcatMapHelper env f rest accRev

                EvErr e ->
                    EvErr e

                otherResult ->
                    andThenValue
                        (\result ->
                            case result of
                                List mapped ->
                                    listConcatMapHelper env f rest (List.foldl (::) accRev mapped)

                                _ ->
                                    listConcatMapHelper env f rest accRev
                        )
                        otherResult


{-| `List.any : (a -> Bool) -> List a -> Bool` -}
listAnyDispatcher : REnv -> List Value -> Maybe (EvalResult Value)
listAnyDispatcher env args =
    case args of
        [ pred, List xs ] ->
            Just (listAnyHelper env pred xs)

        _ ->
            Nothing


listAnyHelper : REnv -> Value -> List Value -> EvalResult Value
listAnyHelper env pred remaining =
    case remaining of
        [] ->
            EvOk (Bool False)

        x :: rest ->
            case applyClosure env pred [ x ] of
                EvOk (Bool True) ->
                    EvOk (Bool True)

                EvOk _ ->
                    listAnyHelper env pred rest

                EvErr e ->
                    EvErr e

                otherResult ->
                    andThenValue
                        (\result ->
                            case result of
                                Bool True ->
                                    EvOk (Bool True)

                                _ ->
                                    listAnyHelper env pred rest
                        )
                        otherResult


{-| `List.all : (a -> Bool) -> List a -> Bool` -}
listAllDispatcher : REnv -> List Value -> Maybe (EvalResult Value)
listAllDispatcher env args =
    case args of
        [ pred, List xs ] ->
            Just (listAllHelper env pred xs)

        _ ->
            Nothing


listAllHelper : REnv -> Value -> List Value -> EvalResult Value
listAllHelper env pred remaining =
    case remaining of
        [] ->
            EvOk (Bool True)

        x :: rest ->
            case applyClosure env pred [ x ] of
                EvOk (Bool False) ->
                    EvOk (Bool False)

                EvOk _ ->
                    listAllHelper env pred rest

                EvErr e ->
                    EvErr e

                otherResult ->
                    andThenValue
                        (\result ->
                            case result of
                                Bool False ->
                                    EvOk (Bool False)

                                _ ->
                                    listAllHelper env pred rest
                        )
                        otherResult


{-| `Dict.foldr : (k -> v -> b -> b) -> b -> Dict k v -> b`
Same as Dict.foldl but traverses right subtree first.
-}
dictFoldrDispatcher : REnv -> List Value -> Maybe (EvalResult Value)
dictFoldrDispatcher env args =
    case args of
        [ f, init, dict ] ->
            Just (dictFoldrHelper env f init dict)

        _ ->
            Nothing


dictFoldrHelper : REnv -> Value -> Value -> Value -> EvalResult Value
dictFoldrHelper env f acc node =
    case node of
        Custom { name } nodeArgs ->
            if name == "RBEmpty_elm_builtin" then
                EvOk acc

            else
                case nodeArgs of
                    [ _, key, value, left, right ] ->
                        case dictFoldrHelper env f acc right of
                            EvOk rightAcc ->
                                case applyClosure env f [ key, value, rightAcc ] of
                                    EvOk midAcc ->
                                        dictFoldrHelper env f midAcc left

                                    EvErr e ->
                                        EvErr e

                                    otherResult ->
                                        andThenValue
                                            (\midAcc -> dictFoldrHelper env f midAcc left)
                                            otherResult

                            EvErr e ->
                                EvErr e

                            otherResult ->
                                andThenValue
                                    (\rightAcc ->
                                        case applyClosure env f [ key, value, rightAcc ] of
                                            EvOk midAcc ->
                                                dictFoldrHelper env f midAcc left

                                            other ->
                                                andThenValue
                                                    (\midAcc -> dictFoldrHelper env f midAcc left)
                                                    other
                                    )
                                    otherResult

                    _ ->
                        EvOk acc

        _ ->
            EvOk acc


{-| `Dict.map : (k -> v -> v2) -> Dict k v -> Dict k v2`
Walks the tree, applies callback to each (key, value), rebuilds with mapped values.
-}
dictMapDispatcher : REnv -> List Value -> Maybe (EvalResult Value)
dictMapDispatcher env args =
    case args of
        [ f, dict ] ->
            Just (dictMapHelper env f dict)

        _ ->
            Nothing


dictMapHelper : REnv -> Value -> Value -> EvalResult Value
dictMapHelper env f node =
    case node of
        Custom ref nodeArgs ->
            if ref.name == "RBEmpty_elm_builtin" then
                EvOk node

            else
                case nodeArgs of
                    [ color, key, value, left, right ] ->
                        case dictMapHelper env f left of
                            EvOk mappedLeft ->
                                case dictMapHelper env f right of
                                    EvOk mappedRight ->
                                        case applyClosure env f [ key, value ] of
                                            EvOk mappedValue ->
                                                EvOk (Custom ref [ color, key, mappedValue, mappedLeft, mappedRight ])

                                            EvErr e ->
                                                EvErr e

                                            otherResult ->
                                                andThenValue
                                                    (\mappedValue -> EvOk (Custom ref [ color, key, mappedValue, mappedLeft, mappedRight ]))
                                                    otherResult

                                    other ->
                                        other

                            other ->
                                other

                    _ ->
                        EvOk node

        _ ->
            EvOk node


