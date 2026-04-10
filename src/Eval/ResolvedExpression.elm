module Eval.ResolvedExpression exposing
    ( REnv
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
import Eval.ResolvedIR as IR exposing (RExpr(..))
import FastDict
import MemoSpec
import Rope
import Syntax
import Types
    exposing
        ( Config
        , Env
        , EvalErrorData
        , EvalErrorKind(..)
        , EvalResult(..)
        , Implementation(..)
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
    , fallbackEnv : Env
    , fallbackConfig : Config
    , currentModule : ModuleName
    , callStack : List QualifiedNameRef
    }


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
    , fallbackEnv = Environment.empty []
    , fallbackConfig = emptyConfig
    , currentModule = []
    , callStack = []
    }


emptyConfig : Config
emptyConfig =
    { trace = False
    , maxSteps = Nothing
    , tcoTarget = Nothing
    , callCounts = Nothing
    , intercepts = FastDict.empty
    , memoizedFunctions = MemoSpec.emptyRegistry
    , collectMemoStats = False
    , useResolvedIR = False
    }


{-| Evaluate a resolved expression to a `Value`.

Iteration 3a handles only the literal-tree subset; every other
constructor returns an `Unsupported` error that iteration 3b will
progressively replace with real handling.

-}
evalR : REnv -> RExpr -> EvalResult Value
evalR env expr =
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
                    evErr env (TypeError ("negate applied to non-numeric " ++ Debug.toString other))

                other ->
                    other

        RList items ->
            evalList env items
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
                                            ++ Debug.toString other
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
                                                                ++ Debug.toString other
                                                            )
                                                        )
                                        )

                            other ->
                                evErr env
                                    (TypeError
                                        ("&& left not Bool: " ++ Debug.toString other)
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
                                                                ++ Debug.toString other
                                                            )
                                                        )
                                        )

                            other ->
                                evErr env
                                    (TypeError
                                        ("|| left not Bool: " ++ Debug.toString other)
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
                                            ++ Debug.toString other
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
                                ++ Debug.toString other
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
            -- Fast path for core-backed globals: evaluate the args in the
            -- new evaluator, then delegate the whole call to the old
            -- evaluator via a synthesized `Application` AST. This
            -- sidesteps the problem of representing old-style closures
            -- in the new evaluator's apply loop.
            case headExpr of
                RGlobal id ->
                    case FastDict.get id env.globals of
                        Just cached ->
                            evalExprList env argExprs
                                |> andThenList
                                    (\argValues ->
                                        applyClosure env cached argValues
                                    )

                        Nothing ->
                            case FastDict.get id env.resolvedBodies of
                                Just _ ->
                                    evalR env headExpr
                                        |> andThenValue
                                            (\headValue ->
                                                evalExprList env argExprs
                                                    |> andThenList
                                                        (\argValues ->
                                                            applyClosure env headValue argValues
                                                        )
                                            )

                                Nothing ->
                                    -- Core dispatch: delegate the whole call.
                                    evalExprList env argExprs
                                        |> andThenList
                                            (\argValues ->
                                                delegateCoreApply env id argValues
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
        (Environment.empty [])
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

                    AstImpl _ ->
                        evErr env
                            (Unsupported
                                "applying new evaluator to old AST-backed closure (Phase 3 wire-up will bridge this)"
                            )

                    KernelImpl _ _ _ ->
                        evErr env
                            (Unsupported
                                "applying new evaluator to kernel-backed closure (Phase 3 wire-up will bridge this)"
                            )

            Custom qualRef existingArgs ->
                -- Constructor application: treat as arg accumulation.
                -- Elm's type checker ensures arity is correct upstream, so
                -- we don't validate here.
                EvOk (Custom qualRef (existingArgs ++ newArgs))

            other ->
                evErr env
                    (TypeError
                        ("apply on non-callable: " ++ Debug.toString other)
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
            { env | locals = bodyLocals }
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
    case FastDict.get id env.globalIdToName of
        Nothing ->
            evErr env
                (Unsupported
                    ("RGlobal " ++ String.fromInt id ++ " has no name metadata")
                )

        Just ( moduleName, name ) ->
            let
                headExpr : Node Expression.Expression
                headExpr =
                    Syntax.fakeNode (Expression.FunctionOrValue moduleName name)

                fullExpr : Node Expression.Expression
                fullExpr =
                    if List.isEmpty args then
                        headExpr

                    else
                        Syntax.fakeNode
                            (Expression.Application
                                (headExpr :: List.map Value.toExpression args)
                            )

                baseEnv : Env
                baseEnv =
                    env.fallbackEnv

                dispatchEnv : Env
                dispatchEnv =
                    { baseEnv
                        | currentModule = moduleName
                        , currentModuleKey = Environment.moduleKey moduleName
                        , currentModuleFunctions =
                            FastDict.get (Environment.moduleKey moduleName)
                                baseEnv.shared.functions
                                |> Maybe.withDefault FastDict.empty
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
                        ++ Debug.toString scrutinee
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
errors from any element; short-circuits on the first failure.
-}
evalExprList : REnv -> List RExpr -> EvalResult (List Value)
evalExprList env exprs =
    List.foldr
        (\expr acc ->
            case acc of
                EvOk vs ->
                    case evalR env expr of
                        EvOk v ->
                            EvOk (v :: vs)

                        EvErr e ->
                            EvErr e

                        other ->
                            unsupportedResult "evalExprList" other

                EvErr _ ->
                    acc

                _ ->
                    acc
        )
        (EvOk [])
        exprs


{-| Evaluate a record's field list in source order, returning the list of
`(name, Value)` pairs. Callers fold this into a `FastDict` to produce the
final `Record` value.
-}
evalRecordFields :
    REnv
    -> List ( String, RExpr )
    -> EvalResult (List ( String, Value ))
evalRecordFields env fields =
    List.foldr
        (\( fieldName, fieldExpr ) acc ->
            case acc of
                EvOk rest ->
                    case evalR env fieldExpr of
                        EvOk v ->
                            EvOk (( fieldName, v ) :: rest)

                        EvErr e ->
                            EvErr e

                        other ->
                            unsupportedResult "record field" other

                EvErr _ ->
                    acc

                _ ->
                    acc
        )
        (EvOk [])
        fields


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
    List.foldl
        (\binding accResult ->
            case accResult of
                EvOk locals ->
                    let
                        bodyEnv : REnv
                        bodyEnv =
                            { env | locals = locals }
                    in
                    if binding.arity > 0 then
                        -- Function binding: the body is an `RLambda` whose
                        -- body was resolved expecting `self` in the locals
                        -- stack (selfRecExtendedLocals in the resolver).
                        -- Build the closure with selfSlots = 1 so the
                        -- evaluator prepends the closure itself to
                        -- captured locals at call time, matching the
                        -- resolver's layout.
                        --
                        -- We look *inside* the RLambda rather than calling
                        -- evalR on it because evalR would produce a
                        -- regular closure with selfSlots = 0.
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
                                EvOk (closureValue :: locals)

                            _ ->
                                evErr env
                                    (TypeError
                                        ("let function binding '"
                                            ++ binding.debugName
                                            ++ "' body is not an RLambda"
                                        )
                                    )

                    else
                        case evalR bodyEnv binding.body of
                            EvOk value ->
                                case matchPattern binding.pattern value locals of
                                    Just newLocals ->
                                        EvOk newLocals

                                    Nothing ->
                                        evErr env
                                            (TypeError
                                                ("pattern match failed in let binding '"
                                                    ++ binding.debugName
                                                    ++ "'"
                                                )
                                            )

                            EvErr e ->
                                EvErr e

                            other ->
                                unsupportedResult "let binding" other

                EvErr _ ->
                    accResult

                _ ->
                    accResult
        )
        (EvOk env.locals)
        bindings


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

        _ ->
            unsupportedResult "andThenValue" result


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

        _ ->
            -- Same limitation as andThenValue: trace/yield/memo not
            -- propagated in iteration 3b1.
            EvErr
                { currentModule = []
                , callStack = []
                , error = Unsupported "andThenList non-Ok/non-Err result"
                }


unsupportedResult : String -> EvalResult a -> EvalResult b
unsupportedResult context _ =
    EvErr
        { currentModule = []
        , callStack = []
        , error =
            Unsupported
                (context ++ ": non-Ok/non-Err EvalResult not supported in iteration 3b1")
        }


evalList : REnv -> List RExpr -> EvalResult (List Value)
evalList env exprs =
    case exprs of
        [] ->
            EvOk []

        head :: rest ->
            case evalR env head of
                EvOk v ->
                    case evalList env rest of
                        EvOk vs ->
                            EvOk (v :: vs)

                        err ->
                            err

                EvErr e ->
                    EvErr e

                EvOkTrace v tree logs ->
                    case evalList env rest of
                        EvOk vs ->
                            EvOkTrace (v :: vs) tree logs

                        EvOkTrace vs tree2 logs2 ->
                            EvOkTrace (v :: vs) (Rope.appendTo tree tree2) (Rope.appendTo logs logs2)

                        other ->
                            other

                EvErrTrace e tree logs ->
                    EvErrTrace e tree logs

                EvYield _ _ _ ->
                    -- Yield/memo support is a later-iteration concern.
                    evErr env (Unsupported "EvYield in RList (iteration 3b)")

                EvMemoLookup _ _ ->
                    evErr env (Unsupported "EvMemoLookup in RList (iteration 3b)")

                EvMemoStore _ _ ->
                    evErr env (Unsupported "EvMemoStore in RList (iteration 3b)")


mapResult : (a -> b) -> EvalResult a -> EvalResult b
mapResult f result =
    case result of
        EvOk v ->
            EvOk (f v)

        EvErr e ->
            EvErr e

        EvOkTrace v tree logs ->
            EvOkTrace (f v) tree logs

        EvErrTrace e tree logs ->
            EvErrTrace e tree logs

        EvYield _ _ _ ->
            EvErr
                { currentModule = []
                , callStack = []
                , error = Unsupported "EvYield in mapResult"
                }

        EvMemoLookup _ _ ->
            EvErr
                { currentModule = []
                , callStack = []
                , error = Unsupported "EvMemoLookup in mapResult"
                }

        EvMemoStore _ _ ->
            EvErr
                { currentModule = []
                , callStack = []
                , error = Unsupported "EvMemoStore in mapResult"
                }


evErr : REnv -> EvalErrorKind -> EvalResult a
evErr env kind =
    EvErr
        { currentModule = env.currentModule
        , callStack = env.callStack
        , error = kind
        }
