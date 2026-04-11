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
import Eval.NativeDispatch as NativeDispatch
import Eval.ResolvedIR as IR exposing (RExpr(..))
import FastDict
import MemoSpec
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
    , nativeDispatchers = FastDict.empty
    , kernelDispatchers = FastDict.empty
    , interceptsByGlobal = FastDict.empty
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
                            delegateCoreApply env id argValues


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
            let
                {- Inject each argument Value into the dispatched env's
                   `values` under a unique synthetic name, and reference
                   those names in the synthesized AST instead of round-
                   tripping each Value through `Value.toExpression`.

                   This avoids the `<resolved-closure>` placeholder that
                   `Value.toExpression` emits for `RExprImpl` closures,
                   which previously crashed the old evaluator with a
                   `NameError` inside `List.foldl` / `List.map` calls
                   whose step function was built by the new evaluator.

                   Literal args (Int, String, etc.) go through injection
                   too — it's cheaper than a `Value.toExpression` round
                   trip and produces smaller synthesized ASTs.
                -}
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
