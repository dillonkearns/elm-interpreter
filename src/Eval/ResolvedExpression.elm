module Eval.ResolvedExpression exposing
    ( REnv
    , emptyREnv
    , evalR
    )

{-| Resolved-IR evaluator (Phase 3). Consumes `Eval.ResolvedIR.RExpr`
produced by `Eval.Resolver` and the `buildProjectEnvFromParsed` pipeline.

**Iteration 3a — the skeleton.** This iteration supports only pure literal
expressions: `RInt`, `RFloat`, `RString`, `RChar`, `RUnit`, `RList` of
anything already supported, `RTuple2`/`RTuple3` of anything already
supported, and `RNegate` of numeric literals. Every other constructor
returns `UnsupportedExpressionKind`, which Phase 3 iteration 3b will fill
in one construct at a time.

The goal of 3a is to prove:

1.  The new module compiles with the right shape and types.
2.  `REnv` is a clean data structure — a flat cons-list for locals, an
    int-keyed dict for globals, plus the minimum context the eval path
    needs for error reporting.
3.  `evalR` can produce a `Value` from an `RExpr` without touching
    `Env`, `shared.functions`, or any of the string-keyed machinery
    that the existing evaluator is built on top of.

Once 3a lands, iteration 3b expands `evalR` to cover every `RExpr`
constructor (variables, applications, lambdas, control flow, records,
case expressions) and wires a `Config.useResolvedIR` flag to gate entry
points in `Eval.Module`.

-}

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Eval.ResolvedIR as IR exposing (RExpr(..))
import FastDict
import Rope
import Types
    exposing
        ( EvalErrorData
        , EvalErrorKind(..)
        , EvalResult(..)
        , Value(..)
        )


{-| Evaluator context for the resolved IR.

`locals` is a cons-list of already-bound values; `RLocal i` evaluates to
`List.drop i env.locals |> List.head`. The head is the innermost binding.

`globals` holds pre-computed top-level values. Phase 3 iteration 3b will
populate this lazily (evaluate a top-level's RExpr body the first time
its `RGlobal` is encountered, cache the result). Iteration 3a doesn't
touch it.

`resolvedBodies` is the map from `GlobalId` to the resolved `RExpr` body
the resolver produced at `buildProjectEnv` time. The evaluator reaches
into this when an `RGlobal id` reference needs to evaluate an unresolved
top-level.

`currentModule` and `callStack` are bookkeeping for error messages and
eventual tracing integration, matching the fields on the existing
`Env`. Iteration 3a doesn't walk into calls, so they're mostly inert.

-}
type alias REnv =
    { locals : List Value
    , globals : FastDict.Dict IR.GlobalId Value
    , resolvedBodies : FastDict.Dict IR.GlobalId RExpr
    , currentModule : ModuleName
    , callStack : List QualifiedNameRef
    }


{-| A completely empty REnv useful for literal-only tests. Real callers
will build one from a `ProjectEnv` in Phase 3 iteration 3b.
-}
emptyREnv : REnv
emptyREnv =
    { locals = []
    , globals = FastDict.empty
    , resolvedBodies = FastDict.empty
    , currentModule = []
    , callStack = []
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

        -- Everything below is "not implemented in iteration 3a" — 3b
        -- replaces each of these with real handling.
        RLocal _ ->
            evErr env (Unsupported "RLocal (iteration 3b)")

        RGlobal _ ->
            evErr env (Unsupported "RGlobal (iteration 3b)")

        RCtor _ ->
            evErr env (Unsupported "RCtor (iteration 3b)")

        RRecordAccessFunction _ ->
            evErr env (Unsupported "RRecordAccessFunction (iteration 3b)")

        RIf _ _ _ ->
            evErr env (Unsupported "RIf (iteration 3b)")

        RAnd _ _ ->
            evErr env (Unsupported "RAnd (iteration 3b)")

        ROr _ _ ->
            evErr env (Unsupported "ROr (iteration 3b)")

        RCase _ _ ->
            evErr env (Unsupported "RCase (iteration 3b)")

        RLambda _ ->
            evErr env (Unsupported "RLambda (iteration 3b)")

        RLet _ _ ->
            evErr env (Unsupported "RLet (iteration 3b)")

        RApply _ _ ->
            evErr env (Unsupported "RApply (iteration 3b)")

        RRecord _ ->
            evErr env (Unsupported "RRecord (iteration 3b)")

        RRecordAccess _ _ ->
            evErr env (Unsupported "RRecordAccess (iteration 3b)")

        RRecordUpdate _ _ ->
            evErr env (Unsupported "RRecordUpdate (iteration 3b)")

        RGLSL _ ->
            evErr env (Unsupported "RGLSL (not supported)")



-- HELPERS


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
