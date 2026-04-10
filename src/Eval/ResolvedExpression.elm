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

        -- Everything below is "not implemented in iteration 3b1" — later
        -- iterations replace each of these with real handling.
        RGlobal _ ->
            evErr env (Unsupported "RGlobal (iteration 3b2)")

        RCtor _ ->
            evErr env (Unsupported "RCtor (iteration 3b2)")

        RRecordAccessFunction _ ->
            evErr env (Unsupported "RRecordAccessFunction (iteration 3b2)")

        RCase _ _ ->
            evErr env (Unsupported "RCase (iteration 3b2)")

        RLambda _ ->
            evErr env (Unsupported "RLambda (iteration 3b2)")

        RApply _ _ ->
            evErr env (Unsupported "RApply (iteration 3b2)")

        RGLSL _ ->
            evErr env (Unsupported "RGLSL (not supported)")



-- HELPERS


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
                    if binding.arity > 0 then
                        evErr env
                            (Unsupported
                                ("RLet function binding '"
                                    ++ binding.debugName
                                    ++ "' (closures arrive in iteration 3b2)"
                                )
                            )

                    else
                        let
                            bodyEnv : REnv
                            bodyEnv =
                                { env | locals = locals }
                        in
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

        -- Patterns below will be supported in iteration 3b2 alongside
        -- RCase. For now, any attempt to match them at runtime fails.
        IR.RPRecord _ ->
            Nothing

        IR.RPCons _ _ ->
            Nothing

        IR.RPList _ ->
            Nothing

        IR.RPCtor _ _ ->
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
