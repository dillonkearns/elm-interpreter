module ResolvedExpressionTests exposing (suite)

{-| Tests for `Eval.ResolvedExpression.evalR` — Phase 3's new evaluator.

Iteration 3a covers only literal trees. Every other expression kind
returns an `Unsupported` error from `evalR`; those constructors will get
real handling in iteration 3b.

-}

import Eval.ResolvedExpression as RE
import Eval.ResolvedIR as IR exposing (RExpr(..))
import Expect
import FastDict
import Test exposing (Test, describe, test)
import Types exposing (EvalErrorKind(..), EvalResult(..), Value(..))


suite : Test
suite =
    describe "Eval.ResolvedExpression.evalR"
        [ literalTests
        , negationTests
        , collectionTests
        , localTests
        , ifTests
        , andOrTests
        , recordTests
        , letTests
        , lambdaTests
        , applyTests
        , constructorTests
        , caseTests
        , recordAccessFunctionTests
        , unsupportedTests
        ]


literalTests : Test
literalTests =
    describe "literals"
        [ test "RInt" <|
            \_ ->
                RE.evalR RE.emptyREnv (RInt 42)
                    |> expectValue (Int 42)
        , test "RFloat" <|
            \_ ->
                RE.evalR RE.emptyREnv (RFloat 3.14)
                    |> expectValue (Float 3.14)
        , test "RString" <|
            \_ ->
                RE.evalR RE.emptyREnv (RString "hello")
                    |> expectValue (String "hello")
        , test "RChar" <|
            \_ ->
                RE.evalR RE.emptyREnv (RChar 'a')
                    |> expectValue (Char 'a')
        , test "RUnit" <|
            \_ ->
                RE.evalR RE.emptyREnv RUnit
                    |> expectValue Unit
        ]


negationTests : Test
negationTests =
    describe "RNegate"
        [ test "negates an integer" <|
            \_ ->
                RE.evalR RE.emptyREnv (RNegate (RInt 5))
                    |> expectValue (Int -5)
        , test "negates a float" <|
            \_ ->
                RE.evalR RE.emptyREnv (RNegate (RFloat 2.5))
                    |> expectValue (Float -2.5)
        , test "nested negation returns original" <|
            \_ ->
                RE.evalR RE.emptyREnv (RNegate (RNegate (RInt 7)))
                    |> expectValue (Int 7)
        , test "negating a non-numeric is a type error" <|
            \_ ->
                case RE.evalR RE.emptyREnv (RNegate (RString "nope")) of
                    EvErr { error } ->
                        case error of
                            TypeError _ ->
                                Expect.pass

                            other ->
                                Expect.fail ("expected TypeError, got " ++ kindToString other)

                    other ->
                        Expect.fail ("expected EvErr, got " ++ resultToString other)
        ]


collectionTests : Test
collectionTests =
    describe "collections"
        [ test "empty list" <|
            \_ ->
                RE.evalR RE.emptyREnv (RList [])
                    |> expectValue (List [])
        , test "list of literals" <|
            \_ ->
                RE.evalR RE.emptyREnv (RList [ RInt 1, RInt 2, RInt 3 ])
                    |> expectValue (List [ Int 1, Int 2, Int 3 ])
        , test "2-tuple" <|
            \_ ->
                RE.evalR RE.emptyREnv (RTuple2 (RInt 1) (RString "two"))
                    |> expectValue (Tuple (Int 1) (String "two"))
        , test "3-tuple" <|
            \_ ->
                RE.evalR RE.emptyREnv
                    (RTuple3 (RInt 1) (RInt 2) (RInt 3))
                    |> expectValue (Triple (Int 1) (Int 2) (Int 3))
        , test "nested list with tuples" <|
            \_ ->
                RE.evalR RE.emptyREnv
                    (RList
                        [ RTuple2 (RInt 1) (RInt 2)
                        , RTuple2 (RInt 3) (RInt 4)
                        ]
                    )
                    |> expectValue
                        (List
                            [ Tuple (Int 1) (Int 2)
                            , Tuple (Int 3) (Int 4)
                            ]
                        )
        , test "list propagates inner errors" <|
            \_ ->
                case RE.evalR RE.emptyREnv (RList [ RInt 1, RLocal 0, RInt 3 ]) of
                    EvErr _ ->
                        Expect.pass

                    other ->
                        Expect.fail ("expected EvErr, got " ++ resultToString other)
        ]


localTests : Test
localTests =
    describe "RLocal"
        [ test "RLocal 0 returns innermost value" <|
            \_ ->
                let
                    env =
                        withLocals [ Int 1, Int 2, Int 3 ]
                in
                RE.evalR env (RLocal 0)
                    |> expectValue (Int 1)
        , test "RLocal 2 returns third value from head" <|
            \_ ->
                let
                    env =
                        withLocals [ Int 1, Int 2, Int 3 ]
                in
                RE.evalR env (RLocal 2)
                    |> expectValue (Int 3)
        , test "RLocal out of bounds is a type error" <|
            \_ ->
                case RE.evalR RE.emptyREnv (RLocal 0) of
                    EvErr { error } ->
                        case error of
                            TypeError _ ->
                                Expect.pass

                            other ->
                                Expect.fail ("expected TypeError, got " ++ kindToString other)

                    other ->
                        Expect.fail ("expected EvErr, got " ++ resultToString other)
        ]


ifTests : Test
ifTests =
    describe "RIf"
        [ test "true branch" <|
            \_ ->
                RE.evalR boolEnv
                    (RIf trueExpr (RInt 1) (RInt 2))
                    |> expectValue (Int 1)
        , test "false branch" <|
            \_ ->
                RE.evalR boolEnv
                    (RIf falseExpr (RInt 1) (RInt 2))
                    |> expectValue (Int 2)
        , test "non-Bool condition is a type error" <|
            \_ ->
                case RE.evalR RE.emptyREnv (RIf (RInt 0) (RInt 1) (RInt 2)) of
                    EvErr { error } ->
                        case error of
                            TypeError _ ->
                                Expect.pass

                            other ->
                                Expect.fail ("expected TypeError, got " ++ kindToString other)

                    other ->
                        Expect.fail ("expected EvErr, got " ++ resultToString other)
        ]


andOrTests : Test
andOrTests =
    describe "RAnd / ROr short-circuit"
        [ test "&& true true = true" <|
            \_ ->
                RE.evalR boolEnv (RAnd trueExpr trueExpr)
                    |> expectValue (Bool True)
        , test "&& true false = false" <|
            \_ ->
                RE.evalR boolEnv (RAnd trueExpr falseExpr)
                    |> expectValue (Bool False)
        , test "&& false [unevaluated] short-circuits to false" <|
            \_ ->
                -- Right side is an out-of-bounds RLocal. If we short-circuit
                -- correctly, we never evaluate it and still get False.
                RE.evalR boolEnv (RAnd falseExpr (RLocal 999))
                    |> expectValue (Bool False)
        , test "|| true [unevaluated] short-circuits to true" <|
            \_ ->
                RE.evalR boolEnv (ROr trueExpr (RLocal 999))
                    |> expectValue (Bool True)
        , test "|| false false = false" <|
            \_ ->
                RE.evalR boolEnv (ROr falseExpr falseExpr)
                    |> expectValue (Bool False)
        ]


recordTests : Test
recordTests =
    describe "records"
        [ test "empty record" <|
            \_ ->
                RE.evalR RE.emptyREnv (RRecord [])
                    |> expectValue (Record FastDict.empty)
        , test "record construction" <|
            \_ ->
                RE.evalR RE.emptyREnv
                    (RRecord
                        [ ( "a", RInt 1 )
                        , ( "b", RString "two" )
                        ]
                    )
                    |> expectValue
                        (Record
                            (FastDict.fromList
                                [ ( "a", Int 1 )
                                , ( "b", String "two" )
                                ]
                            )
                        )
        , test "record access" <|
            \_ ->
                let
                    rec =
                        RRecord [ ( "x", RInt 42 ), ( "y", RInt 0 ) ]
                in
                RE.evalR RE.emptyREnv (RRecordAccess rec "x")
                    |> expectValue (Int 42)
        , test "record access on missing field is a type error" <|
            \_ ->
                let
                    rec =
                        RRecord [ ( "x", RInt 42 ) ]
                in
                case RE.evalR RE.emptyREnv (RRecordAccess rec "nope") of
                    EvErr { error } ->
                        case error of
                            TypeError _ ->
                                Expect.pass

                            other ->
                                Expect.fail ("expected TypeError, got " ++ kindToString other)

                    other ->
                        Expect.fail ("expected EvErr, got " ++ resultToString other)
        , test "record update on slot-resolved local" <|
            \_ ->
                -- Put a record at local slot 0, then update it.
                let
                    original =
                        Record (FastDict.fromList [ ( "x", Int 1 ), ( "y", Int 2 ) ])

                    env =
                        withLocals [ original ]
                in
                RE.evalR env (RRecordUpdate 0 [ ( "y", RInt 99 ) ])
                    |> expectValue
                        (Record
                            (FastDict.fromList
                                [ ( "x", Int 1 )
                                , ( "y", Int 99 )
                                ]
                            )
                        )
        , test "record update on non-record local is a type error" <|
            \_ ->
                let
                    env =
                        withLocals [ Int 42 ]
                in
                case RE.evalR env (RRecordUpdate 0 [ ( "x", RInt 1 ) ]) of
                    EvErr { error } ->
                        case error of
                            TypeError _ ->
                                Expect.pass

                            other ->
                                Expect.fail ("expected TypeError, got " ++ kindToString other)

                    other ->
                        Expect.fail ("expected EvErr, got " ++ resultToString other)
        ]


letTests : Test
letTests =
    describe "RLet (sequential, value bindings only)"
        [ test "single value binding" <|
            \_ ->
                -- let x = 42 in x
                RE.evalR RE.emptyREnv
                    (RLet
                        [ valueBinding IR.RPVar (RInt 42) "x" ]
                        (RLocal 0)
                    )
                    |> expectValue (Int 42)
        , test "sequential: later references earlier" <|
            \_ ->
                -- let x = 1; y = x in y
                -- Matches the resolver's sequential output: y's body is
                -- RLocal 0 (x in ["x"]) and the let body is RLocal 0
                -- (y in ["y", "x"]).
                RE.evalR RE.emptyREnv
                    (RLet
                        [ valueBinding IR.RPVar (RInt 1) "x"
                        , valueBinding IR.RPVar (RLocal 0) "y"
                        ]
                        (RLocal 0)
                    )
                    |> expectValue (Int 1)
        , test "destructuring tuple binding" <|
            \_ ->
                -- let (a, b) = (1, 2) in a
                -- Pattern binds 2 slots: a at index 1, b at index 0.
                RE.evalR RE.emptyREnv
                    (RLet
                        [ { pattern = IR.RPTuple2 IR.RPVar IR.RPVar
                          , arity = 0
                          , body = RTuple2 (RInt 1) (RInt 2)
                          , debugName = "a"
                          }
                        ]
                        (RLocal 1)
                    )
                    |> expectValue (Int 1)
        , test "destructuring with wildcard" <|
            \_ ->
                -- let (_, b) = (1, 2) in b
                -- Wildcard binds 0 slots, so b is at index 0.
                RE.evalR RE.emptyREnv
                    (RLet
                        [ { pattern = IR.RPTuple2 IR.RPWildcard IR.RPVar
                          , arity = 0
                          , body = RTuple2 (RInt 1) (RInt 2)
                          , debugName = "b"
                          }
                        ]
                        (RLocal 0)
                    )
                    |> expectValue (Int 2)
        , test "nested let: inner sees outer" <|
            \_ ->
                -- let x = 1 in let y = x in y
                let
                    inner =
                        RLet
                            [ valueBinding IR.RPVar (RLocal 0) "y" ]
                            (RLocal 0)
                in
                RE.evalR RE.emptyREnv
                    (RLet
                        [ valueBinding IR.RPVar (RInt 7) "x" ]
                        inner
                    )
                    |> expectValue (Int 7)
        , test "non-recursive function binding: identity" <|
            \_ ->
                -- let f x = x in f 42
                let
                    identityLambda =
                        RLambda { arity = 1, body = RLocal 0 }

                    program =
                        RLet
                            [ { pattern = IR.RPVar
                              , arity = 1
                              , body = identityLambda
                              , debugName = "f"
                              }
                            ]
                            (RApply (RLocal 0) [ RInt 42 ])
                in
                RE.evalR RE.emptyREnv program
                    |> expectValue (Int 42)
        ]


lambdaTests : Test
lambdaTests =
    describe "RLambda + RApply (closures and application)"
        [ test "identity lambda applied" <|
            \_ ->
                -- (\x -> x) 7
                RE.evalR RE.emptyREnv
                    (RApply
                        (RLambda { arity = 1, body = RLocal 0 })
                        [ RInt 7 ]
                    )
                    |> expectValue (Int 7)
        , test "const lambda applied fully" <|
            \_ ->
                -- (\x y -> x) 1 2
                -- Body is RLocal 1 (outer x, after y gets pushed)
                RE.evalR RE.emptyREnv
                    (RApply
                        (RLambda { arity = 2, body = RLocal 1 })
                        [ RInt 1, RInt 2 ]
                    )
                    |> expectValue (Int 1)
        , test "partial application returns a closure" <|
            \_ ->
                -- ((\x y -> x) 1) — not yet applied to y
                case
                    RE.evalR RE.emptyREnv
                        (RApply
                            (RLambda { arity = 2, body = RLocal 1 })
                            [ RInt 1 ]
                        )
                of
                    EvOk (PartiallyApplied _ appliedArgs _ _ _ arity) ->
                        ( List.length appliedArgs, arity )
                            |> Expect.equal ( 1, 2 )

                    other ->
                        Expect.fail
                            ("expected PartiallyApplied closure, got "
                                ++ resultToString other
                            )
        , test "partial then complete application" <|
            \_ ->
                -- Apply the 1 first, then apply 2 to the result
                case
                    RE.evalR RE.emptyREnv
                        (RApply
                            (RLambda { arity = 2, body = RLocal 1 })
                            [ RInt 1 ]
                        )
                of
                    EvOk partial ->
                        case
                            RE.evalR RE.emptyREnv
                                (RApply
                                    -- Inject the partial closure via a local
                                    -- slot since we can't embed a Value in
                                    -- an RExpr literal.
                                    (RLocal 0)
                                    [ RInt 2 ]
                                )
                        of
                            -- The RApply above needs locals containing the
                            -- partial closure; rebuild an env for it.
                            _ ->
                                case
                                    RE.evalR
                                        (withLocals [ partial ])
                                        (RApply (RLocal 0) [ RInt 2 ])
                                of
                                    EvOk v ->
                                        v |> Expect.equal (Int 1)

                                    other ->
                                        Expect.fail ("second apply failed: " ++ resultToString other)

                    other ->
                        Expect.fail ("first apply failed: " ++ resultToString other)
        , test "closure captures outer locals" <|
            \_ ->
                -- In an env where slot 0 = 100, evaluate `\x -> x + 0th-local`.
                -- The lambda body is RApply (some add) [RLocal 0, RLocal 1].
                -- Lambda's RLocal 0 = its own arg, RLocal 1 = the captured value.
                --
                -- But we don't have + yet (RGlobal deferred), so instead
                -- build a body that uses RTuple2 to combine them:
                --   \x -> (x, captured)
                -- Apply with 7, expect (7, 100).
                let
                    env =
                        withLocals [ Int 100 ]

                    lambda =
                        RLambda
                            { arity = 1
                            , body = RTuple2 (RLocal 0) (RLocal 1)
                            }
                in
                RE.evalR env (RApply lambda [ RInt 7 ])
                    |> expectValue (Tuple (Int 7) (Int 100))
        ]


applyTests : Test
applyTests =
    describe "RApply edge cases"
        [ test "applying zero args returns the function unchanged" <|
            \_ ->
                -- Should never happen in practice (the resolver doesn't
                -- emit empty-arg applies), but if it does, it should be
                -- a no-op rather than a crash.
                let
                    lambda =
                        RLambda { arity = 1, body = RLocal 0 }
                in
                case RE.evalR RE.emptyREnv (RApply lambda []) of
                    EvOk (PartiallyApplied _ [] _ _ _ 1) ->
                        Expect.pass

                    other ->
                        Expect.fail
                            ("expected unchanged closure, got "
                                ++ resultToString other
                            )
        , test "applying to a non-callable is a type error" <|
            \_ ->
                case RE.evalR RE.emptyREnv (RApply (RInt 0) [ RInt 1 ]) of
                    EvErr { error } ->
                        case error of
                            TypeError _ ->
                                Expect.pass

                            other ->
                                Expect.fail ("expected TypeError, got " ++ kindToString other)

                    other ->
                        Expect.fail ("expected EvErr, got " ++ resultToString other)
        ]


constructorTests : Test
constructorTests =
    describe "RCtor"
        [ test "True → Bool True" <|
            \_ ->
                RE.evalR RE.emptyREnv
                    (RCtor { moduleName = [], name = "True" })
                    |> expectValue (Bool True)
        , test "False → Bool False" <|
            \_ ->
                RE.evalR RE.emptyREnv
                    (RCtor { moduleName = [], name = "False" })
                    |> expectValue (Bool False)
        , test "Nothing → empty Custom" <|
            \_ ->
                RE.evalR RE.emptyREnv
                    (RCtor { moduleName = [ "Maybe" ], name = "Nothing" })
                    |> expectValue
                        (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])
        , test "Just 5 via RApply extends the Custom's args" <|
            \_ ->
                RE.evalR RE.emptyREnv
                    (RApply
                        (RCtor { moduleName = [ "Maybe" ], name = "Just" })
                        [ RInt 5 ]
                    )
                    |> expectValue
                        (Custom { moduleName = [ "Maybe" ], name = "Just" } [ Int 5 ])
        ]


caseTests : Test
caseTests =
    describe "RCase pattern matching"
        [ test "literal match: first branch wins" <|
            \_ ->
                -- case 1 of 1 -> "one"; _ -> "other"
                RE.evalR RE.emptyREnv
                    (RCase (RInt 1)
                        [ ( IR.RPInt 1, RString "one" )
                        , ( IR.RPWildcard, RString "other" )
                        ]
                    )
                    |> expectValue (String "one")
        , test "literal mismatch: fall through to wildcard" <|
            \_ ->
                RE.evalR RE.emptyREnv
                    (RCase (RInt 2)
                        [ ( IR.RPInt 1, RString "one" )
                        , ( IR.RPWildcard, RString "other" )
                        ]
                    )
                    |> expectValue (String "other")
        , test "RPVar binds the scrutinee" <|
            \_ ->
                -- case 42 of x -> x
                -- In the branch body, RLocal 0 = x = 42.
                RE.evalR RE.emptyREnv
                    (RCase (RInt 42)
                        [ ( IR.RPVar, RLocal 0 ) ]
                    )
                    |> expectValue (Int 42)
        , test "tuple pattern destructures" <|
            \_ ->
                -- case (1, 2) of (a, b) -> b
                -- In the branch: locals = [b, a, ...]. RLocal 0 = b = 2.
                RE.evalR RE.emptyREnv
                    (RCase (RTuple2 (RInt 1) (RInt 2))
                        [ ( IR.RPTuple2 IR.RPVar IR.RPVar, RLocal 0 ) ]
                    )
                    |> expectValue (Int 2)
        , test "constructor pattern with one arg: Just" <|
            \_ ->
                -- case (Just 7) of
                --     Just v -> v
                --     Nothing -> 0
                RE.evalR RE.emptyREnv
                    (RCase
                        (RApply
                            (RCtor { moduleName = [ "Maybe" ], name = "Just" })
                            [ RInt 7 ]
                        )
                        [ ( IR.RPCtor
                                { moduleName = [], name = "Just" }
                                [ IR.RPVar ]
                          , RLocal 0
                          )
                        , ( IR.RPCtor
                                { moduleName = [], name = "Nothing" }
                                []
                          , RInt 0
                          )
                        ]
                    )
                    |> expectValue (Int 7)
        , test "cons pattern: head :: tail" <|
            \_ ->
                -- case [1, 2, 3] of
                --     head :: tail -> head
                --     [] -> 0
                RE.evalR RE.emptyREnv
                    (RCase (RList [ RInt 1, RInt 2, RInt 3 ])
                        [ ( IR.RPCons IR.RPVar IR.RPVar, RLocal 1 )
                        , ( IR.RPList [], RInt 0 )
                        ]
                    )
                    |> expectValue (Int 1)
        , test "empty list pattern" <|
            \_ ->
                RE.evalR RE.emptyREnv
                    (RCase (RList [])
                        [ ( IR.RPList [], RString "empty" )
                        , ( IR.RPWildcard, RString "nonempty" )
                        ]
                    )
                    |> expectValue (String "empty")
        , test "record pattern destructures" <|
            \_ ->
                -- case {a=1, b=2} of {a, b} -> a
                -- Fields sorted alphabetically: locals = [b, a] so a is at RLocal 1.
                RE.evalR RE.emptyREnv
                    (RCase
                        (RRecord [ ( "a", RInt 1 ), ( "b", RInt 2 ) ])
                        [ ( IR.RPRecord [ "a", "b" ], RLocal 1 ) ]
                    )
                    |> expectValue (Int 1)
        , test "no branch matches: type error" <|
            \_ ->
                case
                    RE.evalR RE.emptyREnv
                        (RCase (RInt 5)
                            [ ( IR.RPInt 0, RString "zero" )
                            , ( IR.RPInt 1, RString "one" )
                            ]
                        )
                of
                    EvErr { error } ->
                        case error of
                            TypeError _ ->
                                Expect.pass

                            other ->
                                Expect.fail ("expected TypeError, got " ++ kindToString other)

                    other ->
                        Expect.fail ("expected EvErr, got " ++ resultToString other)
        ]


recordAccessFunctionTests : Test
recordAccessFunctionTests =
    describe "RRecordAccessFunction"
        [ test ".field applied to a record returns the value" <|
            \_ ->
                -- (.x) { x = 10, y = 20 }
                let
                    rec =
                        RRecord [ ( "x", RInt 10 ), ( "y", RInt 20 ) ]
                in
                RE.evalR RE.emptyREnv
                    (RApply (RRecordAccessFunction "x") [ rec ])
                    |> expectValue (Int 10)
        ]


unsupportedTests : Test
unsupportedTests =
    describe "still unsupported after iteration 3b2"
        [ test "RGlobal returns Unsupported (deferred to Phase 3 wire-up)" <|
            \_ ->
                expectUnsupported (RGlobal 42)
        , test "RGLSL returns Unsupported (never supported)" <|
            \_ ->
                expectUnsupported RGLSL
        ]


{-| There's no RBool literal in the IR — True/False are constructor
references, and RCtor is unsupported in 3b1. Tests that need Bool
values use this env with `Bool True` at slot 0 and `Bool False` at
slot 1. `trueExpr` / `falseExpr` are RLocal references into it.
-}
boolEnv : RE.REnv
boolEnv =
    withLocals [ Bool True, Bool False ]


{-| Construct a REnv by replacing `emptyREnv`'s locals list. Workaround for
Elm's "record update only on variables" restriction — `{ RE.emptyREnv | ... }`
doesn't parse because `RE.emptyREnv` is a qualified reference, not a bare
identifier.
-}
withLocals : List Value -> RE.REnv
withLocals locals =
    let
        base : RE.REnv
        base =
            RE.emptyREnv
    in
    { base | locals = locals }


trueExpr : RExpr
trueExpr =
    RLocal 0


falseExpr : RExpr
falseExpr =
    RLocal 1


valueBinding : IR.RPattern -> RExpr -> String -> IR.RLetBinding
valueBinding pattern body debugName =
    { pattern = pattern
    , arity = 0
    , body = body
    , debugName = debugName
    }



-- HELPERS


expectValue : Value -> EvalResult Value -> Expect.Expectation
expectValue expected result =
    case result of
        EvOk actual ->
            actual |> Expect.equal expected

        other ->
            Expect.fail ("expected EvOk, got " ++ resultToString other)


expectUnsupported : RExpr -> Expect.Expectation
expectUnsupported expr =
    case RE.evalR RE.emptyREnv expr of
        EvErr { error } ->
            case error of
                Unsupported _ ->
                    Expect.pass

                other ->
                    Expect.fail ("expected Unsupported, got " ++ kindToString other)

        other ->
            Expect.fail ("expected EvErr, got " ++ resultToString other)


resultToString : EvalResult Value -> String
resultToString result =
    case result of
        EvOk v ->
            "EvOk " ++ Debug.toString v

        EvErr e ->
            "EvErr " ++ kindToString e.error

        EvOkTrace _ _ _ ->
            "EvOkTrace"

        EvErrTrace _ _ _ ->
            "EvErrTrace"

        EvYield _ _ _ ->
            "EvYield"

        EvMemoLookup _ _ ->
            "EvMemoLookup"

        EvMemoStore _ _ ->
            "EvMemoStore"

        EvOkCoverage _ _ ->
            "EvOkCoverage"

        EvErrCoverage _ _ ->
            "EvErrCoverage"


kindToString : EvalErrorKind -> String
kindToString kind =
    case kind of
        TypeError msg ->
            "TypeError " ++ msg

        Unsupported msg ->
            "Unsupported " ++ msg

        NameError msg ->
            "NameError " ++ msg

        Todo msg ->
            "Todo " ++ msg

        TailCall _ ->
            "TailCall"

        TailCallLocals _ ->
            "TailCallLocals"
