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
        , test "function binding returns Unsupported (iteration 3b2)" <|
            \_ ->
                case
                    RE.evalR RE.emptyREnv
                        (RLet
                            [ { pattern = IR.RPVar
                              , arity = 1
                              , body =
                                    RLambda { arity = 1, body = RLocal 0 }
                              , debugName = "f"
                              }
                            ]
                            (RLocal 0)
                        )
                of
                    EvErr { error } ->
                        case error of
                            Unsupported _ ->
                                Expect.pass

                            other ->
                                Expect.fail ("expected Unsupported, got " ++ kindToString other)

                    other ->
                        Expect.fail ("expected EvErr, got " ++ resultToString other)
        ]


unsupportedTests : Test
unsupportedTests =
    describe "still unsupported in iteration 3b1"
        [ test "RGlobal returns Unsupported" <|
            \_ ->
                expectUnsupported (RGlobal 42)
        , test "RLambda returns Unsupported" <|
            \_ ->
                expectUnsupported (RLambda { arity = 1, body = RInt 0 })
        , test "RApply returns Unsupported" <|
            \_ ->
                expectUnsupported (RApply (RInt 0) [])
        , test "RCase returns Unsupported" <|
            \_ ->
                expectUnsupported (RCase (RInt 0) [])
        , test "RCtor returns Unsupported" <|
            \_ ->
                expectUnsupported (RCtor { moduleName = [ "Maybe" ], name = "Nothing" })
        , test "RRecordAccessFunction returns Unsupported" <|
            \_ ->
                expectUnsupported (RRecordAccessFunction "x")
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
