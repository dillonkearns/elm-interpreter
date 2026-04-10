module ResolvedExpressionTests exposing (suite)

{-| Tests for `Eval.ResolvedExpression.evalR` — Phase 3's new evaluator.

Iteration 3a covers only literal trees. Every other expression kind
returns an `Unsupported` error from `evalR`; those constructors will get
real handling in iteration 3b.

-}

import Eval.ResolvedExpression as RE
import Eval.ResolvedIR as IR exposing (RExpr(..))
import Expect
import Test exposing (Test, describe, test)
import Types exposing (EvalErrorKind(..), EvalResult(..), Value(..))


suite : Test
suite =
    describe "Eval.ResolvedExpression.evalR (iteration 3a)"
        [ literalTests
        , negationTests
        , collectionTests
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


unsupportedTests : Test
unsupportedTests =
    describe "unsupported in iteration 3a"
        [ test "RLocal returns Unsupported" <|
            \_ ->
                expectUnsupported (RLocal 0)
        , test "RGlobal returns Unsupported" <|
            \_ ->
                expectUnsupported (RGlobal 42)
        , test "RLambda returns Unsupported" <|
            \_ ->
                expectUnsupported (RLambda { arity = 1, body = RInt 0 })
        , test "RApply returns Unsupported" <|
            \_ ->
                expectUnsupported (RApply (RInt 0) [])
        , test "RIf returns Unsupported" <|
            \_ ->
                expectUnsupported (RIf (RInt 0) (RInt 1) (RInt 2))
        , test "RLet returns Unsupported" <|
            \_ ->
                expectUnsupported
                    (RLet
                        [ { pattern = IR.RPVar
                          , arity = 0
                          , body = RInt 1
                          , debugName = "x"
                          }
                        ]
                        (RInt 0)
                    )
        , test "RCase returns Unsupported" <|
            \_ ->
                expectUnsupported (RCase (RInt 0) [])
        , test "RRecord returns Unsupported" <|
            \_ ->
                expectUnsupported (RRecord [])
        ]



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
