module DelegateCounterTests exposing (suite)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Eval.DelegateCounter as DelegateCounter
import Eval.Module
import Expect
import FastDict
import Test exposing (Test, describe, test)
import Types exposing (EvalResult(..), Value(..))


suite : Test
suite =
    describe "delegate counter diagnostics"
        [ collectStopsAtRealYield
        , regexFallbacksAreCounted
        ]


collectStopsAtRealYield : Test
collectStopsAtRealYield =
    test "collect strips delegate diagnostics until the next real yield" <|
        \_ ->
            let
                collected =
                    DelegateCounter.emit [ "Regex" ] "contains"
                        (EvYield "real-yield" (Int 1) (\_ -> EvOk (Int 2)))
                        |> DelegateCounter.collect
            in
            case collected.result of
                EvYield "real-yield" (Int 1) resume ->
                    Expect.all
                        [ \_ -> Expect.equal 1 collected.delegateCallCounter.count
                        , \_ ->
                            Expect.equal
                                (Just 1)
                                (FastDict.get "Regex.contains" collected.delegateCallCounter.countsByName)
                        , \_ ->
                            case resume Unit of
                                EvOk (Int 2) ->
                                    Expect.pass

                                other ->
                                    Expect.fail ("expected resumed yield to finish with EvOk 2, got " ++ resultToString other)
                        ]
                        ()

                other ->
                    Expect.fail ("expected the real yield to remain after collection, got " ++ resultToString other)


regexFallbacksAreCounted : Test
regexFallbacksAreCounted =
    test "resolved-IR diagnostics count unresolved Regex delegates" <|
        \_ ->
            let
                source : String
                source =
                    """module Main exposing (result)

import Regex

result =
    Regex.contains Regex.never "abc"
"""

                expression : Expression
                expression =
                    Expression.FunctionOrValue [ "Main" ] "result"
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Err err ->
                    Expect.fail ("buildProjectEnv failed: " ++ Debug.toString err)

                Ok projectEnv ->
                    let
                        collected =
                            Eval.Module.evalWithResolvedIRFromFilesAndInterceptsAndDelegateCallCounter
                                projectEnv
                                []
                                FastDict.empty
                                FastDict.empty
                                expression
                    in
                    case collected.result of
                        EvOk (Bool False) ->
                            Expect.all
                                [ \_ -> Expect.equal 2 collected.delegateCallCounter.count
                                , \_ ->
                                    Expect.equal
                                        (Just 1)
                                        (FastDict.get "Regex.never" collected.delegateCallCounter.countsByName)
                                , \_ ->
                                    Expect.equal
                                        (Just 1)
                                        (FastDict.get "Regex.contains" collected.delegateCallCounter.countsByName)
                                ]
                                ()

                        other ->
                            Expect.fail ("expected EvOk False, got " ++ resultToString other)


resultToString : EvalResult Value -> String
resultToString result =
    case result of
        EvOk v ->
            "EvOk " ++ Debug.toString v

        EvErr e ->
            "EvErr " ++ Debug.toString e.error

        EvOkTrace _ _ _ ->
            "EvOkTrace"

        EvErrTrace _ _ _ ->
            "EvErrTrace"

        EvYield tag payload _ ->
            "EvYield " ++ tag ++ " " ++ Debug.toString payload

        EvMemoLookup _ _ ->
            "EvMemoLookup"

        EvMemoStore _ _ ->
            "EvMemoStore"

        EvOkCoverage _ _ ->
            "EvOkCoverage"

        EvErrCoverage _ _ ->
            "EvErrCoverage"
