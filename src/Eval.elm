module Eval exposing (eval, evalWithMaxSteps, indent, toModule, trace)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Eval.Module
import FastDict as Dict
import MemoSpec
import Rope exposing (Rope)
import Set
import Types exposing (CallTree, Config, Error, Value)


eval : String -> Result Error Value
eval expressionSource =
    let
        ( result, _, _ ) =
            traceOrEval { trace = False, coverage = False, coverageProbeLines = Set.empty, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False } expressionSource
    in
    result


evalWithMaxSteps : Maybe Int -> String -> Result Error Value
evalWithMaxSteps maxSteps expressionSource =
    let
        ( result, _, _ ) =
            traceOrEval { trace = False, coverage = False, coverageProbeLines = Set.empty, maxSteps = maxSteps, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False } expressionSource
    in
    result


trace : String -> ( Result Error Value, Rope CallTree, Rope String )
trace expressionSource =
    traceOrEval { trace = True, coverage = False, coverageProbeLines = Set.empty, maxSteps = Nothing, tcoTarget = Nothing, callCounts = Nothing, intercepts = Dict.empty, memoizedFunctions = MemoSpec.emptyRegistry, collectMemoStats = False } expressionSource


traceOrEval : Config -> String -> ( Result Error Value, Rope CallTree, Rope String )
traceOrEval cfg expressionSource =
    let
        source : String
        source =
            toModule expressionSource

        expression : Expression
        expression =
            Expression.FunctionOrValue [] "main"
    in
    Eval.Module.traceOrEvalModule cfg source expression


toModule : String -> String
toModule expression =
    "module Main exposing (main)\n\nmain =\n"
        ++ indent 4 expression

indent : Int -> String -> String
indent count input =
    let
        prefix : String
        prefix =
            String.repeat count " "
    in
    input
        |> String.split "\n"
        |> List.map
            (\line ->
                if String.isEmpty line then
                    line

                else
                    prefix ++ line
            )
        |> String.join "\n"
