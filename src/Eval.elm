module Eval exposing (eval, evalWithMaxSteps, indent, toModule, trace)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Eval.Module
import Rope exposing (Rope)
import Types exposing (CallTree, Config, Error, Value)


eval : String -> Result Error Value
eval expressionSource =
    let
        ( result, _, _ ) =
            traceOrEval { trace = True, maxSteps = Nothing, tcoTarget = Nothing } expressionSource
    in
    result


evalWithMaxSteps : Maybe Int -> String -> Result Error Value
evalWithMaxSteps maxSteps expressionSource =
    let
        ( result, _, _ ) =
            traceOrEval { trace = False, maxSteps = maxSteps, tcoTarget = Nothing } expressionSource
    in
    result


trace : String -> ( Result Error Value, Rope CallTree, Rope String )
trace expressionSource =
    traceOrEval { trace = True, maxSteps = Nothing, tcoTarget = Nothing } expressionSource


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
