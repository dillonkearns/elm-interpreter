port module InterpreterRunner exposing (main)

{-| Compiled Elm module that wraps the interpreter for use from Node.js.

Takes module source code as a String flag, evaluates `computeResult`
through the interpreter, and outputs the result via port.

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Eval.Module
import Types exposing (Error(..), EvalErrorKind(..), Value(..))
import Value


port output : String -> Cmd msg


main : Program String () ()
main =
    Platform.worker
        { init = \source -> ( (), interpretModule source )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


interpretModule : String -> Cmd ()
interpretModule source =
    let
        expression =
            FunctionOrValue [] "computeResult"

        result =
            Eval.Module.eval source expression
    in
    case result of
        Ok (String s) ->
            output s

        Ok value ->
            output ("INTERPRETER_TYPE_ERROR: expected String, got " ++ Value.toString value)

        Err (ParsingError deadEnds) ->
            output ("INTERPRETER_PARSE_ERROR: " ++ Debug.toString deadEnds)

        Err (EvalError e) ->
            output
                ("INTERPRETER_EVAL_ERROR: "
                    ++ (case e.error of
                            TypeError msg ->
                                "TypeError: " ++ msg

                            Unsupported msg ->
                                "Unsupported: " ++ msg

                            NameError msg ->
                                "NameError: " ++ msg

                            Todo msg ->
                                "Todo: " ++ msg
                       )
                )
