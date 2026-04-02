port module MultiModuleRunner exposing (main)

{-| Like InterpreterRunner but handles multiple modules.

Flags: JSON string containing an array of module source strings.
The last module's `computeResult` is evaluated.
-}

import Elm.Syntax.Expression exposing (Expression(..))
import Eval.Module
import Json.Decode as Decode
import Types exposing (Error(..), EvalErrorKind(..), Value(..))
import Value


port output : String -> Cmd msg


main : Program String () ()
main =
    Platform.worker
        { init = \flagsJson -> ( (), interpretModules flagsJson )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


interpretModules : String -> Cmd ()
interpretModules flagsJson =
    case Decode.decodeString (Decode.list Decode.string) flagsJson of
        Err decodeErr ->
            output ("DECODE_ERROR: " ++ Decode.errorToString decodeErr)

        Ok sources ->
            let
                expression =
                    FunctionOrValue [] "computeResult"

                result =
                    Eval.Module.evalProject sources expression
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
                                    TypeError msg -> "TypeError: " ++ msg
                                    Unsupported msg -> "Unsupported: " ++ msg
                                    NameError msg -> "NameError: " ++ msg
                                    Todo msg -> "Todo: " ++ msg
                               )
                        )
