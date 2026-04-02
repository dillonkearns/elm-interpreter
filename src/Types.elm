module Types exposing (CallTree(..), Config, Env, EnvValues, Error(..), Eval, EvalErrorData, EvalErrorKind(..), EvalResult(..), Implementation(..), ImportedNames, JsonDecoder(..), JsonVal(..), PartialEval, PartialResult, Value(..), evalErrorKindToString)

import Array exposing (Array)
import Elm.Syntax.Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Regex
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern, QualifiedNameRef)
import FastDict exposing (Dict)
import Parser exposing (DeadEnd)
import Recursion exposing (Rec)
import Rope exposing (Rope)


type alias PartialEval out =
    Config -> Env -> PartialResult out


type alias PartialResult out =
    Rec
        ( Node Expression, Config, Env )
        (EvalResult out)
        (EvalResult out)


type alias Eval out =
    Config -> Env -> EvalResult out


type EvalResult out
    = EvOk out
    | EvErr EvalErrorData
    | EvOkTrace out (Rope CallTree) (Rope String)
    | EvErrTrace EvalErrorData (Rope CallTree) (Rope String)


type alias Config =
    { trace : Bool
    }


type CallTree
    = CallNode
        { expression : Node Expression
        , result : Result EvalErrorData Value
        , children : Rope CallTree
        , env : Env
        }


type Error
    = ParsingError (List DeadEnd)
    | EvalError EvalErrorData


type Value
    = String String
    | Int Int
    | Float Float
    | Char Char
    | Bool Bool
    | Unit
    | Tuple Value Value
    | Triple Value Value Value
    | Record (Dict String Value)
    | Custom QualifiedNameRef (List Value)
    | PartiallyApplied Env (List Value) (List (Node Pattern)) (Maybe QualifiedNameRef) Implementation
    | JsArray (Array Value)
    | List (List Value)
    | JsonValue JsonVal
    | JsonDecoderValue JsonDecoder
    | RegexValue Regex.Regex


{-| JSON value representation for Json.Encode.Value / Json.Decode.Value.
-}
type JsonVal
    = JsonNull
    | JsonBool Bool
    | JsonInt Int
    | JsonFloat Float
    | JsonString String
    | JsonArray (List JsonVal)
    | JsonObject (List ( String, JsonVal ))


{-| Decoder combinator tree for Json.Decode.Decoder.
-}
type JsonDecoder
    = DecodeString
    | DecodeBool
    | DecodeInt
    | DecodeFloat
    | DecodeValue
    | DecodeNull Value
    | DecodeList JsonDecoder
    | DecodeArray JsonDecoder
    | DecodeField String JsonDecoder
    | DecodeIndex Int JsonDecoder
    | DecodeKeyValuePairs JsonDecoder
    | DecodeMap Value (List JsonDecoder)
    | DecodeAndThen Value JsonDecoder
    | DecodeOneOf (List JsonDecoder)
    | DecodeFail String
    | DecodeSucceed Value


{-| Function implementation: either an AST expression to evaluate,
or a pre-resolved kernel function that can be called directly.
-}
type Implementation
    = AstImpl (Node Expression)
    | KernelImpl ModuleName String (List Value -> Eval Value)


type alias Env =
    { currentModule : ModuleName
    , currentModuleKey : String
    , functions : Dict String (Dict String FunctionImplementation)
    , currentModuleFunctions : Dict String FunctionImplementation
    , values : EnvValues
    , callStack : List QualifiedNameRef
    , imports : ImportedNames
    , moduleImports : Dict String ImportedNames
    }


type alias ImportedNames =
    { aliases : Dict String ( ModuleName, String )
    , exposedValues : Dict String ( ModuleName, String )
    , exposedConstructors : Dict String ( ModuleName, String )
    }


type alias EnvValues =
    Dict String Value


type alias EvalErrorData =
    { currentModule : ModuleName
    , callStack : List QualifiedNameRef
    , error : EvalErrorKind
    }


type EvalErrorKind
    = TypeError String
    | Unsupported String
    | NameError String
    | Todo String


evalErrorKindToString : EvalErrorKind -> String
evalErrorKindToString kind =
    case kind of
        TypeError msg ->
            "TypeError: " ++ msg

        Unsupported msg ->
            "Unsupported: " ++ msg

        NameError msg ->
            "NameError: " ++ msg

        Todo msg ->
            "Todo: " ++ msg
