module Types exposing (CallCounts, CallTree(..), Config, Env, EnvValues, Error(..), Eval, EvalErrorData, EvalErrorKind(..), EvalResult(..), Implementation(..), ImportedNames, Intercept(..), InterceptContext, JsonDecoder(..), JsonVal(..), MemoLookupPayload, MemoStorePayload, PartialEval, PartialResult, Value(..), evalErrorKindToString, packRange, unpackRange)

import Array exposing (Array)
import Elm.Syntax.Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Regex
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern, QualifiedNameRef)
import FastDict exposing (Dict)
import MemoSpec
import Parser exposing (DeadEnd)
import Recursion exposing (Rec)
import Rope exposing (Rope)
import Set


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
    | EvYield String Value (Value -> EvalResult out)
    | EvMemoLookup MemoLookupPayload (Maybe Value -> EvalResult out)
    | EvMemoStore MemoStorePayload (EvalResult out)
    | EvOkCoverage out (Set.Set Int)
    | EvErrCoverage EvalErrorData (Set.Set Int)


type alias Config =
    { trace : Bool
    , coverage : Bool
    , coverageProbeLines : Set.Set Int
    , maxSteps : Maybe Int
    , tcoTarget : Maybe String
    , callCounts : Maybe CallCounts
    , intercepts : Dict String Intercept
    , memoizedFunctions : MemoSpec.Registry
    , collectMemoStats : Bool
    }


{-| A function intercept: called instead of the normal AST evaluation
when the interpreter encounters a registered qualified function name.

Receives the fully-applied arguments and the current eval context.
Framework authors use this for callbacks (BackendTask, Test, Cmd)
and for memoization/caching hooks.
-}
type alias InterceptContext =
    { qualifiedName : String
    , evaluateOriginal : () -> EvalResult Value
    }


type Intercept
    = Intercept (InterceptContext -> List Value -> Config -> Env -> EvalResult Value)


{-| Lightweight call counting for profiling. Tracks how many times each
qualified function is called, and how many of those had identical argument
fingerprints (memoization candidates).
-}
type alias CallCounts =
    { calls : Dict String { count : Int, uniqueFingerprints : Int, lastFingerprint : Int }
    }


type CallTree
    = CallNode
        { expression : Node Expression
        , result : Result EvalErrorData Value
        , children : Rope CallTree
        , env : Env
        }
    | CoverageRange Range
    | CoverageSet (Set.Set Int)


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
    | PartiallyApplied Env (List Value) (List (Node Pattern)) (Maybe QualifiedNameRef) Implementation Int {- cached arity (List.length patterns) -}
    | JsArray (Array Value)
    | List (List Value)
    | JsonValue JsonVal
    | JsonDecoderValue JsonDecoder
    | RegexValue Regex.Regex
    | BytesValue (Array Int)


type alias MemoLookupPayload =
    { specId : Int
    , qualifiedName : Maybe String
    , compactFingerprint : Maybe Int
    , args : Maybe (List Value)
    , shallowFingerprint : Maybe Int
    , deepFingerprint : Maybe Int
    }


type alias MemoStorePayload =
    { specId : Int
    , qualifiedName : Maybe String
    , compactFingerprint : Maybe Int
    , args : Maybe (List Value)
    , shallowFingerprint : Maybe Int
    , deepFingerprint : Maybe Int
    , value : Value
    }


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


type alias SharedContext =
    { functions : Dict String (Dict String FunctionImplementation)
    , moduleImports : Dict String ImportedNames

    -- Module-level 0-arg values that have already been evaluated to a
    -- concrete `Value`. Keyed by moduleKey → name. Populated during the
    -- top-level-constant normalization pass (and, in principle, at runtime
    -- as side-effect-free constants are first forced). Before eval walks a
    -- 0-arg module function body, it checks here first and returns the
    -- cached `Value` — avoiding expensive re-evaluation of things like
    -- `String.Diacritics.lookupArray`, which is a 65k-element `Array`
    -- built from a `Dict.fromList` over ~800 unicode code points and took
    -- 87 s per reference without this cache.
    , precomputedValues : Dict String (Dict String Value)
    }


type alias Env =
    { currentModule : ModuleName
    , currentModuleKey : String
    , shared : SharedContext
    , currentModuleFunctions : Dict String FunctionImplementation
    , letFunctions : Dict String FunctionImplementation
    , values : EnvValues
    , callStack : List QualifiedNameRef
    , imports : ImportedNames
    , callDepth : Int
    , recursionCheck : Maybe (Dict String { fingerprint : Int, depth : Int, count : Int, size : Int, growCount : Int, argSizes : List Int, argFingerprints : List Int })
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
    | TailCall EnvValues


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

        TailCall _ ->
            "TailCall (internal TCO signal)"


{-| Pack a Range into a single Int for use in Set Int.
Uses arithmetic (not bitwise, which is 32-bit in Elm/JS).
Supports rows 0-9999 and columns 0-999 (offset by 2 for negatives).
Max packed value: ~1.006 × 10^14, well within JS safe integer range.
-}
packRange : Range -> Int
packRange r =
    let
        sr =
            r.start.row + 2

        sc =
            r.start.column + 2

        er =
            r.end.row + 2

        ec =
            r.end.column + 2
    in
    sr * 10063102027 + sc * 10033009 + er * 1003 + ec


{-| Unpack a packed Int back into a Range.
-}
unpackRange : Int -> Range
unpackRange packed =
    let
        sr =
            packed // 10063102027

        rem1 =
            packed - sr * 10063102027

        sc =
            rem1 // 10033009

        rem2 =
            rem1 - sc * 10033009

        er =
            rem2 // 1003

        ec =
            rem2 - er * 1003
    in
    { start = { row = sr - 2, column = sc - 2 }
    , end = { row = er - 2, column = ec - 2 }
    }

