module Types exposing (CallCounts, CallTree(..), Config, Env, EnvValues, Error(..), Eval, EvalErrorData, EvalErrorKind(..), EvalResult(..), Implementation(..), ImportedNames, Intercept(..), InterceptContext, JsonDecoder(..), JsonVal(..), MemoLookupPayload, MemoStorePayload, PartialEval, PartialResult, Value(..), evalErrorKindToString)

import Array exposing (Array)
import Elm.Syntax.Expression exposing (Expression, FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Regex
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern, QualifiedNameRef)
import Eval.ResolvedIR as IR
import FastDict exposing (Dict)
import MemoSpec
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
    | EvYield String Value (Value -> EvalResult out)
    | EvMemoLookup MemoLookupPayload (Maybe Value -> EvalResult out)
    | EvMemoStore MemoStorePayload (EvalResult out)


type alias Config =
    { trace : Bool
    , maxSteps : Maybe Int
    , tcoTarget : Maybe String
    , callCounts : Maybe CallCounts
    , intercepts : Dict String Intercept
    , memoizedFunctions : MemoSpec.Registry
    , collectMemoStats : Bool
    , useResolvedIR : Bool
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
or a pre-resolved kernel function that can be called directly,
or a resolved-IR body produced by `Eval.Resolver` for Phase 3's
new evaluator.

`RExprImpl` carries the closure's body as an `RExpr`, the captured
locals list that was in scope when the closure was created, and
`selfSlots` — the number of synthetic "self reference" slots the
evaluator should prepend to `capturedLocals` at call time, filled
with the closure itself.

`selfSlots` supports single-binding self-recursion: `let f x = f
(x - 1) in f 5` needs `f` to be visible inside its own body. At
closure creation time we can't put the finished closure into its
own captured locals (that would be a cycle), so the evaluator
substitutes at call time instead. For `RLambda` expressions and
non-recursive contexts, `selfSlots = 0` and the substitution is
skipped.

Mutual recursion of multiple function bindings in the same let
group is NOT supported — the resolver's sequential scoping only
gives a single binding visibility of itself, not siblings. That's
deferred to a later iteration when the evaluator gains a
pre-allocation story.

The new evaluator (`Eval.ResolvedExpression`) is the only code
that creates or reads these values. The old evaluator code treats
them as an error if it ever encounters one, which should only
happen via `useResolvedIR = False` misrouting.

-}
type Implementation
    = AstImpl (Node Expression)
    | KernelImpl ModuleName String (List Value -> Eval Value)
    | RExprImpl
        { body : IR.RExpr
        , capturedLocals : List Value
        , selfSlots : Int
        }


type alias SharedContext =
    { functions : Dict String (Dict String FunctionImplementation)
    , moduleImports : Dict String ImportedNames
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
