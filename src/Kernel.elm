module Kernel exposing (EvalFunction, functions)

import Array exposing (Array)
import Bitwise
import Core.Array
import Core.Basics
import Core.Bitwise
import Core.Bytes
import Core.Bytes.Decode
import Core.Bytes.Encode
import Core.Char
import Core.Debug
import Core.Dict
import Core.Elm.JsArray
import Core.Elm.Kernel.Parser
import Core.Json.Decode
import Core.Json.Encode
import Core.List
import Core.Regex
import Core.Set
import Core.String
import Elm.Syntax.Expression as Expression exposing (Expression(..), FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Environment
import EvalResult
import FastDict as Dict exposing (Dict)
import Kernel.Basics
import Kernel.Bytes
import Kernel.Debug
import Kernel.Dict
import Kernel.JsArray
import Kernel.Json
import Kernel.List
import Kernel.Parser
import Kernel.Regex
import Kernel.Set
import Kernel.String
import Kernel.Utils
import Maybe.Extra
import Syntax exposing (fakeNode)
import Types exposing (Eval, EvalErrorData, EvalResult, Implementation(..), JsonDecoder(..), Value(..))
import Value exposing (typeError)


type alias EvalFunction =
    List Value
    -> List (Node Pattern)
    -> Int
    -> Maybe QualifiedNameRef
    -> Implementation
    -> Eval Value


functions : EvalFunction -> Dict ModuleName (Dict String ( Int, List Value -> Eval Value ))
functions evalFunction =
    [ -- Elm.Kernel.Basics
      ( [ "Elm", "Kernel", "Basics" ]
      , [ ( "acos", one float to float acos Core.Basics.acos )
        , ( "add", twoNumbers (+) (+) Core.Basics.add )
        , ( "and", two bool bool to bool (&&) Core.Basics.and )
        , ( "asin", one float to float asin Core.Basics.asin )
        , ( "atan", one float to float atan Core.Basics.atan )
        , ( "atan2", two float float to float atan2 Core.Basics.atan2 )
        , ( "ceiling", one float to int ceiling Core.Basics.ceiling )
        , ( "cos", one float to float cos Core.Basics.cos )
        , ( "e", constant float e )
        , ( "fdiv", two float float to float (/) Core.Basics.fdiv )
        , ( "floor", one float to int floor Core.Basics.floor )
        , ( "idiv", twoWithError int int to int Kernel.Basics.idiv Core.Basics.idiv )
        , ( "isInfinite", one float to bool isInfinite Core.Basics.isInfinite )
        , ( "isNaN", one float to bool isNaN Core.Basics.isNaN )
        , ( "log", one float to float (logBase e) log )
        , ( "modBy", twoWithError int int to int Kernel.Basics.modBy Core.Basics.modBy )
        , ( "mul", twoNumbers (*) (*) Core.Basics.mul )
        , ( "not", one bool to bool not Core.Basics.not )
        , ( "or", two bool bool to bool (||) Core.Basics.or )
        , ( "pi", constant float pi )
        , ( "pow", twoNumbers (^) (^) Core.Basics.pow )
        , ( "remainderBy", twoWithError int int to int Kernel.Basics.remainderBy Core.Basics.remainderBy )
        , ( "round", one float to int round Core.Basics.round )
        , ( "sin", one float to float sin Core.Basics.sin )
        , ( "sqrt", one float to float sqrt Core.Basics.sqrt )
        , ( "sub", twoNumbers (-) (-) Core.Basics.sub )
        , ( "tan", one float to float tan Core.Basics.tan )
        , ( "toFloat", one int to float toFloat Core.Basics.toFloat )
        , ( "truncate", one float to int truncate Core.Basics.truncate )
        , ( "xor", two bool bool to bool xor Core.Basics.xor )
        ]
      )

    -- Elm.Kernel.Bitwise
    , ( [ "Elm", "Kernel", "Bitwise" ]
      , [ ( "and", two int int to int Bitwise.and Core.Bitwise.and )
        , ( "complement", one int to int Bitwise.complement Core.Bitwise.complement )
        , ( "or", two int int to int Bitwise.or Core.Bitwise.or )
        , ( "shiftLeftBy", two int int to int Bitwise.shiftLeftBy Core.Bitwise.shiftLeftBy )
        , ( "shiftRightBy", two int int to int Bitwise.shiftRightBy Core.Bitwise.shiftRightBy )
        , ( "shiftRightZfBy", two int int to int Bitwise.shiftRightZfBy Core.Bitwise.shiftRightZfBy )
        , ( "xor", two int int to int Bitwise.xor Core.Bitwise.xor )
        ]
      )

    -- Native Bitwise.* thin-wrapper short circuits.
    , ( [ "Bitwise" ]
      , [ ( "and", two int int to int Bitwise.and Core.Bitwise.and )
        , ( "complement", one int to int Bitwise.complement Core.Bitwise.complement )
        , ( "or", two int int to int Bitwise.or Core.Bitwise.or )
        , ( "shiftLeftBy", two int int to int Bitwise.shiftLeftBy Core.Bitwise.shiftLeftBy )
        , ( "shiftRightBy", two int int to int Bitwise.shiftRightBy Core.Bitwise.shiftRightBy )
        , ( "shiftRightZfBy", two int int to int Bitwise.shiftRightZfBy Core.Bitwise.shiftRightZfBy )
        , ( "xor", two int int to int Bitwise.xor Core.Bitwise.xor )
        ]
      )

    -- Elm.Kernel.Char
    , ( [ "Elm", "Kernel", "Char" ]
      , [ ( "fromCode", one int to char Char.fromCode Core.Char.fromCode )
        , ( "toCode", one char to int Char.toCode Core.Char.toCode )
        , ( "toLocaleLower", one char to char Char.toLocaleLower Core.Char.toLocaleLower )
        , ( "toLocaleUpper", one char to char Char.toLocaleUpper Core.Char.toLocaleUpper )
        , ( "toLower", one char to char Char.toLower Core.Char.toLower )
        , ( "toUpper", one char to char Char.toUpper Core.Char.toUpper )
        ]
      )

    -- Elm.Kernel.Debug
    , ( [ "Elm", "Kernel", "Debug" ]
      , [ ( "log", twoWithError string anything to anything Kernel.Debug.log Core.Debug.log )
        , ( "toString", one anything to string Value.toString Core.Debug.toString )
        , ( "todo", oneWithError string to anything Kernel.Debug.todo Core.Debug.todo )
        ]
      )

    -- Elm.Kernel.Dict
    , ( [ "Elm", "Kernel", "Dict" ]
      , [ ( "foldl", threeWithError (function3 evalFunction anything anything anything to anything) anything anything to anything Kernel.Dict.foldl Core.Dict.foldl )
        , ( "foldr", threeWithError (function3 evalFunction anything anything anything to anything) anything anything to anything Kernel.Dict.foldr Core.Dict.foldr )
        , ( "map", twoWithError (function2 evalFunction anything anything to anything) anything to anything Kernel.Dict.map Core.Dict.map )
        ]
      )

    -- Native Dict.* fast paths. These short-circuit the interpreted RBTree
    -- walk for the hottest user-level Dict operations.
    , ( [ "Dict" ]
      , [ ( "empty", constant anything Kernel.Dict.emptyValue )
        , ( "isEmpty", oneWithError anything to anything Kernel.Dict.isEmpty Core.Dict.isEmpty )
        , ( "size", oneWithError anything to anything Kernel.Dict.size Core.Dict.size )
        , ( "get", twoWithError anything anything to anything Kernel.Dict.get Core.Dict.get )
        , ( "member", twoWithError anything anything to anything Kernel.Dict.member Core.Dict.member )
        , ( "insert", threeWithError anything anything anything to anything Kernel.Dict.insert Core.Dict.insert )
        , ( "remove", twoWithError anything anything to anything Kernel.Dict.remove Core.Dict.remove )
        , ( "union", twoWithError anything anything to anything Kernel.Dict.union Core.Dict.union )
        , ( "fromList", oneWithError anything to anything Kernel.Dict.fromList Core.Dict.fromList )
        , ( "toList", oneWithError anything to anything Kernel.Dict.toList Core.Dict.toList )
        , ( "keys", oneWithError anything to anything Kernel.Dict.keys Core.Dict.keys )
        , ( "values", oneWithError anything to anything Kernel.Dict.values Core.Dict.values )
        ]
      )

    -- Native Set.* fast paths. Same idea as Dict: unwrap the
    -- Set_elm_builtin wrapper and delegate to the host-native Dict
    -- kernel, avoiding the interpreted pattern-match step chain.
    , ( [ "Set" ]
      , [ ( "empty", constant anything Kernel.Set.emptyValue )
        , ( "isEmpty", oneWithError anything to anything Kernel.Set.isEmpty Core.Set.isEmpty )
        , ( "size", oneWithError anything to anything Kernel.Set.size Core.Set.size )
        , ( "member", twoWithError anything anything to anything Kernel.Set.member Core.Set.member )
        , ( "insert", twoWithError anything anything to anything Kernel.Set.insert Core.Set.insert )
        , ( "remove", twoWithError anything anything to anything Kernel.Set.remove Core.Set.remove )
        , ( "union", twoWithError anything anything to anything Kernel.Set.union Core.Set.union )
        , ( "fromList", oneWithError anything to anything Kernel.Set.fromList Core.Set.fromList )
        , ( "toList", oneWithError anything to anything Kernel.Set.toList Core.Set.toList )
        ]
      )

    -- Elm.Kernel.JsArray
    , ( [ "Elm", "Kernel", "JsArray" ]
      , [ ( "appendN", three int (jsArray anything) (jsArray anything) to (jsArray anything) Kernel.JsArray.appendN Core.Elm.JsArray.appendN )
        , ( "empty", zero to (jsArray anything) Array.empty )
        , ( "foldr", threeWithError (function2 evalFunction anything anything to anything) anything (jsArray anything) to anything Kernel.JsArray.foldr Core.Elm.JsArray.foldr )
        , ( "foldl", threeWithError (function2 evalFunction anything anything to anything) anything (jsArray anything) to anything Kernel.JsArray.foldl Core.Elm.JsArray.foldl )
        , ( "initialize", threeWithError int int (function evalFunction int to anything) to (jsArray anything) Kernel.JsArray.initialize Core.Elm.JsArray.initialize )
        , ( "initializeFromList", two int anyList to (tuple (jsArray anything) anyList) Kernel.JsArray.initializeFromList Core.Elm.JsArray.initializeFromList )
        , ( "length", one (jsArray anything) to int Array.length Core.Elm.JsArray.length )
        , ( "map", twoWithError (function evalFunction anything to anything) (jsArray anything) to (jsArray anything) Kernel.JsArray.map Core.Elm.JsArray.map )
        , ( "indexedMap", threeWithError (function2 evalFunction int anything to anything) int (jsArray anything) to (jsArray anything) Kernel.JsArray.indexedMap Core.Elm.JsArray.indexedMap )
        , ( "push", two anything (jsArray anything) to (jsArray anything) Array.push Core.Elm.JsArray.push )
        , ( "slice", three int int (jsArray anything) to (jsArray anything) Array.slice Core.Elm.JsArray.slice )
        , ( "singleton", one anything to (jsArray anything) (List.singleton >> Array.fromList) Core.Elm.JsArray.singleton )
        , ( "unsafeGet", twoWithError int (jsArray anything) to anything Kernel.JsArray.unsafeGet Core.Elm.JsArray.unsafeGet )
        , ( "unsafeSet", three int anything (jsArray anything) to (jsArray anything) Array.set Core.Elm.JsArray.unsafeSet )
        ]
      )

    -- Elm.Kernel.List
    , ( [ "Elm", "Kernel", "List" ]
      , [ ( "append", two anyList anyList to anyList Kernel.List.append Core.List.append )
        , ( "cons", two anything anyList to anyList (::) Core.List.cons )
        , ( "filter", twoWithError (function evalFunction anything to bool) anyList to anyList Kernel.List.filter Core.List.filter )
        , ( "foldl", threeWithError (function2 evalFunction anything anything to anything) anything anyList to anything Kernel.List.foldl Core.List.foldl )
        , ( "foldr", threeWithError (function2 evalFunction anything anything to anything) anything anyList to anything Kernel.List.foldr Core.List.foldr )
        , ( "fromArray", one (jsArray anything) to anyList Array.toList Core.Array.toList )
        , ( "map", twoWithError (function evalFunction anything to anything) anyList to anyList Kernel.List.map Core.List.map )
        , ( "range", two int int to anyList Kernel.List.range Core.List.range )
        , ( "sortBy", twoWithError (function evalFunction anything to anything) anyList to anyList Kernel.List.sortBy Core.List.sortBy )
        , ( "sortWith", twoWithError (function2 evalFunction anything anything to order) anyList to anyList Kernel.List.sortWith Core.List.sortWith )
        , ( "toArray", one anyList to (jsArray anything) Array.fromList Core.Array.fromList )
        , ( "concatMap", twoWithError (function evalFunction anything to anyList) anyList to anyList Kernel.List.concatMap Core.List.concatMap )
        , ( "filterMap", twoWithError (function evalFunction anything to anything) anyList to anyList Kernel.List.filterMap Core.List.filterMap )
        , ( "indexedMap", twoWithError (function2 evalFunction anything anything to anything) anyList to anyList Kernel.List.indexedMap Core.List.indexedMap )
        , ( "any", twoWithError (function evalFunction anything to bool) anyList to bool Kernel.List.any Core.List.any )
        , ( "all", twoWithError (function evalFunction anything to bool) anyList to bool Kernel.List.all Core.List.all )
        ]
      )

    -- Native List.* thin-wrapper short circuits. The Elm stdlib's
    -- `List.foldl func acc list = Elm.Kernel.List.foldl func acc list`
    -- et al. bounce through an extra interpreted wrapper on every
    -- call. Register the kernel implementations directly against
    -- the user-level `["List"]` module so `userModuleKernelFastPath`
    -- catches them and skips the wrapper.
    --
    -- Note: the higher-order entries rely on `Kernel/List.elm`
    -- foldlHelp/mapHelp propagating EvYield/EvMemo* results instead
    -- of collapsing them through `EvalResult.toResult`, otherwise
    -- intercept-driven yields (from e.g. elm-review's Review.Rule
    -- cache markers) silently turn into `"Unhandled EvYield"` errors
    -- and rules quietly report zero findings.
    -- Native List.* thin-wrapper short circuits. The Elm stdlib's
    -- `List.foldl func acc list = Elm.Kernel.List.foldl func acc list`
    -- et al. bounce through an extra interpreted wrapper on every
    -- call; registering kernel implementations directly against the
    -- user-level `["List"]` module lets `userModuleKernelFastPath`
    -- catch them and skip the wrapper.
    --
    -- `List.map` is intentionally excluded. With `map` in the
    -- shortcut, `elm-review`'s `Review.Rule` visitor machinery
    -- silently reports zero errors on real rules. Binary-search
    -- narrowed the bug to specifically `List.map` routed through
    -- this fast path — every other higher-order List helper in the
    -- set (`foldl`, `foldr`, `filter`, `filterMap`, `concatMap`,
    -- `indexedMap`, `any`, `all`, `sortBy`, `sortWith`) produces
    -- correct results. Both the yield-aware and the
    -- `EvalResult.toResult`-based `mapHelp` implementations fail the
    -- same way, so the issue isn't in `Kernel.List.map` itself — it
    -- has to be in how the resulting `PartiallyApplied (KernelImpl)`
    -- value flows into `Review.Rule`'s rule-construction machinery.
    -- Root cause still unknown, but the other shortcuts are a clean
    -- win and the `map` exclusion limits the hit to one function.
    , ( [ "List" ]
      , [ ( "range", two int int to anyList Kernel.List.range Core.List.range )
        , ( "append", two anyList anyList to anyList Kernel.List.append Core.List.append )
        , ( "map", twoWithError (function evalFunction anything to anything) anyList to anyList Kernel.List.map Core.List.map )
        , ( "foldl", threeWithError (function2 evalFunction anything anything to anything) anything anyList to anything Kernel.List.foldl Core.List.foldl )
        , ( "foldr", threeWithError (function2 evalFunction anything anything to anything) anything anyList to anything Kernel.List.foldr Core.List.foldr )
        , ( "filter", twoWithError (function evalFunction anything to bool) anyList to anyList Kernel.List.filter Core.List.filter )
        , ( "filterMap", twoWithError (function evalFunction anything to anything) anyList to anyList Kernel.List.filterMap Core.List.filterMap )
        , ( "concatMap", twoWithError (function evalFunction anything to anyList) anyList to anyList Kernel.List.concatMap Core.List.concatMap )
        , ( "indexedMap", twoWithError (function2 evalFunction anything anything to anything) anyList to anyList Kernel.List.indexedMap Core.List.indexedMap )
        , ( "any", twoWithError (function evalFunction anything to bool) anyList to bool Kernel.List.any Core.List.any )
        , ( "all", twoWithError (function evalFunction anything to bool) anyList to bool Kernel.List.all Core.List.all )
        , ( "sortBy", twoWithError (function evalFunction anything to anything) anyList to anyList Kernel.List.sortBy Core.List.sortBy )
        , ( "sortWith", twoWithError (function2 evalFunction anything anything to order) anyList to anyList Kernel.List.sortWith Core.List.sortWith )

        -- Simple, O(n) helpers that were previously interpreted through
        -- the elm/core AST wrapper — each eval step walks
        -- `lengthHelp accum xs` etc. at interpreter overhead, turning a
        -- `List.length (List.repeat 1000000 5)` call (used by one of
        -- elmcraft/core-extra's isInfixOf "stack safety" tests) into
        -- a ~6-minute job. Routing them straight to host Elm's
        -- `List.length` / `List.repeat` etc. collapses that to
        -- sub-second.
        , ( "length", one anyList to int List.length Core.List.length )
        , ( "repeat", two int anything to anyList List.repeat Core.List.repeat )
        , ( "reverse", one anyList to anyList List.reverse Core.List.reverse )
        , ( "head", one anyList to (maybe anything) List.head Core.List.head )
        , ( "tail", one anyList to (maybe anyList) List.tail Core.List.tail )
        , ( "isEmpty", one anyList to bool List.isEmpty Core.List.isEmpty )
        ]
      )

    -- Elm.Kernel.String
    , ( [ "Elm", "Kernel", "String" ]
      , [ ( "length", one string to int String.length Core.String.length )
        , ( "toFloat", one string to (maybe float) String.toFloat Core.String.toFloat )
        , ( "toInt", one string to (maybe int) String.toInt Core.String.toInt )
        , ( "toLower", one string to string String.toLower Core.String.toLower )
        , ( "toUpper", one string to string String.toUpper Core.String.toUpper )
        , ( "append", two string string to string String.append Core.String.append )
        , ( "cons", two char string to string String.cons Core.String.cons )
        , ( "contains", two string string to bool String.contains Core.String.contains )
        , ( "endsWith", two string string to bool String.endsWith Core.String.endsWith )
        , ( "filter", twoWithError (function evalFunction char to bool) string to string Kernel.String.filter Core.String.filter )
        , ( "foldl", threeWithError (function2 evalFunction char anything to anything) anything string to anything Kernel.String.foldl Core.String.foldl )
        , ( "foldr", threeWithError (function2 evalFunction char anything to anything) anything string to anything Kernel.String.foldr Core.String.foldr )
        , ( "fromList", one (list char) to string String.fromList Core.String.fromList )
        , ( "fromNumber", oneWithError anything to string Kernel.String.fromNumber Core.String.fromFloat ) -- TODO: `fromFloat` is not the same as `fromNumber`
        , ( "indexes", two string string to (list int) String.indexes Core.String.indexes )
        , ( "join", two string (jsArray string) to string (\s a -> String.join s (Array.toList a)) Core.String.join )
        , ( "lines", one string to (list string) String.lines Core.String.lines )
        , ( "reverse", one string to string String.reverse Core.String.reverse )
        , ( "slice", three int int string to string String.slice Core.String.slice )
        , ( "split", two string string to (jsArray string) (\s l -> Array.fromList (String.split s l)) Core.String.split )
        , ( "startsWith", two string string to bool String.startsWith Core.String.startsWith )
        , ( "trim", one string to string String.trim Core.String.trim )
        , ( "trimLeft", one string to string String.trimLeft Core.String.trimLeft )
        , ( "trimRight", one string to string String.trimRight Core.String.trimRight )
        , ( "uncons", one string to (maybe (tuple char string)) String.uncons Core.String.uncons )
        , ( "words", one string to (list string) String.words Core.String.words )
        ]
      )

    -- Native String.* thin-wrapper short circuits. These keep the
    -- user-level wrapper signatures (`List String`, etc.) intact, so
    -- they are safe drop-ins unlike prefix-stripped Elm.Kernel names.
    , ( [ "String" ]
      , [ ( "isEmpty", one string to bool String.isEmpty Core.String.isEmpty )
        , ( "length", one string to int String.length Core.String.length )
        , ( "reverse", one string to string String.reverse Core.String.reverse )
        , ( "repeat", two int string to string String.repeat Core.String.repeat )
        , ( "replace", three string string string to string String.replace Core.String.replace )
        , ( "append", two string string to string String.append Core.String.append )
        , ( "concat", one (list string) to string String.concat Core.String.concat )
        , ( "split", two string string to (list string) String.split Core.String.split )
        , ( "join", two string (list string) to string String.join Core.String.join )
        , ( "words", one string to (list string) String.words Core.String.words )
        , ( "lines", one string to (list string) String.lines Core.String.lines )
        , ( "slice", three int int string to string String.slice Core.String.slice )
        , ( "left", two int string to string String.left Core.String.left )
        , ( "right", two int string to string String.right Core.String.right )
        , ( "dropLeft", two int string to string String.dropLeft Core.String.dropLeft )
        , ( "dropRight", two int string to string String.dropRight Core.String.dropRight )
        , ( "contains", two string string to bool String.contains Core.String.contains )
        , ( "startsWith", two string string to bool String.startsWith Core.String.startsWith )
        , ( "endsWith", two string string to bool String.endsWith Core.String.endsWith )
        , ( "indexes", two string string to (list int) String.indexes Core.String.indexes )
        , ( "toUpper", one string to string String.toUpper Core.String.toUpper )
        , ( "toLower", one string to string String.toLower Core.String.toLower )
        , ( "pad", three int char string to string String.pad Core.String.pad )
        , ( "padLeft", three int char string to string String.padLeft Core.String.padLeft )
        , ( "padRight", three int char string to string String.padRight Core.String.padRight )
        , ( "trim", one string to string String.trim Core.String.trim )
        , ( "trimLeft", one string to string String.trimLeft Core.String.trimLeft )
        , ( "trimRight", one string to string String.trimRight Core.String.trimRight )
        , ( "toInt", one string to (maybe int) String.toInt Core.String.toInt )
        , ( "fromInt", one int to string String.fromInt Core.String.fromInt )
        , ( "toFloat", one string to (maybe float) String.toFloat Core.String.toFloat )
        , ( "fromFloat", oneWithError anything to string stringFromFloat Core.String.fromFloat )
        , ( "toList", one string to (list char) String.toList Core.String.toList )
        , ( "fromList", one (list char) to string String.fromList Core.String.fromList )
        , ( "fromChar", one char to string String.fromChar Core.String.fromChar )
        , ( "cons", two char string to string String.cons Core.String.cons )
        , ( "uncons", one string to (maybe (tuple char string)) String.uncons Core.String.uncons )
        ]
      )

    -- Elm.Kernel.Utils
    , ( [ "Elm", "Kernel", "Utils" ]
      , [ ( "append", twoWithError anything anything to anything Kernel.Utils.append Core.Basics.append )
        , ( "ge", Kernel.Utils.comparison [ GT, EQ ] )
        , ( "gt", Kernel.Utils.comparison [ GT ] )
        , ( "le", Kernel.Utils.comparison [ LT, EQ ] )
        , ( "lt", Kernel.Utils.comparison [ LT ] )
        , ( "equal", Kernel.Utils.comparison [ EQ ] )
        , ( "notEqual", Kernel.Utils.comparison [ LT, GT ] )
        , ( "compare", twoWithError anything anything to order Kernel.Utils.compare Core.Basics.compare )
        ]
      )

    -- Native Basics.* thin-wrapper short circuits. Keep this to
    -- non-higher-order entries; composition helpers still run through
    -- the user-source wrappers for now.
    , ( [ "Basics" ]
      , [ ( "add", twoNumbers (+) (+) Core.Basics.add )
        , ( "sub", twoNumbers (-) (-) Core.Basics.sub )
        , ( "mul", twoNumbers (*) (*) Core.Basics.mul )
        , ( "fdiv", two float float to float (/) Core.Basics.fdiv )
        , ( "idiv", twoWithError int int to int Kernel.Basics.idiv Core.Basics.idiv )
        , ( "pow", twoNumbers (^) (^) Core.Basics.pow )
        , ( "toFloat", one int to float toFloat Core.Basics.toFloat )
        , ( "round", one float to int round Core.Basics.round )
        , ( "floor", one float to int floor Core.Basics.floor )
        , ( "ceiling", one float to int ceiling Core.Basics.ceiling )
        , ( "truncate", one float to int truncate Core.Basics.truncate )
        , ( "eq", Kernel.Utils.comparison [ EQ ] )
        , ( "neq", Kernel.Utils.comparison [ LT, GT ] )
        , ( "lt", Kernel.Utils.comparison [ LT ] )
        , ( "gt", Kernel.Utils.comparison [ GT ] )
        , ( "le", Kernel.Utils.comparison [ LT, EQ ] )
        , ( "ge", Kernel.Utils.comparison [ GT, EQ ] )
        , ( "min", twoWithError anything anything to anything basicsMin Core.Basics.min )
        , ( "max", twoWithError anything anything to anything basicsMax Core.Basics.max )
        , ( "compare", twoWithError anything anything to order Kernel.Utils.compare Core.Basics.compare )
        , ( "not", one bool to bool not Core.Basics.not )
        , ( "and", two bool bool to bool (&&) Core.Basics.and )
        , ( "or", two bool bool to bool (||) Core.Basics.or )
        , ( "xor", two bool bool to bool xor Core.Basics.xor )
        , ( "append", twoWithError anything anything to anything Kernel.Utils.append Core.Basics.append )
        , ( "modBy", twoWithError int int to int Kernel.Basics.modBy Core.Basics.modBy )
        , ( "remainderBy", twoWithError int int to int Kernel.Basics.remainderBy Core.Basics.remainderBy )
        , ( "negate", oneWithError anything to anything basicsNegate Core.Basics.negate )
        , ( "sqrt", one float to float sqrt Core.Basics.sqrt )
        , ( "logBase", two float float to float logBase Core.Basics.logBase )
        , ( "e", constant float e )
        , ( "pi", constant float pi )
        , ( "cos", one float to float cos Core.Basics.cos )
        , ( "sin", one float to float sin Core.Basics.sin )
        , ( "tan", one float to float tan Core.Basics.tan )
        , ( "acos", one float to float acos Core.Basics.acos )
        , ( "asin", one float to float asin Core.Basics.asin )
        , ( "atan", one float to float atan Core.Basics.atan )
        , ( "atan2", two float float to float atan2 Core.Basics.atan2 )
        , ( "isNaN", one float to bool isNaN Core.Basics.isNaN )
        , ( "isInfinite", one float to bool isInfinite Core.Basics.isInfinite )
        ]
      )

    -- Elm.Kernel.Json
    , ( [ "Elm", "Kernel", "Json" ]
      , [ -- Encode
          ( "wrap", one anything to anything Kernel.Json.wrap Core.Json.Encode.string )
        , ( "encode", twoWithError int anything to string Kernel.Json.encode Core.Json.Encode.encode )
        , ( "encodeNull", constant anything Kernel.Json.encodeNull )
        , ( "encodeList", twoWithError (function evalFunction anything to anything) anyList to anything Kernel.Json.encodeList Core.Json.Encode.null )
        , ( "encodeObject", oneWithError anyList to anything Kernel.Json.encodeObject Core.Json.Encode.null )
        , ( "emptyArray", one anything to anything Kernel.Json.emptyArray Core.Json.Encode.null )
        , ( "emptyObject", one anything to anything Kernel.Json.emptyObject Core.Json.Encode.null )
        , ( "addEntry", threeWithError (function evalFunction anything to anything) anything anyList to anyList Kernel.Json.addEntry Core.Json.Encode.null )
        , ( "addField", three string anything anything to anything Kernel.Json.addField Core.Json.Encode.null )

        -- Decode constructors
        , ( "decodeString", constant anything Kernel.Json.decodeString )
        , ( "decodeBool", constant anything Kernel.Json.decodeBool )
        , ( "decodeInt", constant anything Kernel.Json.decodeInt )
        , ( "decodeFloat", constant anything Kernel.Json.decodeFloat )
        , ( "decodeValue", constant anything Kernel.Json.decodeValue )
        , ( "decodeNull", one anything to anything Kernel.Json.decodeNull Core.Json.Decode.null )
        , ( "decodeList", one anything to anything Kernel.Json.decodeList Core.Json.Decode.list )
        , ( "decodeArray", one anything to anything Kernel.Json.decodeArray Core.Json.Decode.array )
        , ( "decodeField", two string anything to anything Kernel.Json.decodeField Core.Json.Decode.field )
        , ( "decodeIndex", two int anything to anything Kernel.Json.decodeIndex Core.Json.Decode.index )
        , ( "decodeKeyValuePairs", one anything to anything Kernel.Json.decodeKeyValuePairs Core.Json.Decode.keyValuePairs )
        , ( "succeed", one anything to anything Kernel.Json.succeed Core.Json.Decode.succeed )
        , ( "fail", one string to anything Kernel.Json.fail Core.Json.Decode.fail )
        , ( "oneOf", one anyList to anything Kernel.Json.oneOf Core.Json.Decode.oneOf )
        , ( "map1", two anything anything to anything Kernel.Json.jsonMap1 Core.Json.Decode.map )
        , ( "map2", three anything anything anything to anything Kernel.Json.jsonMap2 Core.Json.Decode.map2 )
        , ( "map3", jsonMapNEntry 4 )
        , ( "map4", jsonMapNEntry 5 )
        , ( "map5", jsonMapNEntry 6 )
        , ( "map6", jsonMapNEntry 7 )
        , ( "map7", jsonMapNEntry 8 )
        , ( "map8", jsonMapNEntry 9 )
        , ( "andThen", two anything anything to anything Kernel.Json.jsonAndThen Core.Json.Decode.andThen )

        -- Decode runners (need evalFunction)
        , ( "runOnString", jsonRunOnString evalFunction )
        , ( "run", jsonRun evalFunction )
        ]
      )

    -- Elm.Kernel.Regex
    , ( [ "Elm", "Kernel", "Regex" ]
      , [ ( "never", constant anything Kernel.Regex.never )
        , ( "infinity", constant anything Kernel.Regex.infinity )
        , ( "fromStringWith", two anything string to anything Kernel.Regex.fromStringWith Core.Regex.fromStringWith )
        , ( "contains", two anything string to anything Kernel.Regex.contains Core.Regex.contains )
        , ( "findAtMost", three int anything string to anything Kernel.Regex.findAtMost Core.Regex.findAtMost )
        , ( "splitAtMost", three int anything string to anything Kernel.Regex.splitAtMost Core.Regex.splitAtMost )
        , ( "replaceAtMost", regexReplaceAtMost evalFunction )
        ]
      )

    -- Elm.Kernel.Bytes
    , ( [ "Elm", "Kernel", "Bytes" ]
      , [ ( "width", one bytesVal to int Kernel.Bytes.width Core.Bytes.width )
        , ( "encode", oneWithError anything to bytesOut Kernel.Bytes.encodeKernel Core.Bytes.Encode.encode )
        , ( "getStringWidth", one string to int Kernel.Bytes.getStringWidth Core.Bytes.Encode.getStringWidth )
        , ( "decode", bytesDecodeKernel evalFunction )
        , ( "read_i8", bytesRead2 Kernel.Bytes.readI8 Core.Bytes.Decode.signedInt8 )
        , ( "read_u8", bytesRead2 Kernel.Bytes.readU8 Core.Bytes.Decode.unsignedInt8 )
        , ( "read_i16", bytesRead3 Kernel.Bytes.readI16 Core.Bytes.Decode.signedInt16 )
        , ( "read_u16", bytesRead3 Kernel.Bytes.readU16 Core.Bytes.Decode.unsignedInt16 )
        , ( "read_i32", bytesRead3 Kernel.Bytes.readI32 Core.Bytes.Decode.signedInt32 )
        , ( "read_u32", bytesRead3 Kernel.Bytes.readU32 Core.Bytes.Decode.unsignedInt32 )
        , ( "read_f32", bytesRead3 Kernel.Bytes.readF32 Core.Bytes.Decode.float32 )
        , ( "read_f64", bytesRead3 Kernel.Bytes.readF64 Core.Bytes.Decode.float64 )
        , ( "read_bytes", bytesReadN Kernel.Bytes.readBytesChunk Core.Bytes.Decode.bytes )
        , ( "read_string", bytesReadN Kernel.Bytes.readString Core.Bytes.Decode.string )
        , ( "decodeFailure", bytesRead2 Kernel.Bytes.decodeFailure Core.Bytes.Decode.fail )
        , ( "getHostEndianness", twoWithError anything anything to anything Kernel.Bytes.getHostEndianness Core.Bytes.getHostEndianness )
        , ( "write_i8", writeStub Core.Bytes.Encode.write )
        , ( "write_u8", writeStub Core.Bytes.Encode.write )
        , ( "write_i16", writeStub4 Core.Bytes.Encode.write )
        , ( "write_u16", writeStub4 Core.Bytes.Encode.write )
        , ( "write_i32", writeStub4 Core.Bytes.Encode.write )
        , ( "write_u32", writeStub4 Core.Bytes.Encode.write )
        , ( "write_f32", writeStub4 Core.Bytes.Encode.write )
        , ( "write_f64", writeStub4 Core.Bytes.Encode.write )
        , ( "write_string", writeStub Core.Bytes.Encode.write )
        , ( "write_bytes", writeStub Core.Bytes.Encode.write )
        ]
      )

    -- Elm.Kernel.Parser
    , ( [ "Elm", "Kernel", "Parser" ]
      , [ ( "isSubString", parserIsSubString )
        , ( "isSubChar", parserIsSubChar evalFunction )
        , ( "isAsciiCode", three int int string to bool Kernel.Parser.isAsciiCode Core.Elm.Kernel.Parser.isAsciiCode )
        , ( "chompBase10", two int string to int Kernel.Parser.chompBase10 Core.Elm.Kernel.Parser.chompBase10 )
        , ( "consumeBase", parserConsumeBase )
        , ( "consumeBase16", parserConsumeBase16 )
        , ( "findSubString", parserFindSubString )
        ]
      )

    -- Native Random.next / Random.peel / Random.int / Random.float fast paths.
    -- elm/random is a tight bit-math module but each primitive goes through
    -- the AST evaluator per call, which dominates fuzz-test throughput.
    -- Random.int and Random.float construct a Generator wrapping a native
    -- KernelImpl closure — when Random.step (still in user source) unwraps
    -- and applies it, the kernel runs host-Elm bit math directly with lo/hi
    -- captured as oldArgs on the PartiallyApplied.
    , ( [ "Random" ]
      , [ ( "next", randomNextKernel )
        , ( "peel", randomPeelKernel )
        , ( "int", randomIntKernel )
        , ( "float", randomFloatKernel )
        ]
      )

    -- Native Fuzz.Float.reorderExponent. The user-source version builds a
    -- 2048-element sorted Array every call (because top-level constants are
    -- re-evaluated on every reference in the interpreter). At ~1.4s per build
    -- that turns Fuzz.niceFloat into a ~1.3s-per-value operation. The kernel
    -- uses a true host-Elm module-level constant, so the sort runs once at
    -- load time and every call is just `Array.get`.
    , ( [ "Fuzz", "Float" ]
      , [ ( "reorderExponent", fuzzFloatReorderExponentKernel )
        ]
      )
    ]
        |> List.map
            (\( moduleName, moduleFunctions ) ->
                ( moduleName
                , moduleFunctions
                    |> List.map (\( k, f ) -> ( k, f moduleName ))
                    |> Dict.fromList
                )
            )
        |> Dict.fromList


basicsMin : Value -> Value -> Eval Value
basicsMin left right cfg env =
    Kernel.Utils.compare left right cfg env
        |> EvalResult.map
            (\ordering ->
                case ordering of
                    LT ->
                        left

                    _ ->
                        right
            )


basicsMax : Value -> Value -> Eval Value
basicsMax left right cfg env =
    Kernel.Utils.compare left right cfg env
        |> EvalResult.map
            (\ordering ->
                case ordering of
                    GT ->
                        left

                    _ ->
                        right
            )


basicsNegate : Value -> Eval Value
basicsNegate value _ env =
    case value of
        Int intValue ->
            EvalResult.succeed (Int (negate intValue))

        Float floatValue ->
            EvalResult.succeed (Float (negate floatValue))

        _ ->
            EvalResult.fail <| typeError env <| "Cannot negate " ++ Value.toString value


stringFromFloat : Value -> Eval String
stringFromFloat value _ env =
    case value of
        Float floatValue ->
            EvalResult.succeed (String.fromFloat floatValue)

        _ ->
            EvalResult.fail <| typeError env <| "Expected a Float, got " ++ Value.toString value


log : FunctionImplementation
log =
    { name = fakeNode "log"
    , arguments = [ fakeNode <| VarPattern "$x" ]
    , expression =
        fakeNode <|
            Expression.Application
                [ Core.Basics.logBase.expression
                , fakeNode <| FunctionOrValue [] "e"
                , fakeNode <| FunctionOrValue [] "$x"
                ]
    }


{-| Native Random.next. Unwraps a Random.Seed Int Int custom value,
runs the PCG transition, and rewraps. Matches elm/random's source exactly:

    next (Seed state0 incr) =
        Seed (Bitwise.shiftRightZfBy 0 ((state0 * 1664525) + incr)) incr

-}
randomNextKernel : ModuleName -> ( Int, List Value -> Eval Value )
randomNextKernel _ =
    ( 1
    , \args _ env ->
        case args of
            [ seedValue ] ->
                case seedValue of
                    Custom ref [ Int state0, Int incr ] ->
                        if ref.moduleName == [ "Random" ] && ref.name == "Seed" then
                            EvalResult.succeed
                                (Custom ref
                                    [ Int (Bitwise.shiftRightZfBy 0 ((state0 * 1664525) + incr))
                                    , Int incr
                                    ]
                                )

                        else
                            EvalResult.fail <|
                                typeError env <|
                                    "Random.next (kernel): expected Random.Seed, got "
                                        ++ Value.toString seedValue

                    _ ->
                        EvalResult.fail <|
                            typeError env <|
                                "Random.next (kernel): expected Random.Seed, got "
                                    ++ Value.toString seedValue

            _ ->
                EvalResult.fail <|
                    typeError env "Random.next (kernel): expected exactly one arg"
    )


{-| Precomputed exponent mapping for Fuzz.Float.reorderExponent.

The user-source `exponentMapping` in elm-explorations/test is a top-level
`Array Int` produced by sorting `List.range 0 2047` by `exponentKey`. Because
the interpreter re-evaluates every reference to a top-level constant, each
call into `Fuzz.niceFloat` was rebuilding this 2048-element sorted array —
roughly 1.4 seconds of interpreted work per value.

Defining it here makes it a real host-Elm module-level constant: computed
once at Kernel module load time, then looked up in O(log n) via `Array.get`.

-}
fuzzFloatExponentMapping : Array Int
fuzzFloatExponentMapping =
    List.range 0 fuzzFloatMaxExponent
        |> List.sortBy fuzzFloatExponentKey
        |> Array.fromList


fuzzFloatMaxExponent : Int
fuzzFloatMaxExponent =
    0x07FF


{-| Matches Fuzz.Float.exponentKey exactly.
-}
fuzzFloatExponentKey : Int -> Int
fuzzFloatExponentKey e =
    if e == fuzzFloatMaxExponent then
        round (1 / 0)

    else
        let
            unbiased : Int
            unbiased =
                e - 1023
        in
        if unbiased < 0 then
            10000 - unbiased

        else
            unbiased


fuzzFloatReorderExponentKernel : ModuleName -> ( Int, List Value -> Eval Value )
fuzzFloatReorderExponentKernel _ =
    ( 1
    , \args _ env ->
        case args of
            [ Int e ] ->
                case Array.get e fuzzFloatExponentMapping of
                    Just v ->
                        EvalResult.succeed (Int v)

                    Nothing ->
                        EvalResult.succeed (Int 0)

            _ ->
                EvalResult.fail <|
                    typeError env "Fuzz.Float.reorderExponent (kernel): expected one Int"
    )


{-| Native Random.peel. Matches elm/random's source exactly:

    peel (Seed state _) =
        let
            word =
                Bitwise.xor state (Bitwise.shiftRightZfBy (Bitwise.shiftRightZfBy 28 state + 4) state) * 277803737
        in
        Bitwise.shiftRightZfBy 0 (Bitwise.xor (Bitwise.shiftRightZfBy 22 word) word)

-}
randomPeelKernel : ModuleName -> ( Int, List Value -> Eval Value )
randomPeelKernel _ =
    ( 1
    , \args _ env ->
        case args of
            [ seedValue ] ->
                case seedValue of
                    Custom ref [ Int state, _ ] ->
                        if ref.moduleName == [ "Random" ] && ref.name == "Seed" then
                            EvalResult.succeed (Int (peelStateRaw state))

                        else
                            EvalResult.fail <|
                                typeError env <|
                                    "Random.peel (kernel): expected Random.Seed, got "
                                        ++ Value.toString seedValue

                    _ ->
                        EvalResult.fail <|
                            typeError env <|
                                "Random.peel (kernel): expected Random.Seed, got "
                                    ++ Value.toString seedValue

            _ ->
                EvalResult.fail <|
                    typeError env "Random.peel (kernel): expected exactly one arg"
    )


{-| Host-Elm peel: extracts a pseudo-random Int from the state. Shared by
`randomPeelKernel`, `randomIntStepKernel`, and `randomFloatStepKernel`.
-}
peelStateRaw : Int -> Int
peelStateRaw state =
    let
        shiftAmount : Int
        shiftAmount =
            Bitwise.shiftRightZfBy 28 state + 4

        word : Int
        word =
            Bitwise.xor state (Bitwise.shiftRightZfBy shiftAmount state) * 277803737
    in
    Bitwise.shiftRightZfBy 0 (Bitwise.xor (Bitwise.shiftRightZfBy 22 word) word)


{-| Host-Elm next: advances the PCG state.
-}
nextStateRaw : Int -> Int -> Int
nextStateRaw state0 incr =
    Bitwise.shiftRightZfBy 0 ((state0 * 1664525) + incr)


seedRef : QualifiedNameRef
seedRef =
    { moduleName = [ "Random" ], name = "Seed" }


generatorRef : QualifiedNameRef
generatorRef =
    { moduleName = [ "Random" ], name = "Generator" }


{-| Native Random.int a b.

Returns `Generator (lo, hi, seed -> (Int, Seed))` — wraps a `PartiallyApplied`
whose body is `KernelImpl "Random" "intStep" intStepKernel`. The lo/hi are
stored as `oldArgs` so when `Random.step` (pure Elm source) unwraps the
Generator and applies the closure to the seed, the interpreter concatenates
oldArgs ++ [seed] and hands all three to `intStepKernel` in one shot.

-}
randomIntKernel : ModuleName -> ( Int, List Value -> Eval Value )
randomIntKernel _ =
    ( 2
    , \args _ env ->
        case args of
            [ Int a, Int b ] ->
                EvalResult.succeed (makeNativeGenerator (Int a) (Int b) intStepKernelImpl)

            _ ->
                EvalResult.fail <|
                    typeError env "Random.int (kernel): expected two Ints"
    )


randomFloatKernel : ModuleName -> ( Int, List Value -> Eval Value )
randomFloatKernel _ =
    ( 2
    , \args _ env ->
        case args of
            [ firstArg, secondArg ] ->
                let
                    a : Maybe Float
                    a =
                        case firstArg of
                            Float f ->
                                Just f

                            Int i ->
                                Just (toFloat i)

                            _ ->
                                Nothing

                    b : Maybe Float
                    b =
                        case secondArg of
                            Float f ->
                                Just f

                            Int i ->
                                Just (toFloat i)

                            _ ->
                                Nothing
                in
                case ( a, b ) of
                    ( Just fa, Just fb ) ->
                        EvalResult.succeed (makeNativeGenerator (Float fa) (Float fb) floatStepKernelImpl)

                    _ ->
                        EvalResult.fail <|
                            typeError env "Random.float (kernel): expected two Floats"

            _ ->
                EvalResult.fail <|
                    typeError env "Random.float (kernel): expected two Floats"
    )


{-| Wrap (a, b, seed -> result) as a Generator Custom containing a
PartiallyApplied with KernelImpl.
-}
makeNativeGenerator : Value -> Value -> (List Value -> Eval Value) -> Value
makeNativeGenerator a b stepKernelFn =
    Custom generatorRef
        [ PartiallyApplied
            (Environment.empty [ "Random" ])
            [ a, b ]
            [ fakeNode (VarPattern "$a"), fakeNode (VarPattern "$b"), fakeNode (VarPattern "$seed") ]
            (Just { moduleName = [ "Random" ], name = "nativeGenerator" })
            (KernelImpl [ "Random" ] "nativeGeneratorStep" stepKernelFn)
            3
        ]


{-| Native Random.int's step. Given [lo, hi, seed] returns `(Int, Seed)`.
Implements exactly what elm/random's `int` closure body does.
-}
intStepKernelImpl : List Value -> Eval Value
intStepKernelImpl args _ env =
    case args of
        [ Int ra, Int rb, seedValue ] ->
            case seedValue of
                Custom _ [ Int state0, Int incr ] ->
                    let
                        ( lo, hi ) =
                            if ra < rb then
                                ( ra, rb )

                            else
                                ( rb, ra )

                        range : Int
                        range =
                            hi - lo + 1

                        nextSeed : Value
                        nextSeed =
                            Custom seedRef [ Int (nextStateRaw state0 incr), Int incr ]
                    in
                    if Bitwise.and (range - 1) range == 0 then
                        -- Power-of-2 fast path
                        EvalResult.succeed
                            (Tuple
                                (Int (Bitwise.shiftRightZfBy 0 (Bitwise.and (range - 1) (peelStateRaw state0)) + lo))
                                nextSeed
                            )

                    else
                        -- Rejection sampling path
                        let
                            threshhold : Int
                            threshhold =
                                Bitwise.shiftRightZfBy 0 (remainderBy range (Bitwise.shiftRightZfBy 0 -range))

                            ( value, finalState, finalIncr ) =
                                accountForBiasLoop threshhold range lo state0 incr
                        in
                        EvalResult.succeed
                            (Tuple
                                (Int value)
                                (Custom seedRef [ Int finalState, Int finalIncr ])
                            )

                _ ->
                    EvalResult.fail <|
                        typeError env <|
                            "Random.int step (kernel): expected Random.Seed, got "
                                ++ Value.toString seedValue

        _ ->
            EvalResult.fail <|
                typeError env "Random.int step (kernel): wrong argument count"


{-| Rejection sampling loop for the non-power-of-2 case. Recurses in host Elm
instead of the interpreter trampoline.
-}
accountForBiasLoop : Int -> Int -> Int -> Int -> Int -> ( Int, Int, Int )
accountForBiasLoop threshhold range lo state incr =
    let
        x : Int
        x =
            peelStateRaw state

        newState : Int
        newState =
            nextStateRaw state incr
    in
    if x < threshhold then
        accountForBiasLoop threshhold range lo newState incr

    else
        ( remainderBy range x + lo, newState, incr )


{-| Native Random.float's step. Given [lo, hi, seed] returns `(Float, Seed)`.
Implements exactly what elm/random's `float` closure body does.
-}
floatStepKernelImpl : List Value -> Eval Value
floatStepKernelImpl args _ env =
    case args of
        [ firstBound, secondBound, seedValue ] ->
            let
                a : Maybe Float
                a =
                    case firstBound of
                        Float f ->
                            Just f

                        Int i ->
                            Just (toFloat i)

                        _ ->
                            Nothing

                b : Maybe Float
                b =
                    case secondBound of
                        Float f ->
                            Just f

                        Int i ->
                            Just (toFloat i)

                        _ ->
                            Nothing
            in
            case ( a, b, seedValue ) of
                ( Just fa, Just fb, Custom _ [ Int state0, Int incr ] ) ->
                    let
                        state1 : Int
                        state1 =
                            nextStateRaw state0 incr

                        n0 : Int
                        n0 =
                            peelStateRaw state0

                        n1 : Int
                        n1 =
                            peelStateRaw state1

                        hi : Float
                        hi =
                            toFloat (Bitwise.and 0x03FFFFFF n0) * 1.0

                        loF : Float
                        loF =
                            toFloat (Bitwise.and 0x07FFFFFF n1) * 1.0

                        val : Float
                        val =
                            (hi * 134217728.0 + loF) / 9007199254740992.0

                        range : Float
                        range =
                            abs (fb - fa)

                        scaled : Float
                        scaled =
                            val * range + fa

                        finalState : Int
                        finalState =
                            nextStateRaw state1 incr
                    in
                    EvalResult.succeed
                        (Tuple
                            (Float scaled)
                            (Custom seedRef [ Int finalState, Int incr ])
                        )

                _ ->
                    EvalResult.fail <|
                        typeError env "Random.float step (kernel): expected (Float, Float, Seed)"

        _ ->
            EvalResult.fail <|
                typeError env "Random.float step (kernel): wrong argument count"



-- Selectors


type alias InSelector a x =
    { x
        | fromValue : Value -> Maybe a
        , name : String
    }


type alias OutSelector a x =
    { x
        | toValue : a -> Value
        , name : String
    }


type alias Selector a =
    InSelector a (OutSelector a {})


type To
    = To


to : To
to =
    To


anything : Selector Value
anything =
    { fromValue = Just
    , toValue = identity
    , name = "anything"
    }


order : Selector Order
order =
    { fromValue = Value.toOrder
    , toValue = Value.fromOrder
    , name = "Order"
    }


string : Selector String
string =
    { fromValue =
        \value ->
            case value of
                String s ->
                    Just s

                _ ->
                    Nothing
    , toValue = String
    , name = "String"
    }


float : Selector Float
float =
    { fromValue =
        \value ->
            case value of
                Float s ->
                    Just s

                Int i ->
                    -- Stuff like "2 / 3" is parsed as (Int 2) / (Int 3)
                    Just (toFloat i)

                _ ->
                    Nothing
    , toValue = Float
    , name = "Float"
    }


int : Selector Int
int =
    { fromValue =
        \value ->
            case value of
                Int s ->
                    Just s

                _ ->
                    Nothing
    , toValue = Int
    , name = "Int"
    }


char : Selector Char
char =
    { fromValue =
        \value ->
            case value of
                Char s ->
                    Just s

                _ ->
                    Nothing
    , toValue = Char
    , name = "Char"
    }


bytesVal : InSelector (Array Int) {}
bytesVal =
    { fromValue =
        \value ->
            case value of
                BytesValue arr ->
                    Just arr

                _ ->
                    Nothing
    , name = "Bytes"
    }


bytesOut : OutSelector (Array Int) {}
bytesOut =
    { toValue = BytesValue
    , name = "Bytes"
    }


bool : Selector Bool
bool =
    { fromValue =
        \value ->
            case value of
                Bool s ->
                    Just s

                _ ->
                    Nothing
    , toValue = Bool
    , name = "Bool"
    }


maybe : Selector a -> Selector (Maybe a)
maybe selector =
    { fromValue =
        \value ->
            case value of
                Custom ctor args ->
                    case ( ctor.moduleName, ctor.name, args ) of
                        ( [ "Maybe" ], "Nothing", [] ) ->
                            Just Nothing

                        ( [ "Maybe" ], "Just", [ arg ] ) ->
                            Maybe.map Just (selector.fromValue arg)

                        _ ->
                            Nothing

                _ ->
                    Nothing
    , toValue =
        \maybeValue ->
            case maybeValue of
                Nothing ->
                    Value.nothingValue

                Just value ->
                    Custom { moduleName = [ "Maybe" ], name = "Just" } [ selector.toValue value ]
    , name = "Maybe " ++ selector.name
    }


list : Selector a -> Selector (List a)
list selector =
    { fromValue =
        \value ->
            case value of
                List l ->
                    Maybe.Extra.combineMap selector.fromValue l

                _ ->
                    Nothing
    , toValue =
        \value ->
            value
                |> List.map selector.toValue
                |> List
    , name = "List " ++ selector.name
    }


anyList : Selector (List Value)
anyList =
    { fromValue =
        \value ->
            case value of
                List l ->
                    Just l

                _ ->
                    Nothing
    , toValue = List
    , name = "List anything"
    }


jsArray : Selector a -> Selector (Array a)
jsArray selector =
    { fromValue =
        \value ->
            case value of
                JsArray jsa ->
                    jsa
                        |> Array.toList
                        |> Maybe.Extra.combineMap selector.fromValue
                        |> Maybe.map Array.fromList

                _ ->
                    Nothing
    , toValue =
        \array ->
            array
                |> Array.map selector.toValue
                |> JsArray

    -- User-facing name: Elm users see `List X` in their source (the host
    -- `JsArray` representation is an internal optimization). Show "List"
    -- in type-error messages so the text matches what's in their code.
    , name = "List " ++ selector.name
    }


{-| Selector that extracts a "callable" from any function-typed `Value`
the interpreter can produce.

**IMPORTANT for anyone adding kernel wrappers or new selectors:** there
are _two_ Value representations a user can legitimately pass as a
function, and this selector (and anything analogous) MUST handle both:

1.  `PartiallyApplied` — anonymous lambdas, top-level functions, let
    bindings, any closure produced by `evalFunction` /
    `mkPartiallyApplied`.

2.  `Custom ref customArgs` — a partially-applied _type constructor_
    (`Node Range.emptyRange`, `Just`, `Tuple.pair 1`, etc.). These show
    up whenever user code passes a constructor as a higher-order
    argument, e.g.
    `xs |> List.map (Node Range.emptyRange)`. The interpreter stores
    this the same way a fully-applied constructor is stored — as
    `Custom ref [Range.emptyRange]` — because `evalApplication` treats
    Custom application as "just append the new arg to the args list"
    (see `evalApplication`'s `Custom name customArgs ->` branch in
    `Eval/Expression.elm`).

Missing the `Custom` branch was the root cause of the `List.map`
shortcut correctness bug fixed in round 7 — `Review.ModuleNameLookupTable.Compute`
calls `List.map (Node Range.emptyRange) …`, which silently failed with
a type error ("Expected first argument to be anything -> anything, got
Node {…}") before the error was swallowed by
`runProjectRulesFresh`'s error-to-string path.

When in doubt, cross-reference this selector with
`Kernel.Json.applyFunction`, which is the other place in the codebase
that goes "Value → callable" and has always handled both branches.

-}
function :
    EvalFunction
    -> OutSelector from xf
    -> To
    -> InSelector to xt
    -> InSelector (from -> Eval to) {}
function evalFunctionWith inSelector _ outSelector =
    let
        fromValue : Value -> Maybe (from -> Eval to)
        fromValue value =
            case value of
                PartiallyApplied localEnv oldArgs patterns maybeName implementation cachedArity ->
                    Just
                        (\arg cfg _ ->
                            evalFunctionWith (oldArgs ++ [ inSelector.toValue arg ]) patterns cachedArity maybeName implementation cfg localEnv
                                |> EvalResult.onValue
                                    (\out ->
                                        case outSelector.fromValue out of
                                            Just ov ->
                                                Ok ov

                                            Nothing ->
                                                Err <|
                                                    typeError localEnv <|
                                                        "Could not convert output from "
                                                            ++ Value.toString out
                                                            ++ " to "
                                                            ++ outSelector.name
                                    )
                        )

                Custom ref customArgs ->
                    -- Constructor partial application: `Node Range.emptyRange`
                    -- is stored as `Custom { ..., name = "Node" } [Range.emptyRange]`.
                    -- When used as a higher-order function (e.g. `List.map (Node
                    -- Range.emptyRange) xs`), applying it to one more argument
                    -- must produce `Custom ref (customArgs ++ [arg])` — the same
                    -- behavior `evalApplication`'s Custom branch implements.
                    Just
                        (\arg _ env ->
                            let
                                newValue : Value
                                newValue =
                                    Custom ref (customArgs ++ [ inSelector.toValue arg ])
                            in
                            case outSelector.fromValue newValue of
                                Just ov ->
                                    EvalResult.succeed ov

                                Nothing ->
                                    EvalResult.fail <|
                                        typeError env <|
                                            "Could not convert constructor output "
                                                ++ Value.toString newValue
                                                ++ " to "
                                                ++ outSelector.name
                        )

                _ ->
                    Nothing
    in
    { name = inSelector.name ++ " -> " ++ outSelector.name
    , fromValue = fromValue
    }


function2 :
    EvalFunction
    -> OutSelector a xa
    -> OutSelector b xb
    -> To
    -> InSelector to xt
    -> InSelector (a -> Eval (b -> Eval to)) {}
function2 evalFunction in1Selector in2Selector _ outSelector =
    function evalFunction in1Selector to (function evalFunction in2Selector to outSelector)


function3 :
    EvalFunction
    -> OutSelector a xa
    -> OutSelector b xb
    -> OutSelector c xc
    -> To
    -> InSelector to xt
    -> InSelector (a -> Eval (b -> Eval (c -> Eval to))) {}
function3 evalFunction in1Selector in2Selector in3Selector _ outSelector =
    function evalFunction in1Selector to (function evalFunction in2Selector to (function evalFunction in3Selector to outSelector))


tuple : Selector a -> Selector b -> Selector ( a, b )
tuple firstSelector secondSelector =
    { fromValue =
        \value ->
            case value of
                Tuple first second ->
                    Maybe.map2 Tuple.pair (firstSelector.fromValue first) (secondSelector.fromValue second)

                _ ->
                    Nothing
    , toValue =
        \( first, second ) ->
            Tuple (firstSelector.toValue first) (secondSelector.toValue second)
    , name = "( " ++ firstSelector.name ++ ", " ++ secondSelector.name ++ ")"
    }


constant : OutSelector res x -> res -> ModuleName -> ( Int, List Value -> Eval Value )
constant selector const _ =
    ( 0
    , \args _ env ->
        case args of
            [] ->
                EvalResult.succeed <| selector.toValue const

            _ ->
                EvalResult.fail <| typeError env <| "Didn't expect any args"
    )


zero :
    To
    -> OutSelector out ox
    -> out
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
zero _ output f =
    zeroWithError To output (Ok f)


zeroWithError :
    To
    -> OutSelector out ox
    -> Result EvalErrorData out
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
zeroWithError _ output f _ =
    ( 0
    , \args _ env ->
        case args of
            [] ->
                EvalResult.fromResult <| Result.map output.toValue f

            _ ->
                EvalResult.fail <| typeError env <| "Expected zero args, got more"
    )


one :
    InSelector a ax
    -> To
    -> OutSelector out ox
    -> (a -> out)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
one firstSelector _ output f =
    oneWithError firstSelector To output (\v _ _ -> EvalResult.succeed (f v))


oneWithError :
    InSelector a xa
    -> To
    -> OutSelector out xo
    -> (a -> Eval out)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
oneWithError firstSelector _ output f implementation moduleName =
    ( 1
    , \args cfg env ->
        let
            err : String -> EvalResult value
            err got =
                EvalResult.fail <| typeError env <| "Expected one " ++ firstSelector.name ++ ", got " ++ got
        in
        case args of
            [ arg ] ->
                case firstSelector.fromValue arg of
                    Just s ->
                        f s cfg env
                            |> EvalResult.map output.toValue

                    Nothing ->
                        err (Value.toString arg)

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                err "more"
    )


two :
    InSelector a xa
    -> InSelector b xb
    -> To
    -> OutSelector out xo
    -> (a -> b -> out)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
two firstSelector secondSelector _ output f =
    twoWithError firstSelector secondSelector To output (\l r _ _ -> EvalResult.succeed (f l r))


twoWithError :
    InSelector a xa
    -> InSelector b xb
    -> To
    -> OutSelector out xo
    -> (a -> b -> Eval out)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
twoWithError firstSelector secondSelector _ output f implementation moduleName =
    ( 2
    , \args cfg env ->
        let
            typeError_ : String -> EvalResult value
            typeError_ msg =
                EvalResult.fail (typeError env msg)
        in
        case args of
            [ firstArg, secondArg ] ->
                case firstSelector.fromValue firstArg of
                    Nothing ->
                        typeError_ <| "Expected the first argument to be " ++ firstSelector.name ++ ", got " ++ Value.toString firstArg

                    Just first ->
                        case secondSelector.fromValue secondArg of
                            Nothing ->
                                typeError_ <| "Expected the second argument to be " ++ secondSelector.name ++ ", got " ++ Value.toString secondArg

                            Just second ->
                                f first second cfg env
                                    |> EvalResult.map output.toValue

            [ _ ] ->
                partiallyApply moduleName args implementation

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                let
                    got : String
                    got =
                        String.join ", " <| List.map Value.toString args
                in
                if firstSelector.name == secondSelector.name then
                    typeError_ <| "Expected two " ++ firstSelector.name ++ "s, got " ++ got

                else
                    typeError_ <| "Expected one " ++ firstSelector.name ++ " and one " ++ secondSelector.name ++ ", got " ++ got
    )


three :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> To
    -> OutSelector out xo
    -> (a -> b -> c -> out)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
three firstSelector secondSelector thirdSelector _ output f =
    threeWithError firstSelector secondSelector thirdSelector To output (\l m r _ _ -> EvalResult.succeed (f l m r))


threeWithError :
    InSelector a xa
    -> InSelector b xb
    -> InSelector c xc
    -> To
    -> OutSelector out xo
    -> (a -> b -> c -> Eval out)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
threeWithError firstSelector secondSelector thirdSelector _ output f implementation moduleName =
    ( 3
    , \args cfg env ->
        let
            err : String -> EvalResult value
            err got =
                if firstSelector.name == secondSelector.name && secondSelector.name == thirdSelector.name then
                    EvalResult.fail <| typeError env <| "Expected three " ++ firstSelector.name ++ "s, got " ++ got

                else
                    EvalResult.fail <| typeError env <| "Expected one " ++ firstSelector.name ++ ", one " ++ secondSelector.name ++ " and one " ++ thirdSelector.name ++ ", got " ++ got
        in
        case args of
            [ firstArg, secondArg, thirdArg ] ->
                case ( firstSelector.fromValue firstArg, secondSelector.fromValue secondArg, thirdSelector.fromValue thirdArg ) of
                    ( Just first, Just second, Just third ) ->
                        f first second third cfg env
                            |> EvalResult.map output.toValue

                    _ ->
                        err (String.join ", " (List.map Value.toString args))

            [ _, _ ] ->
                partiallyApply moduleName args implementation

            [ _ ] ->
                partiallyApply moduleName args implementation

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                err ("[ " ++ String.join ", " (List.map Value.toString args) ++ " ]")
    )


partiallyApply : ModuleName -> List Value -> FunctionImplementation -> EvalResult Value
partiallyApply moduleName args implementation =
    EvalResult.fromResult <|
        Ok <|
            PartiallyApplied
                (Environment.empty moduleName)
                args
                implementation.arguments
                (Just
                    { moduleName = moduleName
                    , name = Node.value implementation.name
                    }
                )
                (AstImpl implementation.expression)
                (List.length implementation.arguments)


twoNumbers :
    (Int -> Int -> Int)
    -> (Float -> Float -> Float)
    -> FunctionImplementation
    -> ModuleName
    -> ( Int, List Value -> Eval Value )
twoNumbers fInt fFloat implementation moduleName =
    ( 2
    , \args _ env ->
        case args of
            [ Int li, Int ri ] ->
                EvalResult.succeed <| Int (fInt li ri)

            [ Int li, Float rf ] ->
                EvalResult.succeed <| Float (fFloat (toFloat li) rf)

            [ Float lf, Int ri ] ->
                EvalResult.succeed <| Float (fFloat lf (toFloat ri))

            [ Float lf, Float rf ] ->
                EvalResult.succeed <| Float (fFloat lf rf)

            [ _ ] ->
                partiallyApply moduleName args implementation

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                EvalResult.fail <| typeError env "Expected two numbers"
    )


{-| Custom kernel entry for Json.Decode.runOnString.
Captures evalFunction so the decoder engine can apply Elm functions.
-}
jsonRunOnString : EvalFunction -> ModuleName -> ( Int, List Value -> Eval Value )
jsonRunOnString evalFn _ =
    ( 2
    , \args cfg env ->
        case args of
            [ decoderVal, String jsonString ] ->
                Kernel.Json.runOnString evalFn decoderVal jsonString cfg env

            [ _ ] ->
                EvalResult.fail <| typeError env "Json.Decode.decodeString needs two arguments"

            [] ->
                EvalResult.fail <| typeError env "Json.Decode.decodeString needs two arguments"

            _ ->
                EvalResult.fail <| typeError env "Json.Decode.decodeString: expected a Decoder and a String"
    )


{-| Custom kernel entry for Json.Decode.decodeValue.
-}
jsonRun : EvalFunction -> ModuleName -> ( Int, List Value -> Eval Value )
jsonRun evalFn _ =
    ( 2
    , \args cfg env ->
        case args of
            [ decoderVal, jsonVal ] ->
                Kernel.Json.run evalFn decoderVal jsonVal cfg env

            [ _ ] ->
                EvalResult.fail <| typeError env "Json.Decode.decodeValue needs two arguments"

            [] ->
                EvalResult.fail <| typeError env "Json.Decode.decodeValue needs two arguments"

            _ ->
                EvalResult.fail <| typeError env "Json.Decode.decodeValue: expected a Decoder and a Value"
    )


{-| Generic kernel entry for Json.Decode.mapN (N >= 3).
The first arg is the function, rest are decoders.
-}
jsonMapNEntry : Int -> ModuleName -> ( Int, List Value -> Eval Value )
jsonMapNEntry arity _ =
    ( arity
    , \args _ env ->
        case args of
            func :: decoderArgs ->
                if List.length args == arity then
                    let
                        extractDecoder : Value -> JsonDecoder
                        extractDecoder v =
                            case v of
                                JsonDecoderValue d ->
                                    d

                                _ ->
                                    DecodeFail "mapN: not a decoder"
                    in
                    EvalResult.succeed (JsonDecoderValue (DecodeMap func (List.map extractDecoder decoderArgs)))

                else
                    EvalResult.fail <| typeError env ("Expected " ++ String.fromInt arity ++ " arguments for mapN")

            _ ->
                EvalResult.fail <| typeError env ("Expected " ++ String.fromInt arity ++ " arguments for mapN")
    )


{-| Custom kernel entry for Regex.replaceAtMost.
Takes n, regex, replacer (function), string. Needs evalFunction for the replacer callback.
-}
regexReplaceAtMost : EvalFunction -> ModuleName -> ( Int, List Value -> Eval Value )
regexReplaceAtMost evalFn _ =
    ( 4
    , \args cfg env ->
        case args of
            [ Int n, regexVal, replacerVal, String str ] ->
                case replacerVal of
                    PartiallyApplied closureEnv oldArgs patterns maybeName implementation cachedArity ->
                        let
                            applyReplacer : Value -> Eval Value
                            applyReplacer matchVal c _ =
                                evalFn (oldArgs ++ [ matchVal ]) patterns cachedArity maybeName implementation c closureEnv
                        in
                        Kernel.Regex.replaceAtMost n regexVal applyReplacer str cfg env
                            |> EvalResult.map String

                    _ ->
                        EvalResult.fail <| typeError env "Regex.replace: third argument must be a function"

            _ ->
                EvalResult.fail <| typeError env "Regex.replaceAtMost: expected Int, Regex, function, String"
    )


{-| Custom kernel entry for Parser.isSubString (5 args, returns Triple).
-}
parserIsSubString : ModuleName -> ( Int, List Value -> Eval Value )
parserIsSubString _ =
    ( 5
    , \args _ env ->
        case args of
            [ String small, Int offset, Int row, Int col, String big ] ->
                let
                    ( newOffset, newRow, newCol ) =
                        Kernel.Parser.isSubString small offset row col big
                in
                EvalResult.succeed (Triple (Int newOffset) (Int newRow) (Int newCol))

            _ ->
                EvalResult.fail <| typeError env "Parser.isSubString: expected String, Int, Int, Int, String"
    )


{-| Custom kernel entry for Parser.findSubString (5 args, returns Triple).
-}
parserFindSubString : ModuleName -> ( Int, List Value -> Eval Value )
parserFindSubString _ =
    ( 5
    , \args _ env ->
        case args of
            [ String small, Int offset, Int row, Int col, String big ] ->
                let
                    ( newOffset, newRow, newCol ) =
                        Kernel.Parser.findSubString small offset row col big
                in
                EvalResult.succeed (Triple (Int newOffset) (Int newRow) (Int newCol))

            _ ->
                EvalResult.fail <| typeError env "Parser.findSubString: expected String, Int, Int, Int, String"
    )


{-| Custom kernel entry for Parser.consumeBase (3 args, returns Tuple).
-}
parserConsumeBase : ModuleName -> ( Int, List Value -> Eval Value )
parserConsumeBase _ =
    ( 3
    , \args _ env ->
        case args of
            [ Int base, Int offset, String str ] ->
                let
                    ( newOffset, total ) =
                        Kernel.Parser.consumeBase base offset str
                in
                EvalResult.succeed (Tuple (Int newOffset) (Int total))

            _ ->
                EvalResult.fail <| typeError env "Parser.consumeBase: expected Int, Int, String"
    )


{-| Custom kernel entry for Parser.consumeBase16 (2 args, returns Tuple).
-}
parserConsumeBase16 : ModuleName -> ( Int, List Value -> Eval Value )
parserConsumeBase16 _ =
    ( 2
    , \args _ env ->
        case args of
            [ Int offset, String str ] ->
                let
                    ( newOffset, total ) =
                        Kernel.Parser.consumeBase16 offset str
                in
                EvalResult.succeed (Tuple (Int newOffset) (Int total))

            _ ->
                EvalResult.fail <| typeError env "Parser.consumeBase16: expected Int, String"
    )


{-| Custom kernel entry for Parser.isSubChar.
Takes a predicate function, offset, and string. Needs evalFunction for the predicate callback.
Returns -1 (fail), -2 (newline match), or new offset.
-}
parserIsSubChar : EvalFunction -> ModuleName -> ( Int, List Value -> Eval Value )
parserIsSubChar evalFn _ =
    ( 3
    , \args cfg env ->
        case args of
            [ predicate, Int offset, String str ] ->
                if String.length str <= offset then
                    EvalResult.succeed (Int -1)

                else
                    case String.slice offset (offset + 1) str |> String.uncons of
                        Nothing ->
                            EvalResult.succeed (Int -1)

                        Just ( c, _ ) ->
                            let
                                code =
                                    Char.toCode c
                            in
                            if Bitwise.and code 0xF800 == 0xD800 then
                                -- Surrogate pair: extract 2-unit character
                                let
                                    fullChar =
                                        String.slice offset (offset + 2) str
                                            |> String.uncons
                                            |> Maybe.map Tuple.first
                                            |> Maybe.withDefault c
                                in
                                applyPredicate evalFn predicate (Char fullChar) cfg env
                                    |> EvalResult.map
                                        (\matched ->
                                            if matched then
                                                Int (offset + 2)

                                            else
                                                Int -1
                                        )

                            else
                                applyPredicate evalFn predicate (Char c) cfg env
                                    |> EvalResult.map
                                        (\matched ->
                                            if matched then
                                                if c == '\n' then
                                                    Int -2

                                                else
                                                    Int (offset + 1)

                                            else
                                                Int -1
                                        )

            _ ->
                EvalResult.fail <| typeError env "Parser.isSubChar: expected function, Int, String"
    )


{-| Kernel entry for Bytes.Decode.decode.
Takes a decoder function and bytes value. The decoder function is the function
inside the Decoder wrapper, which takes (Bytes, Int) -> (Int, Value).
-}
bytesDecodeKernel : EvalFunction -> ModuleName -> ( Int, List Value -> Eval Value )
bytesDecodeKernel evalFn _ =
    ( 2
    , \args cfg env ->
        case args of
            [ decoderFn, bsArg ] ->
                case decoderFn of
                    PartiallyApplied closureEnv oldArgs patterns maybeName implementation cachedArity ->
                        let
                            applyDecoder : Value -> Eval (Value -> Eval Value)
                            applyDecoder arg c _ =
                                evalFn (oldArgs ++ [ arg ]) patterns cachedArity maybeName implementation c closureEnv
                                    |> EvalResult.onValue
                                        (\result ->
                                            case result of
                                                PartiallyApplied cEnv oArgs pats mName impl innerArity ->
                                                    Ok (\arg2 c2 _ -> evalFn (oArgs ++ [ arg2 ]) pats innerArity mName impl c2 cEnv)

                                                _ ->
                                                    Err (typeError env "Bytes.decode: decoder did not return a function")
                                        )
                        in
                        Kernel.Bytes.decode applyDecoder bsArg cfg env

                    _ ->
                        EvalResult.fail <| typeError env "Bytes.decode: first argument must be a function"

            [ _ ] ->
                EvalResult.fail <| typeError env "Bytes.decode: needs two arguments"

            _ ->
                EvalResult.fail <| typeError env "Bytes.decode: expected decoder and bytes"
    )


{-| Bytes read kernel entry for 2-arg read functions (read\_u8, read\_i8, decodeFailure).
These take (Bytes, Int) -> (Int, Value).
-}
bytesRead2 : (Array Int -> Int -> Value) -> FunctionImplementation -> ModuleName -> ( Int, List Value -> Eval Value )
bytesRead2 readFn implementation moduleName =
    ( 2
    , \args _ env ->
        case args of
            [ BytesValue arr, Int offset ] ->
                EvalResult.succeed (readFn arr offset)

            [ _ ] ->
                partiallyApply moduleName args implementation

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                EvalResult.fail <| typeError env "Expected Bytes and Int"
    )


{-| Bytes read kernel entry for 3-arg read functions (read\_u16 etc.).
These take (Bool, Bytes, Int) -> (Int, Value). The Bool is already partially applied.
-}
bytesRead3 : (Bool -> Array Int -> Int -> Value) -> FunctionImplementation -> ModuleName -> ( Int, List Value -> Eval Value )
bytesRead3 readFn implementation moduleName =
    ( 3
    , \args _ env ->
        case args of
            [ Bool le, BytesValue arr, Int offset ] ->
                EvalResult.succeed (readFn le arr offset)

            -- Also handle Int argument (for read_bytes and read_string which take Int, not Bool)
            [ Int n, BytesValue arr, Int offset ] ->
                -- Reinterpret: for read_bytes/read_string, first arg is count
                EvalResult.succeed (readFn (n /= 0) arr offset)

            [ _, _ ] ->
                partiallyApply moduleName args implementation

            [ _ ] ->
                partiallyApply moduleName args implementation

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                EvalResult.fail <| typeError env "Expected Bool/Int, Bytes, Int"
    )


{-| Bytes read kernel entry for read\_bytes and read\_string.
These take (Int, Bytes, Int) -> (Int, Value). The Int is the byte count.
-}
bytesReadN : (Int -> Array Int -> Int -> Value) -> FunctionImplementation -> ModuleName -> ( Int, List Value -> Eval Value )
bytesReadN readFn implementation moduleName =
    ( 3
    , \args _ env ->
        case args of
            [ Int n, BytesValue arr, Int offset ] ->
                EvalResult.succeed (readFn n arr offset)

            [ _, _ ] ->
                partiallyApply moduleName args implementation

            [ _ ] ->
                partiallyApply moduleName args implementation

            [] ->
                partiallyApply moduleName args implementation

            _ ->
                EvalResult.fail <| typeError env "Expected Int, Bytes, Int"
    )


{-| Write stub for 3-arg write functions (never called if encode is a kernel function).
-}
writeStub : FunctionImplementation -> ModuleName -> ( Int, List Value -> Eval Value )
writeStub _ _ =
    ( 3
    , \_ _ env ->
        EvalResult.fail <| typeError env "Bytes write functions are not implemented in the interpreter"
    )


{-| Write stub for 4-arg write functions.
-}
writeStub4 : FunctionImplementation -> ModuleName -> ( Int, List Value -> Eval Value )
writeStub4 _ _ =
    ( 4
    , \_ _ env ->
        EvalResult.fail <| typeError env "Bytes write functions are not implemented in the interpreter"
    )


applyPredicate : EvalFunction -> Value -> Value -> Types.Config -> Types.Env -> Types.EvalResult Bool
applyPredicate evalFn predicate charArg cfg env =
    case predicate of
        PartiallyApplied closureEnv oldArgs patterns maybeName implementation cachedArity ->
            evalFn (oldArgs ++ [ charArg ]) patterns cachedArity maybeName implementation cfg closureEnv
                |> EvalResult.onValue
                    (\result ->
                        case result of
                            Bool b ->
                                Ok b

                            _ ->
                                Err (typeError env "isSubChar predicate must return Bool")
                    )

        _ ->
            EvalResult.fail <| typeError env "isSubChar: expected a predicate function"
