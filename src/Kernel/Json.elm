module Kernel.Json exposing
    ( addEntry
    , addField
    , decodeArray
    , decodeBool
    , decodeField
    , decodeFloat
    , decodeIndex
    , decodeInt
    , decodeKeyValuePairs
    , decodeList
    , decodeNull
    , decodeString
    , decodeValue
    , emptyArray
    , emptyObject
    , encode
    , encodeList
    , encodeNull
    , encodeObject
    , fail
    , jsonAndThen
    , jsonMap1
    , jsonMap2
    , jsonMap3
    , jsonMap4
    , jsonMap5
    , oneOf
    , run
    , runOnString
    , succeed
    , unwrapJsonValue
    , wrap
    )

import Array
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import EvalResult
import FastDict as Dict
import Rope
import Set
import Types exposing (Eval, EvalResult(..), JsonDecoder(..), JsonVal(..), Value(..))
import Value


{-| Wrap an Elm value as a Json.Encode.Value (JsonValue).
Used for Json.Encode.string, Json.Encode.int, Json.Encode.float, Json.Encode.bool.
-}
wrap : Value -> Value
wrap value =
    case value of
        String s ->
            JsonValue (JsonString s)

        Int i ->
            JsonValue (JsonInt i)

        Float f ->
            JsonValue (JsonFloat f)

        Bool b ->
            JsonValue (JsonBool b)

        JsonValue _ ->
            -- Already wrapped (e.g. intermediate builders)
            value

        List items ->
            -- Intermediate array builder from emptyArray/addEntry
            JsonValue (JsonArray (List.filterMap valueToJsonVal items))

        _ ->
            value


valueToJsonVal : Value -> Maybe JsonVal
valueToJsonVal v =
    case v of
        JsonValue j ->
            Just j

        String s ->
            Just (JsonString s)

        Int i ->
            Just (JsonInt i)

        Float f ->
            Just (JsonFloat f)

        Bool b ->
            Just (JsonBool b)

        _ ->
            Nothing


{-| emptyArray () - returns an empty intermediate array for foldl building.
-}
emptyArray : Value -> Value
emptyArray _ =
    List []


{-| emptyObject () - returns an empty intermediate object for foldl building.
Uses a JsonValue (JsonObject []) to preserve insertion order.
-}
emptyObject : Value -> Value
emptyObject _ =
    JsonValue (JsonObject [])


{-| addEntry func entry array - used inside List.foldl to build up a JSON array.
The func has already been extracted by the function selector as (Value -> Eval Value).
-}
addEntry : (Value -> Eval Value) -> Value -> List Value -> Eval (List Value)
addEntry func entry acc cfg env =
    func entry cfg env
        |> EvalResult.map (\encoded -> acc ++ [ unwrapJsonValue encoded ])


{-| addField key value obj - used inside List.foldl to build up a JSON object.
Appends key-value pairs preserving insertion order.
-}
addField : String -> Value -> Value -> Value
addField key val obj =
    case obj of
        JsonValue (JsonObject pairs) ->
            case valueToJsonVal (unwrapJsonValue val) of
                Just jsonVal ->
                    JsonValue (JsonObject (pairs ++ [ ( key, jsonVal ) ]))

                Nothing ->
                    obj

        _ ->
            obj


{-| Extract the inner JsonVal from a JsonValue, or wrap primitives.
-}
unwrapJsonValue : Value -> Value
unwrapJsonValue v =
    case v of
        JsonValue _ ->
            v

        _ ->
            wrap v


{-| Json.Encode.null
-}
encodeNull : Value
encodeNull =
    JsonValue JsonNull


{-| Json.Encode.encode : Int -> Value -> String
Serialize a JsonValue to a JSON string.
-}
encode : Int -> Value -> Eval String
encode indent value _ env =
    case value of
        JsonValue json ->
            EvalResult.succeed (jsonToString indent 0 json)

        _ ->
            EvalResult.fail <| Value.typeError env ("Json.Encode.encode expected a JSON value but got: " ++ Value.toString value)


{-| Serialize a JsonVal to a JSON string with the given indentation level.
-}
jsonToString : Int -> Int -> JsonVal -> String
jsonToString indent depth json =
    case json of
        JsonNull ->
            "null"

        JsonBool b ->
            if b then
                "true"

            else
                "false"

        JsonInt i ->
            String.fromInt i

        JsonFloat f ->
            if isNaN f || isInfinite f then
                "null"

            else
                String.fromFloat f

        JsonString s ->
            "\"" ++ escapeJsonString s ++ "\""

        JsonArray items ->
            if List.isEmpty items then
                "[]"

            else if indent == 0 then
                "[" ++ String.join "," (List.map (jsonToString 0 0) items) ++ "]"

            else
                let
                    newDepth =
                        depth + 1

                    prefix =
                        String.repeat (indent * newDepth) " "

                    closingPrefix =
                        String.repeat (indent * depth) " "
                in
                "[\n"
                    ++ String.join ",\n" (List.map (\item -> prefix ++ jsonToString indent newDepth item) items)
                    ++ "\n"
                    ++ closingPrefix
                    ++ "]"

        JsonObject pairs ->
            if List.isEmpty pairs then
                "{}"

            else if indent == 0 then
                "{"
                    ++ String.join ","
                        (List.map
                            (\( k, v ) ->
                                "\"" ++ escapeJsonString k ++ "\":" ++ jsonToString 0 0 v
                            )
                            pairs
                        )
                    ++ "}"

            else
                let
                    newDepth =
                        depth + 1

                    prefix =
                        String.repeat (indent * newDepth) " "

                    closingPrefix =
                        String.repeat (indent * depth) " "
                in
                "{\n"
                    ++ String.join ",\n"
                        (List.map
                            (\( k, v ) ->
                                prefix ++ "\"" ++ escapeJsonString k ++ "\": " ++ jsonToString indent newDepth v
                            )
                            pairs
                        )
                    ++ "\n"
                    ++ closingPrefix
                    ++ "}"


{-| Json.Encode.list : (a -> Value) -> List a -> Value
Apply a function to each item then wrap as a JSON array.
-}
encodeList : (Value -> Eval Value) -> List Value -> Eval Value
encodeList func items cfg env =
    encodeListHelp func items [] cfg env


encodeListHelp : (Value -> Eval Value) -> List Value -> List JsonVal -> Types.Config -> Types.Env -> Types.EvalResult Value
encodeListHelp func remaining acc cfg env =
    case remaining of
        [] ->
            EvalResult.succeed (JsonValue (JsonArray (List.reverse acc)))

        item :: rest ->
            case func item cfg env of
                Types.EvOk encoded ->
                    case encoded of
                        JsonValue json ->
                            encodeListHelp func rest (json :: acc) cfg env

                        _ ->
                            EvalResult.fail <| Value.typeError env ("Json.Encode.list: encoder did not produce a JSON value, got: " ++ Value.toString encoded)

                Types.EvErr e ->
                    Types.EvErr e

                Types.EvOkTrace encoded traces logs ->
                    case encoded of
                        JsonValue json ->
                            case encodeListHelp func rest (json :: acc) cfg env of
                                Types.EvOkTrace v t2 l2 ->
                                    Types.EvOkTrace v (Rope.appendTo traces t2) (Rope.appendTo logs l2)

                                Types.EvOk v ->
                                    Types.EvOkTrace v traces logs

                                other ->
                                    other

                        _ ->
                            EvalResult.fail <| Value.typeError env ("Json.Encode.list: encoder did not produce a JSON value, got: " ++ Value.toString encoded)

                Types.EvErrTrace e traces logs ->
                    Types.EvErrTrace e traces logs

                Types.EvYield tag payload resume ->
                    Types.EvYield tag payload resume

                Types.EvMemoLookup payload resume ->
                    Types.EvMemoLookup payload resume

                Types.EvMemoStore payload next ->
                    Types.EvMemoStore payload next

                Types.EvOkCoverage encoded s ->
                    case encoded of
                        JsonValue json ->
                            case encodeListHelp func rest (json :: acc) cfg env of
                                Types.EvOkCoverage v s2 ->
                                    Types.EvOkCoverage v (Set.union s s2)

                                Types.EvOk v ->
                                    Types.EvOkCoverage v s

                                other ->
                                    other

                        _ ->
                            EvalResult.fail <| Value.typeError env ("Json.Encode.list: encoder did not produce a JSON value, got: " ++ Value.toString encoded)

                Types.EvErrCoverage e s ->
                    Types.EvErrCoverage e s

{-| Json.Encode.object : List ( String, Value ) -> Value
Build a JSON object from key-value pairs.
-}
encodeObject : List Value -> Eval Value
encodeObject pairs _ env =
    case encodeObjectHelp pairs [] env of
        Ok jsonPairs ->
            EvalResult.succeed (JsonValue (JsonObject jsonPairs))

        Err e ->
            EvalResult.fail e


encodeObjectHelp : List Value -> List ( String, JsonVal ) -> Types.Env -> Result Types.EvalErrorData (List ( String, JsonVal ))
encodeObjectHelp pairs acc env =
    case pairs of
        [] ->
            Ok (List.reverse acc)

        (Tuple (String key) (JsonValue jsonVal)) :: rest ->
            encodeObjectHelp rest (( key, jsonVal ) :: acc) env

        other :: _ ->
            Err (Value.typeError env ("Json.Encode.object: expected (String, Value) pair, got: " ++ Value.toString other))


escapeJsonString : String -> String
escapeJsonString s =
    s
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""
        |> String.replace "\n" "\\n"
        |> String.replace "\u{000D}" "\\r"
        |> String.replace "\t" "\\t"



-- DECODER CONSTRUCTORS


decodeString : Value
decodeString =
    JsonDecoderValue DecodeString


decodeBool : Value
decodeBool =
    JsonDecoderValue DecodeBool


decodeInt : Value
decodeInt =
    JsonDecoderValue DecodeInt


decodeFloat : Value
decodeFloat =
    JsonDecoderValue DecodeFloat


decodeValue : Value
decodeValue =
    JsonDecoderValue DecodeValue


decodeNull : Value -> Value
decodeNull val =
    JsonDecoderValue (DecodeNull val)


decodeList : Value -> Value
decodeList decoderVal =
    case decoderVal of
        JsonDecoderValue decoder ->
            JsonDecoderValue (DecodeList decoder)

        _ ->
            decoderVal


decodeArray : Value -> Value
decodeArray decoderVal =
    case decoderVal of
        JsonDecoderValue decoder ->
            JsonDecoderValue (DecodeArray decoder)

        _ ->
            decoderVal


decodeField : String -> Value -> Value
decodeField fieldName decoderVal =
    case decoderVal of
        JsonDecoderValue decoder ->
            JsonDecoderValue (DecodeField fieldName decoder)

        _ ->
            decoderVal


decodeIndex : Int -> Value -> Value
decodeIndex idx decoderVal =
    case decoderVal of
        JsonDecoderValue decoder ->
            JsonDecoderValue (DecodeIndex idx decoder)

        _ ->
            decoderVal


decodeKeyValuePairs : Value -> Value
decodeKeyValuePairs decoderVal =
    case decoderVal of
        JsonDecoderValue decoder ->
            JsonDecoderValue (DecodeKeyValuePairs decoder)

        _ ->
            decoderVal


succeed : Value -> Value
succeed val =
    JsonDecoderValue (DecodeSucceed val)


fail : String -> Value
fail msg =
    JsonDecoderValue (DecodeFail msg)


oneOf : List Value -> Value
oneOf decoderVals =
    let
        extractDecoder : Value -> JsonDecoder
        extractDecoder v =
            case v of
                JsonDecoderValue d ->
                    d

                _ ->
                    DecodeFail "oneOf: not a decoder"
    in
    JsonDecoderValue (DecodeOneOf (List.map extractDecoder decoderVals))


jsonMap1 : Value -> Value -> Value
jsonMap1 func d1 =
    case d1 of
        JsonDecoderValue decoder1 ->
            JsonDecoderValue (DecodeMap func [ decoder1 ])

        _ ->
            d1


jsonMap2 : Value -> Value -> Value -> Value
jsonMap2 func d1 d2 =
    case ( d1, d2 ) of
        ( JsonDecoderValue decoder1, JsonDecoderValue decoder2 ) ->
            JsonDecoderValue (DecodeMap func [ decoder1, decoder2 ])

        _ ->
            d1


jsonMap3 : Value -> Value -> Value -> Value -> Value
jsonMap3 func d1 d2 d3 =
    case ( d1, d2, d3 ) of
        ( JsonDecoderValue decoder1, JsonDecoderValue decoder2, JsonDecoderValue decoder3 ) ->
            JsonDecoderValue (DecodeMap func [ decoder1, decoder2, decoder3 ])

        _ ->
            d1


jsonMap4 : Value -> Value -> Value -> Value -> Value -> Value
jsonMap4 func d1 d2 d3 d4 =
    case ( d1, d2 ) of
        ( JsonDecoderValue decoder1, JsonDecoderValue decoder2 ) ->
            case ( d3, d4 ) of
                ( JsonDecoderValue decoder3, JsonDecoderValue decoder4 ) ->
                    JsonDecoderValue (DecodeMap func [ decoder1, decoder2, decoder3, decoder4 ])

                _ ->
                    d1

        _ ->
            d1


jsonMap5 : Value -> Value -> Value -> Value -> Value -> Value -> Value
jsonMap5 func d1 d2 d3 d4 d5 =
    case ( d1, d2 ) of
        ( JsonDecoderValue decoder1, JsonDecoderValue decoder2 ) ->
            case ( d3, d4 ) of
                ( JsonDecoderValue decoder3, JsonDecoderValue decoder4 ) ->
                    case d5 of
                        JsonDecoderValue decoder5 ->
                            JsonDecoderValue (DecodeMap func [ decoder1, decoder2, decoder3, decoder4, decoder5 ])

                        _ ->
                            d1

                _ ->
                    d1

        _ ->
            d1


jsonAndThen : Value -> Value -> Value
jsonAndThen callback decoderVal =
    case decoderVal of
        JsonDecoderValue decoder ->
            JsonDecoderValue (DecodeAndThen callback decoder)

        _ ->
            decoderVal



-- RUN DECODERS


{-| Json.Decode.decodeString : Decoder a -> String -> Result Error a
-}
runOnString : EvalFunction -> Value -> String -> Eval Value
runOnString evalFn decoderVal jsonString cfg env =
    case decoderVal of
        JsonDecoderValue decoder ->
            case parseJsonString jsonString of
                Ok json ->
                    case runDecoder evalFn decoder json cfg env of
                        Ok val ->
                            EvalResult.succeed (resultOk val)

                        Err decoderError ->
                            EvalResult.succeed (resultErr decoderError)

                Err parseError ->
                    EvalResult.succeed
                        (resultErr
                            (decoderFailure
                                ("This is not valid JSON! " ++ parseError)
                                (JsonValue (JsonString jsonString))
                            )
                        )

        _ ->
            EvalResult.fail <| Value.typeError env "Json.Decode.decodeString: first argument must be a decoder"


{-| Json.Decode.decodeValue : Decoder a -> Value -> Result Error a
-}
run : EvalFunction -> Value -> Value -> Eval Value
run evalFn decoderVal jsonVal cfg env =
    case ( decoderVal, jsonVal ) of
        ( JsonDecoderValue decoder, JsonValue json ) ->
            case runDecoder evalFn decoder json cfg env of
                Ok val ->
                    EvalResult.succeed (resultOk val)

                Err decoderError ->
                    EvalResult.succeed (resultErr decoderError)

        _ ->
            EvalResult.fail <| Value.typeError env "Json.Decode.decodeValue: expected a decoder and a JSON value"



-- DECODER ENGINE


runDecoder : EvalFunction -> JsonDecoder -> JsonVal -> Types.Config -> Types.Env -> Result Value Value
runDecoder evalFn decoder json cfg env =
    case decoder of
        DecodeString ->
            case json of
                JsonString s ->
                    Ok (String s)

                _ ->
                    Err (decoderFailure "Expecting a STRING" (JsonValue json))

        DecodeBool ->
            case json of
                JsonBool b ->
                    Ok (Bool b)

                _ ->
                    Err (decoderFailure "Expecting a BOOL" (JsonValue json))

        DecodeInt ->
            case json of
                JsonInt i ->
                    Ok (Int i)

                JsonFloat f ->
                    if toFloat (round f) == f && not (isInfinite f) then
                        Ok (Int (round f))

                    else
                        Err (decoderFailure "Expecting an INT" (JsonValue json))

                _ ->
                    Err (decoderFailure "Expecting an INT" (JsonValue json))

        DecodeFloat ->
            case json of
                JsonFloat f ->
                    Ok (Float f)

                JsonInt i ->
                    Ok (Float (toFloat i))

                _ ->
                    Err (decoderFailure "Expecting a FLOAT" (JsonValue json))

        DecodeValue ->
            Ok (JsonValue json)

        DecodeNull val ->
            case json of
                JsonNull ->
                    Ok val

                _ ->
                    Err (decoderFailure "Expecting null" (JsonValue json))

        DecodeSucceed val ->
            Ok val

        DecodeFail msg ->
            Err (decoderFailure msg (JsonValue json))

        DecodeField fieldName innerDecoder ->
            case json of
                JsonObject pairs ->
                    case findField fieldName pairs of
                        Just fieldJson ->
                            case runDecoder evalFn innerDecoder fieldJson cfg env of
                                Ok val ->
                                    Ok val

                                Err err ->
                                    Err (decoderFieldError fieldName err)

                        Nothing ->
                            Err (decoderFailure ("Expecting an OBJECT with a field named `" ++ fieldName ++ "`") (JsonValue json))

                _ ->
                    Err (decoderFailure ("Expecting an OBJECT with a field named `" ++ fieldName ++ "`") (JsonValue json))

        DecodeIndex idx innerDecoder ->
            case json of
                JsonArray items ->
                    case listGetAt idx items of
                        Just item ->
                            case runDecoder evalFn innerDecoder item cfg env of
                                Ok val ->
                                    Ok val

                                Err err ->
                                    Err (decoderIndexError idx err)

                        Nothing ->
                            Err (decoderFailure ("Expecting a LONGER array. Need index " ++ String.fromInt idx ++ " but only see " ++ String.fromInt (List.length items) ++ " entries") (JsonValue json))

                _ ->
                    Err (decoderFailure "Expecting an ARRAY" (JsonValue json))

        DecodeList innerDecoder ->
            case json of
                JsonArray items ->
                    runArrayDecoder evalFn innerDecoder items 0 [] cfg env

                _ ->
                    Err (decoderFailure "Expecting a LIST" (JsonValue json))

        DecodeArray innerDecoder ->
            case json of
                JsonArray items ->
                    case runArrayDecoder evalFn innerDecoder items 0 [] cfg env of
                        Ok (List vals) ->
                            Ok (JsArray (Array.fromList vals))

                        Ok other ->
                            Ok other

                        Err e ->
                            Err e

                _ ->
                    Err (decoderFailure "Expecting an ARRAY" (JsonValue json))

        DecodeKeyValuePairs innerDecoder ->
            case json of
                JsonObject pairs ->
                    runKeyValuePairsDecoder evalFn innerDecoder pairs [] cfg env

                _ ->
                    Err (decoderFailure "Expecting an OBJECT" (JsonValue json))

        DecodeMap func decoders ->
            runMapDecoder evalFn func decoders json cfg env

        DecodeAndThen callback innerDecoder ->
            case runDecoder evalFn innerDecoder json cfg env of
                Ok val ->
                    case applyFunction evalFn callback val cfg env of
                        Ok (JsonDecoderValue newDecoder) ->
                            runDecoder evalFn newDecoder json cfg env

                        Ok _ ->
                            Err (decoderFailure "andThen callback did not return a decoder" (JsonValue json))

                        Err errMsg ->
                            Err (decoderFailure errMsg (JsonValue json))

                Err e ->
                    Err e

        DecodeOneOf decoders ->
            runOneOf evalFn decoders json [] cfg env


runArrayDecoder : EvalFunction -> JsonDecoder -> List JsonVal -> Int -> List Value -> Types.Config -> Types.Env -> Result Value Value
runArrayDecoder evalFn decoder items idx acc cfg env =
    case items of
        [] ->
            Ok (List (List.reverse acc))

        item :: rest ->
            case runDecoder evalFn decoder item cfg env of
                Ok val ->
                    runArrayDecoder evalFn decoder rest (idx + 1) (val :: acc) cfg env

                Err err ->
                    Err (decoderIndexError idx err)


runKeyValuePairsDecoder : EvalFunction -> JsonDecoder -> List ( String, JsonVal ) -> List Value -> Types.Config -> Types.Env -> Result Value Value
runKeyValuePairsDecoder evalFn decoder pairs acc cfg env =
    case pairs of
        [] ->
            Ok (List (List.reverse acc))

        ( key, val ) :: rest ->
            case runDecoder evalFn decoder val cfg env of
                Ok decoded ->
                    runKeyValuePairsDecoder evalFn decoder rest (Tuple (String key) decoded :: acc) cfg env

                Err err ->
                    Err (decoderFieldError key err)


runMapDecoder : EvalFunction -> Value -> List JsonDecoder -> JsonVal -> Types.Config -> Types.Env -> Result Value Value
runMapDecoder evalFn func decoders json cfg env =
    runMapDecoderHelp evalFn func decoders json cfg env


runMapDecoderHelp : EvalFunction -> Value -> List JsonDecoder -> JsonVal -> Types.Config -> Types.Env -> Result Value Value
runMapDecoderHelp evalFn currentFunc decoders json cfg env =
    case decoders of
        [] ->
            Ok currentFunc

        decoder :: rest ->
            case runDecoder evalFn decoder json cfg env of
                Ok val ->
                    case applyFunction evalFn currentFunc val cfg env of
                        Ok result ->
                            runMapDecoderHelp evalFn result rest json cfg env

                        Err errMsg ->
                            Err (decoderFailure errMsg (JsonValue json))

                Err e ->
                    Err e


runOneOf : EvalFunction -> List JsonDecoder -> JsonVal -> List Value -> Types.Config -> Types.Env -> Result Value Value
runOneOf evalFn decoders json errors cfg env =
    case decoders of
        [] ->
            Err (decoderOneOfError (List.reverse errors))

        decoder :: rest ->
            case runDecoder evalFn decoder json cfg env of
                Ok val ->
                    Ok val

                Err err ->
                    runOneOf evalFn rest json (err :: errors) cfg env



-- APPLY FUNCTION (bridge to interpreter)


type alias EvalFunction =
    List Value
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
    -> Int
    -> Maybe Elm.Syntax.Pattern.QualifiedNameRef
    -> Types.Implementation
    -> Types.Eval Value


{-| Apply an Elm function (stored as a PartiallyApplied value) to an argument.
Uses the interpreter's evalFunction to handle the actual evaluation.
-}
applyFunction : EvalFunction -> Value -> Value -> Types.Config -> Types.Env -> Result String Value
applyFunction evalFn func arg cfg _ =
    case func of
        PartiallyApplied closureEnv oldArgs patterns maybeName implementation arity ->
            let
                newArgs =
                    oldArgs ++ [ arg ]
            in
            if List.length newArgs >= arity then
                case evalFn newArgs patterns arity maybeName implementation cfg closureEnv of
                    EvOk val ->
                        Ok val

                    EvErr e ->
                        Err (Types.evalErrorKindToString e.error)

                    EvOkTrace val _ _ ->
                        Ok val

                    EvErrTrace e _ _ ->
                        Err (Types.evalErrorKindToString e.error)

                    EvYield _ _ _ ->
                        Err "Cannot yield inside JSON decoder"

                    EvMemoLookup _ _ ->
                        Err "Cannot yield inside JSON decoder"

                    EvMemoStore _ _ ->
                        Err "Cannot yield inside JSON decoder"

                    EvOkCoverage val _ ->
                        Ok val

                    EvErrCoverage e _ ->
                        Err (Types.evalErrorKindToString e.error)
            else
                Ok (PartiallyApplied closureEnv newArgs patterns maybeName implementation arity)

        Custom qualifiedName args ->
            -- Constructor application (e.g., Just value, Tuple.pair a b)
            Ok (Custom qualifiedName (args ++ [ arg ]))

        _ ->
            Err ("Expected a function, got: " ++ Value.toString func)



-- HELPERS


findField : String -> List ( String, JsonVal ) -> Maybe JsonVal
findField key pairs =
    case pairs of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if k == key then
                Just v

            else
                findField key rest


listGetAt : Int -> List a -> Maybe a
listGetAt idx items =
    case items of
        [] ->
            Nothing

        x :: rest ->
            if idx == 0 then
                Just x

            else
                listGetAt (idx - 1) rest



-- RESULT AND ERROR CONSTRUCTORS


resultOk : Value -> Value
resultOk val =
    Custom { moduleName = [ "Result" ], name = "Ok" } [ val ]


resultErr : Value -> Value
resultErr val =
    Custom { moduleName = [ "Result" ], name = "Err" } [ val ]


decoderFailure : String -> Value -> Value
decoderFailure msg jsonValue =
    Custom { moduleName = [ "Json", "Decode" ], name = "Failure" } [ String msg, jsonValue ]


decoderFieldError : String -> Value -> Value
decoderFieldError fieldName innerError =
    Custom { moduleName = [ "Json", "Decode" ], name = "Field" } [ String fieldName, innerError ]


decoderIndexError : Int -> Value -> Value
decoderIndexError idx innerError =
    Custom { moduleName = [ "Json", "Decode" ], name = "Index" } [ Int idx, innerError ]


decoderOneOfError : List Value -> Value
decoderOneOfError errors =
    Custom { moduleName = [ "Json", "Decode" ], name = "OneOf" } [ List errors ]



-- JSON PARSER


parseJsonString : String -> Result String JsonVal
parseJsonString input =
    case String.trim input of
        "" ->
            Err "Unexpected end of JSON input"

        trimmed ->
            case parseValue trimmed of
                Ok ( val, rest ) ->
                    if String.isEmpty (String.trim rest) then
                        Ok val

                    else
                        Err ("Unexpected content after JSON value: " ++ String.left 20 rest)

                Err e ->
                    Err e


parseValue : String -> Result String ( JsonVal, String )
parseValue input =
    case String.uncons input of
        Nothing ->
            Err "Unexpected end of input"

        Just ( c, _ ) ->
            if c == '"' then
                parseString input

            else if c == '{' then
                parseObject input

            else if c == '[' then
                parseArray input

            else if c == 't' then
                if String.startsWith "true" input then
                    Ok ( JsonBool True, String.dropLeft 4 input )

                else
                    Err ("Unexpected token: " ++ String.left 10 input)

            else if c == 'f' then
                if String.startsWith "false" input then
                    Ok ( JsonBool False, String.dropLeft 5 input )

                else
                    Err ("Unexpected token: " ++ String.left 10 input)

            else if c == 'n' then
                if String.startsWith "null" input then
                    Ok ( JsonNull, String.dropLeft 4 input )

                else
                    Err ("Unexpected token: " ++ String.left 10 input)

            else if c == '-' || Char.isDigit c then
                parseNumber input

            else
                Err ("Unexpected character: " ++ String.fromChar c)


parseString : String -> Result String ( JsonVal, String )
parseString input =
    -- input starts with "
    case parseStringContent (String.dropLeft 1 input) "" of
        Ok ( s, rest ) ->
            Ok ( JsonString s, rest )

        Err e ->
            Err e


parseStringContent : String -> String -> Result String ( String, String )
parseStringContent input acc =
    case String.uncons input of
        Nothing ->
            Err "Unterminated string"

        Just ( '"', rest ) ->
            Ok ( acc, rest )

        Just ( '\\', rest ) ->
            case String.uncons rest of
                Nothing ->
                    Err "Unterminated escape sequence"

                Just ( 'n', rest2 ) ->
                    parseStringContent rest2 (acc ++ "\n")

                Just ( 't', rest2 ) ->
                    parseStringContent rest2 (acc ++ "\t")

                Just ( 'r', rest2 ) ->
                    parseStringContent rest2 (acc ++ "\u{000D}")

                Just ( '"', rest2 ) ->
                    parseStringContent rest2 (acc ++ "\"")

                Just ( '\\', rest2 ) ->
                    parseStringContent rest2 (acc ++ "\\")

                Just ( '/', rest2 ) ->
                    parseStringContent rest2 (acc ++ "/")

                Just ( 'u', rest2 ) ->
                    let
                        hex =
                            String.left 4 rest2

                        remaining =
                            String.dropLeft 4 rest2
                    in
                    case hexToInt hex of
                        Just code ->
                            parseStringContent remaining (acc ++ String.fromChar (Char.fromCode code))

                        Nothing ->
                            Err ("Invalid unicode escape: \\u" ++ hex)

                Just ( ec, _ ) ->
                    Err ("Invalid escape character: \\" ++ String.fromChar ec)

        Just ( c, rest ) ->
            parseStringContent rest (acc ++ String.fromChar c)


hexToInt : String -> Maybe Int
hexToInt hex =
    if String.length hex /= 4 then
        Nothing

    else
        String.foldl
            (\c macc ->
                macc
                    |> Maybe.andThen
                        (\a ->
                            hexDigitToInt c |> Maybe.map (\d -> a * 16 + d)
                        )
            )
            (Just 0)
            hex


hexDigitToInt : Char -> Maybe Int
hexDigitToInt c =
    if Char.isDigit c then
        Just (Char.toCode c - Char.toCode '0')

    else if c >= 'a' && c <= 'f' then
        Just (Char.toCode c - Char.toCode 'a' + 10)

    else if c >= 'A' && c <= 'F' then
        Just (Char.toCode c - Char.toCode 'A' + 10)

    else
        Nothing


parseNumber : String -> Result String ( JsonVal, String )
parseNumber input =
    let
        ( numStr, rest ) =
            spanNumber input

        isFloat =
            String.contains "." numStr || String.contains "e" numStr || String.contains "E" numStr
    in
    if isFloat then
        case String.toFloat numStr of
            Just f ->
                Ok ( JsonFloat f, rest )

            Nothing ->
                Err ("Invalid number: " ++ numStr)

    else
        case String.toInt numStr of
            Just i ->
                Ok ( JsonInt i, rest )

            Nothing ->
                Err ("Invalid number: " ++ numStr)


spanNumber : String -> ( String, String )
spanNumber input =
    spanNumberHelp input 0


spanNumberHelp : String -> Int -> ( String, String )
spanNumberHelp input idx =
    case String.uncons (String.dropLeft idx input) of
        Nothing ->
            ( input, "" )

        Just ( c, _ ) ->
            if Char.isDigit c || c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E' then
                spanNumberHelp input (idx + 1)

            else
                ( String.left idx input, String.dropLeft idx input )


parseArray : String -> Result String ( JsonVal, String )
parseArray input =
    -- input starts with [
    let
        rest =
            String.trim (String.dropLeft 1 input)
    in
    case String.uncons rest of
        Just ( ']', rest2 ) ->
            Ok ( JsonArray [], rest2 )

        _ ->
            parseArrayItems rest []


parseArrayItems : String -> List JsonVal -> Result String ( JsonVal, String )
parseArrayItems input acc =
    case parseValue (String.trim input) of
        Ok ( val, rest ) ->
            let
                trimmedRest =
                    String.trim rest
            in
            case String.uncons trimmedRest of
                Just ( ',', rest2 ) ->
                    parseArrayItems rest2 (val :: acc)

                Just ( ']', rest2 ) ->
                    Ok ( JsonArray (List.reverse (val :: acc)), rest2 )

                _ ->
                    Err ("Expected ',' or ']' in array, got: " ++ String.left 20 trimmedRest)

        Err e ->
            Err e


parseObject : String -> Result String ( JsonVal, String )
parseObject input =
    -- input starts with {
    let
        rest =
            String.trim (String.dropLeft 1 input)
    in
    case String.uncons rest of
        Just ( '}', rest2 ) ->
            Ok ( JsonObject [], rest2 )

        _ ->
            parseObjectPairs rest []


parseObjectPairs : String -> List ( String, JsonVal ) -> Result String ( JsonVal, String )
parseObjectPairs input acc =
    let
        trimmed =
            String.trim input
    in
    case parseString trimmed of
        Ok ( JsonString key, rest ) ->
            let
                afterColon =
                    String.trim rest
            in
            case String.uncons afterColon of
                Just ( ':', rest2 ) ->
                    case parseValue (String.trim rest2) of
                        Ok ( val, rest3 ) ->
                            let
                                trimmedRest3 =
                                    String.trim rest3
                            in
                            case String.uncons trimmedRest3 of
                                Just ( ',', rest4 ) ->
                                    parseObjectPairs rest4 (( key, val ) :: acc)

                                Just ( '}', rest4 ) ->
                                    Ok ( JsonObject (List.reverse (( key, val ) :: acc)), rest4 )

                                _ ->
                                    Err ("Expected ',' or '}' in object, got: " ++ String.left 20 trimmedRest3)

                        Err e ->
                            Err e

                _ ->
                    Err ("Expected ':' after object key, got: " ++ String.left 20 afterColon)

        _ ->
            Err ("Expected string key in object, got: " ++ String.left 20 trimmed)
