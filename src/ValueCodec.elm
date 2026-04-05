module ValueCodec exposing (decodeValue, encodeValue)

{-| Serialization of interpreter Values to/from JSON strings.

Used for persisting elm-review's ProjectRuleCache across process runs.
Handles all data-only Value variants. PartiallyApplied (closures),
RegexValue, and JsonDecoderValue are encoded as sentinel values.

-}

import Array exposing (Array)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import FastDict as Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Types exposing (JsonVal(..), Value(..))


{-| Encode a Value to a JSON string.
-}
encodeValue : Value -> String
encodeValue value =
    E.encode 0 (encodeValueJson value)


encodeValueJson : Value -> E.Value
encodeValueJson value =
    case value of
        String s ->
            E.object [ ( "t", E.string "S" ), ( "v", E.string s ) ]

        Int i ->
            E.object [ ( "t", E.string "I" ), ( "v", E.int i ) ]

        Float f ->
            E.object [ ( "t", E.string "F" ), ( "v", E.float f ) ]

        Char c ->
            E.object [ ( "t", E.string "Ch" ), ( "v", E.string (String.fromChar c) ) ]

        Bool b ->
            E.object [ ( "t", E.string "B" ), ( "v", E.bool b ) ]

        Unit ->
            E.object [ ( "t", E.string "U" ) ]

        Tuple a b ->
            E.object [ ( "t", E.string "T2" ), ( "a", encodeValueJson a ), ( "b", encodeValueJson b ) ]

        Triple a b c ->
            E.object [ ( "t", E.string "T3" ), ( "a", encodeValueJson a ), ( "b", encodeValueJson b ), ( "c", encodeValueJson c ) ]

        Record dict ->
            E.object
                [ ( "t", E.string "R" )
                , ( "v"
                  , dict
                        |> Dict.toList
                        |> E.list (\( k, v ) -> E.object [ ( "k", E.string k ), ( "v", encodeValueJson v ) ])
                  )
                ]

        Custom ref args ->
            E.object
                [ ( "t", E.string "C" )
                , ( "m", E.list E.string ref.moduleName )
                , ( "n", E.string ref.name )
                , ( "a", E.list encodeValueJson args )
                ]

        List items ->
            E.object [ ( "t", E.string "L" ), ( "v", E.list encodeValueJson items ) ]

        JsArray arr ->
            E.object [ ( "t", E.string "A" ), ( "v", E.list encodeValueJson (Array.toList arr) ) ]

        JsonValue jv ->
            E.object [ ( "t", E.string "J" ), ( "v", encodeJsonVal jv ) ]

        BytesValue bytes ->
            E.object [ ( "t", E.string "By" ), ( "v", E.list E.int (Array.toList bytes) ) ]

        -- Non-serializable types get a sentinel
        PartiallyApplied _ _ _ _ _ _ ->
            E.object [ ( "t", E.string "X" ), ( "k", E.string "PA" ) ]

        JsonDecoderValue _ ->
            E.object [ ( "t", E.string "X" ), ( "k", E.string "JD" ) ]

        RegexValue _ ->
            E.object [ ( "t", E.string "X" ), ( "k", E.string "RX" ) ]


encodeJsonVal : JsonVal -> E.Value
encodeJsonVal jv =
    case jv of
        JsonNull ->
            E.null

        JsonBool b ->
            E.bool b

        JsonInt i ->
            E.int i

        JsonFloat f ->
            E.float f

        JsonString s ->
            E.string s

        JsonArray items ->
            E.list encodeJsonVal items

        JsonObject pairs ->
            E.object (List.map (\( k, v ) -> ( k, encodeJsonVal v )) pairs)


{-| Decode a Value from a JSON string.
Returns Nothing if the string can't be parsed.
-}
decodeValue : String -> Maybe Value
decodeValue json =
    D.decodeString valueDecoder json
        |> Result.toMaybe


valueDecoder : D.Decoder Value
valueDecoder =
    D.field "t" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "S" ->
                        D.field "v" D.string |> D.map String

                    "I" ->
                        D.field "v" D.int |> D.map Int

                    "F" ->
                        D.field "v" D.float |> D.map Float

                    "Ch" ->
                        D.field "v" D.string
                            |> D.map
                                (\s ->
                                    case String.uncons s of
                                        Just ( c, _ ) ->
                                            Char c

                                        Nothing ->
                                            Unit
                                )

                    "B" ->
                        D.field "v" D.bool |> D.map Bool

                    "U" ->
                        D.succeed Unit

                    "T2" ->
                        D.map2 Tuple
                            (D.field "a" (D.lazy (\_ -> valueDecoder)))
                            (D.field "b" (D.lazy (\_ -> valueDecoder)))

                    "T3" ->
                        D.map3 Triple
                            (D.field "a" (D.lazy (\_ -> valueDecoder)))
                            (D.field "b" (D.lazy (\_ -> valueDecoder)))
                            (D.field "c" (D.lazy (\_ -> valueDecoder)))

                    "R" ->
                        D.field "v"
                            (D.list
                                (D.map2 Tuple.pair
                                    (D.field "k" D.string)
                                    (D.field "v" (D.lazy (\_ -> valueDecoder)))
                                )
                            )
                            |> D.map (Dict.fromList >> Record)

                    "C" ->
                        D.map3
                            (\moduleName name args ->
                                Custom { moduleName = moduleName, name = name } args
                            )
                            (D.field "m" (D.list D.string))
                            (D.field "n" D.string)
                            (D.field "a" (D.list (D.lazy (\_ -> valueDecoder))))

                    "L" ->
                        D.field "v" (D.list (D.lazy (\_ -> valueDecoder)))
                            |> D.map List

                    "A" ->
                        D.field "v" (D.list (D.lazy (\_ -> valueDecoder)))
                            |> D.map (Array.fromList >> JsArray)

                    "J" ->
                        D.field "v" jsonValDecoder |> D.map JsonValue

                    "By" ->
                        D.field "v" (D.list D.int)
                            |> D.map (Array.fromList >> BytesValue)

                    "X" ->
                        -- Sentinel for non-serializable types
                        -- Return a Custom value that won't match anything
                        D.field "k" D.string
                            |> D.map
                                (\kind ->
                                    Custom { moduleName = [ "ValueCodec" ], name = "NonSerializable_" ++ kind } []
                                )

                    _ ->
                        D.fail ("Unknown Value tag: " ++ tag)
            )


jsonValDecoder : D.Decoder JsonVal
jsonValDecoder =
    D.oneOf
        [ D.null JsonNull
        , D.bool |> D.map JsonBool
        , D.int |> D.map JsonInt
        , D.float |> D.map JsonFloat
        , D.string |> D.map JsonString
        , D.list (D.lazy (\_ -> jsonValDecoder)) |> D.map JsonArray
        , D.keyValuePairs (D.lazy (\_ -> jsonValDecoder)) |> D.map JsonObject
        ]
