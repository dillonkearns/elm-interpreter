module CoreTests.JsonDecode exposing (suite)

import Elm.Syntax.Expression as Expression
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Json.Decode"
        [ primitiveDecoderTests
        , succeedFailTests
        , fieldTests
        , indexTests
        , mapTests
        , listTests
        , nullableTests
        , oneOfTests
        , andThenTests
        ]


evalJsonModule : String -> String -> Value -> Test
evalJsonModule name source expected =
    test name <|
        \_ ->
            case Eval.Module.eval source (Expression.FunctionOrValue [] "main") of
                Ok value ->
                    value |> Expect.equal expected

                Err e ->
                    Expect.fail (Debug.toString e)


ok : Value -> Value
ok v =
    Custom { moduleName = [ "Result" ], name = "Ok" } [ v ]


primitiveDecoderTests : Test
primitiveDecoderTests =
    describe "Primitive decoders"
        [ evalJsonModule "decode int"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString Json.Decode.int "42"
"""
            (ok (Int 42))
        , evalJsonModule "decode negative int"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString Json.Decode.int "-7"
"""
            (ok (Int -7))
        , evalJsonModule "decode string"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString Json.Decode.string "\\"hello\\""
"""
            (ok (String "hello"))
        , evalJsonModule "decode float"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString Json.Decode.float "3.14"
"""
            (ok (Float 3.14))
        , evalJsonModule "decode bool true"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString Json.Decode.bool "true"
"""
            (ok (Bool True))
        , evalJsonModule "decode bool false"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString Json.Decode.bool "false"
"""
            (ok (Bool False))
        , evalJsonModule "decode int from float fails"
            """module Test exposing (main)

import Json.Decode

main =
    case Json.Decode.decodeString Json.Decode.int "3.14" of
        Err _ -> "error"
        Ok _ -> "ok"
"""
            (String "error")
        , evalJsonModule "decode string from int fails"
            """module Test exposing (main)

import Json.Decode

main =
    case Json.Decode.decodeString Json.Decode.string "42" of
        Err _ -> "error"
        Ok _ -> "ok"
"""
            (String "error")
        ]


succeedFailTests : Test
succeedFailTests =
    describe "succeed and fail"
        [ evalJsonModule "succeed always succeeds"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString (Json.Decode.succeed 42) "null"
"""
            (ok (Int 42))
        , evalJsonModule "fail always fails"
            """module Test exposing (main)

import Json.Decode

main =
    case Json.Decode.decodeString (Json.Decode.fail "nope") "null" of
        Err _ -> "error"
        Ok _ -> "ok"
"""
            (String "error")
        ]


fieldTests : Test
fieldTests =
    describe "field"
        [ evalJsonModule "field decode string"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString (Json.Decode.field "name" Json.Decode.string) "{\\"name\\":\\"Tom\\"}"
"""
            (ok (String "Tom"))
        , evalJsonModule "field decode int"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString (Json.Decode.field "age" Json.Decode.int) "{\\"age\\":42}"
"""
            (ok (Int 42))
        , evalJsonModule "missing field fails"
            """module Test exposing (main)

import Json.Decode

main =
    case Json.Decode.decodeString (Json.Decode.field "missing" Json.Decode.int) "{\\"age\\":42}" of
        Err _ -> "error"
        Ok _ -> "ok"
"""
            (String "error")
        ]


indexTests : Test
indexTests =
    describe "index"
        [ evalJsonModule "index 0"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString (Json.Decode.index 0 Json.Decode.string) "[\\"alice\\",\\"bob\\"]"
"""
            (ok (String "alice"))
        , evalJsonModule "index 1"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString (Json.Decode.index 1 Json.Decode.string) "[\\"alice\\",\\"bob\\"]"
"""
            (ok (String "bob"))
        ]


mapTests : Test
mapTests =
    describe "map"
        [ evalJsonModule "map String.length"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString (Json.Decode.map String.length Json.Decode.string) "\\"hello\\""
"""
            (ok (Int 5))
        , evalJsonModule "map2"
            """module Test exposing (main)

import Json.Decode exposing (field)

main =
    let
        decoder =
            Json.Decode.map2 Tuple.pair
                (field "x" Json.Decode.int)
                (field "y" Json.Decode.int)
    in
    Json.Decode.decodeString decoder "{\\"x\\":3,\\"y\\":4}"
"""
            (ok (Tuple (Int 3) (Int 4)))
        ]


listTests : Test
listTests =
    describe "list"
        [ evalJsonModule "decode list of ints"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString (Json.Decode.list Json.Decode.int) "[1,2,3]"
"""
            (ok (List [ Int 1, Int 2, Int 3 ]))
        , evalJsonModule "decode empty list"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString (Json.Decode.list Json.Decode.string) "[]"
"""
            (ok (List []))
        ]


nullableTests : Test
nullableTests =
    describe "nullable"
        [ evalJsonModule "nullable int with value"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString (Json.Decode.nullable Json.Decode.int) "42"
"""
            (ok (Custom { moduleName = [ "Maybe" ], name = "Just" } [ Int 42 ]))
        , evalJsonModule "nullable int with null"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString (Json.Decode.nullable Json.Decode.int) "null"
"""
            (ok (Custom { moduleName = [ "Maybe" ], name = "Nothing" } []))
        ]


oneOfTests : Test
oneOfTests =
    describe "oneOf"
        [ evalJsonModule "oneOf first matches"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString (Json.Decode.oneOf [ Json.Decode.int, Json.Decode.succeed 0 ]) "42"
"""
            (ok (Int 42))
        , evalJsonModule "oneOf falls through"
            """module Test exposing (main)

import Json.Decode

main =
    Json.Decode.decodeString (Json.Decode.oneOf [ Json.Decode.string, Json.Decode.succeed \"fallback\" ]) "42"
"""
            (ok (String "fallback"))
        ]


andThenTests : Test
andThenTests =
    describe "andThen"
        [ evalJsonModule "andThen basic"
            """module Test exposing (main)

import Json.Decode

main =
    let
        decoder =
            Json.Decode.int
                |> Json.Decode.andThen
                    (\\n ->
                        if n > 0 then
                            Json.Decode.succeed "positive"
                        else
                            Json.Decode.succeed "non-positive"
                    )
    in
    Json.Decode.decodeString decoder "42"
"""
            (ok (String "positive"))
        ]
