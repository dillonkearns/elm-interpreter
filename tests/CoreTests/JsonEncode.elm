module CoreTests.JsonEncode exposing (suite)

import Elm.Syntax.Expression as Expression
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Json.Encode"
        [ primitiveTests
        , collectionTests
        , objectTests
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


primitiveTests : Test
primitiveTests =
    describe "Primitives"
        [ evalJsonModule "encode int"
            """module Test exposing (main)

import Json.Encode

main =
    Json.Encode.encode 0 (Json.Encode.int 42)
"""
            (String "42")
        , evalJsonModule "encode negative int"
            """module Test exposing (main)

import Json.Encode

main =
    Json.Encode.encode 0 (Json.Encode.int -7)
"""
            (String "-7")
        , evalJsonModule "encode string"
            """module Test exposing (main)

import Json.Encode

main =
    Json.Encode.encode 0 (Json.Encode.string "hello")
"""
            (String "\"hello\"")
        , evalJsonModule "encode float"
            """module Test exposing (main)

import Json.Encode

main =
    Json.Encode.encode 0 (Json.Encode.float 3.14)
"""
            (String "3.14")
        , evalJsonModule "encode bool true"
            """module Test exposing (main)

import Json.Encode

main =
    Json.Encode.encode 0 (Json.Encode.bool True)
"""
            (String "true")
        , evalJsonModule "encode bool false"
            """module Test exposing (main)

import Json.Encode

main =
    Json.Encode.encode 0 (Json.Encode.bool False)
"""
            (String "false")
        , evalJsonModule "encode null"
            """module Test exposing (main)

import Json.Encode

main =
    Json.Encode.encode 0 Json.Encode.null
"""
            (String "null")
        ]


collectionTests : Test
collectionTests =
    describe "Collections"
        [ evalJsonModule "encode list of ints"
            """module Test exposing (main)

import Json.Encode

main =
    Json.Encode.encode 0 (Json.Encode.list Json.Encode.int [1, 2, 3])
"""
            (String "[1,2,3]")
        , evalJsonModule "encode empty list"
            """module Test exposing (main)

import Json.Encode

main =
    Json.Encode.encode 0 (Json.Encode.list Json.Encode.string [])
"""
            (String "[]")
        , evalJsonModule "encode list of strings"
            """module Test exposing (main)

import Json.Encode

main =
    Json.Encode.encode 0 (Json.Encode.list Json.Encode.string ["a", "b"])
"""
            (String """["a","b"]""")
        ]


objectTests : Test
objectTests =
    describe "Objects"
        [ evalJsonModule "encode object"
            """module Test exposing (main)

import Json.Encode

main =
    Json.Encode.encode 0 (Json.Encode.object [ ( "name", Json.Encode.string "Tom" ), ( "age", Json.Encode.int 42 ) ])
"""
            (String """{"name":"Tom","age":42}""")
        , evalJsonModule "encode empty object"
            """module Test exposing (main)

import Json.Encode

main =
    Json.Encode.encode 0 (Json.Encode.object [])
"""
            (String "{}")
        ]
