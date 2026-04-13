module KernelGapTests exposing (suite)

import Elm.Syntax.Expression as Expression
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import TestUtils exposing (evalTest, list, maybe, tuple)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Kernel gap tests"
        [ listMapNTests
        , stringTests
        , bytesFloatTests
        , bytesEdgeCaseTests
        ]


evalMod : String -> String -> (a -> Value) -> a -> Test
evalMod name source toValue a =
    test name <|
        \_ ->
            Eval.Module.eval source (Expression.FunctionOrValue [] "main")
                |> Expect.equal (Ok (toValue a))


listMapNTests : Test
listMapNTests =
    describe "List.mapN"
        [ evalTest "List.map2"
            "List.map2 (+) [1, 2, 3] [10, 20, 30]"
            (list Int)
            [ 11, 22, 33 ]
        , evalTest "List.map2 uneven"
            "List.map2 (+) [1, 2] [10, 20, 30]"
            (list Int)
            [ 11, 22 ]
        , evalTest "List.map3"
            "let add3 a b c = a + b + c in List.map3 add3 [1,2] [10,20] [100,200]"
            (list Int)
            [ 111, 222 ]
        , evalTest "List.map4"
            "let add4 a b c d = a + b + c + d in List.map4 add4 [1] [10] [100] [1000]"
            (list Int)
            [ 1111 ]
        , evalTest "List.map5"
            "let add5 a b c d e = a + b + c + d + e in List.map5 add5 [1] [2] [3] [4] [5]"
            (list Int)
            [ 15 ]
        ]


stringTests : Test
stringTests =
    describe "String kernel functions"
        [ evalTest "String.any digit"
            """String.any Char.isDigit "abc123" """
            Bool
            True
        , evalTest "String.any no match"
            """String.any Char.isDigit "abc" """
            Bool
            False
        , evalTest "String.all digits"
            """String.all Char.isDigit "123" """
            Bool
            True
        , evalTest "String.all mixed"
            """String.all Char.isDigit "12a3" """
            Bool
            False
        , evalTest "String.map toUpper"
            """String.map Char.toUpper "hello" """
            String
            "HELLO"
        ]


bytesFloatTests : Test
bytesFloatTests =
    describe "Bytes float encoding"
        [ evalMod "float64 BE round-trip 0.0"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
import Bytes.Decode
main = Bytes.Decode.decode (Bytes.Decode.float64 Bytes.BE) (Bytes.Encode.encode (Bytes.Encode.float64 Bytes.BE 0.0))
"""
            (maybe Float)
            (Just 0.0)
        , evalMod "float64 BE round-trip 1.0"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
import Bytes.Decode
main = Bytes.Decode.decode (Bytes.Decode.float64 Bytes.BE) (Bytes.Encode.encode (Bytes.Encode.float64 Bytes.BE 1.0))
"""
            (maybe Float)
            (Just 1.0)
        , evalMod "float64 BE round-trip -42.5"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
import Bytes.Decode
main = Bytes.Decode.decode (Bytes.Decode.float64 Bytes.BE) (Bytes.Encode.encode (Bytes.Encode.float64 Bytes.BE -42.5))
"""
            (maybe Float)
            (Just -42.5)
        , evalMod "float32 BE round-trip 0.0"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
import Bytes.Decode
main = Bytes.Decode.decode (Bytes.Decode.float32 Bytes.BE) (Bytes.Encode.encode (Bytes.Encode.float32 Bytes.BE 0.0))
"""
            (maybe Float)
            (Just 0.0)
        ]


bytesEdgeCaseTests : Test
bytesEdgeCaseTests =
    describe "Bytes edge cases"
        [ evalMod "decode too few bytes returns Nothing"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
import Bytes.Decode
main = Bytes.Decode.decode (Bytes.Decode.unsignedInt16 Bytes.BE) (Bytes.Encode.encode (Bytes.Encode.unsignedInt8 42))
"""
            (maybe Int)
            Nothing
        , evalMod "encode then decode sequence"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
import Bytes.Decode
main = Bytes.Decode.decode (Bytes.Decode.map2 Tuple.pair Bytes.Decode.unsignedInt8 Bytes.Decode.unsignedInt8) (Bytes.Encode.encode (Bytes.Encode.sequence [Bytes.Encode.unsignedInt8 1, Bytes.Encode.unsignedInt8 2]))
"""
            (maybe (tuple Int Int))
            (Just ( 1, 2 ))
        , evalMod "getStringWidth unicode"
            """module Temp exposing (main)
import Bytes.Encode
main = Bytes.Encode.getStringWidth "€"
"""
            Int
            3
        , evalMod "encode/decode LE uint16"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
import Bytes.Decode
main = Bytes.Decode.decode (Bytes.Decode.unsignedInt16 Bytes.LE) (Bytes.Encode.encode (Bytes.Encode.unsignedInt16 Bytes.LE 256))
"""
            (maybe Int)
            (Just 256)
        , evalMod "int32 round-trip"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
import Bytes.Decode
main = Bytes.Decode.decode (Bytes.Decode.signedInt32 Bytes.BE) (Bytes.Encode.encode (Bytes.Encode.signedInt32 Bytes.BE -100000))
"""
            (maybe Int)
            (Just -100000)
        , evalMod "Bytes.Decode.andThen"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
import Bytes.Decode
main =
    let
        decoder = Bytes.Decode.unsignedInt8 |> Bytes.Decode.andThen (\\n -> Bytes.Decode.succeed (n * 2))
        encoded = Bytes.Encode.encode (Bytes.Encode.unsignedInt8 21)
    in
    Bytes.Decode.decode decoder encoded
"""
            (maybe Int)
            (Just 42)
        ]
