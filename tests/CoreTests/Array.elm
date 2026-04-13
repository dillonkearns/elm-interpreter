module CoreTests.Array exposing (suite)

import Array exposing (Array)
import Fuzz exposing (Fuzzer, intRange)
import Test exposing (Test, describe, fuzz, fuzz2)
import TestUtils exposing (evalExpect, evalTest, list, maybe, withInt)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Array"
        [ initTests
        , isEmptyTests
        , lengthTests
        , getSetTests
        , conversionTests
        , transformTests
        , sliceTests
        , runtimeCrashTests
        ]


defaultSizeRange : Fuzzer Int
defaultSizeRange =
    intRange 1 100


fuzzEvalTest : String -> String -> (a -> Value) -> (Int -> a) -> Test
fuzzEvalTest name source kind value =
    fuzz defaultSizeRange name <|
        \size ->
            evalExpect (withInt "size" size source)
                kind
                (value size)


initTests : Test
initTests =
    describe "Initialization"
        [ fuzzEvalTest "initialize"
            "Array.toList <| Array.initialize size identity"
            List
            (\size -> Array.toList <| Array.initialize size Int)
        , fuzzEvalTest "push"
            "Array.toList <| List.foldl Array.push Array.empty (List.range 0 (size - 1))"
            List
            (\size -> Array.toList <| List.foldl Array.push Array.empty (List.map Int <| List.range 0 (size - 1)))
        , evalTest "initialize non-identity"
            "Array.toList (Array.initialize 4 (\\n -> n * n))"
            (list Int)
            (Array.toList (Array.initialize 4 (\n -> n * n)))
        , evalTest "initialize empty"
            "Array.toList (Array.initialize 0 identity)"
            (list Int)
            (Array.toList (Array.initialize 0 identity))
        , evalTest "initialize negative"
            "Array.toList (Array.initialize -2 identity)"
            (list Int)
            (Array.toList (Array.initialize -2 identity))
        , evalTest "initialize large (10000 elements)"
            "Array.length (Array.initialize 10000 identity)"
            Int
            (Array.length (Array.initialize 10000 identity))
        ]


isEmptyTests : Test
isEmptyTests =
    describe "isEmpty"
        [ evalTest "all empty arrays are equal"
            "(Array.empty == Array.fromList [])"
            Bool
            (Array.empty == Array.fromList [])
        , evalTest "empty array"
            "Array.isEmpty Array.empty"
            Bool
            (Array.isEmpty Array.empty)
        , evalTest "empty converted array"
            "Array.isEmpty (Array.fromList [])"
            Bool
            (Array.isEmpty (Array.fromList []))
        , evalTest "non-empty array"
            "Array.isEmpty (Array.fromList [ 1 ])"
            Bool
            (Array.isEmpty (Array.fromList [ 1 ]))
        ]


lengthTests : Test
lengthTests =
    describe "Length"
        [ evalTest "empty array"
            "Array.length Array.empty"
            Int
            (Array.length Array.empty)
        , fuzzEvalTest "non-empty array"
            "Array.length (Array.initialize size identity)"
            Int
            (\size -> Array.length (Array.initialize size identity))
        , fuzzEvalTest "push"
            "Array.length (Array.push size (Array.initialize size identity))"
            Int
            (\size -> Array.length (Array.push size (Array.initialize size identity)))
        , fuzzEvalTest "append"
            "Array.length (Array.append (Array.initialize size identity) (Array.initialize (size // 2) identity))"
            Int
            (\size -> Array.length (Array.append (Array.initialize size identity) (Array.initialize (size // 2) identity)))
        , fuzzEvalTest "set does not increase"
            "Array.length (Array.set (size // 2) 1 (Array.initialize size identity))"
            Int
            (\size -> Array.length (Array.set (size // 2) 1 (Array.initialize size identity)))
        ]


getSetTests : Test
getSetTests =
    describe "Get and set"
        [ fuzz2 defaultSizeRange defaultSizeRange "can retrieve element" <|
            \x y ->
                let
                    n =
                        min x y

                    size =
                        max x y
                in
                evalExpect
                    ("Array.get " ++ String.fromInt n ++ " (Array.initialize " ++ String.fromInt (size + 1) ++ " identity)")
                    (maybe Int)
                    (Array.get n (Array.initialize (size + 1) identity))
        , fuzz2 defaultSizeRange defaultSizeRange "set replaces value" <|
            \x y ->
                let
                    n =
                        min x y

                    size =
                        max x y
                in
                evalExpect
                    ("Array.get " ++ String.fromInt n ++ " (Array.set " ++ String.fromInt n ++ " 5 (Array.initialize " ++ String.fromInt (size + 1) ++ " identity))")
                    (maybe Int)
                    (Array.get n (Array.set n 5 (Array.initialize (size + 1) identity)))
        , evalTest "Retrieval works from tail"
            "Array.get 1030 (Array.set 1030 5 (Array.initialize 1035 identity))"
            (maybe Int)
            (Array.get 1030 (Array.set 1030 5 (Array.initialize 1035 identity)))
        ]


conversionTests : Test
conversionTests =
    describe "Conversion"
        [ fuzzEvalTest "back and forth"
            "Array.toList (Array.fromList (List.range 0 (size - 1)))"
            (list Int)
            (\size -> List.range 0 (size - 1))
        , fuzzEvalTest "indexed"
            "Array.toIndexedList (Array.initialize size ((+) 1)) == Array.toList (Array.initialize size (\\idx -> ( idx, idx + 1 )))"
            Bool
            (\size -> Array.toIndexedList (Array.initialize size ((+) 1)) == Array.toList (Array.initialize size (\idx -> ( idx, idx + 1 ))))
        ]


transformTests : Test
transformTests =
    describe "Transform"
        [ fuzzEvalTest "foldl"
            "Array.foldl (\\n acc -> n :: acc) [] (Array.initialize size identity)"
            (list Int)
            (\size -> List.reverse (List.range 0 (size - 1)))
        , fuzzEvalTest "foldr"
            "Array.foldr (\\n acc -> n :: acc) [] (Array.initialize size identity)"
            (list Int)
            (\size -> List.range 0 (size - 1))
        , fuzzEvalTest "filter"
            "Array.toList (Array.filter (\\a -> modBy 2 a == 0) (Array.initialize size identity))"
            (list Int)
            (\size -> List.filter (\a -> modBy 2 a == 0) (List.range 0 (size - 1)))
        , fuzzEvalTest "map"
            "Array.map ((+) 1) (Array.initialize size identity) == Array.initialize size ((+) 1)"
            Bool
            (\size -> Array.map ((+) 1) (Array.initialize size identity) == Array.initialize size ((+) 1))
        , fuzzEvalTest "indexedMap"
            "Array.indexedMap (*) (Array.repeat size 5) == Array.initialize size ((*) 5)"
            Bool
            (\size -> Array.indexedMap (*) (Array.repeat size 5) == Array.initialize size ((*) 5))
        , fuzzEvalTest "push appends one element"
            "Array.push size (Array.initialize size identity) == Array.initialize (size + 1) identity"
            Bool
            (\size -> Array.push size (Array.initialize size identity) == Array.initialize (size + 1) identity)
        , fuzzEvalTest "append"
            "Array.append (Array.initialize size identity) (Array.initialize size ((+) size)) == Array.initialize (size * 2) identity"
            Bool
            (\size -> Array.append (Array.initialize size identity) (Array.initialize size ((+) size)) == Array.initialize (size * 2) identity)
        ]


sliceTests : Test
sliceTests =
    let
        smallSample : Array Int
        smallSample =
            Array.fromList (List.range 1 8)
    in
    describe "Slice"
        [ evalTest "both small"
            "let smallSample = Array.fromList (List.range 1 8) in Array.toList (Array.slice 2 5 smallSample)"
            (list Int)
            (Array.toList (Array.slice 2 5 smallSample))
        , evalTest "start small"
            "let smallSample = Array.fromList (List.range 1 8) in Array.toList (Array.slice 2 (Array.length smallSample) smallSample)"
            (list Int)
            (Array.toList (Array.slice 2 (Array.length smallSample) smallSample))
        , evalTest "negative"
            "let smallSample = Array.fromList (List.range 1 8) in Array.toList (Array.slice -5 -2 smallSample)"
            (list Int)
            (Array.toList (Array.slice -5 -2 smallSample))
        , evalTest "impossible"
            "let smallSample = Array.fromList (List.range 1 8) in Array.toList (Array.slice -1 -2 smallSample)"
            (list Int)
            (Array.toList (Array.slice -1 -2 smallSample))
        ]


runtimeCrashTests : Test
runtimeCrashTests =
    describe "Runtime crashes in core"
        [ evalTest "magic slice"
            """Array.initialize 40 identity
                    |> Array.slice 10 40
                    |> Array.slice 10 30
                    |> Array.slice 10 20
                    |> Array.slice 10 10
                    |> (\\a -> a == a)"""
            Bool
            (Array.initialize 40 identity
                |> Array.slice 10 40
                |> Array.slice 10 30
                |> Array.slice 10 20
                |> Array.slice 10 10
                |> (\a -> a == a)
            )
        , evalTest "magic slice 2"
            "let ary = Array.fromList (List.range 0 32) in let res = Array.append (Array.slice 1 32 ary) (Array.slice (32 + 1) -1 ary) in res == res"
            Bool
            True
        , evalTest "magic append"
            """let res = Array.append (Array.initialize 1 (always 1)) (Array.initialize (32 ^ 2 - 1 * 32 + 1) (\\i -> i)) in res == res"""
            Bool
            True
        ]
