port module Benchmark exposing (main)

{-| Benchmark harness for the interpreter.
Compile with: elm make benchmark/Benchmark.elm --optimize --output=benchmark/benchmark.js
Run with: node benchmark/run.js
-}

import Elm.Syntax.Expression as Expression
import Eval.Module
import Json.Encode as Encode
import Platform
import Types exposing (Value(..))


port reportResults : Encode.Value -> Cmd msg


type alias Flags =
    ()


type Msg
    = NoOp


main : Program Flags () Msg
main =
    Platform.worker
        { init = \_ -> ( (), reportResults (runAllBenchmarks ()) )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


runAllBenchmarks : () -> Encode.Value
runAllBenchmarks () =
    Encode.list identity
        [ runBench "List.foldl 10k" listFoldl10k
        , runBench "List.map 10k" listMap10k
        , runBench "List.concatMap 5k" listConcatMap5k
        , runBench "List.filterMap 5k" listFilterMap5k
        , runBench "List.indexedMap 5k" listIndexedMap5k
        , runBench "List.any 5k (worst)" listAny5k
        , runBench "Dict.foldl 1k (10x)" dictFoldl1k
        , runBench "Dict.map 1k (5x)" dictMap1k
        , runBench "TCO countdown 50k" tcoCountdown50k
        , runBench "ReviewRule load+eval" reviewRuleEval
        ]


runBench : String -> (() -> Result String String) -> Encode.Value
runBench name thunk =
    case thunk () of
        Ok result ->
            Encode.object
                [ ( "name", Encode.string name )
                , ( "status", Encode.string "ok" )
                , ( "result", Encode.string result )
                ]

        Err err ->
            Encode.object
                [ ( "name", Encode.string name )
                , ( "status", Encode.string "error" )
                , ( "error", Encode.string err )
                ]


evalSimple : String -> Result String String
evalSimple source =
    case Eval.Module.evalProject [ source ] (Expression.FunctionOrValue [] "main") of
        Ok (Int n) ->
            Ok (String.fromInt n)

        Ok (Bool b) ->
            Ok
                (if b then
                    "True"

                 else
                    "False"
                )

        Ok (String s) ->
            Ok s

        Ok _ ->
            Ok "other"

        Err _ ->
            Err "eval error"



-- Benchmarks


listFoldl10k : () -> Result String String
listFoldl10k () =
    evalSimple "module T exposing (main)\nmain = List.foldl (+) 0 (List.range 1 10000)"


listMap10k : () -> Result String String
listMap10k () =
    evalSimple "module T exposing (main)\nmain = List.map (\\n -> n * 2) (List.range 1 10000) |> List.length"


listConcatMap5k : () -> Result String String
listConcatMap5k () =
    evalSimple "module T exposing (main)\nmain = List.concatMap (\\n -> [n, n * 2]) (List.range 1 5000) |> List.length"


listFilterMap5k : () -> Result String String
listFilterMap5k () =
    evalSimple "module T exposing (main)\nmain = List.filterMap (\\n -> if modBy 2 n == 0 then Just n else Nothing) (List.range 1 5000) |> List.length"


listIndexedMap5k : () -> Result String String
listIndexedMap5k () =
    evalSimple "module T exposing (main)\nmain = List.indexedMap (\\i x -> i + x) (List.range 1 5000) |> List.foldl (+) 0"


listAny5k : () -> Result String String
listAny5k () =
    evalSimple "module T exposing (main)\nmain = List.any (\\n -> n == 5000) (List.range 1 5000)"


dictFoldl1k : () -> Result String String
dictFoldl1k () =
    evalSimple """module T exposing (main)
import Dict
main =
    let
        d = List.foldl (\\n acc -> Dict.insert (String.fromInt n) n acc) Dict.empty (List.range 1 1000)
        s x = Dict.foldl (\\_ v acc -> acc + v) 0 x
    in
    s d + s d + s d + s d + s d + s d + s d + s d + s d + s d
"""


dictMap1k : () -> Result String String
dictMap1k () =
    evalSimple """module T exposing (main)
import Dict
main =
    let
        d = List.foldl (\\n acc -> Dict.insert (String.fromInt n) n acc) Dict.empty (List.range 1 1000)
        d1 = Dict.map (\\_ v -> v + 1) d
        d2 = Dict.map (\\_ v -> v + 1) d1
        d3 = Dict.map (\\_ v -> v + 1) d2
        d4 = Dict.map (\\_ v -> v + 1) d3
        d5 = Dict.map (\\_ v -> v + 1) d4
    in
    Dict.foldl (\\_ v acc -> acc + v) 0 d5
"""


tcoCountdown50k : () -> Result String String
tcoCountdown50k () =
    evalSimple "module T exposing (main)\ngo acc n = if n <= 0 then acc else go (acc + n) (n - 1)\nmain = go 0 50000"


reviewRuleEval : () -> Result String String
reviewRuleEval () =
    -- Heavy computation: build large data structures through the interpreter
    evalSimple """module T exposing (main)
import Dict

main =
    let
        d1 = List.foldl (\\n acc -> Dict.insert (String.fromInt n) n acc) Dict.empty (List.range 1 2000)
        d2 = Dict.map (\\_ v -> v * 3) d1
        d3 = Dict.filter (\\_ v -> modBy 3 v == 0) d2
        total = Dict.foldl (\\_ v acc -> acc + v) 0 d3
        keys = Dict.foldl (\\k _ acc -> k :: acc) [] d3 |> List.length
    in
    total + keys
"""
