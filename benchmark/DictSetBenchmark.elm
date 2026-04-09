port module DictSetBenchmark exposing (main)

{-| Focused benchmark harness for Dict/Set operations through the interpreter.

Compile with:
    elm make benchmark/DictSetBenchmark.elm --optimize --output=benchmark/dict-set-benchmark.js

Run with:
    node benchmark/run-dict-set.js

Takes a single benchmark name as a flag and runs just that one benchmark,
so per-benchmark wall-clock can be measured cleanly from the outer Node process.

-}

import Elm.Syntax.Expression as Expression
import Eval.Module
import Json.Encode as Encode
import Platform
import Types exposing (Value(..))


port reportResults : Encode.Value -> Cmd msg


type alias Flags =
    { benchmark : String }


type Msg
    = NoOp


main : Program Flags () Msg
main =
    Platform.worker
        { init = \flags -> ( (), reportResults (runBenchmark flags.benchmark) )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


runBenchmark : String -> Encode.Value
runBenchmark name =
    let
        thunk : () -> Result String String
        thunk =
            case name of
                "dict_insert_string_1k" ->
                    dictInsertString1k

                "dict_insert_string_5k" ->
                    dictInsertString5k

                "dict_insert_tuple_1k" ->
                    dictInsertTuple1k

                "dict_union_string_1k" ->
                    dictUnionString1k

                "dict_member_string_1k" ->
                    dictMemberString1k

                "dict_get_string_1k" ->
                    dictGetString1k

                "dict_foldl_string_1k" ->
                    dictFoldlString1k

                "set_insert_string_1k" ->
                    setInsertString1k

                "set_union_string_1k" ->
                    setUnionString1k

                "set_member_string_1k" ->
                    setMemberString1k

                "review_rule_mock" ->
                    reviewRuleMock

                "review_rule_mock_large" ->
                    reviewRuleMockLarge

                _ ->
                    \() -> Err ("Unknown benchmark: " ++ name)
    in
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



-- BENCHMARKS
--
-- Each benchmark is shaped to match a piece of the elm-review workload:
-- building and merging Dicts/Sets of keys that look like the ones used
-- in NoUnused.Exports, NoUnused.Parameters, etc.


{-| Build a Dict with 1000 insert calls, then return its size.
Measures raw Dict.insert cost on String keys.
-}
dictInsertString1k : () -> Result String String
dictInsertString1k () =
    evalSimple """module T exposing (main)
import Dict
main =
    List.foldl (\\n acc -> Dict.insert (String.fromInt n) n acc) Dict.empty (List.range 1 1000)
        |> Dict.size
"""


{-| Same as above but with 5000 inserts to stress the insert path harder.
-}
dictInsertString5k : () -> Result String String
dictInsertString5k () =
    evalSimple """module T exposing (main)
import Dict
main =
    List.foldl (\\n acc -> Dict.insert (String.fromInt n) n acc) Dict.empty (List.range 1 5000)
        |> Dict.size
"""


{-| Dict with tuple-of-String keys — matches the ElementIdentifier
(ModuleNameStr, String) shape used by NoUnused.Exports and friends.
-}
dictInsertTuple1k : () -> Result String String
dictInsertTuple1k () =
    evalSimple """module T exposing (main)
import Dict
main =
    List.foldl
        (\\n acc ->
            Dict.insert ( "Module" ++ String.fromInt (modBy 10 n), String.fromInt n ) n acc
        )
        Dict.empty
        (List.range 1 1000)
        |> Dict.size
"""


{-| Build two Dicts and union them — this is the hot path for
`foldProjectContexts` in elm-review project rules.
-}
dictUnionString1k : () -> Result String String
dictUnionString1k () =
    evalSimple """module T exposing (main)
import Dict
main =
    let
        d1 = List.foldl (\\n acc -> Dict.insert (String.fromInt n) n acc) Dict.empty (List.range 1 1000)
        d2 = List.foldl (\\n acc -> Dict.insert (String.fromInt n) n acc) Dict.empty (List.range 500 1500)
        merged = Dict.union d1 d2
    in
    Dict.size merged
"""


{-| Check membership of 1000 keys against a 1000-entry Dict.
-}
dictMemberString1k : () -> Result String String
dictMemberString1k () =
    evalSimple """module T exposing (main)
import Dict
main =
    let
        d = List.foldl (\\n acc -> Dict.insert (String.fromInt n) n acc) Dict.empty (List.range 1 1000)
    in
    List.foldl
        (\\n count ->
            if Dict.member (String.fromInt n) d then
                count + 1

            else
                count
        )
        0
        (List.range 1 1000)
"""


dictGetString1k : () -> Result String String
dictGetString1k () =
    evalSimple """module T exposing (main)
import Dict
main =
    let
        d = List.foldl (\\n acc -> Dict.insert (String.fromInt n) n acc) Dict.empty (List.range 1 1000)
    in
    List.foldl
        (\\n total ->
            case Dict.get (String.fromInt n) d of
                Just v ->
                    total + v

                Nothing ->
                    total
        )
        0
        (List.range 1 1000)
"""


dictFoldlString1k : () -> Result String String
dictFoldlString1k () =
    evalSimple """module T exposing (main)
import Dict
main =
    let
        d = List.foldl (\\n acc -> Dict.insert (String.fromInt n) n acc) Dict.empty (List.range 1 1000)
    in
    Dict.foldl (\\_ v acc -> acc + v) 0 d
"""


setInsertString1k : () -> Result String String
setInsertString1k () =
    evalSimple """module T exposing (main)
import Set
main =
    List.foldl (\\n acc -> Set.insert (String.fromInt n) acc) Set.empty (List.range 1 1000)
        |> Set.size
"""


setUnionString1k : () -> Result String String
setUnionString1k () =
    evalSimple """module T exposing (main)
import Set
main =
    let
        s1 = List.foldl (\\n acc -> Set.insert (String.fromInt n) acc) Set.empty (List.range 1 1000)
        s2 = List.foldl (\\n acc -> Set.insert (String.fromInt n) acc) Set.empty (List.range 500 1500)
        merged = Set.union s1 s2
    in
    Set.size merged
"""


setMemberString1k : () -> Result String String
setMemberString1k () =
    evalSimple """module T exposing (main)
import Set
main =
    let
        s = List.foldl (\\n acc -> Set.insert (String.fromInt n) acc) Set.empty (List.range 1 1000)
    in
    List.foldl
        (\\n count ->
            if Set.member (String.fromInt n) s then
                count + 1

            else
                count
        )
        0
        (List.range 1 1000)
"""


{-| Mini approximation of an `ImportersOf` fold: build N module-level
contexts and merge them into a project context. This is the shape of
work that dominates `foldProjectContexts` in rules like NoUnused.Exports.
-}
reviewRuleMock : () -> Result String String
reviewRuleMock () =
    evalSimple """module T exposing (main)
import Dict
import Set
main =
    let
        buildModuleContext i =
            { usedValues =
                List.foldl
                    (\\n acc -> Set.insert ( "Module" ++ String.fromInt i, "fn" ++ String.fromInt n ) acc)
                    Set.empty
                    (List.range 1 50)
            , exports =
                List.foldl
                    (\\n acc -> Dict.insert ( "Module" ++ String.fromInt i, "fn" ++ String.fromInt n ) n acc)
                    Dict.empty
                    (List.range 1 50)
            }

        modules =
            List.map buildModuleContext (List.range 1 20)

        foldProjectContext m acc =
            { usedValues = Set.union m.usedValues acc.usedValues
            , exports = Dict.union m.exports acc.exports
            }

        empty =
            { usedValues = Set.empty, exports = Dict.empty }

        project =
            List.foldl foldProjectContext empty modules
    in
    Set.size project.usedValues + Dict.size project.exports
"""


{-| Same shape as review_rule_mock, scaled up ~10x so the profile has
enough Elm-side samples to see the hot frames above Node startup.
-}
reviewRuleMockLarge : () -> Result String String
reviewRuleMockLarge () =
    evalSimple """module T exposing (main)
import Dict
import Set
main =
    let
        buildModuleContext i =
            { usedValues =
                List.foldl
                    (\\n acc -> Set.insert ( "Module" ++ String.fromInt i, "fn" ++ String.fromInt n ) acc)
                    Set.empty
                    (List.range 1 200)
            , exports =
                List.foldl
                    (\\n acc -> Dict.insert ( "Module" ++ String.fromInt i, "fn" ++ String.fromInt n ) n acc)
                    Dict.empty
                    (List.range 1 200)
            }

        modules =
            List.map buildModuleContext (List.range 1 100)

        foldProjectContext m acc =
            { usedValues = Set.union m.usedValues acc.usedValues
            , exports = Dict.union m.exports acc.exports
            }

        empty =
            { usedValues = Set.empty, exports = Dict.empty }

        project =
            List.foldl foldProjectContext empty modules
    in
    Set.size project.usedValues + Dict.size project.exports
"""
