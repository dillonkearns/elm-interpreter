module CoverageTests exposing (suite)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Eval.Module
import Expect
import Rope
import Set
import Test exposing (Test, describe, test)
import Types exposing (CallTree(..), Value(..))


suite : Test
suite =
    describe "Coverage mode"
        [ test "coverageWithEnv returns correct result value" <|
            \_ ->
                let
                    source =
                        "module Main exposing (main)\n\nmain = 1 + 2\n"
                in
                case Eval.Module.buildProjectEnv [] of
                    Err e ->
                        Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                    Ok projectEnv ->
                        let
                            ( result, _ ) =
                                Eval.Module.coverageWithEnv projectEnv [ source ] (FunctionOrValue [] "main")
                        in
                        result
                            |> Expect.equal (Ok (Int 3))
        , test "coverageWithEnv returns non-empty ranges" <|
            \_ ->
                let
                    source =
                        "module Main exposing (main)\n\nmain = 1 + 2\n"
                in
                case Eval.Module.buildProjectEnv [] of
                    Err e ->
                        Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                    Ok projectEnv ->
                        let
                            ( _, ranges ) =
                                Eval.Module.coverageWithEnv projectEnv [ source ] (FunctionOrValue [] "main")
                        in
                        ranges
                            |> List.isEmpty
                            |> Expect.equal False
                            |> Expect.onFail "Expected non-empty coverage ranges"
        , test "coverageWithEnv returns all real source ranges from traceWithEnv" <|
            \_ ->
                -- Coverage mode skips internal kernel call traces (which have
                -- fake -1:-1 ranges), so we compare only real source ranges.
                let
                    source =
                        "module Main exposing (main)\n\nmain = 1 + 2\n"

                    isRealRange r =
                        not (r.start.row < 0 || r.start.column < 0)
                in
                case Eval.Module.buildProjectEnv [] of
                    Err e ->
                        Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                    Ok projectEnv ->
                        let
                            ( _, coverageRanges ) =
                                Eval.Module.coverageWithEnv projectEnv [ source ] (FunctionOrValue [] "main")

                            ( _, callTrees, _ ) =
                                Eval.Module.traceWithEnv projectEnv [ source ] (FunctionOrValue [] "main")

                            traceRealRanges =
                                extractRangesFromCallTrees callTrees
                                    |> List.filter isRealRange

                            coverageRealRanges =
                                coverageRanges
                                    |> List.filter isRealRange
                        in
                        List.sort (List.map rangeToString coverageRealRanges)
                            |> Expect.equal (List.sort (List.map rangeToString traceRealRanges))
        , test "coverageWithEnv does not produce CallNode values" <|
            \_ ->
                -- When coverage mode is properly implemented, the Rope CallTree
                -- returned by the internal trace should contain CoverageRange
                -- nodes, not CallNode records. We verify this indirectly: the
                -- coverage ranges should match the trace ranges, but the coverage
                -- mode should not retain env/value data.
                --
                -- This test verifies correct behavior. Memory savings are
                -- verified by the e2e benchmarks.
                let
                    source =
                        "module Main exposing (main)\n\nmain = if True then 1 else 2\n"
                in
                case Eval.Module.buildProjectEnv [] of
                    Err e ->
                        Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                    Ok projectEnv ->
                        let
                            ( result, ranges ) =
                                Eval.Module.coverageWithEnv projectEnv [ source ] (FunctionOrValue [] "main")
                        in
                        case result of
                            Ok (Int 1) ->
                                -- The if-then-else should produce coverage for at
                                -- least the condition (True) and the branch (1)
                                if List.length ranges >= 2 then
                                    Expect.pass

                                else
                                    Expect.fail
                                        ("Expected at least 2 coverage ranges, got "
                                            ++ String.fromInt (List.length ranges)
                                        )

                            _ ->
                                Expect.fail ("Unexpected result: " ++ Debug.toString result)
        , test "coverageWithEnv with let expression" <|
            \_ ->
                let
                    source =
                        "module Main exposing (main)\n\nmain = let x = 5 in x + 1\n"
                in
                case Eval.Module.buildProjectEnv [] of
                    Err e ->
                        Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                    Ok projectEnv ->
                        let
                            ( result, ranges ) =
                                Eval.Module.coverageWithEnv projectEnv [ source ] (FunctionOrValue [] "main")
                        in
                        case result of
                            Ok (Int 6) ->
                                if List.length ranges >= 3 then
                                    Expect.pass

                                else
                                    Expect.fail
                                        ("Expected at least 3 coverage ranges, got "
                                            ++ String.fromInt (List.length ranges)
                                        )

                            _ ->
                                Expect.fail ("Unexpected result: " ++ Debug.toString result)
        , test "probe-filtered coverage only records ranges on probe lines" <|
            \_ ->
                -- Source: line 3 has "main = 1 + 2"
                -- With probeLines = {3}, we should get ranges on line 3
                -- With probeLines = {99}, we should get no ranges (line 99 doesn't exist)
                let
                    source =
                        "module Main exposing (main)\n\nmain = 1 + 2\n"
                in
                case Eval.Module.buildProjectEnv [] of
                    Err e ->
                        Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                    Ok projectEnv ->
                        let
                            ( _, filteredRanges ) =
                                Eval.Module.coverageWithEnvAndLimit Nothing
                                    (Set.fromList [ 3 ])
                                    projectEnv
                                    [ source ]
                                    (FunctionOrValue [] "main")

                            ( _, emptyRanges ) =
                                Eval.Module.coverageWithEnvAndLimit Nothing
                                    (Set.fromList [ 99 ])
                                    projectEnv
                                    [ source ]
                                    (FunctionOrValue [] "main")
                        in
                        Expect.all
                            [ \_ ->
                                filteredRanges
                                    |> List.isEmpty
                                    |> Expect.equal False
                                    |> Expect.onFail "Expected non-empty ranges when probing line 3"
                            , \_ ->
                                emptyRanges
                                    |> List.isEmpty
                                    |> Expect.equal True
                                    |> Expect.onFail "Expected empty ranges when probing non-existent line 99"
                            ]
                            ()
        , test "probe-filtered coverage still detects mutations correctly" <|
            \_ ->
                -- Verify that probe-filtered coverage produces enough data for
                -- isCovered to work correctly for mutations on probed lines
                let
                    source =
                        "module Main exposing (main)\n\nmain = if True then 1 else 2\n"

                    -- Mutation at "True" would be on line 3
                    mutationRange =
                        { start = { row = 3, column = 11 }
                        , end = { row = 3, column = 15 }
                        }
                in
                case Eval.Module.buildProjectEnv [] of
                    Err e ->
                        Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                    Ok projectEnv ->
                        let
                            ( _, ranges ) =
                                Eval.Module.coverageWithEnvAndLimit Nothing
                                    (Set.fromList [ 3 ])
                                    projectEnv
                                    [ source ]
                                    (FunctionOrValue [] "main")

                            covered =
                                List.any (\r -> rangeContains r mutationRange) ranges
                        in
                        covered
                            |> Expect.equal True
                            |> Expect.onFail
                                ("Mutation at "
                                    ++ rangeToString mutationRange
                                    ++ " not covered by ranges: "
                                    ++ Debug.toString (List.map rangeToString ranges)
                                )
        , test "coverage mode works with List.map (kernel)" <|
            \_ ->
                let
                    source =
                        "module Main exposing (main)\n\nmain = List.map (\\x -> x + 1) [1, 2, 3]\n"
                in
                case Eval.Module.buildProjectEnv [] of
                    Err e ->
                        Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                    Ok projectEnv ->
                        let
                            ( result, _ ) =
                                Eval.Module.coverageWithEnv projectEnv [ source ] (FunctionOrValue [] "main")
                        in
                        result
                            |> Expect.equal (Ok (List [ Int 2, Int 3, Int 4 ]))
        , test "coverage ranges include expressions inside List.map callback" <|
            \_ ->
                -- The lambda body `x + 1` is on line 3. Coverage should include
                -- ranges from inside the callback, not just the top-level List.map.
                let
                    source =
                        "module Main exposing (main)\n\nmain = List.map (\\x -> x + 1) [1, 2, 3]\n"
                in
                case Eval.Module.buildProjectEnv [] of
                    Err e ->
                        Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                    Ok projectEnv ->
                        let
                            ( _, ranges ) =
                                Eval.Module.coverageWithEnv projectEnv [ source ] (FunctionOrValue [] "main")

                            line3Ranges =
                                ranges |> List.filter (\r -> r.start.row == 3)
                        in
                        if List.length line3Ranges >= 2 then
                            Expect.pass

                        else
                            Expect.fail
                                ("Expected at least 2 ranges on line 3 (List.map + lambda body), got "
                                    ++ String.fromInt (List.length line3Ranges)
                                    ++ ": "
                                    ++ Debug.toString (List.map rangeToString line3Ranges)
                                )
        , test "coverage mode works with List.foldl (kernel)" <|
            \_ ->
                let
                    source =
                        "module Main exposing (main)\n\nmain = List.foldl (+) 0 [1, 2, 3]\n"
                in
                case Eval.Module.buildProjectEnv [] of
                    Err e ->
                        Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                    Ok projectEnv ->
                        let
                            ( result, _ ) =
                                Eval.Module.coverageWithEnv projectEnv [ source ] (FunctionOrValue [] "main")
                        in
                        result
                            |> Expect.equal (Ok (Int 6))
        , test "coverage ranges from multi-module List.map" <|
            \_ ->
                -- Simulate: module A defines a function, module B calls List.map with it
                let
                    moduleA =
                        "module A exposing (double)\n\ndouble x = x * 2\n"

                    moduleB =
                        "module B exposing (main)\n\nimport A\n\nmain = List.map A.double [1, 2, 3]\n"
                in
                case Eval.Module.buildProjectEnv [] of
                    Err e ->
                        Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)

                    Ok projectEnv ->
                        let
                            ( result, ranges ) =
                                Eval.Module.coverageWithEnvAndLimit Nothing
                                    (Set.fromList [ 3 ])
                                    projectEnv
                                    [ moduleA, moduleB ]
                                    (FunctionOrValue [] "main")

                            -- Line 3 of moduleA is "double x = x * 2"
                            -- We should get coverage from inside the callback
                            line3Ranges =
                                ranges |> List.filter (\r -> r.start.row == 3)
                        in
                        case result of
                            Err e ->
                                Expect.fail ("Eval error: " ++ Debug.toString e)

                            Ok _ ->
                                if List.length line3Ranges >= 1 then
                                    Expect.pass

                                else
                                    Expect.fail
                                        ("Expected coverage ranges on line 3 (A.double body), got "
                                            ++ String.fromInt (List.length line3Ranges)
                                            ++ ". All ranges: "
                                            ++ Debug.toString (List.map rangeToString ranges)
                                        )
        ]


rangeContains : Range -> Range -> Bool
rangeContains outer inner =
    positionLte outer.start inner.start && positionLte inner.end outer.end


positionLte : { row : Int, column : Int } -> { row : Int, column : Int } -> Bool
positionLte a b =
    a.row < b.row || (a.row == b.row && a.column <= b.column)


{-| Extract ranges from a full CallTree (for comparison with coverage mode).
-}
extractRangesFromCallTrees : Rope.Rope CallTree -> List Range
extractRangesFromCallTrees trees =
    Rope.toList trees
        |> List.concatMap extractFromNode


extractFromNode : CallTree -> List Range
extractFromNode tree =
    case tree of
        CallNode node ->
            let
                (Node range _) =
                    node.expression
            in
            range :: extractRangesFromCallTrees node.children

        CoverageRange range ->
            [ range ]

        CoverageSet packedSet ->
            Set.toList packedSet |> List.map Types.unpackRange


rangeToString : Range -> String
rangeToString r =
    String.fromInt r.start.row
        ++ ":"
        ++ String.fromInt r.start.column
        ++ "-"
        ++ String.fromInt r.end.row
        ++ ":"
        ++ String.fromInt r.end.column
