module PerfTests exposing (suite)

import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.File
import Elm.Syntax.Node as Node exposing (Node(..))
import Eval
import Eval.Module
import Expect
import Expression.Extra
import ListFusion
import TcoAnalysis
import Test exposing (Test, describe, skip, test)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Performance"
        [ listEqualityTests
        , tcoCorrectnessTests
        , tcoProofTests
        , largeListTailRecursionTests
        , tcoAnalysisTests
        , constantFoldingTests
        , dependencySummaryOptimizationTests
        , dictInliningTests
        , listFusionTests
        , normalizationLosslessnessTests
        , spineCollectionTests
        , overApplicationTests
        , aliasedPrecomputedLookupTests
        ]


{-| Regression coverage for over-application: a call site supplies
more arguments than the outer function's arity, and each surplus arg
has to be applied to the value returned by the preceding call. The
evaluator's general path handles this by re-entering the application
machinery after the inner call completes. These tests exist to catch
any breakage of that re-entry when the over-application path is
refactored (ZAM task C — replacing AST reconstruction with a direct
synchronous dispatch).
-}
overApplicationTests : Test
overApplicationTests =
    describe "over-application re-entry"
        [ test "outer arity-1 returning inner lambda, called with 2 args" <|
            \_ ->
                Eval.Module.eval
                    "module Temp exposing (main)\n\nmain : Int\nmain = (\\a -> \\b -> a + b) 3 5\n"
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 8))
        , test "outer arity-1 returning inner lambda, called with 3 args" <|
            \_ ->
                Eval.Module.eval
                    "module Temp exposing (main)\n\nmain : Int\nmain = (\\a -> \\b -> \\c -> a * 100 + b * 10 + c) 1 2 3\n"
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 123))
        , test "if-returning-function over-applied picks correct branch" <|
            \_ ->
                let
                    source : String
                    source =
                        """module Temp exposing (main)

pickOp : Int -> (Int -> Int)
pickOp flag =
    if flag > 0 then
        \\n -> n + 1

    else
        \\n -> n - 1

main : Int
main =
    pickOp 1 10
"""
                in
                Eval.Module.eval source (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 11))
        , test "user function returning partial application, over-applied" <|
            \_ ->
                let
                    source : String
                    source =
                        """module Temp exposing (main)

add : Int -> Int -> Int
add a b =
    a + b

makeAdder : Int -> (Int -> Int)
makeAdder k =
    add k

main : Int
main =
    makeAdder 3 7
"""
                in
                Eval.Module.eval source (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 10))
        , test "deeply over-applied (5 args into arity-1 chain)" <|
            \_ ->
                Eval.Module.eval
                    "module Temp exposing (main)\n\nmain : Int\nmain = (\\a -> \\b -> \\c -> \\d -> \\e -> a + b + c + d + e) 1 2 3 4 5\n"
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 15))
        ]


{-| Regression coverage for the evalApplication spine-collection pass
that flattens nested `Application` / `ParenthesizedExpression` head
nodes before dispatch. The optimization is semantically neutral, so
these tests only assert that the results still match the obvious
flat-form evaluation — they exist to catch any slip in the flattening
logic (wrong arg order, dropped args, or an infinite loop on empty
applications).
-}
spineCollectionTests : Test
spineCollectionTests =
    describe "application spine collection"
        [ test "(f a) b returns the same value as f a b" <|
            \_ ->
                let
                    source : String
                    source =
                        """module Temp exposing (main)

add : Int -> Int -> Int
add a b =
    a + b

main : Int
main =
    ((add 3) 5)
"""
                in
                Eval.Module.eval source (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 8))
        , test "((f a) b) c is deeply nested and still produces the correct value" <|
            \_ ->
                let
                    source : String
                    source =
                        """module Temp exposing (main)

add3 : Int -> Int -> Int -> Int
add3 a b c =
    a + b + c

main : Int
main =
    (((add3 1) 2) 3)
"""
                in
                Eval.Module.eval source (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 6))
        , test "pipeline into nested partial application still binds correctly" <|
            \_ ->
                let
                    source : String
                    source =
                        """module Temp exposing (main)

sub : Int -> Int -> Int
sub a b =
    a - b

main : Int
main =
    10 |> sub 3
"""
                in
                Eval.Module.eval source (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int -7))
        , test "nested application inside List.map callback body" <|
            \_ ->
                let
                    source : String
                    source =
                        """module Temp exposing (main)

add : Int -> Int -> Int
add a b =
    a + b

main : List Int
main =
    List.map ((add 1)) [10, 20, 30]
"""
                in
                Eval.Module.eval source (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (List [ Int 11, Int 21, Int 31 ]))
        ]


{-| Regression: the elm-review package defines `infinity = round (1 / 0)`,
which evaluated to `Int Infinity` at normalization time. The normalizer
replaced the body with `Expression.Integer Infinity`, which the Wire3
codec can't round-trip — making the package summary cache silently
refuse to persist (package\_summary\_cache\_roundtrip\_ok = 0 on every
warm run, ~500ms wasted on body-edit scenarios).

Fix: `isLosslessValue` rejects non-finite Int/Float values, so the
normalizer leaves the original expression alone. But
`tryNormalizeConstant` still returns the evaluated value to the caller,
so the in-memory `precomputedValues` cache still hits — recovering the
runtime speedup without re-introducing the cache corruption. The
on-disk user-norm cache filters non-lossless values at encode time in
`InterpreterProject.encodeUserNormCacheEntry`.

-}
normalizationLosslessnessTests : Test
normalizationLosslessnessTests =
    describe "normalization skips non-finite Int/Float"
        [ test "round (1/0) stays as an Application, not Integer Infinity" <|
            \_ ->
                let
                    source : String
                    source =
                        "module Test exposing (infinity)\n\ninfinity : Int\ninfinity =\n    round (1 / 0)\n"
                in
                case Eval.Module.parseProjectSources [ source ] of
                    Err _ ->
                        Expect.fail "parse failed"

                    Ok parsedModules ->
                        let
                            normalized =
                                Eval.Module.normalizeSummaries
                                    (Eval.Module.buildCachedModuleSummariesFromParsed parsedModules)

                            infinityBody =
                                normalized
                                    |> List.concatMap .functions
                                    |> List.filter (\f -> Node.value f.name == "infinity")
                                    |> List.head
                                    |> Maybe.map (\f -> Node.value f.expression)
                        in
                        case infinityBody of
                            Just (Integer _) ->
                                Expect.fail "infinity was normalized to an Integer literal (Infinity cannot round-trip through Wire3)"

                            Just (Application _) ->
                                Expect.pass

                            Just _ ->
                                Expect.pass

                            Nothing ->
                                Expect.fail "infinity function not found in normalized output"
        , test "1/0 stays as an OperatorApplication, not Floatable Infinity" <|
            \_ ->
                let
                    source : String
                    source =
                        "module Test exposing (inf)\n\ninf : Float\ninf =\n    1 / 0\n"
                in
                case Eval.Module.parseProjectSources [ source ] of
                    Err _ ->
                        Expect.fail "parse failed"

                    Ok parsedModules ->
                        let
                            normalized =
                                Eval.Module.normalizeSummaries
                                    (Eval.Module.buildCachedModuleSummariesFromParsed parsedModules)

                            infBody =
                                normalized
                                    |> List.concatMap .functions
                                    |> List.filter (\f -> Node.value f.name == "inf")
                                    |> List.head
                                    |> Maybe.map (\f -> Node.value f.expression)
                        in
                        case infBody of
                            Just (Floatable _) ->
                                Expect.fail "inf was normalized to a Float literal (Infinity cannot round-trip through Wire3)"

                            _ ->
                                Expect.pass
        ]


{-| Regression: cross-module qualified references via a module alias
(e.g. `import String.Diacritics as Diacritics`, then
`Diacritics.lookupArray`) must resolve to the canonical module's
entry in `precomputedValues`, not re-walk the original AST. Before
the fix, `evalNonVariant` keyed the cache lookup on the raw alias
(`"Diacritics"`) which never matched the canonical key
(`"String.Diacritics"`), so every aliased reference to an expensive
0-arg constant silently re-evaluated it from scratch —
`removeDiacritics` was paying the 65K-element `Array.initialize`
cost on every character iteration.

The test uses `round (1 / 0)` so the precomputed value is
non-lossless (`Int Infinity`), which means normalization keeps the
original expression around instead of rewriting it to a literal.
The `recurse 50 acc` wrapper makes that re-eval cost hundreds of
interpreter steps, so a low `maxSteps` budget distinguishes the two
paths: a cache hit uses ~5 steps and succeeds, a cache miss walks
the recursion and runs out of budget.

-}
aliasedPrecomputedLookupTests : Test
aliasedPrecomputedLookupTests =
    describe "aliased cross-module precomputed-value lookup"
        [ skip <|
            test "aliased qualified ref hits precomputedValues cache" <|
                \_ ->
                    -- SKIPPED on the `elm-review-runner` branch.
                    --
                    -- This test requires `tryNormalizeConstant` in
                    -- `Eval.Module` to populate `precomputedValues` with
                    -- non-lossless values (`Int Infinity`, `Float NaN`, …)
                    -- so the aliased lookup in `evalNonVariant`
                    -- (upstream `8c2f76e`) can hit. The population side
                    -- was added upstream in `5d861fa` and then reverted
                    -- on this branch in `d9c39de` because the original
                    -- implementation cost +17.7% cold on small-12 via
                    -- interactions with the fixpoint normalization loop.
                    --
                    -- Upstream `1a71da1` replaced the fixpoint with a
                    -- topological single-pass. The theory was this would
                    -- neutralize the regression, and I briefly reintroduced
                    -- the fix in submodule commit `07d382c`. An n=5 bench
                    -- on small-12 showed it still costs +13.7% cold (280
                    -- → 319 legacy, 251 → 272 resolved-list-unplanned),
                    -- and a single-run isolation bench confirmed flipping
                    -- just this one line is the cause. The fixpoint loop
                    -- wasn't the dominant factor; the real cost is
                    -- elsewhere (likely per-pass dict-merge work or
                    -- later cache-lookup hot-path interactions).
                    --
                    -- Re-enable this test after finding a mechanism that
                    -- populates non-lossless values without regressing
                    -- cold. Candidate approaches include lazy memoization
                    -- at the `evalNonVariant` cache-miss site (populate on
                    -- first access, not at normalization time) or
                    -- narrowing the population to only aliased lookups.
                    let
                        innerSource : String
                        innerSource =
                            """module Inner exposing (slow)

slow : Int
slow =
    recurse 50 (round (1 / 0))


recurse : Int -> Int -> Int
recurse n acc =
    if n <= 0 then
        acc

    else
        recurse (n - 1) acc
"""

                        -- `Probe` has the `import Inner as I` alias we want
                        -- to exercise, but we DON'T normalize it — that would
                        -- precompute `main` to `Int Infinity` and short-circuit
                        -- the top-level lookup before we ever hit the aliased
                        -- `I.slow` path we're trying to test.
                        probeSource : String
                        probeSource =
                            """module Probe exposing (main)

import Inner as I


main : Int
main =
    0
"""

                        parseFile : String -> Result String Elm.Syntax.File.File
                        parseFile src =
                            Elm.Parser.parseToFile src
                                |> Result.mapError (\_ -> "parse error")
                    in
                    case ( Eval.Module.buildProjectEnv [], parseFile innerSource ) of
                        ( Ok baseEnv, Ok innerFile ) ->
                            case Eval.Module.extendWithFilesNormalized baseEnv [ innerFile ] of
                                Err _ ->
                                    Expect.fail "extendWithFilesNormalized failed"

                                Ok projectEnvWithInner ->
                                    case
                                        Eval.Module.evalWithEnvAndLimit
                                            (Just 30)
                                            projectEnvWithInner
                                            [ probeSource ]
                                            (Expression.FunctionOrValue [ "I" ] "slow")
                                    of
                                        Ok _ ->
                                            Expect.pass

                                        Err e ->
                                            Expect.fail
                                                ("I.slow eval ran out of step budget — the aliased cross-module ref to Inner.slow is not hitting the precomputedValues cache. Error: "
                                                    ++ Debug.toString e
                                                )

                        _ ->
                            Expect.fail "setup failed (parse or buildProjectEnv)"
        ]


listFusionTests : Test
listFusionTests =
    describe "List.map fusion"
        [ test "fused double map on 10K list returns correct sum" <|
            \_ ->
                Eval.Module.eval
                    (String.join "\n"
                        [ "module T exposing (main)"
                        , "main = List.sum (List.map (\\x -> x * 2) (List.map (\\x -> x + 1) (List.range 1 10000)))"
                        ]
                    )
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 100030000))
        , test "pipeline-style triple map is correct" <|
            \_ ->
                Eval.Module.eval
                    (String.join "\n"
                        [ "module T exposing (main)"
                        , "main ="
                        , "    [1, 2, 3]"
                        , "        |> List.map (\\x -> x + 1)"
                        , "        |> List.map (\\x -> x * 2)"
                        , "        |> List.map (\\x -> x - 3)"
                        ]
                    )
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (List [ Int 1, Int 3, Int 5 ]))
        , test "List.reverse (List.reverse xs) → xs" <|
            \_ ->
                Eval.eval "List.reverse (List.reverse [1, 2, 3])"
                    |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3 ]))
        , test "List.map identity xs → xs" <|
            \_ ->
                Eval.eval "List.map identity [1, 2, 3]"
                    |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3 ]))
        , test "List.concat (List.map f xs) → List.concatMap f xs" <|
            \_ ->
                Eval.eval "List.concat (List.map (\\x -> [x, x + 1]) [1, 2, 3])"
                    |> Expect.equal (Ok (List [ Int 1, Int 2, Int 2, Int 3, Int 3, Int 4 ]))
        , test "consecutive List.filter collapses" <|
            \_ ->
                Eval.eval "List.filter (\\x -> x < 10) (List.filter (\\x -> x > 2) [1, 2, 3, 5, 8, 13, 21])"
                    |> Expect.equal (Ok (List [ Int 3, Int 5, Int 8 ]))
        , test "List.foldl f init (List.reverse xs) → List.foldr f init xs" <|
            \_ ->
                Eval.eval "List.foldl (::) [] (List.reverse [1, 2, 3])"
                    |> Expect.equal (Ok (List [ Int 1, Int 2, Int 3 ]))
        , test "Dict.fromList (Dict.toList d) → d" <|
            \_ ->
                Eval.eval "Dict.fromList (Dict.toList (Dict.fromList [(1, \"a\"), (2, \"b\")])) == Dict.fromList [(1, \"a\"), (2, \"b\")]"
                    |> Expect.equal (Ok (Bool True))
        , test "List.map Tuple.first (Dict.toList d) → Dict.keys d" <|
            \_ ->
                Eval.eval "List.map Tuple.first (Dict.toList (Dict.fromList [(1, \"a\"), (2, \"b\")]))"
                    |> Expect.equal (Ok (List [ Int 1, Int 2 ]))
        , test "List.map Tuple.second (Dict.toList d) → Dict.values d" <|
            \_ ->
                Eval.eval "List.map Tuple.second (Dict.toList (Dict.fromList [(1, \"a\"), (2, \"b\")]))"
                    |> Expect.equal (Ok (List [ String "a", String "b" ]))
        , test "Dict.size (Dict.map f d) → Dict.size d" <|
            \_ ->
                Eval.eval "Dict.size (Dict.map (\\k v -> v + 1) (Dict.fromList [(1, 10), (2, 20), (3, 30)]))"
                    |> Expect.equal (Ok (Int 3))
        , test "List.length (List.map f xs) → List.length xs" <|
            \_ ->
                Eval.eval "List.length (List.map (\\x -> x * 2) [1, 2, 3, 4, 5])"
                    |> Expect.equal (Ok (Int 5))
        , test "Maybe.map f (Maybe.map g x) fuses" <|
            \_ ->
                Eval.eval "Maybe.map (\\x -> x * 2) (Maybe.map (\\x -> x + 1) (Just 5))"
                    |> Expect.equal (Ok (Custom { moduleName = [ "Maybe" ], name = "Just" } [ Int 12 ]))
        , test "Maybe.map identity x → x" <|
            \_ ->
                Eval.eval "Maybe.map identity (Just 42)"
                    |> Expect.equal (Ok (Custom { moduleName = [ "Maybe" ], name = "Just" } [ Int 42 ]))
        , test "Result.map f (Result.map g r) fuses" <|
            \_ ->
                Eval.eval "Result.map (\\x -> x * 2) (Result.map (\\x -> x + 1) (Ok 5))"
                    |> Expect.equal (Ok (Custom { moduleName = [ "Result" ], name = "Ok" } [ Int 12 ]))
        , test "double map produces correct result" <|
            \_ ->
                Eval.eval
                    "List.map (\\x -> x * 2) (List.map (\\x -> x + 1) [1, 2, 3])"
                    |> Expect.equal (Ok (List [ Int 4, Int 6, Int 8 ]))
        , test "triple map produces correct result" <|
            \_ ->
                Eval.eval
                    "List.map (\\x -> x + 100) (List.map (\\x -> x * 2) (List.map (\\x -> x + 1) [1, 2, 3]))"
                    |> Expect.equal (Ok (List [ Int 104, Int 106, Int 108 ]))
        , test "ListFusion.fuse rewrites double map" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.map (\\x -> x * 2) (List.map (\\x -> x + 1) [1, 2, 3])\n"
                in
                case Elm.Parser.parseToFile source of
                    Ok file ->
                        case file.declarations of
                            (Node _ (FunctionDeclaration f)) :: _ ->
                                let
                                    body =
                                        (Node.value f.declaration).expression

                                    fused =
                                        ListFusion.fuse body

                                    isSingleMap (Node _ expr) =
                                        case expr of
                                            Application ((Node _ (FunctionOrValue [ "List" ] "map")) :: _ :: (Node _ innerExpr) :: []) ->
                                                case innerExpr of
                                                    Application ((Node _ (FunctionOrValue [ "List" ] "map")) :: _) ->
                                                        False

                                                    _ ->
                                                        True

                                            _ ->
                                                False
                                in
                                Expect.equal True (isSingleMap fused)

                            _ ->
                                Expect.fail "No function declaration"

                    Err _ ->
                        Expect.fail "Parse error"
        ]


tcoAnalysisTests : Test
tcoAnalysisTests =
    let
        analyzeModule src funcName params =
            case Elm.Parser.parseToFile src of
                Ok file ->
                    let
                        body =
                            file.declarations
                                |> List.filterMap
                                    (\(Node _ decl) ->
                                        case decl of
                                            FunctionDeclaration f ->
                                                let
                                                    impl =
                                                        Node.value f.declaration
                                                in
                                                if Node.value impl.name == funcName then
                                                    Just impl.expression

                                                else
                                                    Nothing

                                            _ ->
                                                Nothing
                                    )
                                |> List.head
                    in
                    case body of
                        Just expr ->
                            TcoAnalysis.analyze funcName params expr

                        Nothing ->
                            TcoAnalysis.TcoGeneral

                Err _ ->
                    TcoAnalysis.TcoGeneral
    in
    describe "TcoAnalysis"
        [ test "list drain detected as TcoListDrain" <|
            \_ ->
                let
                    result =
                        analyzeModule
                            "module T exposing (..)\nwalk xs = case xs of\n    [] -> 0\n    _ :: rest -> walk rest\n"
                            "walk"
                            [ "xs" ]
                in
                case result of
                    TcoAnalysis.TcoListDrain info ->
                        Expect.all
                            [ \i -> Expect.equal "xs" i.listArgName
                            , \i -> Expect.equal Nothing i.headBindingName
                            , \i -> Expect.equal "rest" i.tailBindingName
                            ]
                            info

                    _ ->
                        Expect.fail ("Expected TcoListDrain, got " ++ Debug.toString result)
        , test "isInfixOfHelp-like pattern detected as TcoListDrain" <|
            \_ ->
                let
                    result =
                        analyzeModule
                            "module T exposing (..)\ncheck h t list = case list of\n    [] -> False\n    x :: xs -> if x == h then True else check h t xs\n"
                            "check"
                            [ "h", "list", "t" ]
                in
                case result of
                    TcoAnalysis.TcoListDrain info ->
                        Expect.all
                            [ \i -> Expect.equal "list" i.listArgName
                            , \i -> Expect.equal (Just "x") i.headBindingName
                            , \i -> Expect.equal "xs" i.tailBindingName
                            ]
                            info

                    _ ->
                        Expect.fail ("Expected TcoListDrain, got " ++ Debug.toString result)
        , test "non-shrinking recursion is TcoGeneral" <|
            \_ ->
                analyzeModule
                    "module T exposing (..)\nloop n = if n == 0 then 0 else loop n\n"
                    "loop"
                    [ "n" ]
                    |> Expect.equal TcoAnalysis.TcoGeneral
        , test "countdown is TcoGeneral (no list drain)" <|
            \_ ->
                analyzeModule
                    "module T exposing (..)\ncountdown n = if n <= 0 then 0 else countdown (n - 1)\n"
                    "countdown"
                    [ "n" ]
                    |> Expect.equal TcoAnalysis.TcoGeneral
        , test "isPrefixOf-like tuple-scrutinee is TcoSafe" <|
            \_ ->
                -- Idiomatic Elm: dispatch on two list args via a tuple
                -- case. Every tail call passes strict sublists of BOTH
                -- args (`ps`, `xs`), so the cycle-probe is provably
                -- redundant. Classified as TcoGeneral before extending
                -- `collectConsTailBindings` to walk tuple scrutinees.
                analyzeModule
                    """module T exposing (..)


isPrefixOf prefix list =
    case ( prefix, list ) of
        ( [], _ ) ->
            True

        ( _ :: _, [] ) ->
            False

        ( p :: ps, x :: xs ) ->
            if p == x then
                isPrefixOf ps xs

            else
                False
"""
                    "isPrefixOf"
                    [ "prefix", "list" ]
                    |> Expect.equal TcoAnalysis.TcoSafe
        ]


{-| Regression: tail-recursive iteration over large lists was O(n²) because
`sizeOfValue (List items) = List.length items` ran on every cycle-check
(every 16 iterations). For a 1M-element list shrinking by 1 each iteration,
total sizeOfValue cost was ~31 billion node traversals.

These tests verify that iterating over large lists completes in bounded time
(i.e. the cycle-check cost is O(1) per check, not O(listLen)).

-}
largeListTailRecursionTests : Test
largeListTailRecursionTests =
    describe "large list tail-recursion (sizeOfValue regression)"
        [ test "walk 100K-element list in bounded time" <|
            \_ ->
                Eval.Module.eval
                    (String.join "\n"
                        [ "module T exposing (main)"
                        , "walk xs = case xs of"
                        , "    [] -> 0"
                        , "    _ :: rest -> walk rest"
                        , "main = walk (List.repeat 100000 1)"
                        ]
                    )
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 0))
        , test "isInfixOf-like pattern on 100K list" <|
            \_ ->
                Eval.Module.eval
                    (String.join "\n"
                        [ "module T exposing (main)"
                        , "check needle haystack ="
                        , "    case haystack of"
                        , "        [] -> False"
                        , "        x :: xs ->"
                        , "            if x == needle then True"
                        , "            else check needle xs"
                        , "main = check 999 (List.repeat 100000 1)"
                        ]
                    )
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Bool False))
        ]


{-| Test list equality at various sizes to find where perf degrades.
-}
listEqualityTests : Test
listEqualityTests =
    describe "List equality scaling"
        [ listEqTest 100
        , listEqTest 500
        , listEqTest 1000
        , listEqTest 2000
        ]


listEqTest : Int -> Test
listEqTest n =
    test ("List.range 0 " ++ String.fromInt n ++ " == List.range 0 " ++ String.fromInt n) <|
        \_ ->
            Eval.eval
                ("let size = "
                    ++ String.fromInt n
                    ++ " in List.range 0 size == List.range 0 size"
                )
                |> Expect.equal (Ok (Bool True))


{-| Correctness tests for tail-recursive functions.
-}
tcoCorrectnessTests : Test
tcoCorrectnessTests =
    describe "Tail-call optimization correctness"
        [ test "simple countdown 10000" <|
            \_ ->
                Eval.eval
                    "let countdown n = if n <= 0 then 0 else countdown (n - 1) in countdown 10000"
                    |> Expect.equal (Ok (Int 0))
        , test "accumulator sum 10000" <|
            \_ ->
                Eval.eval
                    "let sum acc n = if n <= 0 then acc else sum (acc + n) (n - 1) in sum 0 10000"
                    |> Expect.equal (Ok (Int 50005000))
        , test "list build via cons 5000" <|
            \_ ->
                Eval.eval
                    "let build acc n = if n <= 0 then acc else build (n :: acc) (n - 1) in List.length (build [] 5000)"
                    |> Expect.equal (Ok (Int 5000))
        , test "countdown 10000" <|
            \_ ->
                Eval.eval
                    "let countdown n = if n <= 0 then 0 else countdown (n - 1) in countdown 10000"
                    |> Expect.equal (Ok (Int 0))
        , test "accumulator build 10000" <|
            \_ ->
                Eval.eval
                    "let build acc n = if n <= 0 then acc else build (n :: acc) (n - 1) in List.length (build [] 10000)"
                    |> Expect.equal (Ok (Int 10000))
        , test "mutual recursion (non-tail) still works" <|
            \_ ->
                Eval.eval
                    "let even n = if n == 0 then True else odd (n - 1)\n    odd n = if n == 0 then False else even (n - 1)\nin even 100"
                    |> Expect.equal (Ok (Bool True))
        ]


{-| TCO PROOF tests: these prove TCO is actually engaging, not just that
results are correct. The technique: use evalWithMaxSteps with a step limit
that is generous enough for TCO (which uses ~0 trampoline steps per iteration
since tcoLoop calls evalExpression directly) but far too tight for the
non-TCO trampoline path (which uses ~5-10 trampoline steps per iteration).

If TCO is NOT engaging, these tests FAIL with "Step limit exceeded".
If TCO IS engaging, the step budget is sufficient and they pass.

-}
tcoProofTests : Test
tcoProofTests =
    describe "TCO proof (step-budget bounded)"
        [ test "module-level countdown 100000 within 110000 steps" <|
            -- Without TCO: ~200k+ trampoline steps for 100k iterations
            -- With TCO: 100k tcoLoop iterations + small startup
            \_ ->
                let
                    source =
                        "module T exposing (main)\ncountdown n = if n <= 0 then 0 else countdown (n - 1)\nmain = countdown 100000"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 110000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 0))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "module-level accumulator sum 10000 within 55000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmySum acc n = if n <= 0 then acc else mySum (acc + n) (n - 1)\nmain = mySum 0 10000"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 55000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 50005000))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "module-level list build 30000 within 35000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nbuild acc n = if n <= 0 then acc else build (n :: acc) (n - 1)\nmain = List.length (build [] 30000)"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 35000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 30000))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "case-based tail recursion within step budget" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nloop xs acc = case xs of\n  [] -> acc\n  x :: rest -> loop rest (acc + x)\nmain = loop [1,2,3,4,5,6,7,8,9,10] 0"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 5000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 55))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "non-tail-recursive List.map still works (not broken by TCO)" <|
            \_ ->
                Eval.eval
                    "List.map (\\x -> x * 2) [1, 2, 3, 4, 5]"
                    |> Expect.equal (Ok (List [ Int 2, Int 4, Int 6, Int 8, Int 10 ]))
        , test "non-tail-recursive fib still works" <|
            \_ ->
                Eval.eval
                    "let fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10"
                    |> Expect.equal (Ok (Int 55))

        -- === Let-defined self-recursion (TCO proof) ===
        , test "let-defined countdown 100000 within 110000 steps" <|
            -- Without TCO: needs ~200k+ trampoline steps (2+ per iteration)
            -- With TCO: needs 100k tcoLoop iterations + small trampoline startup
            -- 110000 proves TCO is engaging (impossible with 2+ steps/iter)
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = let countdown n = if n <= 0 then 0 else countdown (n - 1) in countdown 100000"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 110000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 0))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "let-defined accumulator 10000 within 55000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = let mySum acc n = if n <= 0 then acc else mySum (acc + n) (n - 1) in mySum 0 10000"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 55000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 50005000))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "let non-tail-recursive fib still correct" <|
            \_ ->
                Eval.evalWithMaxSteps (Just 10000)
                    "let fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10"
                    |> Expect.equal (Ok (Int 55))

        -- === Mutual recursion (correctness, not TCO proof) ===
        -- Mutual recursion doesn't get TCO yet (needs call-group detection).
        -- These verify correctness at moderate scale.
        , test "mutual recursion isEven/isOdd correctness (even)" <|
            \_ ->
                Eval.eval
                    "let isEven n = if n == 0 then True else isOdd (n - 1)\n    isOdd n = if n == 0 then False else isEven (n - 1)\nin isEven 100"
                    |> Expect.equal (Ok (Bool True))
        , test "mutual recursion isEven/isOdd correctness (odd)" <|
            \_ ->
                Eval.eval
                    "let isEven n = if n == 0 then True else isOdd (n - 1)\n    isOdd n = if n == 0 then False else isEven (n - 1)\nin isEven 101"
                    |> Expect.equal (Ok (Bool False))

        -- === TCO with maxSteps=Nothing (via Eval.eval) ===
        , test "Eval.eval list build 10000 (needs TCO for memory)" <|
            -- Eval.eval uses trace=True, maxSteps=Nothing.
            -- Without TCO, building a 10k-element list through the trampoline
            -- would be extremely slow due to memory pressure.
            -- With TCO, tcoLoop keeps memory bounded.
            \_ ->
                Eval.eval
                    "let build acc n = if n <= 0 then List.length acc else build (n :: acc) (n - 1) in build [] 10000"
                    |> Expect.equal (Ok (Int 10000))

        -- === Kernel list operations proof ===
        -- Without kernel: List.map/foldl/filter go through foldr/foldl which
        -- use the trampoline. With kernel: direct host-Elm iteration.
        -- Step budgets are tight enough to prove kernel engagement.
        , test "List.map 10000 within 15000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.length (List.map (\\x -> x + 1) (List.range 0 10000))"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 15000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 10001))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "List.foldl 10000 within 15000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.foldl (+) 0 (List.range 0 10000)"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 15000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 50005000))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "List.filter 10000 within 15000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.length (List.filter (\\x -> modBy 2 x == 0) (List.range 0 10000))"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 15000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 5001))

                    Err e ->
                        Expect.fail (Debug.toString e)

        -- === Kernel List.length, List.foldr, List.append proof ===
        -- Step budgets prove kernel engagement (too tight for interpreted path)
        , test "List.length 100000 within 100 steps" <|
            -- List.length uses kernel foldl — uses ~0 trampoline steps for iteration
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.length (List.range 0 100000)"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 100)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 100001))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "List.foldr 50000 within 500 steps" <|
            -- Without kernel foldr: 50k elements via foldrHelper needs ~12500 TCO iterations
            -- With kernel: ~0 trampoline steps
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.foldr (+) 0 (List.range 0 50000)"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 500)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 1250025000))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "List.append 50000+50000 within 500 steps" <|
            -- Without kernel: foldr over 50k elements needs many steps
            -- With kernel: direct list concatenation, ~0 trampoline steps
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.length (List.append (List.range 0 50000) (List.range 50001 100000))"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 500)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 100001))

                    Err e ->
                        Expect.fail (Debug.toString e)

        -- === Pipe operators should NOT get TCO (per Elm compiler) ===
        , test "pipe operator is not tail-optimized but still correct" <|
            \_ ->
                Eval.eval
                    "let f n = if n <= 0 then 0 else (n - 1) |> f in f 100"
                    |> Expect.equal (Ok (Int 0))
        ]


{-| Verify that constant folding inlines precomputed module-level constants
into function bodies, avoiding dict lookups at eval time.
-}
constantFoldingTests : Test
constantFoldingTests =
    describe "Constant folding"
        [ test "module constant inlined into function body" <|
            \_ ->
                Eval.Module.evalProject
                    [ "module T exposing (main)\n\nmyList = [1, 2, 3]\n\nmain = List.length myList\n" ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 3))
        , test "constant reference in recursive function" <|
            \_ ->
                Eval.Module.evalProject
                    [ String.join "\n"
                        [ "module T exposing (main)"
                        , ""
                        , "target = 42"
                        , ""
                        , "find xs ="
                        , "    case xs of"
                        , "        [] -> -1"
                        , "        x :: rest ->"
                        , "            if x == target then x"
                        , "            else find rest"
                        , ""
                        , "main = find [1, 2, 42, 99]"
                        ]
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 42))
        , test "cross-module constant reference" <|
            \_ ->
                Eval.Module.evalProject
                    [ "module Constants exposing (threshold)\n\nthreshold = 10\n"
                    , String.join "\n"
                        [ "module Main exposing (main)"
                        , "import Constants"
                        , "main = Constants.threshold + 5"
                        ]
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 15))
        ]


dependencySummaryOptimizationTests : Test
dependencySummaryOptimizationTests =
    let
        normalizedBodyString :
            List String
            -> String
            -> String
            -> Result String String
        normalizedBodyString sources moduleName functionName =
            case Eval.Module.parseProjectSources sources of
                Err _ ->
                    Err "parse failed"

                Ok parsedModules ->
                    case
                        Eval.Module.normalizeSummaries
                            (Eval.Module.buildCachedModuleSummariesFromParsed parsedModules)
                            |> List.filter (\summary -> summary.moduleName == [ moduleName ])
                            |> List.concatMap .functions
                            |> List.filter (\func -> Node.value func.name == functionName)
                            |> List.head
                    of
                        Just func ->
                            Ok (Expression.Extra.toString func.expression)

                        Nothing ->
                            Err "function not found"
    in
    describe "Dependency summary optimization"
        [ test "cross-module simple helper is inlined in normalized summaries" <|
            \_ ->
                normalizedBodyString
                    [ String.join "\n"
                        [ "module Helpers exposing (inc)"
                        , "inc n ="
                        , "    n + 1"
                        ]
                    , String.join "\n"
                        [ "module Main exposing (bump)"
                        , "import Helpers"
                        , "bump x ="
                        , "    Helpers.inc x"
                        ]
                    ]
                    "Main"
                    "bump"
                    |> Expect.equal (Ok "x + 1")
        , test "cross-module precomputed constant is substituted in normalized summaries" <|
            \_ ->
                normalizedBodyString
                    [ String.join "\n"
                        [ "module Constants exposing (threshold)"
                        , "threshold ="
                        , "    10"
                        ]
                    , String.join "\n"
                        [ "module Main exposing (bump)"
                        , "import Constants"
                        , "bump x ="
                        , "    x + Constants.threshold"
                        ]
                    ]
                    "Main"
                    "bump"
                    |> Expect.equal (Ok "x + 10")
        ]


dictInliningTests : Test
dictInliningTests =
    describe "Dict equality with inlining"
        [ test "Dict.fromList compared via == is order-independent" <|
            \_ ->
                Eval.Module.eval
                    (String.join "\n"
                        [ "module T exposing (main)"
                        , "import Dict"
                        , "main ="
                        , "    Dict.fromList [(1, \"a\"), (2, \"b\")] == Dict.fromList [(2, \"b\"), (1, \"a\")]"
                        ]
                    )
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Bool True))
        , test "Dict with Set values compared via ==" <|
            \_ ->
                Eval.Module.eval
                    (String.join "\n"
                        [ "module T exposing (main)"
                        , "import Dict"
                        , "import Set"
                        , "main ="
                        , "    let"
                        , "        d1 = Dict.fromList [(1, Set.fromList [\"a\", \"b\"]), (2, Set.fromList [\"c\"])]"
                        , "        d2 = Dict.fromList [(2, Set.fromList [\"c\"]), (1, Set.fromList [\"b\", \"a\"])]"
                        , "    in"
                        , "    d1 == d2"
                        ]
                    )
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Bool True))
        ]
