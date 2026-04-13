module IncrementalEnvTests exposing (suite)

{-| Tests for incremental env building: replacing a single module in a ProjectEnv
should produce identical evaluation results to building the entire env from scratch.
-}

import Elm.Interface exposing (Exposed)
import Elm.Parser
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File exposing (File)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Eval.Expression
import Eval.Module
import Expect
import FastDict
import MemoRuntime
import Set
import Test exposing (Test, describe, test)
import Types exposing (EvalResult(..), Intercept(..), Value(..))
import ValueCodec


suite : Test
suite =
    describe "Incremental Env"
        [ describe "replaceModuleInEnv"
            [ replaceProducesSameResult
            , replaceWithChangedBody
            , replacePreservesImportResolution
            , replaceWithCustomType
            , replaceWithRecordAlias
            , replaceRefreshesResolvedSidecar
            , extendWithFilesRefreshesResolvedSidecar
            ]
        , describe "evalWithEnvFromFilesAndValues"
            [ valueInjectionSimple
            , valueInjectionOverridesLocal
            , valueInjectionWithModuleCode
            ]
        , describe "function intercepts"
            [ interceptReplacesResult
            , interceptReceivesArgs
            , interceptMissPassesThrough
            ]
        , describe "combined function eval"
            [ combinedListConcatMap
            , combinedFilterMapApply
            , combinedSelectedByIndex
            , combinedFilterMapDropLargeIndices
            ]
        , describe "EvYield"
            [ interceptCanYield
            , yieldInLetBindingPropagates
            , yieldFromCalledFunctionInLet
            , multipleYieldsFromSequentialCalls
            , yieldsAcrossNestedOperatorChain
            , yieldsAcrossImportedZeroArgOperatorChain
            , yieldsAcrossAnnotatedImportedZeroArgOperatorChain
            , multipleYieldsFromLetBindings
            , yieldInsideListFoldl
            , yieldInsideManualFold
            , yieldInsideListMap
            , resolvedIRYieldPropagates
            , resolvedIRMultipleYieldsThroughDrive
            ]
        , describe "runtime memoization"
            [ runtimeMemoReusesCacheAcrossInvocations
            ]
        , describe "deepHashValue"
            [ deepHashSameValue
            , deepHashDifferentValues
            , deepHashNestedStructure
            , deepHashClosureSentinel
            ]
        , describe "ValueCodec"
            [ codecRoundTripPrimitives
            , codecRoundTripRecord
            , codecRoundTripCustom
            , codecRoundTripList
            , codecRoundTripNestedCustom
            , codecRoundTripDict
            , codecContentHashEquality
            , codecContextHashListOrderIndependent
            ]
        ]


{-| Test: List.concatMap over a list of functions produces results from ALL functions.
This simulates Rule.review's pattern of running multiple rules.
-}
combinedListConcatMap : Test
combinedListConcatMap =
    test "List.concatMap over function list includes all results" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nruleA items = List.map (\\x -> x + 10) items\n\nruleB items = List.map (\\x -> x + 20) items\n\nresults = List.concatMap (\\f -> f [1, 2]) [ruleA, ruleB]\n"
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Err e ->
                    Expect.fail ("Build error: " ++ Debug.toString e)

                Ok env ->
                    case Eval.Module.evalWithEnv env [] (Expression.FunctionOrValue [] "results") of
                        Ok (List values) ->
                            -- Should be [11, 12, 21, 22] (ruleA results ++ ruleB results)
                            Expect.equal 4 (List.length values)

                        Ok other ->
                            Expect.fail ("Expected List, got: " ++ Debug.toString other)

                        Err e ->
                            Expect.fail ("Eval error: " ++ Debug.toString e)


{-| Test: List.filterMap selecting from a config list, then concatMap.
Simulates: selectedRules = indices |> List.filterMap (\\i -> List.head (List.drop i config))
-}
combinedFilterMapApply : Test
combinedFilterMapApply =
    test "filterMap + concatMap over selected rules" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nconfig = [ (\\x -> x + 1), (\\x -> x + 2), (\\x -> x + 3) ]\n\nselected = [0, 2] |> List.filterMap (\\i -> List.head (List.drop i config))\n\nresults = List.concatMap (\\f -> [f 10]) selected\n"
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Err e ->
                    Expect.fail ("Build error: " ++ Debug.toString e)

                Ok env ->
                    case Eval.Module.evalWithEnv env [] (Expression.FunctionOrValue [] "results") of
                        Ok (List [ Int a, Int b ]) ->
                            -- selected = [config[0], config[2]] = [(+1), (+3)]
                            -- results = [11, 13]
                            Expect.equal [ 11, 13 ] [ a, b ]

                        Ok other ->
                            Expect.fail ("Expected [11, 13], got: " ++ Debug.toString other)

                        Err e ->
                            Expect.fail ("Eval error: " ++ Debug.toString e)


{-| Test: the exact pattern used by runReviewCachingWithProject.
Select rules by index from a config list, apply them, concat results.
-}
combinedSelectedByIndex : Test
combinedSelectedByIndex =
    test "select rules by index and run all - results from ALL rules present" <|
        \_ ->
            let
                source =
                    String.join "\n"
                        [ "module Main exposing (..)"
                        , ""
                        , "type alias Rule = List Int -> List String"
                        , ""
                        , "ruleA : Rule"
                        , "ruleA items = List.map (\\x -> \"A:\" ++ String.fromInt x) items"
                        , ""
                        , "ruleB : Rule"
                        , "ruleB items = List.map (\\x -> \"B:\" ++ String.fromInt x) items"
                        , ""
                        , "ruleC : Rule"
                        , "ruleC items = List.map (\\x -> \"C:\" ++ String.fromInt x) items"
                        , ""
                        , "config : List Rule"
                        , "config = [ ruleA, ruleB, ruleC ]"
                        , ""
                        , "runSelected : List Int -> List Int -> List String"
                        , "runSelected indices items ="
                        , "    let"
                        , "        selectedRules = indices |> List.filterMap (\\i -> List.head (List.drop i config))"
                        , "    in"
                        , "    List.concatMap (\\rule -> rule items) selectedRules"
                        , ""
                        , "results = runSelected [2, 0] [1, 2, 3]"
                        ]
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Err e ->
                    Expect.fail ("Build error: " ++ Debug.toString e)

                Ok env ->
                    case Eval.Module.evalWithEnv env [] (Expression.FunctionOrValue [] "results") of
                        Ok (List values) ->
                            let
                                strings =
                                    values
                                        |> List.filterMap
                                            (\v ->
                                                case v of
                                                    String s ->
                                                        Just s

                                                    _ ->
                                                        Nothing
                                            )

                                hasA =
                                    List.any (String.startsWith "A:") strings

                                hasC =
                                    List.any (String.startsWith "C:") strings
                            in
                            Expect.all
                                [ \_ -> Expect.equal 6 (List.length strings)
                                , \_ ->
                                    if hasC && hasA then
                                        Expect.pass

                                    else
                                        Expect.fail ("Expected both A: and C: results, got: " ++ Debug.toString strings)
                                ]
                                ()

                        Ok other ->
                            Expect.fail ("Expected List, got: " ++ Debug.toString other)

                        Err e ->
                            Expect.fail ("Eval error: " ++ Debug.toString e)


{-| Test: List.filterMap with List.drop on large indices (8, 7) from 9-element list.
This is the EXACT pattern used by runReviewCachingWithProject.
-}
combinedFilterMapDropLargeIndices : Test
combinedFilterMapDropLargeIndices =
    test "List.filterMap + List.drop with indices [8, 7] from 9-element config" <|
        \_ ->
            let
                source =
                    String.join "\n"
                        [ "module Main exposing (..)"
                        , ""
                        , "config = [ \"r0\", \"r1\", \"r2\", \"r3\", \"r4\", \"r5\", \"r6\", \"r7\", \"r8\" ]"
                        , ""
                        , "selected = [8, 7] |> List.filterMap (\\i -> List.head (List.drop i config))"
                        , ""
                        , "results = String.join \",\" selected"
                        ]
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Err e ->
                    Expect.fail ("Build error: " ++ Debug.toString e)

                Ok env ->
                    case Eval.Module.evalWithEnv env [] (Expression.FunctionOrValue [] "results") of
                        Ok (String s) ->
                            Expect.equal "r8,r7" s

                        Ok other ->
                            Expect.fail ("Expected String, got: " ++ Debug.toString other)

                        Err e ->
                            Expect.fail ("Eval error: " ++ Debug.toString e)


{-| Test: injecting an Int value makes it available in the expression.
-}
valueInjectionSimple : Test
valueInjectionSimple =
    test "injected Int value is accessible in expression" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nresults = x + 1\n"

                injected =
                    FastDict.singleton "x" (Int 41)
            in
            case evalWithInjectedValues [ source ] injected "results" of
                Ok (Int 42) ->
                    Expect.pass

                Ok other ->
                    Expect.fail ("Expected Int 42, got: " ++ Debug.toString other)

                Err e ->
                    Expect.fail ("Eval error: " ++ Debug.toString e)


{-| Test: injected value overrides a local let binding.
-}
valueInjectionOverridesLocal : Test
valueInjectionOverridesLocal =
    test "injected value available alongside module code" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nfoo = 10\n\nresults = foo + injectedVal\n"

                injected =
                    FastDict.singleton "injectedVal" (Int 32)
            in
            case evalWithInjectedValues [ source ] injected "results" of
                Ok (Int 42) ->
                    Expect.pass

                Ok other ->
                    Expect.fail ("Expected Int 42, got: " ++ Debug.toString other)

                Err e ->
                    Expect.fail ("Eval error: " ++ Debug.toString e)


{-| Test: injected complex value (List) is usable.
-}
valueInjectionWithModuleCode : Test
valueInjectionWithModuleCode =
    test "injected List value is accessible" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nresults = List.length myList\n"

                injected =
                    FastDict.singleton "myList" (List [ Int 1, Int 2, Int 3 ])
            in
            case evalWithInjectedValues [ source ] injected "results" of
                Ok (Int 3) ->
                    Expect.pass

                Ok other ->
                    Expect.fail ("Expected Int 3, got: " ++ Debug.toString other)

                Err e ->
                    Expect.fail ("Eval error: " ++ Debug.toString e)


{-| Helper: evaluate with injected values.
-}
evalWithInjectedValues : List String -> FastDict.Dict String Value -> String -> Result Types.Error Value
evalWithInjectedValues sources injected exprName =
    case Eval.Module.buildProjectEnv sources of
        Err e ->
            Err e

        Ok env ->
            Eval.Module.evalWithEnvFromFilesAndValues env [] injected (Expression.FunctionOrValue [] exprName)


{-| Helper: evaluate with function intercepts.
-}
evalWithIntercepts : List String -> FastDict.Dict String Types.Intercept -> String -> Result Types.Error Value
evalWithIntercepts sources intercepts exprName =
    case Eval.Module.buildProjectEnv sources of
        Err e ->
            Err e

        Ok env ->
            Eval.Module.evalWithIntercepts env [] intercepts (Expression.FunctionOrValue [] exprName)


{-| Test: intercepted function returns replacement value.
-}
interceptReplacesResult : Test
interceptReplacesResult =
    test "intercept replaces function result" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nimport Helpers\n\nresults = Helpers.greet \"world\"\n"

                helperSource =
                    "module Helpers exposing (greet)\n\ngreet name = \"Hello, \" ++ name ++ \"!\"\n"

                intercepts =
                    FastDict.singleton "Helpers.greet"
                        (Intercept
                            (\_ _ _ _ ->
                                -- Always return "INTERCEPTED" regardless of args
                                EvOk (String "INTERCEPTED")
                            )
                        )
            in
            case evalWithIntercepts [ helperSource, source ] intercepts "results" of
                Ok (String "INTERCEPTED") ->
                    Expect.pass

                Ok other ->
                    Expect.fail ("Expected String \"INTERCEPTED\", got: " ++ Debug.toString other)

                Err e ->
                    Expect.fail ("Eval error: " ++ Debug.toString e)


{-| Test: intercepted function receives the actual arguments.
-}
interceptReceivesArgs : Test
interceptReceivesArgs =
    test "intercept receives function arguments" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nimport Helpers\n\nresults = Helpers.add 10 32\n"

                helperSource =
                    "module Helpers exposing (add)\n\nadd a b = a + b\n"

                intercepts =
                    FastDict.singleton "Helpers.add"
                        (Intercept
                            (\_ args _ _ ->
                                -- Return the sum (proving we got the args)
                                case args of
                                    [ Int a, Int b ] ->
                                        EvOk (Int (a + b + 100))

                                    _ ->
                                        EvOk (String "wrong args")
                            )
                        )
            in
            case evalWithIntercepts [ helperSource, source ] intercepts "results" of
                Ok (Int 142) ->
                    -- 10 + 32 + 100 = 142
                    Expect.pass

                Ok other ->
                    Expect.fail ("Expected Int 142, got: " ++ Debug.toString other)

                Err e ->
                    Expect.fail ("Eval error: " ++ Debug.toString e)


{-| Test: non-intercepted functions evaluate normally.
-}
interceptMissPassesThrough : Test
interceptMissPassesThrough =
    test "non-intercepted function evaluates normally" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nimport Helpers\n\nresults = Helpers.add 10 32\n"

                helperSource =
                    "module Helpers exposing (add)\n\nadd a b = a + b\n"

                -- Intercept a DIFFERENT function
                intercepts =
                    FastDict.singleton "Helpers.greet"
                        (Intercept (\_ _ _ _ -> EvOk (String "INTERCEPTED")))
            in
            case evalWithIntercepts [ helperSource, source ] intercepts "results" of
                Ok (Int 42) ->
                    Expect.pass

                Ok other ->
                    Expect.fail ("Expected Int 42, got: " ++ Debug.toString other)

                Err e ->
                    Expect.fail ("Eval error: " ++ Debug.toString e)


{-| Helper: evaluate an expression using full rebuild from sources.
-}
evalFull : List String -> String -> Result Types.Error Value
evalFull sources exprStr =
    Eval.Module.evalProject sources (Expression.FunctionOrValue [] exprStr)


{-| Parse a source string to the components needed for env building.
-}
parseModule : String -> Maybe { file : File, moduleName : ModuleName, interface : List Exposed }
parseModule src =
    Elm.Parser.parseToFile src
        |> Result.toMaybe
        |> Maybe.map
            (\file ->
                { file = file
                , moduleName = Eval.Module.fileModuleName file
                , interface = Eval.Module.buildInterfaceFromFile file
                }
            )


{-| Helper: evaluate using incremental replacement.

1.  Build base env from original sources (all modules)
2.  Replace one module with its mutated version
3.  Evaluate in the context of the "main" module (passed as additional file)

-}
evalIncremental : List String -> String -> String -> String -> Result Types.Error Value
evalIncremental originalSources mutatedModuleSource mainModuleSource exprStr =
    let
        parsedOriginal =
            originalSources |> List.filterMap parseModule

        parsedMutated =
            parseModule mutatedModuleSource

        parsedMain =
            Elm.Parser.parseToFile mainModuleSource
                |> Result.toMaybe
    in
    case ( Eval.Module.buildProjectEnvFromParsed parsedOriginal, parsedMutated, parsedMain ) of
        ( Ok baseEnv, Just replacement, Just mainFile ) ->
            case Eval.Module.replaceModuleInEnv baseEnv replacement of
                Ok updatedEnv ->
                    -- Pass mainModule as additional file to set it as current module context
                    Eval.Module.evalWithEnvFromFiles updatedEnv [ mainFile ] (Expression.FunctionOrValue [] exprStr)

                Err e ->
                    Err e

        _ ->
            Err (Types.ParsingError [])


moduleA : String
moduleA =
    """module A exposing (helper, value)

helper x = x + 1

value = helper 10
"""


moduleAMutated : String
moduleAMutated =
    """module A exposing (helper, value)

helper x = x * 2

value = helper 10
"""


moduleB : String
moduleB =
    """module B exposing (main)

import A

main = A.value + A.helper 5
"""


replaceProducesSameResult : Test
replaceProducesSameResult =
    test "replacing unchanged module produces same result as full build" <|
        \_ ->
            let
                -- Full rebuild with original A
                fullResult =
                    evalFull [ moduleA, moduleB ] "main"

                -- Incremental: build with original, replace A with... original A (no change)
                incrementalResult =
                    evalIncremental [ moduleA, moduleB ] moduleA moduleB "main"
            in
            incrementalResult
                |> Expect.equal fullResult


replaceWithChangedBody : Test
replaceWithChangedBody =
    test "replacing module with changed function body produces correct result" <|
        \_ ->
            let
                -- Full rebuild with mutated A (helper x = x * 2)
                -- value = helper 10 = 20, main = value + helper 5 = 20 + 10 = 30
                fullResult =
                    evalFull [ moduleAMutated, moduleB ] "main"

                -- Incremental: build with original, replace A with mutated A
                incrementalResult =
                    evalIncremental [ moduleA, moduleB ] moduleAMutated moduleB "main"
            in
            incrementalResult
                |> Expect.equal fullResult


replacePreservesImportResolution : Test
replacePreservesImportResolution =
    test "import resolution in dependent module still works after replacement" <|
        \_ ->
            let
                moduleC =
                    """module C exposing (greet)

greet name = "Hello, " ++ name
"""

                moduleCMutated =
                    """module C exposing (greet)

greet name = "Hi, " ++ name
"""

                moduleD =
                    """module D exposing (main)

import C exposing (greet)

main = greet "World"
"""

                fullResult =
                    evalFull [ moduleCMutated, moduleD ] "main"

                incrementalResult =
                    evalIncremental [ moduleC, moduleD ] moduleCMutated moduleD "main"
            in
            incrementalResult
                |> Expect.equal fullResult


replaceWithCustomType : Test
replaceWithCustomType =
    test "replacing module with custom type constructors works" <|
        \_ ->
            let
                moduleWithType =
                    """module MyType exposing (Shape(..), area)

type Shape
    = Circle Float
    | Square Float

area shape =
    case shape of
        Circle r -> 3.14 * r * r
        Square s -> s * s
"""

                moduleWithTypeMutated =
                    """module MyType exposing (Shape(..), area)

type Shape
    = Circle Float
    | Square Float

area shape =
    case shape of
        Circle r -> 3.0 * r * r
        Square s -> s * s
"""

                moduleUser =
                    """module User exposing (main)

import MyType exposing (Shape(..))

main = MyType.area (Circle 10.0)
"""

                fullResult =
                    evalFull [ moduleWithTypeMutated, moduleUser ] "main"

                incrementalResult =
                    evalIncremental [ moduleWithType, moduleUser ] moduleWithTypeMutated moduleUser "main"
            in
            incrementalResult
                |> Expect.equal fullResult


replaceWithRecordAlias : Test
replaceWithRecordAlias =
    test "replacing module with record alias constructor works" <|
        \_ ->
            let
                moduleWithAlias =
                    """module Point exposing (Point, origin)

type alias Point = { x : Int, y : Int }

origin = Point 0 0
"""

                moduleWithAliasMutated =
                    """module Point exposing (Point, origin)

type alias Point = { x : Int, y : Int }

origin = Point 1 1
"""

                modulePointUser =
                    """module PointUser exposing (main)

import Point

main = (Point.origin).x
"""

                fullResult =
                    evalFull [ moduleWithAliasMutated, modulePointUser ] "main"

                incrementalResult =
                    evalIncremental [ moduleWithAlias, modulePointUser ] moduleWithAliasMutated modulePointUser "main"
            in
            incrementalResult
                |> Expect.equal fullResult


{-| Test that an intercept can yield and the framework can resume with a value.
-}
interceptCanYield : Test
interceptCanYield =
    test "intercept yields, framework resumes with value" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nimport Helpers\n\nresults = Helpers.fetch \"key1\"\n"

                helperSource =
                    "module Helpers exposing (fetch)\n\nfetch key = key\n"

                -- The intercept yields instead of returning directly
                intercepts =
                    FastDict.singleton "Helpers.fetch"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ String key ] ->
                                        -- Yield to framework: "please look up this key"
                                        EvYield "cache-lookup"
                                            (String key)
                                            (\resumeValue ->
                                                -- Framework resumes with the looked-up value
                                                EvOk resumeValue
                                            )

                                    _ ->
                                        EvOk (String "error")
                            )
                        )
            in
            case Eval.Module.buildProjectEnv [ helperSource, source ] of
                Err _ ->
                    Expect.fail "Failed to build env"

                Ok env ->
                    let
                        evalResult =
                            Eval.Module.evalWithIntercepts env [] intercepts (Expression.FunctionOrValue [] "results")
                    in
                    case evalResult of
                        Err e ->
                            -- Check if it's the "Unhandled EvYield" error from toResult
                            -- This means the yield propagated correctly but wasn't handled
                            case e of
                                Types.EvalError evalErr ->
                                    if String.contains "EvYield" (Types.evalErrorKindToString evalErr.error) then
                                        -- The yield reached toResult — it propagated correctly!
                                        -- Now simulate the framework handling it:
                                        -- We can't easily resume from here in a test, but we can
                                        -- verify the yield happened by checking the error message.
                                        Expect.pass

                                    else
                                        Expect.fail ("Unexpected error: " ++ Types.evalErrorKindToString evalErr.error)

                                _ ->
                                    Expect.fail "Unexpected error type"

                        Ok _ ->
                            -- If we got Ok, the yield was somehow resolved (shouldn't happen without a driver)
                            Expect.fail "Expected yield to propagate, but got Ok"


{-| Test: yield from a function called inside a let binding.
This is the pattern elm-review uses for initialCacheMarker:
let
cache = initialCacheMarker name id emptyCache
...
in result
-}
yieldInLetBindingPropagates : Test
yieldInLetBindingPropagates =
    test "yield from function called in let binding propagates to driver" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nimport Helpers\n\nresults =\n    let\n        x = Helpers.marker 1\n    in\n    x + 10\n"

                helperSource =
                    "module Helpers exposing (marker)\n\nmarker n = n\n"

                intercepts =
                    FastDict.singleton "Helpers.marker"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ Int n ] ->
                                        EvYield "test-yield" (Int n) (\_ -> EvOk (Int (n * 100)))

                                    _ ->
                                        EvOk (Int 0)
                            )
                        )
            in
            case Eval.Module.buildProjectEnv [ helperSource, source ] of
                Err _ ->
                    Expect.fail "Failed to build env"

                Ok env ->
                    let
                        rawResult =
                            Eval.Module.evalWithInterceptsRaw env [] intercepts (Expression.FunctionOrValue [] "results")
                    in
                    case rawResult of
                        EvYield "test-yield" (Int 1) resume ->
                            -- The yield propagated! Now resume with Unit
                            case resume Unit of
                                EvOk (Int 110) ->
                                    -- 1 * 100 = 100, then 100 + 10 = 110
                                    Expect.pass

                                EvOk other ->
                                    Expect.fail ("Expected Int 110 after resume, got: " ++ Debug.toString other)

                                EvErr e ->
                                    Expect.fail ("Error after resume: " ++ Debug.toString e)

                                EvYield _ _ _ ->
                                    Expect.fail "Got another yield after resume (expected final result)"

                                _ ->
                                    Expect.fail "Unexpected result after resume"

                        EvOk _ ->
                            Expect.fail "Yield didn't propagate (got EvOk directly)"

                        EvErr e ->
                            Expect.fail ("Error (yield swallowed?): " ++ Types.evalErrorKindToString e.error)

                        _ ->
                            Expect.fail "Unexpected result type"


{-| Test: yield from a function that's called (not directly in let binding).
let
result = someFunc (marker 1)
in result
-}
yieldFromCalledFunctionInLet : Test
yieldFromCalledFunctionInLet =
    test "yield from function argument in let binding" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nimport Helpers\n\nresults =\n    let\n        x = Helpers.add (Helpers.marker 1) 10\n    in\n    x\n"

                helperSource =
                    "module Helpers exposing (marker, add)\n\nmarker n = n\n\nadd a b = a + b\n"

                intercepts =
                    FastDict.singleton "Helpers.marker"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ Int n ] ->
                                        EvYield "test-yield" (Int n) (\_ -> EvOk (Int (n * 100)))

                                    _ ->
                                        EvOk (Int 0)
                            )
                        )
            in
            case Eval.Module.buildProjectEnv [ helperSource, source ] of
                Err _ ->
                    Expect.fail "Failed to build env"

                Ok env ->
                    let
                        rawResult =
                            Eval.Module.evalWithInterceptsRaw env [] intercepts (Expression.FunctionOrValue [] "results")
                    in
                    case rawResult of
                        EvYield "test-yield" (Int 1) resume ->
                            case resume Unit of
                                EvOk (Int 110) ->
                                    Expect.pass

                                EvOk other ->
                                    Expect.fail ("Expected Int 110, got: " ++ Debug.toString other)

                                EvErr e ->
                                    Expect.fail ("Error: " ++ Debug.toString e)

                                _ ->
                                    Expect.fail "Unexpected result"

                        EvOk _ ->
                            Expect.fail "Yield didn't propagate through let binding"

                        EvErr e ->
                            Expect.fail ("Error: " ++ Types.evalErrorKindToString e.error)

                        _ ->
                            Expect.fail "Unexpected result"


{-| Test: multiple functions that yield, called sequentially.
This is the pattern for saving ALL rule caches:
let
cache1 = marker "rule1" emptyCache
cache2 = marker "rule2" emptyCache
in ...
-}
multipleYieldsFromSequentialCalls : Test
multipleYieldsFromSequentialCalls =
    test "multiple yields from sequential function calls" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nimport Helpers\n\nresults = Helpers.marker 1 + Helpers.marker 2\n"

                helperSource =
                    "module Helpers exposing (marker)\n\nmarker n = n\n"

                intercepts =
                    FastDict.singleton "Helpers.marker"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ Int n ] ->
                                        EvYield "test-yield" (Int n) (\_ -> EvOk (Int (n * 10)))

                                    _ ->
                                        EvOk (Int 0)
                            )
                        )
            in
            case Eval.Module.buildProjectEnv [ helperSource, source ] of
                Err _ ->
                    Expect.fail "Failed to build env"

                Ok env ->
                    let
                        rawResult =
                            Eval.Module.evalWithInterceptsRaw env [] intercepts (Expression.FunctionOrValue [] "results")

                        -- Drive yields manually (simulating the BackendTask driver)
                        driveResult =
                            driveYieldsSync rawResult []
                    in
                    case driveResult of
                        { finalResult, yields } ->
                            Expect.all
                                [ \_ ->
                                    -- Should have yielded twice (once per marker call)
                                    Expect.atLeast 1 (List.length yields)
                                , \_ ->
                                    case finalResult of
                                        EvOk (Int 30) ->
                                            -- marker 1 → 10, marker 2 → 20, total = 30
                                            Expect.pass

                                        EvOk other ->
                                            Expect.fail ("Expected Int 30, got: " ++ Debug.toString other)

                                        EvErr e ->
                                            Expect.fail ("Error: " ++ Types.evalErrorKindToString e.error)

                                        _ ->
                                            Expect.fail "Unexpected final result"
                                ]
                                ()


{-| Test: yields continue correctly across a nested left-associated operator chain.
This matches the benchmark shape `a + b + c + d`, where the outer continuation
must remain attached after each nested resume.
-}
yieldsAcrossNestedOperatorChain : Test
yieldsAcrossNestedOperatorChain =
    test "multiple yields across nested operator chain" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nimport Helpers\n\nresults = Helpers.marker 1 + Helpers.marker 2 + Helpers.marker 3 + Helpers.marker 4\n"

                helperSource =
                    "module Helpers exposing (marker)\n\nmarker n = n\n"

                intercepts =
                    FastDict.singleton "Helpers.marker"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ Int n ] ->
                                        EvYield "test-yield" (Int n) (\_ -> EvOk (Int (n * 10)))

                                    _ ->
                                        EvOk (Int 0)
                            )
                        )
            in
            case Eval.Module.buildProjectEnv [ helperSource, source ] of
                Err _ ->
                    Expect.fail "Failed to build env"

                Ok env ->
                    let
                        driveResult =
                            Eval.Module.evalWithInterceptsRaw env [] intercepts (Expression.FunctionOrValue [] "results")
                                |> (\rawResult -> driveYieldsSync rawResult [])
                    in
                    case driveResult of
                        { finalResult, yields } ->
                            Expect.all
                                [ \_ -> Expect.equal 4 (List.length yields)
                                , \_ ->
                                    case finalResult of
                                        EvOk (Int 100) ->
                                            Expect.pass

                                        EvOk other ->
                                            Expect.fail ("Expected Int 100, got: " ++ Debug.toString other)

                                        EvErr e ->
                                            Expect.fail ("Error: " ++ Types.evalErrorKindToString e.error)

                                        _ ->
                                            Expect.fail ("Unexpected: " ++ Debug.toString finalResult)
                                ]
                                ()


{-| Test: yields continue correctly when the repeated operator chain lives inside
an imported zero-argument value. This matches the benchmark shape
`Wrapper.results = MemoBench.probeResults`.
-}
yieldsAcrossImportedZeroArgOperatorChain : Test
yieldsAcrossImportedZeroArgOperatorChain =
    test "multiple yields across imported zero-arg operator chain" <|
        \_ ->
            let
                helperSource =
                    "module Helpers exposing (marker)\n\nmarker n = n\n"

                memoBenchSource =
                    "module MemoBench exposing (probeResults)\n\nimport Helpers\n\nprobeResults = Helpers.marker 1 + Helpers.marker 2 + Helpers.marker 3 + Helpers.marker 4\n"

                source =
                    "module Main exposing (results)\n\nimport MemoBench\n\nresults = MemoBench.probeResults\n"

                intercepts =
                    FastDict.singleton "Helpers.marker"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ Int n ] ->
                                        EvYield "test-yield" (Int n) (\_ -> EvOk (Int (n * 10)))

                                    _ ->
                                        EvOk (Int 0)
                            )
                        )
            in
            case Eval.Module.buildProjectEnv [ helperSource, memoBenchSource, source ] of
                Err _ ->
                    Expect.fail "Failed to build env"

                Ok env ->
                    let
                        driveResult =
                            Eval.Module.evalWithInterceptsRaw env [] intercepts (Expression.FunctionOrValue [] "results")
                                |> (\rawResult -> driveYieldsSync rawResult [])
                    in
                    case driveResult of
                        { finalResult, yields } ->
                            Expect.all
                                [ \_ -> Expect.equal 4 (List.length yields)
                                , \_ ->
                                    case finalResult of
                                        EvOk (Int 100) ->
                                            Expect.pass

                                        EvOk other ->
                                            Expect.fail ("Expected Int 100, got: " ++ Debug.toString other)

                                        EvErr e ->
                                            Expect.fail ("Error: " ++ Types.evalErrorKindToString e.error)

                                        _ ->
                                            Expect.fail ("Unexpected: " ++ Debug.toString finalResult)
                                ]
                                ()


{-| Test: yields continue correctly through the benchmark's annotated module shape.
-}
yieldsAcrossAnnotatedImportedZeroArgOperatorChain : Test
yieldsAcrossAnnotatedImportedZeroArgOperatorChain =
    test "multiple yields across annotated imported zero-arg operator chain" <|
        \_ ->
            let
                helperSource =
                    "module ExpensiveHelper exposing (probe)\n\nprobe : Int -> Int\nprobe n =\n    n + 1\n"

                memoBenchSource =
                    "module MemoBench exposing (probeResults)\n\nimport ExpensiveHelper\n\nprobeResults : Int\nprobeResults =\n    ExpensiveHelper.probe 1\n        + ExpensiveHelper.probe 2\n        + ExpensiveHelper.probe 3\n        + ExpensiveHelper.probe 4\n        + ExpensiveHelper.probe 5\n        + ExpensiveHelper.probe 6\n        + ExpensiveHelper.probe 7\n        + ExpensiveHelper.probe 8\n"

                source =
                    "module Main exposing (results)\n\nimport MemoBench\n\nresults : String\nresults =\n    MemoBench.probeResults\n"

                intercepts =
                    FastDict.singleton "ExpensiveHelper.probe"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ Int n ] ->
                                        EvYield "test-yield" (Int n) (\_ -> EvOk (Int (n + 1)))

                                    _ ->
                                        EvOk (Int 0)
                            )
                        )
            in
            case Eval.Module.buildProjectEnv [ helperSource, memoBenchSource, source ] of
                Err _ ->
                    Expect.fail "Failed to build env"

                Ok env ->
                    let
                        driveResult =
                            Eval.Module.evalWithInterceptsRaw env [] intercepts (Expression.FunctionOrValue [] "results")
                                |> (\rawResult -> driveYieldsSync rawResult [])
                    in
                    case driveResult of
                        { finalResult, yields } ->
                            Expect.all
                                [ \_ -> Expect.equal 8 (List.length yields)
                                , \_ ->
                                    case finalResult of
                                        EvOk (Int 44) ->
                                            Expect.pass

                                        EvOk other ->
                                            Expect.fail ("Expected Int 44, got: " ++ Debug.toString other)

                                        EvErr e ->
                                            Expect.fail ("Error: " ++ Types.evalErrorKindToString e.error)

                                        _ ->
                                            Expect.fail ("Unexpected: " ++ Debug.toString finalResult)
                                ]
                                ()


runtimeMemoReusesCacheAcrossInvocations : Test
runtimeMemoReusesCacheAcrossInvocations =
    test "runtime memo reuses cache across invocations" <|
        \_ ->
            let
                helperSource =
                    """module ExpensiveHelper exposing (expensive)

expensive : Int -> Int
expensive n =
    let
        doubled =
            List.map (\\i -> i * 2) (List.range 1 n)
    in
    List.foldl (+) 0 doubled
"""

                memoBenchSource =
                    """module MemoBench exposing (results)

import ExpensiveHelper

results : Int
results =
    ExpensiveHelper.expensive 20
        + ExpensiveHelper.expensive 20
        + ExpensiveHelper.expensive 20
        + ExpensiveHelper.expensive 20
"""
            in
            case Eval.Module.buildProjectEnv [ helperSource ] of
                Err e ->
                    Expect.fail ("Build error: " ++ Debug.toString e)

                Ok env ->
                    case
                        Eval.Module.evalWithMemoizedFunctions
                            env
                            [ memoBenchSource ]
                            (Set.singleton "ExpensiveHelper.expensive")
                            MemoRuntime.emptyMemoCache
                            True
                            (Expression.FunctionOrValue [] "results")
                    of
                        Err e ->
                            Expect.fail ("Cold memo eval error: " ++ Debug.toString e)

                        Ok coldResult ->
                            case
                                Eval.Module.evalWithMemoizedFunctions
                                    env
                                    [ memoBenchSource ]
                                    (Set.singleton "ExpensiveHelper.expensive")
                                    coldResult.memoCache
                                    True
                                    (Expression.FunctionOrValue [] "results")
                            of
                                Err e ->
                                    Expect.fail ("Warm memo eval error: " ++ Debug.toString e)

                                Ok warmResult ->
                                    Expect.all
                                        [ \_ -> Expect.equal (Int 1680) coldResult.value
                                        , \_ -> Expect.equal coldResult.value warmResult.value
                                        , \_ -> Expect.equal 4 coldResult.memoStats.lookups
                                        , \_ -> Expect.equal 3 coldResult.memoStats.hits
                                        , \_ -> Expect.equal 1 coldResult.memoStats.misses
                                        , \_ -> Expect.equal 1 coldResult.memoStats.stores
                                        , \_ -> Expect.equal 4 warmResult.memoStats.lookups
                                        , \_ -> Expect.equal 4 warmResult.memoStats.hits
                                        , \_ -> Expect.equal 0 warmResult.memoStats.misses
                                        , \_ -> Expect.equal 0 warmResult.memoStats.stores
                                        ]
                                        ()


{-| Test: multiple yields from sequential let bindings.
let x = marker 1
y = marker 2
in x + y
-}
multipleYieldsFromLetBindings : Test
multipleYieldsFromLetBindings =
    test "multiple yields from sequential let bindings" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nimport Helpers\n\nresults =\n    let\n        x = Helpers.marker 1\n        y = Helpers.marker 2\n    in\n    x + y\n"

                helperSource =
                    "module Helpers exposing (marker)\n\nmarker n = n\n"

                intercepts =
                    FastDict.singleton "Helpers.marker"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ Int n ] ->
                                        EvYield "test-yield" (Int n) (\_ -> EvOk (Int (n * 10)))

                                    _ ->
                                        EvOk (Int 0)
                            )
                        )
            in
            case Eval.Module.buildProjectEnv [ helperSource, source ] of
                Err _ ->
                    Expect.fail "Failed to build env"

                Ok env ->
                    let
                        rawResult =
                            Eval.Module.evalWithInterceptsRaw env [] intercepts (Expression.FunctionOrValue [] "results")

                        driveResult =
                            driveYieldsSync rawResult []
                    in
                    case driveResult of
                        { finalResult, yields } ->
                            Expect.all
                                [ \_ ->
                                    -- Should yield 2 times
                                    Expect.equal 2 (List.length yields)
                                , \_ ->
                                    case finalResult of
                                        EvOk (Int 30) ->
                                            -- 1*10 + 2*10 = 30
                                            Expect.pass

                                        EvOk other ->
                                            Expect.fail ("Expected Int 30, got: " ++ Debug.toString other)

                                        EvErr e ->
                                            Expect.fail ("Error: " ++ Types.evalErrorKindToString e.error)

                                        _ ->
                                            Expect.fail ("Unexpected: " ++ Debug.toString finalResult)
                                ]
                                ()


{-| Test: yields from within List.foldl — the elm-review pattern.
Each fold iteration calls a yielding function.
-}
yieldInsideListFoldl : Test
yieldInsideListFoldl =
    test "multiple yields from List.foldl iterations" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nimport Helpers\n\nresults = List.foldl (\\n acc -> acc + Helpers.marker n) 0 [1, 2, 3]\n"

                helperSource =
                    "module Helpers exposing (marker)\n\nmarker n = n\n"

                intercepts =
                    FastDict.singleton "Helpers.marker"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ Int n ] ->
                                        EvYield "test-yield" (Int n) (\_ -> EvOk (Int (n * 10)))

                                    _ ->
                                        EvOk (Int 0)
                            )
                        )
            in
            case Eval.Module.buildProjectEnv [ helperSource, source ] of
                Err _ ->
                    Expect.fail "Failed to build env"

                Ok env ->
                    let
                        rawResult =
                            Eval.Module.evalWithInterceptsRaw env [] intercepts (Expression.FunctionOrValue [] "results")
                    in
                    -- Manually drive yields to see what happens at each step
                    case rawResult of
                        EvYield _ (Int n1) resume1 ->
                            let
                                step2 =
                                    resume1 Unit
                            in
                            case step2 of
                                EvYield _ (Int n2) resume2 ->
                                    -- Great, second yield! Continue...
                                    let
                                        step3 =
                                            resume2 Unit
                                    in
                                    case step3 of
                                        EvYield _ (Int _) resume3 ->
                                            case resume3 Unit of
                                                EvOk (Int total) ->
                                                    Expect.equal 60 total

                                                other ->
                                                    Expect.fail ("After 3 yields, got: " ++ Debug.toString other)

                                        EvOk (Int total) ->
                                            -- Only 2 yields, check value
                                            Expect.fail ("Only 2 yields (expected 3), final value: " ++ String.fromInt total ++ ", payloads: " ++ String.fromInt n1 ++ ", " ++ String.fromInt n2)

                                        EvErr e ->
                                            Expect.fail ("Error after 2 yields: " ++ Types.evalErrorKindToString e.error)

                                        other ->
                                            Expect.fail ("After yield 2, unexpected: " ++ Debug.toString other)

                                EvOk val ->
                                    Expect.fail ("Only 1 yield (payload=" ++ String.fromInt n1 ++ "), resume gave EvOk: " ++ Debug.toString val)

                                EvErr e ->
                                    Expect.fail ("Only 1 yield (payload=" ++ String.fromInt n1 ++ "), resume gave error: " ++ Types.evalErrorKindToString e.error)

                                _ ->
                                    Expect.fail ("Only 1 yield (payload=" ++ String.fromInt n1 ++ "), resume gave: " ++ Debug.toString step2)

                        EvOk val ->
                            Expect.fail ("No yields at all, got EvOk: " ++ Debug.toString val)

                        EvErr e ->
                            Expect.fail ("No yields, got error: " ++ Types.evalErrorKindToString e.error)

                        _ ->
                            Expect.fail ("Unexpected initial result: " ++ Debug.toString rawResult)


{-| Test: yields from within a manual fold (no kernel) to isolate kernel vs trampoline issue.
-}
yieldInsideManualFold : Test
yieldInsideManualFold =
    test "multiple yields from manual fold (no kernel)" <|
        \_ ->
            let
                source =
                    "module Main exposing (..)\n\nimport Helpers\n\nmyFold f acc list =\n    case list of\n        [] -> acc\n        x :: rest -> myFold f (f x acc) rest\n\nresults = myFold (\\n acc -> acc + Helpers.marker n) 0 [1, 2, 3]\n"

                helperSource =
                    "module Helpers exposing (marker)\n\nmarker n = n\n"

                intercepts =
                    FastDict.singleton "Helpers.marker"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ Int n ] ->
                                        EvYield "test-yield" (Int n) (\_ -> EvOk (Int (n * 10)))

                                    _ ->
                                        EvOk (Int 0)
                            )
                        )
            in
            case Eval.Module.buildProjectEnv [ helperSource, source ] of
                Err _ ->
                    Expect.fail "Failed to build env"

                Ok env ->
                    let
                        rawResult =
                            Eval.Module.evalWithInterceptsRaw env [] intercepts (Expression.FunctionOrValue [] "results")

                        driveResult =
                            driveYieldsSync rawResult []
                    in
                    case driveResult of
                        { finalResult, yields } ->
                            Expect.all
                                [ \_ ->
                                    -- Should yield 3 times
                                    Expect.equal 3 (List.length yields)
                                , \_ ->
                                    case finalResult of
                                        EvOk (Int 60) ->
                                            Expect.pass

                                        EvOk other ->
                                            Expect.fail ("Expected Int 60, got: " ++ Debug.toString other)

                                        EvErr e ->
                                            Expect.fail ("Error: " ++ Types.evalErrorKindToString e.error)

                                        _ ->
                                            Expect.fail ("Unexpected: " ++ Debug.toString finalResult)
                                ]
                                ()


{-| Test: yields from within List.map iterations.
-}
yieldInsideListMap : Test
yieldInsideListMap =
    test "multiple yields from List.map iterations" <|
        \_ ->
            let
                -- Test with 3 elements via custom map (no kernel)
                source =
                    "module Main exposing (..)\n\nimport Helpers\n\nmyMap f list =\n    case list of\n        [] -> []\n        x :: rest -> f x :: myMap f rest\n\nresults = myMap (\\n -> Helpers.marker n) [1, 2, 3]\n"

                helperSource =
                    "module Helpers exposing (marker)\n\nmarker n = n\n"

                -- Use a different intercept: return Int (n * 10) directly (no yield).
                -- This tests if the intercept fires for ALL elements.
                interceptsNoYield =
                    FastDict.singleton "Helpers.marker"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ Int n ] ->
                                        EvOk (Int (n * 10))

                                    _ ->
                                        EvOk (Int 0)
                            )
                        )

                interceptsWithYield =
                    FastDict.singleton "Helpers.marker"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ Int n ] ->
                                        EvYield "test-yield" (Int n) (\_ -> EvOk (Int (n * 10)))

                                    _ ->
                                        EvOk (Int 0)
                            )
                        )
            in
            case Eval.Module.buildProjectEnv [ helperSource, source ] of
                Err _ ->
                    Expect.fail "Failed to build env"

                Ok env ->
                    let
                        -- First check: do intercepts fire at all (no yield)?
                        noYieldResult =
                            Eval.Module.evalWithInterceptsRaw env [] interceptsNoYield (Expression.FunctionOrValue [] "results")

                        -- Then check with yields
                        yieldResult =
                            Eval.Module.evalWithInterceptsRaw env [] interceptsWithYield (Expression.FunctionOrValue [] "results")

                        countYields result n pacc =
                            case result of
                                EvYield _ payload resume ->
                                    countYields (resume Unit) (n + 1) (payload :: pacc)

                                _ ->
                                    ( n, result, List.reverse pacc )

                        ( yieldCount, final, payloads ) =
                            countYields yieldResult 0 []
                    in
                    Expect.all
                        [ \_ ->
                            -- Non-yield intercept: should give [10, 20, 30]
                            case noYieldResult of
                                EvOk (List [ Int 10, Int 20, Int 30 ]) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail ("Non-yield intercept gave: " ++ Debug.toString noYieldResult)
                        , \_ ->
                            if yieldCount /= 3 then
                                Expect.fail ("Expected 3 yields, got " ++ String.fromInt yieldCount ++ ". Payloads: " ++ Debug.toString payloads ++ ". Final: " ++ Debug.toString final)

                            else
                                Expect.pass
                        ]
                        ()


{-| Synchronous yield driver for tests: drives yields until EvOk/EvErr,
collecting all yield payloads along the way.
-}
driveYieldsSync : Types.EvalResult Value -> List ( String, Value ) -> { finalResult : Types.EvalResult Value, yields : List ( String, Value ) }
driveYieldsSync result acc =
    case result of
        EvYield tag payload resume ->
            -- Handle the yield (return Unit as resume value) and continue
            driveYieldsSync (resume Unit) (( tag, payload ) :: acc)

        other ->
            { finalResult = other, yields = List.reverse acc }


deepHashSameValue : Test
deepHashSameValue =
    test "same value produces same hash" <|
        \_ ->
            let
                v1 =
                    Record (FastDict.fromList [ ( "name", String "Alice" ), ( "age", Int 30 ) ])

                v2 =
                    Record (FastDict.fromList [ ( "name", String "Alice" ), ( "age", Int 30 ) ])
            in
            Eval.Expression.deepHashValue v1
                |> Expect.equal (Eval.Expression.deepHashValue v2)


deepHashDifferentValues : Test
deepHashDifferentValues =
    test "different values produce different hashes" <|
        \_ ->
            let
                v1 =
                    Record (FastDict.fromList [ ( "name", String "Alice" ) ])

                v2 =
                    Record (FastDict.fromList [ ( "name", String "Bob" ) ])
            in
            Eval.Expression.deepHashValue v1
                |> Expect.notEqual (Eval.Expression.deepHashValue v2)


deepHashNestedStructure : Test
deepHashNestedStructure =
    test "nested structures produce stable hashes" <|
        \_ ->
            let
                inner =
                    Record (FastDict.fromList [ ( "x", Int 1 ) ])

                outer =
                    List [ inner, inner ]
            in
            Eval.Expression.deepHashValue outer
                |> Expect.equal (Eval.Expression.deepHashValue outer)


deepHashClosureSentinel : Test
deepHashClosureSentinel =
    test "data-only values hash to non-zero" <|
        \_ ->
            let
                dataOnly =
                    List [ Int 1, String "hello", Bool True ]
            in
            Eval.Expression.deepHashValue dataOnly
                |> Expect.notEqual 0


codecRoundTripPrimitives : Test
codecRoundTripPrimitives =
    test "round-trip primitives" <|
        \_ ->
            let
                values =
                    [ String "hello", Int 42, Float 3.14, Bool True, Bool False, Unit, Char 'x' ]

                roundTripped =
                    values |> List.filterMap (\v -> ValueCodec.encodeValue v |> ValueCodec.decodeValue)
            in
            Expect.equal values roundTripped


codecRoundTripRecord : Test
codecRoundTripRecord =
    test "round-trip Record with nested values" <|
        \_ ->
            let
                value =
                    Record
                        (FastDict.fromList
                            [ ( "name", String "Alice" )
                            , ( "age", Int 30 )
                            , ( "active", Bool True )
                            ]
                        )
            in
            case ValueCodec.encodeValue value |> ValueCodec.decodeValue of
                Just decoded ->
                    Expect.equal value decoded

                Nothing ->
                    Expect.fail "Failed to decode Record"


codecRoundTripCustom : Test
codecRoundTripCustom =
    test "round-trip Custom type (Maybe Just)" <|
        \_ ->
            let
                value =
                    Custom { moduleName = [ "Maybe" ], name = "Just" } [ Int 42 ]
            in
            case ValueCodec.encodeValue value |> ValueCodec.decodeValue of
                Just decoded ->
                    Expect.equal value decoded

                Nothing ->
                    Expect.fail "Failed to decode Custom"


codecRoundTripList : Test
codecRoundTripList =
    test "round-trip List of Records" <|
        \_ ->
            let
                value =
                    List
                        [ Record (FastDict.fromList [ ( "x", Int 1 ) ])
                        , Record (FastDict.fromList [ ( "x", Int 2 ) ])
                        ]
            in
            case ValueCodec.encodeValue value |> ValueCodec.decodeValue of
                Just decoded ->
                    Expect.equal value decoded

                Nothing ->
                    Expect.fail "Failed to decode List of Records"


codecRoundTripNestedCustom : Test
codecRoundTripNestedCustom =
    test "round-trip nested Custom types preserves hash fields" <|
        \_ ->
            let
                -- Simulate ContentHash (Int 455258608)
                contentHash =
                    Custom { moduleName = [ "Review", "Cache", "ContentHash" ], name = "ContentHash" } [ Int 455258608 ]

                -- Simulate ContextHash (Int 2104301476)
                contextHash =
                    Custom { moduleName = [ "Review", "Cache", "ContextHash" ], name = "ContextHash" } [ Int 2104301476 ]

                -- Simulate ComparableContextHash [ContextHash ...]
                comparableHash =
                    Custom { moduleName = [ "Review", "Cache", "ContextHash" ], name = "ComparableContextHash" } [ List [ contextHash ] ]

                -- Simulate a ModuleCacheEntry
                entry =
                    Custom { moduleName = [ "Review", "Cache", "Module" ], name = "Entry" }
                        [ Record
                            (FastDict.fromList
                                [ ( "contentHash", contentHash )
                                , ( "inputContextHashes", comparableHash )
                                , ( "isFileIgnored", Bool False )
                                , ( "isFileFixable", Bool False )
                                , ( "errors", List [] )
                                , ( "outputContext", Record (FastDict.fromList [ ( "unused", List [] ) ]) )
                                , ( "outputContextHash", contextHash )
                                ]
                            )
                        ]
            in
            case ValueCodec.encodeValue entry |> ValueCodec.decodeValue of
                Just (Custom _ [ Record fields ]) ->
                    -- Verify the hash fields round-trip correctly (these are what match uses)
                    -- Note: Record == may fail due to FastDict tree structure, but
                    -- individual field lookups should work.
                    Expect.all
                        [ \_ ->
                            FastDict.get "contentHash" fields
                                |> Expect.equal (Just contentHash)
                        , \_ ->
                            FastDict.get "inputContextHashes" fields
                                |> Expect.equal (Just comparableHash)
                        , \_ ->
                            FastDict.get "outputContextHash" fields
                                |> Expect.equal (Just contextHash)
                        , \_ ->
                            FastDict.get "isFileIgnored" fields
                                |> Expect.equal (Just (Bool False))
                        ]
                        ()

                Just _ ->
                    Expect.fail "Decoded to unexpected shape"

                Nothing ->
                    Expect.fail "Failed to decode ModuleCacheEntry-shaped value"


{-| Test that ContentHash and ContextHash survive round-trip with == equality.
This is what elm-review's match function checks.
-}
codecContentHashEquality : Test
codecContentHashEquality =
    test "ContentHash/ContextHash round-trip preserves == equality" <|
        \_ ->
            let
                contentHash =
                    Custom { moduleName = [ "Review", "Cache", "ContentHash" ], name = "ContentHash" } [ Int 455258608 ]

                contextHash =
                    Custom { moduleName = [ "Review", "Cache", "ContextHash" ], name = "ContextHash" } [ Int 2104301476 ]

                comparableHash =
                    Custom { moduleName = [ "Review", "Cache", "ContextHash" ], name = "ComparableContextHash" }
                        [ List [ contextHash, contextHash ] ]

                -- Round-trip each
                contentRoundTrip =
                    ValueCodec.encodeValue contentHash |> ValueCodec.decodeValue

                contextRoundTrip =
                    ValueCodec.encodeValue contextHash |> ValueCodec.decodeValue

                comparableRoundTrip =
                    ValueCodec.encodeValue comparableHash |> ValueCodec.decodeValue
            in
            Expect.all
                [ \_ -> Expect.equal (Just contentHash) contentRoundTrip
                , \_ -> Expect.equal (Just contextHash) contextRoundTrip
                , \_ -> Expect.equal (Just comparableHash) comparableRoundTrip
                ]
                ()


{-| Test that ContextHash lists in different orders become equal after sorting by hash value.
This is what the sort intercept does for ComparableContextHash determinism.
-}
codecContextHashListOrderIndependent : Test
codecContextHashListOrderIndependent =
    test "ContextHash lists sorted by hash produce equal ComparableContextHash" <|
        \_ ->
            let
                mkCtxHash h =
                    Custom { moduleName = [ "Review", "Cache", "ContextHash" ], name = "ContextHash" } [ Int h ]

                list1 =
                    [ mkCtxHash 300, mkCtxHash 100, mkCtxHash 200 ]

                list2 =
                    [ mkCtxHash 100, mkCtxHash 200, mkCtxHash 300 ]

                sortByHash items =
                    List.sortBy
                        (\item ->
                            case item of
                                Custom _ [ Int h ] ->
                                    h

                                _ ->
                                    0
                        )
                        items

                comparable1 =
                    Custom { moduleName = [ "Review", "Cache", "ContextHash" ], name = "ComparableContextHash" }
                        [ List (sortByHash list1) ]

                comparable2 =
                    Custom { moduleName = [ "Review", "Cache", "ContextHash" ], name = "ComparableContextHash" }
                        [ List (sortByHash list2) ]
            in
            Expect.equal comparable1 comparable2


codecRoundTripDict : Test
codecRoundTripDict =
    test "round-trip Dict tree (RBNode_elm_builtin)" <|
        \_ ->
            let
                -- Simulate a small Dict with 2 entries (like moduleContexts)
                leaf =
                    Custom { moduleName = [ "Dict" ], name = "RBEmpty_elm_builtin" } []

                node1 =
                    Custom { moduleName = [ "Dict" ], name = "RBNode_elm_builtin" }
                        [ Custom { moduleName = [ "Dict" ], name = "Black" } []
                        , String "key1"
                        , Int 100
                        , leaf
                        , leaf
                        ]

                node2 =
                    Custom { moduleName = [ "Dict" ], name = "RBNode_elm_builtin" }
                        [ Custom { moduleName = [ "Dict" ], name = "Black" } []
                        , String "key2"
                        , Int 200
                        , node1
                        , leaf
                        ]
            in
            case ValueCodec.encodeValue node2 |> ValueCodec.decodeValue of
                Just decoded ->
                    Expect.equal node2 decoded

                Nothing ->
                    Expect.fail "Failed to decode Dict tree"


{-| Phase 1a coherence check: after `replaceModuleInEnv`, the `resolved`
sidecar must describe the same (moduleName, name) entries for the replaced
module as `env.shared.functions`. Before Phase 1a the sidecar was kept stale
(`resolved = projectEnv.resolved`), which silently broke the resolved-IR path
whenever mutation testing used `replaceModuleInEnv` in a hot loop.
-}
replaceRefreshesResolvedSidecar : Test
replaceRefreshesResolvedSidecar =
    test "replaceModuleInEnv rebuilds resolved sidecar for the replaced module" <|
        \_ ->
            let
                originalA : String
                originalA =
                    "module A exposing (..)\n\noldHelper x = x + 1\n"

                mutatedA : String
                mutatedA =
                    "module A exposing (..)\n\nnewHelper x = x + 2\n\nanotherHelper y = y * 2\n"
            in
            case ( Eval.Module.buildProjectEnv [ originalA ], parseModule mutatedA ) of
                ( Ok baseEnv, Just replacement ) ->
                    case Eval.Module.replaceModuleInEnv baseEnv replacement of
                        Ok updatedEnv ->
                            let
                                resolved =
                                    Eval.Module.projectEnvResolved updatedEnv

                                resolvedNamesForModuleA =
                                    resolved.globalIds
                                        |> FastDict.keys
                                        |> List.filterMap
                                            (\( mod, name ) ->
                                                if mod == [ "A" ] then
                                                    Just name

                                                else
                                                    Nothing
                                            )
                                        |> Set.fromList

                                envNamesForModuleA =
                                    Eval.Module.getModuleFunctions [ "A" ] updatedEnv
                                        |> FastDict.keys
                                        |> Set.fromList
                            in
                            -- env has newHelper and anotherHelper; resolved should too, and should NOT have oldHelper
                            envNamesForModuleA
                                |> Expect.all
                                    [ Expect.equal (Set.fromList [ "newHelper", "anotherHelper" ])
                                    , \_ -> Expect.equal envNamesForModuleA resolvedNamesForModuleA
                                    ]

                        Err e ->
                            Expect.fail ("replaceModuleInEnv failed: " ++ Debug.toString e)

                _ ->
                    Expect.fail "Setup failed: could not build base env or parse replacement"


{-| Phase 1a coherence check: after `extendWithFiles`, the `resolved` sidecar
must describe every module that ended up in `env.shared.functions`. Before
Phase 1a the sidecar only contained modules present at original
`buildProjectEnv` time, so newly-extended modules were invisible to the
resolved-IR path.
-}
extendWithFilesRefreshesResolvedSidecar : Test
extendWithFilesRefreshesResolvedSidecar =
    test "extendWithFiles rebuilds resolved sidecar to include new modules" <|
        \_ ->
            let
                baseModule : String
                baseModule =
                    "module Base exposing (..)\n\nbaseHelper x = x + 1\n"

                extraModule : String
                extraModule =
                    "module Extra exposing (..)\n\nextraHelper y = y * 2\n"
            in
            case ( Eval.Module.buildProjectEnv [ baseModule ], Elm.Parser.parseToFile extraModule ) of
                ( Ok baseEnv, Ok extraFile ) ->
                    case Eval.Module.extendWithFiles baseEnv [ extraFile ] of
                        Ok extendedEnv ->
                            let
                                resolved =
                                    Eval.Module.projectEnvResolved extendedEnv

                                resolvedModulesAndNames =
                                    resolved.globalIds
                                        |> FastDict.keys
                                        |> List.filter
                                            (\( mod, _ ) ->
                                                mod == [ "Base" ] || mod == [ "Extra" ]
                                            )
                                        |> Set.fromList
                            in
                            -- Both modules should show up in the resolved sidecar after extend
                            resolvedModulesAndNames
                                |> Expect.equal
                                    (Set.fromList
                                        [ ( [ "Base" ], "baseHelper" )
                                        , ( [ "Extra" ], "extraHelper" )
                                        ]
                                    )

                        Err e ->
                            Expect.fail ("extendWithFiles failed: " ++ Debug.toString e)

                _ ->
                    Expect.fail "Setup failed: could not build base env or parse extra module"


{-| Phase 1b verification: `evalWithResolvedIRFromFilesAndIntercepts` must
propagate `EvYield` from a user-land function call through the resolved-IR
evaluator so that a BackendTask driver can resume it. Before this test the
resolved-IR entry point was unproven for the intercept/yield flow even
though the return type is raw `EvalResult Value`.
-}
resolvedIRYieldPropagates : Test
resolvedIRYieldPropagates =
    test "evalWithResolvedIRFromFilesAndIntercepts propagates EvYield from a yielding intercept" <|
        \_ ->
            let
                helperSource : String
                helperSource =
                    "module Helpers exposing (fetch)\n\nfetch key = key\n"

                mainSource : String
                mainSource =
                    "module Main exposing (..)\n\nimport Helpers\n\nresults = Helpers.fetch \"key1\"\n"

                yieldingIntercepts =
                    FastDict.singleton "Helpers.fetch"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ String key ] ->
                                        EvYield "cache-lookup"
                                            (String key)
                                            (\resumeValue -> EvOk resumeValue)

                                    _ ->
                                        EvOk (String "error")
                            )
                        )
            in
            case ( Eval.Module.buildProjectEnv [ helperSource ], Elm.Parser.parseToFile mainSource ) of
                ( Ok projectEnv, Ok mainFile ) ->
                    let
                        evalResult =
                            Eval.Module.evalWithResolvedIRFromFilesAndIntercepts
                                projectEnv
                                [ mainFile ]
                                FastDict.empty
                                yieldingIntercepts
                                (Expression.FunctionOrValue [] "results")
                    in
                    case evalResult of
                        EvYield "cache-lookup" (String "key1") resume ->
                            case resume (String "resumed-value") of
                                EvOk (String "resumed-value") ->
                                    Expect.pass

                                EvOk other ->
                                    Expect.fail ("Expected resumed-value, got: " ++ Debug.toString other)

                                EvErr e ->
                                    Expect.fail ("Error after resume: " ++ Debug.toString e)

                                _ ->
                                    Expect.fail "Unexpected EvalResult variant after resume"

                        EvOk _ ->
                            Expect.fail "Yield didn't propagate through resolved-IR path (got EvOk)"

                        EvErr e ->
                            Expect.fail ("Eval error: " ++ Debug.toString e.error ++ " [module: " ++ String.join "." e.currentModule ++ "]")

                        _ ->
                            Expect.fail ("Unexpected EvalResult variant: " ++ Debug.toString evalResult)

                _ ->
                    Expect.fail "Setup failed: could not build project env or parse main module"


{-| Phase 1b verification: multiple sequential yields through the resolved-IR
entry point, driven by `driveYieldsSync`. Matches the shape of
`multipleYieldsFromSequentialCalls` but routes through
`evalWithResolvedIRFromFilesAndIntercepts` so we can prove the resolved
path supports the full BackendTask-style yield protocol, not just a single
top-level yield.
-}
resolvedIRMultipleYieldsThroughDrive : Test
resolvedIRMultipleYieldsThroughDrive =
    test "evalWithResolvedIRFromFilesAndIntercepts supports multiple yields driven by the sync driver" <|
        \_ ->
            let
                helperSource : String
                helperSource =
                    "module Helpers exposing (marker)\n\nmarker n = n\n"

                mainSource : String
                mainSource =
                    "module Main exposing (..)\n\nimport Helpers\n\nresults = Helpers.marker 1 + Helpers.marker 2\n"

                yieldingIntercepts =
                    FastDict.singleton "Helpers.marker"
                        (Intercept
                            (\_ args _ _ ->
                                case args of
                                    [ Int n ] ->
                                        EvYield "test-yield" (Int n) (\_ -> EvOk (Int (n * 10)))

                                    _ ->
                                        EvOk (Int 0)
                            )
                        )
            in
            case Eval.Module.buildProjectEnv [ helperSource, mainSource ] of
                Ok projectEnv ->
                    let
                        rawResult =
                            Eval.Module.evalWithResolvedIRFromFilesAndIntercepts
                                projectEnv
                                []
                                FastDict.empty
                                yieldingIntercepts
                                (Expression.FunctionOrValue [] "results")

                        driven =
                            driveYieldsSync rawResult []
                    in
                    Expect.all
                        [ \_ ->
                            Expect.equal 2 (List.length driven.yields)
                        , \_ ->
                            case driven.finalResult of
                                EvOk (Int 30) ->
                                    Expect.pass

                                EvOk other ->
                                    Expect.fail ("Expected Int 30, got: " ++ Debug.toString other ++ ", yields: " ++ Debug.toString driven.yields)

                                EvErr e ->
                                    Expect.fail ("Error: " ++ Debug.toString e.error ++ " [module: " ++ String.join "." e.currentModule ++ "]")

                                _ ->
                                    Expect.fail "Unexpected driven final result"
                        ]
                        ()

                Err e ->
                    Expect.fail ("buildProjectEnv failed: " ++ Debug.toString e)
