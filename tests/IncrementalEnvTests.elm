module IncrementalEnvTests exposing (suite)

{-| Tests for incremental env building: replacing a single module in a ProjectEnv
should produce identical evaluation results to building the entire env from scratch.
-}

import Elm.Interface exposing (Exposed)
import Elm.Parser
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File exposing (File)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Incremental Env (replaceModuleInEnv)"
        [ replaceProducesSameResult
        , replaceWithChangedBody
        , replacePreservesImportResolution
        , replaceWithCustomType
        , replaceWithRecordAlias
        ]


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

1. Build base env from original sources (all modules)
2. Replace one module with its mutated version
3. Evaluate in the context of the "main" module (passed as additional file)

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
