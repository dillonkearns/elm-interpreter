module ResolvedProjectTests exposing (suite)

{-| Smoke tests for the resolver integration in `Eval.Module`.

The resolver runs automatically on every `buildProjectEnv` invocation, and
stores its output on the `ProjectEnv` record. These tests verify:

  - The resolver produces RExpr bodies for user declarations (not just
    silently erroring).
  - Same-module references work.
  - Cross-module references work.
  - Operator desugaring reaches the right core function.
  - No resolver errors for the fixture programs (a stronger signal than
    "the old evaluator still ran the program successfully").

If this suite ever fires a resolve error, it means the resolver is missing
coverage for something the parser accepts and the old evaluator handles —
that's the signal Phase 2 iteration 2b was built to catch.

-}

import Elm.Parser
import Eval.Module
import Eval.ResolvedIR as IR
import Expect
import FastDict
import Test exposing (Test, describe, test)
import Types


suite : Test
suite =
    describe "Eval.Module resolver integration"
        [ coreIdsPopulated
        , singleModuleSmoke
        , multiModuleSmoke
        , normalizationPreservesExistingGlobalIds
        , noErrorsForBasicProgram
        , broadLanguageCoverage
        ]


coreIdsPopulated : Test
coreIdsPopulated =
    test "core declarations get GlobalIds assigned" <|
        \_ ->
            case Eval.Module.buildProjectEnv [] of
                Ok projectEnv ->
                    let
                        resolved =
                            Eval.Module.projectEnvResolved projectEnv
                    in
                    -- Basics.add must be in the id map — user code depends on it for `+`.
                    FastDict.get ( [ "Basics" ], "add" ) resolved.globalIds
                        |> Expect.notEqual Nothing

                Err err ->
                    Expect.fail (errToString err)


singleModuleSmoke : Test
singleModuleSmoke =
    test "single-module user code produces RExpr bodies" <|
        \_ ->
            let
                src =
                    """module Foo exposing (..)

identity : a -> a
identity x =
    x

constant : a -> b -> a
constant x _ =
    x

answer : Int
answer =
    42
"""
            in
            case Eval.Module.buildProjectEnv [ src ] of
                Ok projectEnv ->
                    let
                        resolved =
                            Eval.Module.projectEnvResolved projectEnv

                        bodiesFor : String -> Maybe IR.RExpr
                        bodiesFor name =
                            FastDict.get ( [ "Foo" ], name ) resolved.globalIds
                                |> Maybe.andThen (\id -> FastDict.get id resolved.bodies)
                    in
                    Expect.all
                        [ \_ ->
                            bodiesFor "identity"
                                |> Expect.equal
                                    (Just (IR.RLambda { arity = 1, body = IR.RLocal 0 }))
                        , \_ ->
                            bodiesFor "constant"
                                |> Expect.equal
                                    (Just (IR.RLambda { arity = 2, body = IR.RLocal 1 }))
                        , \_ ->
                            bodiesFor "answer"
                                |> Expect.equal (Just (IR.RInt 42))
                        , \_ ->
                            List.length resolved.errors
                                |> Expect.equal 0
                        ]
                        ()

                Err err ->
                    Expect.fail (errToString err)


multiModuleSmoke : Test
multiModuleSmoke =
    test "multi-module: cross-module references resolve to RGlobal" <|
        \_ ->
            let
                moduleA =
                    """module A exposing (..)

foo : Int
foo =
    42

bar : Int -> Int
bar x =
    x + foo
"""

                moduleB =
                    """module B exposing (..)

import A

baz : Int
baz =
    A.bar 5
"""
            in
            case Eval.Module.buildProjectEnv [ moduleA, moduleB ] of
                Ok projectEnv ->
                    let
                        resolved =
                            Eval.Module.projectEnvResolved projectEnv

                        bazId =
                            FastDict.get ( [ "B" ], "baz" ) resolved.globalIds

                        barId =
                            FastDict.get ( [ "A" ], "bar" ) resolved.globalIds
                    in
                    Expect.all
                        [ \_ ->
                            -- `baz`'s body is `A.bar 5`, which should resolve to
                            -- `RApply (RGlobal <id of A.bar>) [RInt 5]`.
                            case ( bazId, barId ) of
                                ( Just bid, Just brid ) ->
                                    FastDict.get bid resolved.bodies
                                        |> Expect.equal
                                            (Just (IR.RApply (IR.RGlobal brid) [ IR.RInt 5 ]))

                                _ ->
                                    Expect.fail "expected ids for both A.bar and B.baz"
                        , \_ ->
                            List.length resolved.errors
                                |> Expect.equal 0
                        ]
                        ()

                Err err ->
                    Expect.fail (errToString err)


normalizationPreservesExistingGlobalIds : Test
normalizationPreservesExistingGlobalIds =
    test "normalizing one module preserves existing GlobalIds and reverse lookup" <|
        \_ ->
            let
                moduleFoo =
                    """module Foo exposing (answer)

answer : Int
answer =
    1 + 2
"""

                moduleBar =
                    """module Bar exposing (value)

import Foo

value : Int
value =
    Foo.answer
"""
            in
            case ( Eval.Module.buildProjectEnv [], Elm.Parser.parseToFile moduleFoo, Elm.Parser.parseToFile moduleBar ) of
                ( Ok baseEnv, Ok fooFile, Ok barFile ) ->
                    case Eval.Module.extendWithFiles baseEnv [ fooFile, barFile ] of
                        Ok extendedEnv ->
                            let
                                before =
                                    Eval.Module.projectEnvResolved extendedEnv

                                after =
                                    Eval.Module.normalizeUserModulesInEnv [ [ "Foo" ] ] extendedEnv
                                        |> Eval.Module.projectEnvResolved
                            in
                            case
                                ( FastDict.get ( [ "Foo" ], "answer" ) before.globalIds
                                , FastDict.get ( [ "Bar" ], "value" ) before.globalIds
                                )
                            of
                                ( Just fooId, Just barId ) ->
                                    Expect.all
                                        [ \_ ->
                                            FastDict.get ( [ "Foo" ], "answer" ) after.globalIds
                                                |> Expect.equal (Just fooId)
                                        , \_ ->
                                            FastDict.get fooId after.globalIdToName
                                                |> Expect.equal (Just ( [ "Foo" ], "answer" ))
                                        , \_ ->
                                            FastDict.get barId after.bodies
                                                |> Expect.equal (Just (IR.RGlobal fooId))
                                        , \_ ->
                                            FastDict.member fooId after.bodies
                                                |> Expect.equal True
                                        ]
                                        ()

                                _ ->
                                    Expect.fail "expected ids for Foo.answer and Bar.value"

                        Err err ->
                            Expect.fail (errToString err)

                ( Err err, _, _ ) ->
                    Expect.fail (errToString err)

                _ ->
                    Expect.fail "failed to parse normalization fixtures"


noErrorsForBasicProgram : Test
noErrorsForBasicProgram =
    test "a program using records, case, let, and operators resolves cleanly" <|
        \_ ->
            -- Broad exercise: records, case on Maybe, let bindings, operators,
            -- lambdas, cross-module reference, pipeline operator.
            let
                src =
                    """module Demo exposing (..)

type alias Point =
    { x : Int, y : Int }


origin : Point
origin =
    { x = 0, y = 0 }


distance : Point -> Point -> Int
distance p q =
    let
        dx =
            p.x - q.x

        dy =
            p.y - q.y
    in
    dx * dx + dy * dy


safeDiv : Int -> Int -> Maybe Int
safeDiv a b =
    case b of
        0 ->
            Nothing

        _ ->
            Just (a // b)


pipeline : Int -> Int
pipeline n =
    n
        |> (\\x -> x + 1)
        |> (\\x -> x * 2)
"""
            in
            case Eval.Module.buildProjectEnv [ src ] of
                Ok projectEnv ->
                    let
                        resolved =
                            Eval.Module.projectEnvResolved projectEnv

                        errorSummary : String
                        errorSummary =
                            resolved.errors
                                |> List.map (\e -> e.name)
                                |> String.join ", "
                    in
                    if List.isEmpty resolved.errors then
                        Expect.pass

                    else
                        Expect.fail ("resolver failed on: " ++ errorSummary)

                Err err ->
                    Expect.fail (errToString err)


broadLanguageCoverage : Test
broadLanguageCoverage =
    test "a broad language-coverage fixture resolves with zero errors" <|
        \_ ->
            -- This fixture is intentionally dense — every construct here
            -- should survive the resolver, and any addition to this test
            -- should come with a confirmation that the parser produces the
            -- expected AST. If the resolver ever regresses on one of these
            -- constructs, the test's error list pinpoints the specific
            -- declaration.
            let
                src =
                    """module BigFixture exposing (..)

type Tree a
    = Leaf
    | Node (Tree a) a (Tree a)


type alias Person =
    { name : String
    , age : Int
    , friends : List String
    }


insert : comparable -> Tree comparable -> Tree comparable
insert value tree =
    case tree of
        Leaf ->
            Node Leaf value Leaf

        Node left x right ->
            if value < x then
                Node (insert value left) x right

            else if value > x then
                Node left x (insert value right)

            else
                tree


toList : Tree a -> List a
toList tree =
    case tree of
        Leaf ->
            []

        Node left x right ->
            toList left ++ (x :: toList right)


greet : Person -> String
greet { name, age } =
    "Hello, " ++ name ++ "! You are " ++ String.fromInt age ++ " years old."


birthday : Person -> Person
birthday person =
    { person | age = person.age + 1 }


addFriend : String -> Person -> Person
addFriend friend person =
    { person | friends = friend :: person.friends }


pairUp : List a -> List b -> List ( a, b )
pairUp xs ys =
    case ( xs, ys ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( x :: xrest, y :: yrest ) ->
            ( x, y ) :: pairUp xrest yrest


foldr : (a -> b -> b) -> b -> List a -> b
foldr f acc list =
    case list of
        [] ->
            acc

        head :: rest ->
            f head (foldr f acc rest)


map : (a -> b) -> List a -> List b
map f list =
    foldr (\\x acc -> f x :: acc) [] list


maybeMap2 : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeMap2 f ma mb =
    case ( ma, mb ) of
        ( Just a, Just b ) ->
            Just (f a b)

        _ ->
            Nothing


type Result err ok
    = Err err
    | Ok ok


andThen : (a -> Result err b) -> Result err a -> Result err b
andThen f result =
    case result of
        Ok value ->
            f value

        Err e ->
            Err e


pipeline : Int -> String
pipeline n =
    n
        |> (+) 1
        |> String.fromInt
        |> (\\s -> "n+1 = " ++ s)


complexLet : Int -> Int
complexLet n =
    let
        doubled =
            n * 2

        tripled =
            n * 3

        sumOfBoth =
            doubled + tripled

        quadrupled x =
            x * 4
    in
    quadrupled sumOfBoth


destructuringLet : ( Int, Int ) -> Int
destructuringLet pair =
    let
        ( a, b ) =
            pair
    in
    a + b


lambdaInsideLambda : Int -> Int -> Int
lambdaInsideLambda x =
    \\y -> x + y


higherOrder : (Int -> Int) -> (Int -> Int) -> Int -> Int
higherOrder f g n =
    f (g n)
"""
            in
            case Eval.Module.buildProjectEnv [ src ] of
                Ok projectEnv ->
                    let
                        resolved =
                            Eval.Module.projectEnvResolved projectEnv

                        errorDescriptions : List String
                        errorDescriptions =
                            resolved.errors
                                |> List.map (\e -> e.name)
                    in
                    if List.isEmpty errorDescriptions then
                        -- Also spot-check that we resolved something. A
                        -- silent "zero errors because we resolved zero
                        -- declarations" would pass the error check but
                        -- isn't what we want.
                        FastDict.size resolved.bodies
                            |> Expect.atLeast 15

                    else
                        Expect.fail
                            ("resolver failed on: " ++ String.join ", " errorDescriptions)

                Err err ->
                    Expect.fail (errToString err)


errToString : Types.Error -> String
errToString err =
    case err of
        Types.ParsingError _ ->
            "parse error"

        Types.EvalError _ ->
            "eval error"
