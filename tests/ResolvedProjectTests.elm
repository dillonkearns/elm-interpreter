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

import Elm.Syntax.Expression
import Eval.Module
import Eval.ResolvedIR as IR
import Expect
import FastDict
import Syntax
import Test exposing (Test, describe, test)
import Types


suite : Test
suite =
    describe "Eval.Module resolver integration"
        [ coreIdsPopulated
        , singleModuleSmoke
        , multiModuleSmoke
        , noErrorsForBasicProgram
        , broadLanguageCoverage
        , rebuildDispatchersMatchesInitialBuild
        , wireFieldsRoundtripPreservesEval
        ]


wireFieldsRoundtripPreservesEval : Test
wireFieldsRoundtripPreservesEval =
    test "toWireFields → fromWireFields preserves eval behavior" <|
        \_ ->
            -- Step 8b prep: the worker pool's main→worker handoff goes through
            -- toWireFields → encode → ship → decode → fromWireFields. Before
            -- writing the wire codec, prove the toWireFields/fromWireFields
            -- round-trip itself is observationally correct.
            let
                src =
                    """module Foo exposing (..)

answer : Int
answer =
    6 * 7
"""

                expression =
                    Elm.Syntax.Expression.FunctionOrValue [ "Foo" ] "answer"
            in
            case Eval.Module.buildProjectEnv [ src ] of
                Ok original ->
                    let
                        roundtripped =
                            original
                                |> Eval.Module.toWireFields
                                |> Eval.Module.fromWireFields

                        evalAndStringify env =
                            case Eval.Module.evalWithEnv env [] expression of
                                Ok value ->
                                    "ok:" ++ Debug.toString value

                                Err err ->
                                    "err:" ++ errToString err
                    in
                    Expect.equal
                        (evalAndStringify original)
                        (evalAndStringify roundtripped)

                Err err ->
                    Expect.fail (errToString err)


rebuildDispatchersMatchesInitialBuild : Test
rebuildDispatchersMatchesInitialBuild =
    test "rebuildDispatchers reproduces native/higherOrder/kernel dispatcher key sets" <|
        \_ ->
            -- Step 7 (parallel-ceiling defunctionalization): preparation for
            -- step 8's Wire3 codec for ResolvedProject. The codec will skip
            -- the three function-bearing dispatcher fields on encode and
            -- call rebuildDispatchers on decode to repopulate them. This
            -- test guards that the rebuild produces the same set of keys
            -- (and same arities for kernelDispatchers) as the original
            -- inline construction inside resolveProject.
            case Eval.Module.buildProjectEnv [] of
                Ok projectEnv ->
                    let
                        resolved =
                            Eval.Module.projectEnvResolved projectEnv

                        rebuilt =
                            Eval.Module.rebuildDispatchers resolved.globalIds
                    in
                    Expect.all
                        [ \_ ->
                            FastDict.keys rebuilt.native
                                |> Expect.equal (FastDict.keys resolved.nativeDispatchers)
                        , \_ ->
                            FastDict.keys rebuilt.higherOrder
                                |> Expect.equal (FastDict.keys resolved.higherOrderDispatchers)
                        , \_ ->
                            FastDict.keys rebuilt.kernel
                                |> Expect.equal (FastDict.keys resolved.kernelDispatchers)
                        , \_ ->
                            -- Arities must match exactly — kernelDispatchers
                            -- is the only field with a comparable scalar
                            -- alongside its function pointer.
                            FastDict.toList rebuilt.kernel
                                |> List.map (\( id, d ) -> ( id, d.arity ))
                                |> Expect.equal
                                    (FastDict.toList resolved.kernelDispatchers
                                        |> List.map (\( id, d ) -> ( id, d.arity ))
                                    )
                        ]
                        ()

                Err err ->
                    Expect.fail (errToString err)


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
                                    (Just (IR.mkLambda 1 (IR.RLocal 0)))
                        , \_ ->
                            bodiesFor "constant"
                                |> Expect.equal
                                    (Just (IR.mkLambda 2 (IR.RLocal 1)))
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
    \"Hello, \" ++ name ++ \"! You are \" ++ String.fromInt age ++ \" years old.\"


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
        |> (\\s -> \"n+1 = \" ++ s)


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
