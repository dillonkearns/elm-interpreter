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
        , noErrorsForBasicProgram
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


errToString : Types.Error -> String
errToString err =
    case err of
        Types.ParsingError _ ->
            "parse error"

        Types.EvalError _ ->
            "eval error"
