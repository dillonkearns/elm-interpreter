module HigherOrderDispatchTests exposing (suite)

{-| Tests for `Eval.ResolvedExpression`'s higher-order kernel dispatchers
(e.g. `List.foldl`). These dispatchers invoke user callbacks via
`applyClosure` directly, bypassing `Kernel.function` marshaling — which
is what was mis-handling `RExprImpl` closures (patterns = [] but
arity > 0) under the `initContextWithImports` resolver widening.

Each test goes through `Eval.Module.evalWithResolvedIR`, which is the
new-evaluator entry point. The dispatchers receive pre-evaluated args
and should produce results that match the host-Elm implementations.
-}

import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import Types exposing (Error(..), Value(..))


suite : Test
suite =
    describe "Eval.ResolvedExpression higher-order kernel dispatchers"
        [ describe "List.foldl"
            [ foldlSum
            , foldlReverse
            , foldlEmpty
            , foldlWithUserDefinedCallback
            , foldlUserLetBoundCallback
            ]
        ]


{-| Simplest case: sum a list with a core operator as the callback.
-}
foldlSum : Test
foldlSum =
    test "List.foldl (+) 0 [1..5] == 15" <|
        \_ ->
            case Eval.Module.buildProjectEnv [] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "List.foldl (+) 0 [1, 2, 3, 4, 5]"
                        |> expectValue (Int 15)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| Accumulator that rebuilds the list in reverse. Exercises the
callback receiving a List accumulator.
-}
foldlReverse : Test
foldlReverse =
    test "List.foldl (::) [] [1,2,3] == [3,2,1]" <|
        \_ ->
            case Eval.Module.buildProjectEnv [] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "List.foldl (::) [] [1, 2, 3]"
                        |> expectValue (List [ Int 3, Int 2, Int 1 ])

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| Empty list returns the initial accumulator without invoking the
callback at all — verifies the base case.
-}
foldlEmpty : Test
foldlEmpty =
    test "List.foldl (+) 42 [] == 42" <|
        \_ ->
            case Eval.Module.buildProjectEnv [] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "List.foldl (+) 42 []"
                        |> expectValue (Int 42)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| User-defined callback defined at the top level — this is what
exercises the resolved-closure code path the old Kernel.function
marshaling was mishandling.
-}
foldlWithUserDefinedCallback : Test
foldlWithUserDefinedCallback =
    test "user-defined 2-arg callback invoked via applyClosure" <|
        \_ ->
            let
                source : String
                source =
                    """module Foo exposing (..)

addThenDouble : Int -> Int -> Int
addThenDouble x acc =
    (x + acc) * 2

result : Int
result =
    List.foldl addThenDouble 0 [ 1, 2, 3 ]
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "Foo.result"
                        -- ((0 + 1) * 2) = 2; ((2 + 2) * 2) = 8; ((8 + 3) * 2) = 22
                        |> expectValue (Int 22)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| Let-bound callback — this is the exact shape the elm-spa-parity
failure had: a 2-arg function defined in a let, passed to List.foldl.
The let-bound function becomes an `RExprImpl` closure with
`patterns = []` and `arity = 2`, which was the combination mishandled
by `Kernel.function`'s marshaling layer.
-}
foldlUserLetBoundCallback : Test
foldlUserLetBoundCallback =
    test "let-bound 2-arg closure passed to List.foldl" <|
        \_ ->
            let
                source : String
                source =
                    """module Foo exposing (..)

result : Int
result =
    let
        step : Int -> Int -> Int
        step element accumulator =
            element + accumulator + 1
    in
    List.foldl step 0 [ 10, 20, 30 ]
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "Foo.result"
                        -- 0 → (10+0+1) = 11 → (20+11+1) = 32 → (30+32+1) = 63
                        |> expectValue (Int 63)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"



-- HELPERS


expectValue : Value -> Result Error Value -> Expect.Expectation
expectValue expected actual =
    case actual of
        Ok v ->
            v |> Expect.equal expected

        Err err ->
            Expect.fail ("expected value, got Err " ++ Debug.toString err)
