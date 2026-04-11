module StackSafetyTests exposing (suite)

{-| Tests for `Eval.ResolvedExpression.evalR` stack safety.

The resolved-IR evaluator is currently direct-recursive, which means
deep Elm recursion accumulates JS stack frames. Under site 2a of the
`initContextWithImports` flip, recursive parser combinators from
elm/parser (used by `Elm.Type.decoder` via `Parser.run`) blow the
stack at real workload depths.

These tests pin stack safety at the evaluator level with synthetic
workloads that don't require elm/parser or the full review-runner
pipeline. Start shallow (depth 500, expected to pass) and grow until
the evaluator trampolines properly (depth 5000+, currently fails).

-}

import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import Types exposing (Error(..), Value(..))


suite : Test
suite =
    describe "Eval.ResolvedExpression.evalR stack safety"
        [ nonTailRecursiveDepth500
        , nonTailRecursiveDepth5000
        , nonTailRecursiveDepth50000
        , nonTailRecursiveDepth500000
        , mutuallyRecursiveDepth10000
        , caseDispatchDepth10000
        , letBindingDepth10000
        ]


{-| Shallow non-tail-recursive baseline: depth 500 should pass
regardless of trampolining since it fits in the default JS stack.
Serves as a smoke test to confirm the reproducer itself works.
-}
nonTailRecursiveDepth500 : Test
nonTailRecursiveDepth500 =
    test "non-tail-recursive depth 500 (baseline)" <|
        \_ ->
            let
                source : String
                source =
                    """module StackTest exposing (..)

depth : Int -> Int
depth n =
    if n <= 0 then
        0

    else
        1 + depth (n - 1)


result : Int
result =
    depth 500
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "StackTest.result"
                        |> expectValue (Int 500)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| Deep non-tail-recursive: should still produce the right answer
if the evaluator trampolines cross-body calls properly. Currently
expected to stack-overflow on `evalR`'s direct recursion through
`applyClosure → runRExprClosure → evalR`.

Depth 5000 is chosen to blow past JS's default stack (~10k frames
with a few frames per evalR hop) while still being cheap enough
that a trampolined evaluator finishes quickly.
-}
nonTailRecursiveDepth5000 : Test
nonTailRecursiveDepth5000 =
    test "non-tail-recursive depth 5000 (needs stack safety)" <|
        \_ ->
            let
                source : String
                source =
                    """module StackTest exposing (..)

depth : Int -> Int
depth n =
    if n <= 0 then
        0

    else
        1 + depth (n - 1)


result : Int
result =
    depth 5000
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "StackTest.result"
                        |> expectValue (Int 5000)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"



nonTailRecursiveDepth50000 : Test
nonTailRecursiveDepth50000 =
    test "non-tail-recursive depth 50000 (stress test)" <|
        \_ ->
            let
                source : String
                source =
                    """module StackTest exposing (..)

depth : Int -> Int
depth n =
    if n <= 0 then
        0

    else
        1 + depth (n - 1)


result : Int
result =
    depth 50000
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "StackTest.result"
                        |> expectValue (Int 50000)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| 500k depth — well beyond what any real Elm program needs. If this
completes without running out of memory, the trampoline is genuinely
linear in both time and space. If it OOMs, there's a leak in the
continuation-stack capture somewhere.
-}
nonTailRecursiveDepth500000 : Test
nonTailRecursiveDepth500000 =
    test "non-tail-recursive depth 500000 (memory scaling)" <|
        \_ ->
            let
                source : String
                source =
                    """module StackTest exposing (..)

depth : Int -> Int
depth n =
    if n <= 0 then
        0

    else
        1 + depth (n - 1)


result : Int
result =
    depth 500000
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "StackTest.result"
                        |> expectValue (Int 500000)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| Mutual recursion between two functions — exercises the cross-body
trampoline across two different resolved bodies, not just one
self-recursive function.
-}
mutuallyRecursiveDepth10000 : Test
mutuallyRecursiveDepth10000 =
    test "mutually recursive even/odd depth 10000" <|
        \_ ->
            let
                source : String
                source =
                    """module StackTest exposing (..)

isEven : Int -> Bool
isEven n =
    if n == 0 then
        True

    else
        isOdd (n - 1)


isOdd : Int -> Bool
isOdd n =
    if n == 0 then
        False

    else
        isEven (n - 1)


result : Bool
result =
    isEven 10000
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "StackTest.result"
                        |> expectValue (Bool True)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"



{-| Deep recursion via a `case` expression as the dispatch shape —
parser combinators use this pattern heavily. Each level pattern-matches
on a wrapped value before recursing.
-}
caseDispatchDepth10000 : Test
caseDispatchDepth10000 =
    test "case-dispatch recursion depth 10000" <|
        \_ ->
            let
                source : String
                source =
                    """module StackTest exposing (..)

type Wrap = Wrap Int


unwrap : Wrap -> Int
unwrap w =
    case w of
        Wrap n ->
            n


countDown : Wrap -> Int
countDown w =
    case w of
        Wrap 0 ->
            0

        Wrap n ->
            1 + countDown (Wrap (n - 1))


result : Int
result =
    countDown (Wrap 10000)
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "StackTest.result"
                        |> expectValue (Int 10000)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| Deep recursion with a `let` binding between each recursive call —
another common shape in real Elm code. Each level builds a small
intermediate value in a let before recursing.
-}
letBindingDepth10000 : Test
letBindingDepth10000 =
    test "let-binding recursion depth 10000" <|
        \_ ->
            let
                source : String
                source =
                    """module StackTest exposing (..)

go : Int -> Int
go n =
    if n <= 0 then
        0

    else
        let
            next : Int
            next =
                n - 1

            prior : Int
            prior =
                go next
        in
        prior + 1


result : Int
result =
    go 10000
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "StackTest.result"
                        |> expectValue (Int 10000)

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
