module ResolvedEntryTests exposing (suite)

{-| Smoke tests for `Eval.Module.evalWithResolvedIR` — the Phase 3
iteration 3b3 wire-up entry point that routes an expression through the
new resolver + `Eval.ResolvedExpression.evalR` path.

These tests use a small project fixture (a single user module) and
exercise expressions that reference user declarations via `RGlobal`. Any
expression that touches a core declaration (operators, list functions,
etc.) is expected to return `Unsupported` — core dispatch is deferred to
a follow-up iteration and is the clear next step.

Iteration 3b3's goal: prove end-to-end wire-up works. An expression
source string round-trips from parsing → resolving → evaluating → a
correct `Value`, without touching any of the old string-keyed
evaluator code.

-}

import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import Types exposing (Error(..), EvalErrorKind(..), Value(..))


suite : Test
suite =
    describe "Eval.Module.evalWithResolvedIR (Phase 3 iter 3b3 wire-up)"
        [ literalWireUp
        , userDeclarationDispatch
        , constructorCase
        , coreDispatchStillUnsupported
        ]


{-| The smallest end-to-end test: a literal expression has no global
references at all, so the new evaluator can handle it without even
touching `resolvedBodies`. This primarily verifies that parsing,
resolving, evaluating, and Result-wrapping all line up.
-}
literalWireUp : Test
literalWireUp =
    test "literal integer evaluates through the new path" <|
        \_ ->
            case Eval.Module.buildProjectEnv [] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "42"
                        |> expectValue (Int 42)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| A reference to a user declaration hits `resolvedBodies` and the new
evaluator's `evalGlobal` path. The declaration's body contains only
literals, so there's no cascade into core dispatch.
-}
userDeclarationDispatch : Test
userDeclarationDispatch =
    test "expression referencing a user declaration resolves via resolvedBodies" <|
        \_ ->
            let
                source : String
                source =
                    """module Foo exposing (..)

answer : Int
answer =
    42

nested : Int
nested =
    answer
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "Foo.nested"
                        |> expectValue (Int 42)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| An expression that exercises closures, constructors, and case
expressions end-to-end — all of the 3b2 machinery going through the
3b3 wire-up entry point. Still no core references.
-}
constructorCase : Test
constructorCase =
    test "case on a constructor through the wire-up entry point" <|
        \_ ->
            let
                source : String
                source =
                    """module Foo exposing (..)

extract : Int
extract =
    case wrapped of
        Just v ->
            v

        Nothing ->
            0

wrapped : Maybe Int
wrapped =
    Just 17
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "Foo.extract"
                        |> expectValue (Int 17)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| An expression that tries to use a core function (`+`) still returns
`Unsupported` — core dispatch is the next iteration. This test documents
the current boundary and will need to be updated (or deleted) once
core-dispatch wire-up lands.
-}
coreDispatchStillUnsupported : Test
coreDispatchStillUnsupported =
    test "expression using a core operator returns Unsupported" <|
        \_ ->
            case Eval.Module.buildProjectEnv [] of
                Ok projectEnv ->
                    case Eval.Module.evalWithResolvedIR projectEnv "1 + 2" of
                        Err (EvalError { error }) ->
                            case error of
                                Unsupported _ ->
                                    Expect.pass

                                other ->
                                    Expect.fail
                                        ("expected Unsupported, got " ++ Debug.toString other)

                        Err other ->
                            Expect.fail ("expected EvalError, got " ++ Debug.toString other)

                        Ok v ->
                            Expect.fail ("expected Err, got Ok " ++ Debug.toString v)

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
