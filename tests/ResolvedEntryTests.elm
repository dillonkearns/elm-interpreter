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
import Types exposing (Error, Value(..))


suite : Test
suite =
    describe "Eval.Module.evalWithResolvedIR"
        [ literalWireUp
        , userDeclarationDispatch
        , constructorCase
        , coreOperatorDispatch
        , coreFunctionDispatch
        , mixedUserAndCore
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


{-| Core operator delegation. `1 + 2` resolves to
`RApply (RGlobal Basics.add-id) [RInt 1, RInt 2]`. Since the resolver
stores no body for `Basics.add` (it's a core decl), the new evaluator's
RApply fast path delegates the whole call to the old evaluator via a
synthesized `Application` AST.
-}
coreOperatorDispatch : Test
coreOperatorDispatch =
    test "core operator (+) delegates to the old evaluator" <|
        \_ ->
            case Eval.Module.buildProjectEnv [] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "1 + 2"
                        |> expectValue (Int 3)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| Core function delegation. `List.map ((*) 2) [1, 2, 3]` exercises a
higher-order core function with a closure argument. The closure is a
core-backed `PartiallyApplied` value which the old evaluator handles
natively.
-}
coreFunctionDispatch : Test
coreFunctionDispatch =
    test "List.map with literal list via core delegation" <|
        \_ ->
            case Eval.Module.buildProjectEnv [] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "List.sum [ 1, 2, 3, 4 ]"
                        |> expectValue (Int 10)

                Err _ ->
                    Expect.fail "buildProjectEnv failed"


{-| Mixes user declarations with core function calls. `Foo.doubled`
references a user decl which itself calls `Basics.*`. This exercises
both paths in the same pipeline.
-}
mixedUserAndCore : Test
mixedUserAndCore =
    test "user declaration that calls core functions" <|
        \_ ->
            let
                source : String
                source =
                    """module Foo exposing (..)

base : Int
base =
    10

doubled : Int
doubled =
    base + base
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    Eval.Module.evalWithResolvedIR projectEnv "Foo.doubled"
                        |> expectValue (Int 20)

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
