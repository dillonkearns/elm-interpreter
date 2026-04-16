module ParserResolverBugTests exposing (suite)

{-| Pinpointing the `NameError "identity"` that surfaces when calling
anything in `Parser.Advanced` via `evalWithResolvedIR`.

Under the default project env (site 2a off, so base package bodies
are not resolved), calls to `Parser.int` go:

    evalR (StackTest.result)
      → RApply (RGlobal Parser.run) [Parser.int, "12345"]
      → dispatchGlobalApply → resolved-body lookup misses
      → delegateCoreApply → delegateByName (old-eval path)
      → old-eval dispatches `Parser.run` which calls into
        `Parser.Advanced` internals that reference `identity`
      → old-eval looks up `identity` in `env.imports` for the
        CURRENT module at dispatch time
      → current module is `Parser.Advanced`, which never
        explicitly imports `Basics` but gets `identity` via
        the implicit `import Basics exposing (..)` default
      → but `env.shared.moduleImports[Parser.Advanced]` isn't
        populated, so `delegateByName` falls back to the entry
        module's imports, which don't expose `identity`

So the failure is in `delegateByName`'s env construction — it
doesn't know about `Parser.Advanced`'s default-imported `Basics`
exposure.

This test reproduces the issue in isolation so we can iterate
on the fix without re-running the full review-runner bundle.

-}

import Elm.Parser
import Elm.Syntax.Expression exposing (Expression(..))
import Eval.Module
import Eval.ResolvedIR as IR
import Expect
import FastDict
import Test exposing (Test, describe, test)
import Types exposing (EvalResult(..), Value(..))


suite : Test
suite =
    describe "Parser.Advanced resolver bug"
        [ identityDirectCall
        , identityFromBasicsModule
        , parserIntViaResolvedIR
        , parserOperatorWrapperResolvesToAdvancedImplementation
        ]


identityDirectCall : Test
identityDirectCall =
    test "calling Basics.identity directly works" <|
        \_ ->
            let
                source : String
                source =
                    """module Foo exposing (..)


result : Int
result =
    identity 42
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    case Eval.Module.evalWithResolvedIR projectEnv "Foo.result" of
                        Ok v ->
                            v |> Expect.equal (Int 42)

                        Err err ->
                            Expect.fail ("expected Ok, got: " ++ Debug.toString err)

                Err err ->
                    Expect.fail ("buildProjectEnv failed: " ++ Debug.toString err)


identityFromBasicsModule : Test
identityFromBasicsModule =
    test "calling Basics.identity via qualified name" <|
        \_ ->
            let
                source : String
                source =
                    """module Foo exposing (..)


result : Int
result =
    Basics.identity 42
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    case Eval.Module.evalWithResolvedIR projectEnv "Foo.result" of
                        Ok v ->
                            v |> Expect.equal (Int 42)

                        Err err ->
                            Expect.fail ("expected Ok, got: " ++ Debug.toString err)

                Err err ->
                    Expect.fail ("buildProjectEnv failed: " ++ Debug.toString err)


parserIntViaResolvedIR : Test
parserIntViaResolvedIR =
    test "Parser.run Parser.int \"12345\" via evalWithResolvedIR" <|
        \_ ->
            let
                source : String
                source =
                    """module Foo exposing (..)

import Parser


result : Result (List Parser.DeadEnd) Int
result =
    Parser.run Parser.int "12345"
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    case Eval.Module.evalWithResolvedIR projectEnv "Foo.result" of
                        Ok _ ->
                            Expect.pass

                        Err err ->
                            Expect.fail ("expected Ok, got: " ++ Debug.toString err)

                Err err ->
                    Expect.fail ("buildProjectEnv failed: " ++ Debug.toString err)


parserOperatorWrapperResolvesToAdvancedImplementation : Test
parserOperatorWrapperResolvesToAdvancedImplementation =
    test "user operator application resolves to Parser.Advanced.ignorer in resolved IR" <|
        \_ ->
            let
                source : String
                source =
                    """module Foo exposing (result)

import Parser
import Parser.Advanced as Advanced exposing ((|.), succeed)


result : Result (List (Advanced.DeadEnd () Parser.Problem)) ()
result =
    Advanced.run
        (succeed ()
            |. Advanced.token (Advanced.Token "a" (Parser.Expecting "a"))
        )
        "a"
"""
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Ok projectEnv ->
                    let
                        resolved =
                            Eval.Module.projectEnvResolved projectEnv
                    in
                    case
                        ( FastDict.get ( [ "Foo" ], "result" ) resolved.globalIds
                        , FastDict.get ( [ "Parser", "Advanced" ], "ignorer" ) resolved.globalIds
                        )
                    of
                        ( Just resultId, Just advancedIgnorerId ) ->
                            case FastDict.get resultId resolved.bodies of
                                Just body ->
                                    case body of
                                        IR.RApply (IR.RGlobal runId) [ IR.RApply (IR.RGlobal targetId) _, IR.RString "a" ] ->
                                            if targetId == advancedIgnorerId then
                                                Expect.pass

                                            else
                                                Expect.fail
                                                    ("expected operator application to target Parser.Advanced.ignorer, got id "
                                                        ++ String.fromInt targetId
                                                        ++ " instead of "
                                                        ++ String.fromInt advancedIgnorerId
                                                        ++ " (run id "
                                                        ++ String.fromInt runId
                                                        ++ ")"
                                                    )

                                        other ->
                                            Expect.fail ("unexpected resolved body for Foo.result: " ++ Debug.toString other)

                                Nothing ->
                                    Expect.fail "missing resolved body for Foo.result"

                        ( Nothing, _ ) ->
                            Expect.fail "missing global id for Foo.result"

                        ( _, Nothing ) ->
                            Expect.fail "missing global id for Parser.Advanced.ignorer"

                Err err ->
                    Expect.fail ("buildProjectEnv failed: " ++ Debug.toString err)
