module BridgeParityTests exposing (suite)

{-| Phase 0 A/B harness for the OLD → resolved-IR evaluator migration.

Each test runs the same top-level binding through two evaluator entry
points and asserts the resulting `Value` is equal:

  - `Eval.Module.evalWithEnv` — OLD eval (string-keyed
    `Eval.Expression`).
  - `Eval.Module.evalWithResolvedIRFromFilesAndIntercepts` — the bridge
    path (resolved-IR `Eval.ResolvedExpression`, with `installedBridge`
    letting kernel callbacks re-enter OLD eval for unresolved bodies).

When later phases migrate individual API families onto the resolved-IR
path, this suite is the contract they can't silently break.

The cases are drawn from shapes that motivated the migration: higher-
order dispatch (`List.foldl` / `Dict.foldl`), let-bound closures passed
to kernel callbacks (the `RExprImpl (patterns = [], arity > 0)` shape
that originally required the bridge), and a handful of precomputed-
cache-interaction shapes that caught silent bugs in past phases.

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Eval.Module
import Expect
import FastDict
import Test exposing (Test, describe, test)
import Types exposing (Error(..), EvalErrorKind(..), EvalResult(..), Value(..))


suite : Test
suite =
    describe "BridgeParityTests — OLD vs resolved-IR bridge agreement"
        [ describe "higher-order dispatch"
            [ parity "List.foldl (+) over literal list"
                "result = List.foldl (+) 0 [1, 2, 3, 4, 5]"
            , parity "List.foldl (::) accumulator"
                "result = List.foldl (::) [] [1, 2, 3]"
            , parity "List.foldl with user-defined top-level callback"
                """addThenDouble x acc =
    (x + acc) * 2

result =
    List.foldl addThenDouble 0 [ 1, 2, 3 ]
"""
            , parity "List.foldl with let-bound 2-arg closure"
                """result =
    let
        step element accumulator =
            element + accumulator + 1
    in
    List.foldl step 0 [ 10, 20, 30 ]
"""
            , parity "Dict.foldl counts keys with inline lambda"
                """import Dict

result =
    Dict.foldl (\\_ _ acc -> acc + 1) 0 (Dict.fromList [ ( "a", 1 ), ( "b", 2 ), ( "c", 3 ) ])
"""
            , parity "Dict.foldl with let-bound 3-arg closure"
                """import Dict

result =
    let
        step _ value acc =
            acc + value
    in
    Dict.foldl step 0 (Dict.fromList [ ( "a", 10 ), ( "b", 20 ), ( "c", 30 ) ])
"""
            ]
        , describe "arithmetic and data construction"
            [ parity "simple arithmetic" "result = 1 + 2 * 3"
            , parity "String.join over list"
                """result = String.join ", " [ "a", "b", "c" ]"""
            , parity "Maybe.andThen chain"
                "result = Maybe.andThen (\\x -> Just (x + 1)) (Just 5)"
            ]
        , describe "precomputed cache interaction"
            [ parity "aliased lookup into precomputed top-level list"
                """lookup = List.range 1 10

result = List.head (List.drop 3 lookup)
"""
            , parity "list fusion / sum"
                """doubled = List.map (\\x -> x * 2) [ 1, 2, 3, 4, 5 ]

result = List.sum doubled
"""
            ]
        ]



-- HELPERS


{-| Build a one-module project whose top-level `result` binding is
defined by `body`, then run both evaluators on `FunctionOrValue
[ "ParityModule" ] "result"` and compare.

`body` is dropped verbatim under a synthetic `module ParityModule
exposing (..)` header, so callers can write just the declaration they
care about (including imports). No parser round-trip is needed.

-}
parity : String -> String -> Test
parity label body =
    test label <|
        \_ ->
            let
                source : String
                source =
                    "module ParityModule exposing (..)\n\n" ++ body

                expression : Expression
                expression =
                    Expression.FunctionOrValue [ "ParityModule" ] "result"
            in
            case Eval.Module.buildProjectEnv [ source ] of
                Err err ->
                    Expect.fail ("buildProjectEnv failed: " ++ Debug.toString err)

                Ok projectEnv ->
                    let
                        oldResult : Result Error Value
                        oldResult =
                            Eval.Module.evalWithEnv projectEnv [] expression

                        bridgeResult : Result Error Value
                        bridgeResult =
                            case
                                Eval.Module.evalWithResolvedIRFromFilesAndIntercepts
                                    projectEnv
                                    []
                                    FastDict.empty
                                    FastDict.empty
                                    expression
                            of
                                EvOk v ->
                                    Ok v

                                EvErr errData ->
                                    Err (EvalError errData)

                                other ->
                                    Err
                                        (EvalError
                                            { currentModule = []
                                            , callStack = []
                                            , error =
                                                Unsupported
                                                    ("BridgeParityTests: unexpected EvalResult from bridge: "
                                                        ++ Debug.toString other
                                                    )
                                            }
                                        )
                    in
                    case ( oldResult, bridgeResult ) of
                        ( Ok oldValue, Ok bridgeValue ) ->
                            if oldValue == bridgeValue then
                                Expect.pass

                            else
                                Expect.fail
                                    ("OLD vs bridge disagree:\n  OLD:    "
                                        ++ Debug.toString oldValue
                                        ++ "\n  bridge: "
                                        ++ Debug.toString bridgeValue
                                    )

                        ( Err oldErr, Err bridgeErr ) ->
                            Expect.fail
                                ("both evaluators errored — parity undecidable:\n  OLD:    "
                                    ++ Debug.toString oldErr
                                    ++ "\n  bridge: "
                                    ++ Debug.toString bridgeErr
                                )

                        ( Ok oldValue, Err bridgeErr ) ->
                            Expect.fail
                                ("OLD succeeded, bridge errored:\n  OLD:    "
                                    ++ Debug.toString oldValue
                                    ++ "\n  bridge: "
                                    ++ Debug.toString bridgeErr
                                )

                        ( Err oldErr, Ok bridgeValue ) ->
                            Expect.fail
                                ("OLD errored, bridge succeeded:\n  OLD:    "
                                    ++ Debug.toString oldErr
                                    ++ "\n  bridge: "
                                    ++ Debug.toString bridgeValue
                                )
