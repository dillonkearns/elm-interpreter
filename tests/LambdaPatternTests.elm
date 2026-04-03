module LambdaPatternTests exposing (suite)

{-| Tests for "Could not match lambda patterns" in multi-module evaluation.

The bug manifests when Rule.review [anyRule] project is evaluated through
the interpreter. Rule.review [] project succeeds, proving the outer function
boundary works. The failure happens inside rule processing when:
- Functions stored in record fields inside Custom variants are extracted and called
- Lambdas with complex patterns (record destructuring, as-patterns, tuple+constructor)
  are applied

Reference: .scratch/bug-rule-review-lambda-pattern-mismatch.md
-}

import Elm.Syntax.Expression as Expression
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import Types exposing (Error(..), Value(..))


suite : Test
suite =
    describe "Lambda pattern matching in multi-module evaluation"
        [ functionInRecordFieldTests
        , recordDestructuringTests
        , tupleConstructorPatternTests
        , storedLambdaExtractionTests
        , complexExtractionTests
        ]



-- Functions stored in record fields inside Custom variants


functionInRecordFieldTests : Test
functionInRecordFieldTests =
    describe "Functions stored in record fields inside Custom variants"
        [ evalProjectTest "extract function from record inside Custom, apply it"
            [ """module Types exposing (Rule(..))

type Rule =
    Rule { name : String, visitor : String -> List String }
"""
            , """module Runner exposing (run)

import Types exposing (Rule(..))

run rules =
    case rules of
        [] ->
            []

        (Rule rule) :: rest ->
            rule.visitor "test" ++ run rest
"""
            , """module Main exposing (main)

import Types exposing (Rule(..))
import Runner

myRule =
    Rule { name = "MyRule", visitor = \\input -> [ "found: " ++ input ] }

main = Runner.run [ myRule ]
"""
            ]
            (list String)
            [ "found: test" ]
        , evalProjectTest "function in Result Ok inside record inside Custom"
            [ """module Types exposing (Rule(..))

type Rule =
    Rule
        { name : String
        , visitor : Result String (String -> List String)
        }
"""
            , """module Runner exposing (run)

import Types exposing (Rule(..))

run rules input =
    case rules of
        [] ->
            []

        (Rule rule) :: rest ->
            case rule.visitor of
                Ok visit ->
                    visit input ++ run rest input

                Err _ ->
                    run rest input
"""
            , """module Main exposing (main)

import Types exposing (Rule(..))
import Runner

myRule =
    Rule
        { name = "MyRule"
        , visitor = Ok (\\s -> [ "visited: " ++ s ])
        }

main = Runner.run [ myRule ] "hello"
"""
            ]
            (list String)
            [ "visited: hello" ]
        ]



-- Record destructuring with as-patterns in function arguments


recordDestructuringTests : Test
recordDestructuringTests =
    describe "Record destructuring with as-patterns in function args"
        [ evalProjectTest "record pattern in function arg"
            [ """module Main exposing (main)

getName { name } = name

main = getName { name = "hello", age = 42 }
"""
            ]
            String
            "hello"
        , evalProjectTest "record-as pattern in function arg"
            [ """module Main exposing (main)

process ({ name } as record) =
    name ++ " (" ++ String.fromInt record.age ++ ")"

main = process { name = "Alice", age = 30 }
"""
            ]
            String
            "Alice (30)"
        , evalProjectTest "record-as pattern, cross-module"
            [ """module Types exposing (Config)

type alias Config = { cache : List String, data : String }
"""
            , """module Processor exposing (process)

import Types exposing (Config)

process ({ cache } as config) =
    cache ++ [ config.data ]
"""
            , """module Main exposing (main)

import Processor

main = Processor.process { cache = [ "old" ], data = "new" }
"""
            ]
            (list String)
            [ "old", "new" ]
        , evalProjectTest "record-as pattern inside let binding (like raise function)"
            [ """module Main exposing (main)

type alias State = { cache : List String, ruleData : String }

process state =
    let
        raise ({ cache } as hidden) =
            if List.length cache >= 3 then
                hidden.cache
            else
                raise { hidden | cache = cache ++ [ hidden.ruleData ] }
    in
    raise state

main = process { cache = [], ruleData = "x" }
"""
            ]
            (list String)
            [ "x", "x", "x" ]
        , evalProjectTest "record-as pattern in cross-module recursive let binding"
            [ """module Types exposing (State)

type alias State = { cache : List String, ruleData : String }
"""
            , """module Visitor exposing (visit)

import Types exposing (State)

visit : State -> List String
visit state =
    let
        raise : State -> List String
        raise ({ cache } as hidden) =
            if List.length cache >= 3 then
                hidden.cache
            else
                raise { hidden | cache = cache ++ [ hidden.ruleData ] }
    in
    raise state
"""
            , """module Main exposing (main)

import Visitor

main = Visitor.visit { cache = [], ruleData = "y" }
"""
            ]
            (list String)
            [ "y", "y", "y" ]
        ]



-- Tuple patterns with constructor destructuring in function args


tupleConstructorPatternTests : Test
tupleConstructorPatternTests =
    describe "Tuple patterns with constructor destructuring"
        [ evalProjectTest "tuple with constructor pattern in function arg"
            [ """module Types exposing (Schema(..))

type Schema = Schema { name : String }
"""
            , """module Runner exposing (run)

import Types exposing (Schema(..))

run ( Schema schema, context ) =
    schema.name ++ ": " ++ context
"""
            , """module Main exposing (main)

import Types exposing (Schema(..))
import Runner

main = Runner.run ( Schema { name = "MySchema" }, "hello" )
"""
            ]
            String
            "MySchema: hello"
        , evalProjectTest "function with tuple+constructor pattern stored in record and called later"
            [ """module Types exposing (Schema(..), Handler)

type Schema = Schema { name : String }

type alias Handler =
    { process : ( Schema, String ) -> String }
"""
            , """module Processor exposing (makeHandler)

import Types exposing (Schema(..), Handler)

makeHandler : Handler
makeHandler =
    { process = \\( Schema schema, input ) -> schema.name ++ ": " ++ input }
"""
            , """module Main exposing (main)

import Types exposing (Schema(..))
import Processor

main =
    let
        handler = Processor.makeHandler
    in
    handler.process ( Schema { name = "Test" }, "world" )
"""
            ]
            String
            "Test: world"
        ]



-- Functions extracted from nested structures and applied


storedLambdaExtractionTests : Test
storedLambdaExtractionTests =
    describe "Lambdas stored in nested structures, extracted and applied"
        [ evalProjectTest "lambda in Ok in record in Custom, extracted through pattern matching"
            [ """module Types exposing (Rule(..), ChangeableData)

type alias ChangeableData = { ruleId : Int, name : String }

type Rule =
    Rule
        { name : String
        , process : Result String (ChangeableData -> List String)
        }
"""
            , """module RuleFactory exposing (makeRule)

import Types exposing (Rule(..), ChangeableData)

makeRule : String -> Rule
makeRule name =
    Rule
        { name = name
        , process =
            Ok
                (\\data ->
                    [ data.name ++ " (id=" ++ String.fromInt data.ruleId ++ ")" ]
                )
        }
"""
            , """module Runner exposing (runAll)

import Types exposing (Rule(..), ChangeableData)

runAll : List Rule -> List String
runAll rules =
    case rules of
        [] ->
            []

        (Rule rule) :: rest ->
            case rule.process of
                Ok processor ->
                    processor { ruleId = 1, name = rule.name }
                        ++ runAll rest

                Err _ ->
                    runAll rest
"""
            , """module Main exposing (main)

import RuleFactory
import Runner

main =
    Runner.runAll
        [ RuleFactory.makeRule "RuleA"
        , RuleFactory.makeRule "RuleB"
        ]
"""
            ]
            (list String)
            [ "RuleA (id=1)", "RuleB (id=1)" ]
        , evalProjectTest "function returning function, stored in Custom, applied in stages"
            [ """module Types exposing (Rule(..))

type Rule =
    Rule
        { name : String
        , makeVisitor : String -> String -> List String
        }
"""
            , """module Factory exposing (create)

import Types exposing (Rule(..))

create name =
    Rule
        { name = name
        , makeVisitor = \\project module_ -> [ name ++ ": " ++ project ++ "/" ++ module_ ]
        }
"""
            , """module Runner exposing (run)

import Types exposing (Rule(..))

run rules project =
    case rules of
        [] -> []
        (Rule rule) :: rest ->
            rule.makeVisitor project "Module1" ++ run rest project
"""
            , """module Main exposing (main)

import Factory
import Runner

main = Runner.run [ Factory.create "Check" ] "MyProject"
"""
            ]
            (list String)
            [ "Check: MyProject/Module1" ]
        ]



-- Complex extraction: function in list in record in Custom, applied, result destructured


complexExtractionTests : Test
complexExtractionTests =
    describe "Complex nested extraction and application"
        [ evalProjectTest "function in list inside schema, applied and result destructured"
            [ """module Types exposing (Schema(..), Visitor(..))

type Schema context =
    Schema
        { name : String
        , visitors : List (() -> ( Visitor context, context ))
        }

type Visitor context = Visitor { visit : String -> context -> context }
"""
            , """module Factory exposing (create)

import Types exposing (Schema(..), Visitor(..))

create : String -> Schema (List String)
create name =
    Schema
        { name = name
        , visitors =
            [ \\() ->
                ( Visitor { visit = \\input ctx -> input :: ctx }
                , []
                )
            ]
        }
"""
            , """module Runner exposing (run)

import Types exposing (Schema(..), Visitor(..))

run : Schema context -> String -> context
run (Schema schema) input =
    case schema.visitors of
        [] ->
            -- Should not happen in test
            run (Schema schema) input

        makeVisitor :: _ ->
            let
                ( Visitor visitor, initialCtx ) = makeVisitor ()
            in
            visitor.visit input initialCtx
"""
            , """module Main exposing (main)

import Factory
import Runner

main = Runner.run (Factory.create "TestRule") "hello"
"""
            ]
            (list String)
            [ "hello" ]
        , evalProjectTest "multi-arg function with complex last param (tuple+constructor)"
            [ """module Types exposing (Schema(..), ModuleSchema(..))

type Schema = Schema { name : String }
type ModuleSchema = ModuleSchema { visitor : String -> List String }
"""
            , """module Processor exposing (process)

import Types exposing (Schema(..), ModuleSchema(..))

process : Schema -> String -> ( ModuleSchema, String ) -> List String
process (Schema schema) project ( ModuleSchema modSchema, moduleName ) =
    modSchema.visitor (schema.name ++ "/" ++ project ++ "/" ++ moduleName)
"""
            , """module Main exposing (main)

import Types exposing (Schema(..), ModuleSchema(..))
import Processor

main =
    Processor.process
        (Schema { name = "Rule1" })
        "MyProject"
        ( ModuleSchema { visitor = \\path -> [ "visited: " ++ path ] }
        , "Page.elm"
        )
"""
            ]
            (list String)
            [ "visited: Rule1/MyProject/Page.elm" ]
        ]



-- Helpers


evalProjectTest : String -> List String -> (a -> Value) -> a -> Test
evalProjectTest name sources toValue a =
    test name <|
        \_ ->
            case Eval.Module.evalProject sources (Expression.FunctionOrValue [] "main") of
                Err (EvalError e) ->
                    Expect.fail <|
                        Debug.toString e.error
                            ++ " [module: "
                            ++ String.join "." e.currentModule
                            ++ "]"

                Err e ->
                    Expect.fail (Debug.toString e)

                Ok value ->
                    Expect.equal (toValue a) value


list : (a -> Value) -> List a -> Value
list f xs =
    List (List.map f xs)
