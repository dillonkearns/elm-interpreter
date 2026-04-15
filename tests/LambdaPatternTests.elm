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
        , ruleVisitorPatternTests
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
        , evalProjectTest "multi-arg function with constructor+as-pattern first param"
            [ """module Types exposing (Node(..), Range)

type alias Range =
    { start : Int
    , end : Int
    }

type Node a
    = Node Range a
"""
            , """module Runner exposing (run)

import Types exposing (Node(..))

run ((Node range name) as whole) middle suffix =
    let
        repeated =
            case whole of
                Node _ value ->
                    value
    in
    name
        ++ ":"
        ++ String.fromInt range.start
        ++ ":"
        ++ middle
        ++ ":"
        ++ suffix
        ++ ":"
        ++ repeated
"""
            , """module Main exposing (main)

import Runner
import Types exposing (Node(..))

main =
    Runner.run
        (Node { start = 1, end = 2 } "Ctor")
        "middle"
        "suffix"
"""
            ]
            String
            "Ctor:1:middle:suffix:Ctor"
        , evalProjectTest "valueConstructorOptimisticLayout callback shape evaluates correctly outside parser control flow"
            [ """module Main exposing (main)

import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation

prependComments left right =
    left ++ right

apply3 func a b c =
    func a b c

combine ((Node nameRange _) as name) commentsAfterName argumentsReverse =
    let
        fullRange =
            case argumentsReverse.syntax of
                (Node lastArgRange _) :: _ ->
                    { start = nameRange.start, end = lastArgRange.end }

                [] ->
                    nameRange
    in
    { comments =
        prependComments commentsAfterName argumentsReverse.comments
    , syntax =
        Node fullRange
            { name = name
            , arguments = List.reverse argumentsReverse.syntax
            }
    }

main =
    let
        stringRange =
            { start = { row = 3, column = 17 }, end = { row = 3, column = 23 } }

        nameRange =
            { start = { row = 3, column = 5 }, end = { row = 3, column = 16 } }

        argNode =
            Node stringRange (TypeAnnotation.Typed (Node stringRange ( [], "String" )) [])

        result =
            apply3 combine
                (Node nameRange "Constructor")
                []
                { comments = []
                , syntax = [ argNode ]
                }
    in
    case result.syntax of
        Node _ ctor ->
            case ctor.arguments of
                [ Node _ typeAnnotation ] ->
                    case typeAnnotation of
                        TypeAnnotation.Typed (Node _ ( [], "String" )) [] ->
                            "ok"

                        _ ->
                            "wrong-type-annotation"

                _ ->
                    "wrong-arguments"
"""
            ]
            String
            "ok"
        , evalProjectTest "partial application through `|>` preserves reducer accumulator shape"
            [ """module Main exposing (main)

reduce pResult ( commentsSoFar, itemsSoFar ) =
    ( commentsSoFar ++ pResult.comments
    , pResult.syntax :: itemsSoFar
    )

main =
    let
        pResult =
            { comments = [ "c1" ]
            , syntax = "item"
            }

        result =
            ( [ "base" ], [] )
                |> reduce pResult
    in
    case result of
        ( comments, items ) ->
            String.join ":" (comments ++ items)
"""
            ]
            String
            "base:c1:item"
        ]



-- Models the exact Review.Rule pattern: two types in same module,
-- ModuleVisitor wraps record with field returning ProjectVisitor,
-- runVisitor expects ModuleVisitor via constructor pattern,
-- raise/createVisitor cycle.


ruleVisitorPatternTests : Test
ruleVisitorPatternTests =
    describe "Review.Rule visitor pattern (two types, raise cycle, runVisitor)"
        [ evalProjectTest "exact elm-review pattern: record alias ops, runVisitor, raise cycle, mutatingMap"
            -- Models Review.Rule exactly: ModuleVisitorOps is a type alias,
            -- ModuleVisitor wraps it, runVisitor takes (ops -> Maybe ...) accessor,
            -- raise creates recursive ModuleVisitor, mutatingMap chains visitors.
            [ """module Rule exposing (main)

type ProjectVisitor = ProjectVisitor { getErrors : () -> List String }

type alias ModuleVisitorOps =
    { expressionVisitor : Maybe (String -> ModuleVisitor)
    , toProjectVisitor : () -> ProjectVisitor
    }

type ModuleVisitor = ModuleVisitor ModuleVisitorOps

createModuleVisitor : (List String -> ProjectVisitor) -> List String -> ModuleVisitor
createModuleVisitor toProject errors =
    let
        raise newErrors =
            ModuleVisitor
                { expressionVisitor = Just (\\expr -> raise (expr :: newErrors))
                , toProjectVisitor = \\() -> toProject newErrors
                }
    in
    raise errors

runVisitor : (ModuleVisitorOps -> Maybe (a -> ModuleVisitor)) -> a -> ModuleVisitor -> ModuleVisitor
runVisitor field a ((ModuleVisitor ops) as original) =
    case field ops of
        Just visitor ->
            visitor a

        Nothing ->
            original

main =
    let
        toProject errors =
            ProjectVisitor { getErrors = \\() -> errors }

        visitor = createModuleVisitor toProject []

        visitors = [ visitor ]

        updatedVisitors =
            visitors
                |> List.map (\\acc -> runVisitor .expressionVisitor "hello" acc)
    in
    case updatedVisitors of
        (ModuleVisitor ops) :: _ ->
            case ops.toProjectVisitor () of
                ProjectVisitor pv ->
                    pv.getErrors ()

        [] ->
            []
"""
            ]
            (list String)
            [ "hello" ]
        , evalProjectTest "two custom types same module, raise pattern, List.map through runVisitor"
            -- Closer to actual elm-review: separate createVisitor, runVisitor, mutatingMap chain
            [ """module Rule exposing (main)

type ProjectVisitor = ProjectVisitor { getErrors : () -> List String }

type alias ModuleVisitorOps =
    { expressionVisitor : Maybe (String -> ModuleVisitor)
    , finalEval : Maybe (() -> ModuleVisitor)
    , toProjectVisitor : () -> ProjectVisitor
    }

type ModuleVisitor = ModuleVisitor ModuleVisitorOps

createVisitor : (List String -> ModuleVisitor) -> List String -> Maybe (a -> List String -> ( List String, context )) -> Maybe (a -> ModuleVisitor)
createVisitor raise errors maybeVisitorFn =
    case maybeVisitorFn of
        Nothing -> Nothing
        Just visitorFn ->
            Just (\\a -> raise (Tuple.first (visitorFn a errors)))

createModuleVisitor : Maybe (String -> List String -> ( List String, () )) -> (List String -> ProjectVisitor) -> ModuleVisitor
createModuleVisitor exprVisitorSchema toProject =
    let
        raise : List String -> ModuleVisitor
        raise errors =
            ModuleVisitor
                { expressionVisitor = createVisitor raise errors exprVisitorSchema
                , finalEval = Nothing
                , toProjectVisitor = \\() -> toProject errors
                }
    in
    raise []

runVisitor : (ModuleVisitorOps -> Maybe (a -> ModuleVisitor)) -> a -> ModuleVisitor -> ModuleVisitor
runVisitor field a ((ModuleVisitor ops) as original) =
    case field ops of
        Just visitor -> visitor a
        Nothing -> original

main =
    let
        toProject errors =
            ProjectVisitor { getErrors = \\() -> errors }

        exprVisitor = Just (\\expr errors -> ( expr :: errors, () ))

        visitors =
            [ createModuleVisitor exprVisitor toProject ]

        afterExpr =
            visitors
                |> List.map (\\acc -> runVisitor .expressionVisitor "found_expr" acc)

        afterFinal =
            afterExpr
                |> List.map (\\acc -> runVisitor .finalEval () acc)
    in
    case afterFinal of
        (ModuleVisitor ops) :: _ ->
            case ops.toProjectVisitor () of
                ProjectVisitor pv -> pv.getErrors ()

        [] -> []
"""
            ]
            (list String)
            [ "found_expr" ]
        , evalProjectTest "cross-module: types in one module, visitor logic in another"
            [ """module Rule.Types exposing (ProjectVisitor(..), ModuleVisitor(..), ModuleVisitorOps)

type ProjectVisitor = ProjectVisitor { getErrors : () -> List String }

type alias ModuleVisitorOps =
    { expressionVisitor : Maybe (String -> ModuleVisitor)
    , toProjectVisitor : () -> ProjectVisitor
    }

type ModuleVisitor = ModuleVisitor ModuleVisitorOps
"""
            , """module Rule.Visitor exposing (createModuleVisitor, runVisitor)

import Rule.Types exposing (ProjectVisitor(..), ModuleVisitor(..), ModuleVisitorOps)

createModuleVisitor : Maybe (String -> List String -> List String) -> (List String -> ProjectVisitor) -> ModuleVisitor
createModuleVisitor exprVisitorSchema toProject =
    let
        raise : List String -> ModuleVisitor
        raise errors =
            ModuleVisitor
                { expressionVisitor =
                    case exprVisitorSchema of
                        Nothing -> Nothing
                        Just visitorFn ->
                            Just (\\a -> raise (visitorFn a errors))
                , toProjectVisitor = \\() -> toProject errors
                }
    in
    raise []

runVisitor : (ModuleVisitorOps -> Maybe (a -> ModuleVisitor)) -> a -> ModuleVisitor -> ModuleVisitor
runVisitor field a ((ModuleVisitor ops) as original) =
    case field ops of
        Just visitor -> visitor a
        Nothing -> original
"""
            , """module Main exposing (main)

import Rule.Types exposing (ProjectVisitor(..), ModuleVisitor(..), ModuleVisitorOps)
import Rule.Visitor exposing (createModuleVisitor, runVisitor)

main =
    let
        toProject errors =
            ProjectVisitor { getErrors = \\() -> errors }

        visitors =
            [ createModuleVisitor (Just (\\expr errors -> expr :: errors)) toProject ]

        result =
            visitors
                |> List.map (\\acc -> runVisitor .expressionVisitor "hello" acc)
    in
    case result of
        (ModuleVisitor ops) :: _ ->
            case ops.toProjectVisitor () of
                ProjectVisitor pv -> pv.getErrors ()

        [] -> []
"""
            ]
            (list String)
            [ "hello" ]
        , evalProjectTest "newtype wrapping list, mutatingMap, large record (14 fields)"
            -- The real ModuleVisitorOps has 14 fields. Test if large records
            -- interact badly with newtype wrapping + List.map.
            [ """module Rule exposing (main)

type ProjectVisitor = ProjectVisitor { getErrors : () -> List String }

type alias ModuleVisitorOps =
    { f1 : Maybe (String -> ModuleVisitor)
    , f2 : Maybe (String -> ModuleVisitor)
    , f3 : Maybe (String -> ModuleVisitor)
    , f4 : Maybe (String -> ModuleVisitor)
    , f5 : Maybe (String -> ModuleVisitor)
    , f6 : Maybe (String -> ModuleVisitor)
    , f7 : Maybe (String -> ModuleVisitor)
    , f8 : Maybe (String -> ModuleVisitor)
    , f9 : Maybe (String -> ModuleVisitor)
    , f10 : Maybe (String -> ModuleVisitor)
    , f11 : Maybe (String -> ModuleVisitor)
    , f12 : Maybe (String -> ModuleVisitor)
    , f13 : Maybe (() -> ModuleVisitor)
    , toProjectVisitor : () -> ProjectVisitor
    }

type ModuleVisitor = ModuleVisitor ModuleVisitorOps

type MyArray a = MyArray (List a)

fromList : List a -> MyArray a
fromList = MyArray

toList : MyArray a -> List a
toList (MyArray list) = list

mutatingMap : (a -> a) -> MyArray a -> MyArray a
mutatingMap mapper (MyArray list) =
    MyArray (List.map mapper list)

createModuleVisitor : (List String -> ProjectVisitor) -> List String -> ModuleVisitor
createModuleVisitor toProject errors =
    let
        raise newErrors =
            ModuleVisitor
                { f1 = Just (\\expr -> raise (expr :: newErrors))
                , f2 = Nothing, f3 = Nothing, f4 = Nothing
                , f5 = Nothing, f6 = Nothing, f7 = Nothing
                , f8 = Nothing, f9 = Nothing, f10 = Nothing
                , f11 = Nothing, f12 = Nothing, f13 = Nothing
                , toProjectVisitor = \\() -> toProject newErrors
                }
    in
    raise errors

runVisitor : (ModuleVisitorOps -> Maybe (a -> ModuleVisitor)) -> a -> ModuleVisitor -> ModuleVisitor
runVisitor field a ((ModuleVisitor ops) as original) =
    case field ops of
        Just visitor -> visitor a
        Nothing -> original

main =
    let
        toProject errors = ProjectVisitor { getErrors = \\() -> errors }
        visitors = [ createModuleVisitor toProject [] ]

        result =
            visitors
                |> fromList
                |> mutatingMap (\\acc -> runVisitor .f1 "e1" acc)
                |> mutatingMap (\\acc -> runVisitor .f2 "e2" acc)
                |> mutatingMap (\\acc -> runVisitor .f13 () acc)
                |> toList
    in
    case result of
        (ModuleVisitor ops) :: _ ->
            case ops.toProjectVisitor () of
                ProjectVisitor pv -> pv.getErrors ()
        [] -> []
"""
            ]
            (list String)
            [ "e1" ]
        , evalProjectTest "cross-module two-phase: types as package, visitor logic as user code"
            -- Tests the buildProjectEnv + evalWithEnv path
            [ """module Rule.Types exposing (ProjectVisitor(..), ModuleVisitor(..), ModuleVisitorOps)

type ProjectVisitor = ProjectVisitor { getErrors : () -> List String }

type alias ModuleVisitorOps =
    { expressionVisitor : Maybe (String -> ModuleVisitor)
    , toProjectVisitor : () -> ProjectVisitor
    }

type ModuleVisitor = ModuleVisitor ModuleVisitorOps
"""
            , """module Rule.Visitor exposing (createModuleVisitor, runVisitor)

import Rule.Types exposing (ProjectVisitor(..), ModuleVisitor(..), ModuleVisitorOps)

createModuleVisitor : Maybe (String -> List String -> List String) -> (List String -> ProjectVisitor) -> ModuleVisitor
createModuleVisitor exprVisitorSchema toProject =
    let
        raise : List String -> ModuleVisitor
        raise errors =
            ModuleVisitor
                { expressionVisitor =
                    case exprVisitorSchema of
                        Nothing -> Nothing
                        Just visitorFn ->
                            Just (\\a -> raise (visitorFn a errors))
                , toProjectVisitor = \\() -> toProject errors
                }
    in
    raise []

runVisitor : (ModuleVisitorOps -> Maybe (a -> ModuleVisitor)) -> a -> ModuleVisitor -> ModuleVisitor
runVisitor field a ((ModuleVisitor ops) as original) =
    case field ops of
        Just visitor -> visitor a
        Nothing -> original
"""
            , """module Main exposing (main)

import Rule.Types exposing (ProjectVisitor(..), ModuleVisitor(..), ModuleVisitorOps)
import Rule.Visitor exposing (createModuleVisitor, runVisitor)

main =
    let
        toProject errors =
            ProjectVisitor { getErrors = \\() -> errors }

        visitors =
            [ createModuleVisitor (Just (\\expr errors -> expr :: errors)) toProject
            , createModuleVisitor Nothing toProject
            ]

        afterExpr =
            visitors
                |> List.map (\\acc -> runVisitor .expressionVisitor "e1" acc)
                |> List.map (\\acc -> runVisitor .expressionVisitor "e2" acc)
    in
    case afterExpr of
        (ModuleVisitor ops1) :: (ModuleVisitor ops2) :: _ ->
            let
                (ProjectVisitor pv1) = ops1.toProjectVisitor ()
                (ProjectVisitor pv2) = ops2.toProjectVisitor ()
            in
            pv1.getErrors () ++ pv2.getErrors ()

        _ -> [ "WRONG_SHAPE" ]
"""
            ]
            (list String)
            [ "e2", "e1" ]
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
