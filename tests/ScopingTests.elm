module ScopingTests exposing (suite)

import Elm.Syntax.Expression as Expression
import Eval
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import TestUtils exposing (evalTest, evalTest_, list)
import Types exposing (Error(..), EvalErrorKind(..), Value(..))


suite : Test
suite =
    describe "Variable scoping across module calls"
        [ recordAliasConstructorTests
        , moduleDispatchTests
        , letFunctionScopingTests
        , nameErrorMessageTests
        , describe "caller's local variables should not leak into callee"
            [ evalProjectTest "local var does not shadow callee's module-level function"
                [ """module Foo exposing (greet, helper)

helper name =
    "Hello " ++ name

greet name =
    helper name
"""
                , """module Main exposing (main)

import Foo

main =
    let
        helper = 42
    in
    Foo.greet "world"
"""
                ]
                String
                "Hello world"
            , evalProjectTest "local var does not shadow callee's unqualified reference"
                [ """module Mathlib exposing (double, addOne)

addOne x =
    x + 1

double x =
    addOne x + addOne x
"""
                , """module Main exposing (main)

import Mathlib

main =
    let
        addOne = 999
    in
    Mathlib.double 5
"""
                ]
                Int
                12
            , evalProjectTest "deeply nested: caller's range doesn't shadow List-like range"
                [ """module MyList exposing (indexedMap, range)

range lo hi =
    if lo > hi then
        []
    else
        lo :: range (lo + 1) hi

indexedMap f xs =
    map2 f (range 0 (length xs - 1)) xs

length xs =
    case xs of
        [] -> 0
        _ :: rest -> 1 + length rest

map2 f as_ bs =
    case (as_, bs) of
        (a :: restA, b :: restB) ->
            f a b :: map2 f restA restB
        _ ->
            []
"""
                , """module Main exposing (main)

import MyList

main =
    let
        range = 1114112
    in
    MyList.indexedMap (\\i x -> i + x) [10, 20, 30]
"""
                ]
                (list Int)
                [ 10, 21, 32 ]
            ]
        ]


recordAliasConstructorTests : Test
recordAliasConstructorTests =
    describe "Record type alias constructors"
        [ test "Record alias constructor field access .x" <|
            \_ ->
                Eval.Module.eval
                    """module Test exposing (main)

type alias Point = { x : Int, y : Int }

main = (Point 1 2).x
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 1))
        , test "Record alias constructor field access .y" <|
            \_ ->
                Eval.Module.eval
                    """module Test exposing (main)

type alias Point = { x : Int, y : Int }

main = (Point 3 4).y
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 4))
        , test "Record alias constructor used as function" <|
            \_ ->
                Eval.Module.eval
                    """module Test exposing (main)

type alias Point = { x : Int, y : Int }

main = List.map .x [Point 10 20, Point 30 40]
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (List [ Int 10, Int 30 ]))
        ]


moduleDispatchTests : Test
moduleDispatchTests =
    describe "Same-module vs cross-module function dispatch"
        [ evalProjectTest "same-module helper resolves correctly"
            [ """module Main exposing (main)

double x = x * 2

main = double 7
"""
            ]
            Int
            14
        , evalProjectTest "cross-module call resolves to target module"
            [ """module Helpers exposing (double)

double x = x * 2
"""
            , """module Main exposing (main)

import Helpers

main = Helpers.double 7
"""
            ]
            Int
            14
        , evalProjectTest "same-named functions in different modules resolve independently"
            [ """module A exposing (compute)

compute x = x + 1
"""
            , """module B exposing (compute)

compute x = x * 10
"""
            , """module Main exposing (main)

import A
import B

main = A.compute 5 + B.compute 5
"""
            ]
            Int
            56
        , evalProjectTest "nested module names dispatch correctly"
            [ """module Data.List exposing (double)

double x = x * 2
"""
            , """module Data.List.Extra exposing (triple)

triple x = x * 3
"""
            , """module Main exposing (main)

import Data.List
import Data.List.Extra

main = Data.List.double 5 + Data.List.Extra.triple 5
"""
            ]
            Int
            25
        , evalProjectTest "cross-module call back to caller's module"
            [ """module Helpers exposing (applyDouble)

import Main exposing (double)

applyDouble x = double x
"""
            , """module Main exposing (main, double)

import Helpers

double x = x * 2

main = Helpers.applyDouble 7
"""
            ]
            Int
            14
        ]


letFunctionScopingTests : Test
letFunctionScopingTests =
    describe "let-function scoping (same name in different scopes)"
        [ test "two functions with same-named let-binding 'go' producing different values" <|
            \_ ->
                -- Minimal repro: two functions each define a let-function named "go".
                -- The second "go" should NOT see the first "go"'s body.
                Eval.Module.eval
                    """module Test exposing (main)

first =
    let
        go x = x + 1
    in
    go 10

second =
    let
        go x = x * 2
    in
    go 10

main = ( first, second )
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Tuple (Int 11) (Int 20)))
        , test "same-named let-function 'raise' in two functions producing different types" <|
            \_ ->
                -- Models the elm-review bug: createRuleProjectVisitor and
                -- createRuleModuleVisitor both define a let-function named "raise"
                -- that creates different Custom types.
                Eval.Module.eval
                    """module Test exposing (main)

type A = A String
type B = B String

makeA =
    let
        raise val = A val
    in
    raise "fromA"

makeB =
    let
        raise val = B val
    in
    raise "fromB"

main =
    case ( makeA, makeB ) of
        ( A a, B b ) -> a ++ " " ++ b
        _ -> "WRONG"
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (String "fromA fromB"))
        , evalProjectTest "same-named let-function across modules"
            [ """module Mod exposing (makeA, makeB)

type A = A String
type B = B String

makeA =
    let
        raise val = A val
    in
    raise "fromA"

makeB =
    let
        raise val = B val
    in
    raise "fromB"
"""
            , """module Main exposing (main)

import Mod exposing (makeA, makeB)

main =
    case ( makeA, makeB ) of
        ( Mod.A a, Mod.B b ) -> a ++ " " ++ b
        _ -> "WRONG"
"""
            ]
            String
            "fromA fromB"
        , test "let-function with recursive raise pattern (elm-review pattern)" <|
            \_ ->
                -- Exact pattern from Review.Rule: two functions each with
                -- recursive let-function "raise" creating different wrappers.
                Eval.Module.eval
                    """module Test exposing (main)

type ProjectVisitor = ProjectVisitor { errors : List String }
type ModuleVisitor = ModuleVisitor { visitor : String -> ModuleVisitor, toProject : () -> ProjectVisitor }

makeProjectVisitor errors =
    let
        raise errs =
            ProjectVisitor { errors = errs }
    in
    raise errors

makeModuleVisitor toProjectFn initial =
    let
        raise errors =
            ModuleVisitor
                { visitor = \\item -> raise (item :: errors)
                , toProject = \\() -> toProjectFn errors
                }
    in
    raise initial

main =
    let
        pv = makeProjectVisitor [ "p1" ]

        mv = makeModuleVisitor (\\errs -> makeProjectVisitor errs) []

        (ModuleVisitor ops) = mv
        mv2 = ops.visitor "m1"

        (ModuleVisitor ops2) = mv2
        (ProjectVisitor pvOps) = ops2.toProject ()
    in
    case pv of
        ProjectVisitor p ->
            String.join "," (p.errors ++ pvOps.errors)
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (String "p1,m1"))
        , test "nested closure: outer function with 'raise' calls inner function that also defines 'raise'" <|
            \_ ->
                -- Models the actual elm-review pattern:
                -- createRuleProjectVisitor defines raise (-> ProjectVisitor)
                -- Inside its body, it stores a closure that later calls
                -- createRuleModuleVisitor which defines its own raise (-> ModuleVisitor)
                Eval.Module.eval
                    """module Test exposing (main)

type PV = PV { makeModule : () -> MV }
type MV = MV { name : String }

createProjectVisitor =
    let
        raise cache =
            PV { makeModule = \\() -> createModuleVisitor cache }
    in
    raise "cached"

createModuleVisitor cache =
    let
        raise label =
            MV { name = label ++ "+" ++ cache }
    in
    raise "mod"

main =
    let
        (PV pv) = createProjectVisitor
        (MV mv) = pv.makeModule ()
    in
    mv.name
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (String "mod+cached"))
        , test "let raise in function A, A's body calls function B which also has let raise" <|
            \_ ->
                -- A defines raise, then in A's body (still in A's let scope),
                -- it calls B. B defines its own raise. The question: does B's
                -- raise get its own body or A's?
                Eval.Module.eval
                    """module Test exposing (main)

type A = A String
type B = B String

funcB () =
    let
        raise val = B val
    in
    raise "fromB"

funcA () =
    let
        raise val = A val
    in
    case raise "fromA" of
        A s ->
            case funcB () of
                B s2 -> s ++ " " ++ s2
                A _ -> "WRONG: B got A's raise"
        B _ -> "WRONG: A got B's raise"

main = funcA ()
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (String "fromA fromB"))
        , test "let raise in function A, A stores closure, closure later calls B with its own raise" <|
            \_ ->
                -- A defines raise, creates a closure that captures the env,
                -- then the closure is called later which invokes B.
                -- B defines its own raise. Does B get A's raise body?
                Eval.Module.eval
                    """module Test exposing (main)

type A = A { callB : () -> B }
type B = B String

funcB tag =
    let
        raise val = B (tag ++ ":" ++ val)
    in
    raise "inner"

funcA () =
    let
        raise tag =
            A { callB = \\() -> funcB tag }
    in
    raise "outer"

main =
    case funcA () of
        A record ->
            case record.callB () of
                B s -> s

main2 = main
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (String "outer:inner"))
        , test "function call INSIDE let scope of another raise, callee defines own raise" <|
            \_ ->
                -- funcA has let raise = A. In its let body, it calls funcB.
                -- funcB has let raise = B. funcB's raise should produce B.
                -- This tests whether funcA's let-function-dict leaks into funcB.
                Eval.Module.eval
                    """module Test exposing (main)

type A = A String
type B = B String

funcB () =
    let
        raise val = B val
    in
    raise "inner"

funcA () =
    let
        raise val = A val
        result = funcB ()
    in
    case result of
        B s -> "OK: " ++ s
        A s -> "BUG: got A instead of B"

main = funcA ()
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (String "OK: inner"))
        , test "SCOPING: caller's parameter should not leak into same-module callee" <|
            \_ ->
                -- funcA has parameter 'x'. It calls funcB in the same module.
                -- funcB defines let y = x + 1. In proper lexical scoping,
                -- 'x' is NOT in scope inside funcB. In dynamic scoping, it leaks.
                -- The interpreter currently can't test this directly (Elm compiler
                -- would reject funcB referencing x), but we CAN test the consequence:
                -- a let-function named the same as a caller's parameter.
                Eval.Module.eval
                    """module Test exposing (main)

funcB () =
    let
        helper n = n * 10
    in
    helper 5

funcA helper =
    -- 'helper' is a parameter here (a function).
    -- funcB defines its own 'helper' as a let-function.
    -- funcB should use ITS let-function, not funcA's parameter.
    funcB ()

main = funcA (\\n -> n + 1)
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 50))
        , test "SCOPING: same-module function call does not inherit caller's let bindings" <|
            \_ ->
                -- funcA has a let-binding 'x'. It calls funcB.
                -- funcB has its own let-binding 'x'. funcB should use its own.
                Eval.Module.eval
                    """module Test exposing (main)

funcB () =
    let
        x = 100
    in
    x

funcA () =
    let
        x = 999
    in
    funcB ()

main = funcA ()
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 100))
        , test "BUG: let-function should shadow parameter with same name" <|
            \_ ->
                -- A function takes a parameter named 'go'. Inside, a let-function
                -- also named 'go' should shadow the parameter. But addFunction
                -- puts it in currentModuleFunctions while the parameter stays
                -- in env.values which has HIGHER lookup priority.
                Eval.Module.eval
                    """module Test exposing (main)

outer go =
    let
        go x = x * 10
    in
    go 5

main = outer (\\x -> x + 1)
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 50))
        , test "BUG: let-function shadows parameter, used in nested closure" <|
            \_ ->
                Eval.Module.eval
                    """module Test exposing (main)

type A = A String
type B = B String

process raise =
    let
        raise val = B val
    in
    raise "test"

main =
    case process (\\val -> A val) of
        B s -> "OK: " ++ s
        A s -> "BUG: parameter raise was used instead of let raise"
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (String "OK: test"))
        , test "TYPE-ANNOTATED let function: two functions with same-named typed let-binding" <|
            \_ ->
                -- The elm-review 'raise' has a type annotation. elm-syntax may
                -- parse this as TWO declarations (annotation + function),
                -- which would route through evalLetBlockFull instead of
                -- evalLetBlockSingle. This could cause the scoping bug.
                Eval.Module.eval
                    """module Test exposing (main)

type A = A String
type B = B String

funcA () =
    let
        raise : String -> A
        raise val = A val
    in
    raise "fromA"

funcB () =
    let
        raise : String -> B
        raise val = B val
    in
    raise "fromB"

main =
    case ( funcA (), funcB () ) of
        ( A a, B b ) -> a ++ " " ++ b
        _ -> "WRONG"
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (String "fromA fromB"))
        , test "TYPE-ANNOTATED: funcA calls funcB while A's typed raise is in scope" <|
            \_ ->
                Eval.Module.eval
                    """module Test exposing (main)

type A = A String
type B = B String

funcB () =
    let
        raise : String -> B
        raise val = B val
    in
    raise "inner"

funcA () =
    let
        raise : String -> A
        raise val = A val
        result = funcB ()
    in
    case result of
        B s -> "OK: " ++ s
        A _ -> "BUG: got A"

main = funcA ()
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (String "OK: inner"))
        , test "FAITHFUL: recursive raise creates record with closure that calls another function with its own raise" <|
            \_ ->
                Eval.Module.eval
                    """module Test exposing (main)

type PV = PV PVOps
type alias PVOps =
    { createModule : () -> MV
    , getResult : () -> String
    }

type MV = MV MVOps
type alias MVOps =
    { visit : String -> MV
    , toProject : () -> PV
    }

createPV : List String -> PV
createPV initialErrors =
    let
        raiseCache : List String -> PV
        raiseCache errors =
            PV
                { createModule = \\() ->
                    createMV (\\moduleErrors -> raiseCache moduleErrors)
                , getResult = \\() -> String.join "," errors
                }
    in
    raiseCache initialErrors

createMV : (List String -> PV) -> MV
createMV toProjectFn =
    let
        raise : List String -> MV
        raise errors =
            MV
                { visit = \\item -> raise (item :: errors)
                , toProject = \\() -> toProjectFn errors
                }
    in
    raise []

main =
    let
        (PV pvOps) = createPV []
        mv = pvOps.createModule ()
        (MV mvOps) = mv
        mv2 = mvOps.visit "hello"
        (MV mvOps2) = mv2
        (PV pvOps2) = mvOps2.toProject ()
    in
    pvOps2.getResult ()
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (String "hello"))
        , test "EXACT elm-review pattern: raise in project visitor closure calls function with its own raise" <|
            \_ ->
                -- The critical pattern: createRuleProjectVisitor's raise creates
                -- a record with a field that, when called, invokes
                -- createRuleModuleVisitor. That function defines its own raise.
                -- The bug: the inner raise body gets the outer raise's body.
                Eval.Module.eval
                    """module Test exposing (main)

type PV = PV { moduleCreator : () -> MV }
type MV = MV { visit : String -> MV, toProject : () -> PV }

createPV =
    let
        raise hidden =
            PV
                { moduleCreator = \\() ->
                    createMV (\\errors -> raise { hidden | errors = errors })
                }
    in
    raise { errors = [] }

createMV toProjectFn =
    let
        raise errors =
            MV
                { visit = \\item -> raise (item :: errors)
                , toProject = \\() -> toProjectFn errors
                }
    in
    raise []

main =
    let
        (PV pv) = createPV
        mv = pv.moduleCreator ()
    in
    case mv of
        MV ops ->
            let
                (MV ops2) = ops.visit "hello"
            in
            case ops2.toProject () of
                PV _ -> "got PV"
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (String "got PV"))
        ]


evalProjectTest : String -> List String -> (a -> Value) -> a -> Test
evalProjectTest name sources toValue a =
    test name <|
        \_ ->
            case Eval.Module.evalProject sources (Expression.FunctionOrValue [] "main") of
                Err e ->
                    Expect.fail (Debug.toString e)

                Ok value ->
                    Expect.equal (toValue a) value


nameErrorMessageTests : Test
nameErrorMessageTests =
    describe "NameError messages report the name as written in source"
        [ test "unqualified unresolved identifier keeps bare name (no module prefix)" <|
            \_ ->
                case
                    Eval.Module.evalProject
                        [ """module Main exposing (main)

main =
    case Nothing of
        Just _ ->
            0

        Nothing ->
            a + 1
"""
                        ]
                        (Expression.FunctionOrValue [] "main")
                of
                    Err (EvalError e) ->
                        case e.error of
                            NameError msg ->
                                msg
                                    |> Expect.equal "a"

                            other ->
                                Expect.fail ("expected NameError, got " ++ Debug.toString other)

                    Err other ->
                        Expect.fail ("expected NameError, got " ++ Debug.toString other)

                    Ok v ->
                        Expect.fail ("expected NameError, got Ok " ++ Debug.toString v)
        , test "type error from wrong list element type reports Elm-facing 'List', not host 'JsArray'" <|
            \_ ->
                case
                    Eval.Module.evalProject
                        [ """module Main exposing (main)

main =
    -- String.join expects List String; pass List (List Char) to force a type error.
    -- The error message should reference "List" (the Elm type), not "JsArray"
    -- (a host-internal representation Elm users don't know about).
    String.join ", " [ [ 'a', 'b' ], [ 'c', 'd' ] ]
"""
                        ]
                        (Expression.FunctionOrValue [] "main")
                of
                    Err (EvalError e) ->
                        case e.error of
                            TypeError msg ->
                                msg
                                    |> String.contains "JsArray"
                                    |> Expect.equal False
                                    |> Expect.onFail ("error message mentioned host-internal 'JsArray' instead of 'List': " ++ msg)

                            other ->
                                Expect.fail ("expected TypeError, got " ++ Debug.toString other)

                    Err other ->
                        Expect.fail ("expected TypeError, got " ++ Debug.toString other)

                    Ok v ->
                        Expect.fail ("expected TypeError, got Ok " ++ Debug.toString v)
        , test "self-call in case scrutinee is not a tail position" <|
            \_ ->
                -- Reduced from elmcraft/core-extra's `List.Extra.scanr`:
                --     scanr f acc xs_ =
                --         case xs_ of
                --             [] -> [ acc ]
                --             x :: xs ->
                --                 case scanr f acc xs of
                --                     (q :: _) as qs -> f x q :: qs
                --                     [] -> []
                --
                -- The recursive call lives in the scrutinee of an inner
                -- case — the scrutinee must be evaluated before pattern
                -- matching can run, so it is NOT a tail-recursion position.
                -- Our `isTailRecursive` used to treat `CaseExpression` as
                -- tail-safe whenever every branch body was tail-safe,
                -- ignoring the scrutinee. `rec` then got wrapped in a
                -- `tcoLoop`, the recursive call produced a TailCall signal
                -- instead of a value, and the surrounding case matched that
                -- signal against `(q :: _) as qs` / `[]` — which returned
                -- the base case's own result instead of threading through
                -- the case. For `rec 1` that meant `[99]` instead of
                -- `[100, 99]`.
                Eval.Module.evalProject
                    [ """module Main exposing (main)

rec : Int -> List Int
rec n =
    if n == 0 then
        [ 99 ]

    else
        case rec (n - 1) of
            (q :: _) as qs ->
                (n + q) :: qs

            [] ->
                [ -1 ]

main =
    rec 1
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (List [ Int 100, Int 99 ]))
        , test "qualified unresolved identifier keeps its module prefix" <|
            \_ ->
                case
                    Eval.Module.evalProject
                        [ """module Main exposing (main)

main =
    Foo.bar
"""
                        ]
                        (Expression.FunctionOrValue [] "main")
                of
                    Err (EvalError e) ->
                        case e.error of
                            NameError msg ->
                                msg
                                    |> Expect.equal "Foo.bar"

                            other ->
                                Expect.fail ("expected NameError, got " ++ Debug.toString other)

                    Err other ->
                        Expect.fail ("expected NameError, got " ++ Debug.toString other)

                    Ok v ->
                        Expect.fail ("expected NameError, got Ok " ++ Debug.toString v)
        ]


