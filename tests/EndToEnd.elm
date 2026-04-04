module EndToEnd exposing (suite)

import Elm.Syntax.Expression as Expression
import Eval
import Eval.Module
import Expect
import Array
import FastDict as Dict
import Test exposing (Test, describe, test)
import TestUtils exposing (evalTest, evalTest_, list, maybe, slowTest)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Some end to end tests"
        [ helloWorldTest
        , sumTest
        , fibonacciTest
        , recordTest
        , customTypeTest
        , standardLibraryTest
        , tailCallTest
        , closureTest
        , tooMuchApplyTest
        , mutualRecursionTest
        , tuplesTest
        , negationTest
        , kernelTest
        , joinTest
        , modulesTest
        , higherOrderTest
        , shadowingTest
        , recordUpdateTest
        , caseBoolPatternTest
        , kernelFunctionAsArgTest
        , edgeCaseTests
        , patternMatchTests
        , lambdaInlineTests
        , sortStabilityTests
        , debugToStringTests
        , parserEdgeCaseTests
        , stepLimitTests
        , accumulatorLoopDetectionTests
        , growingArgsLoopDetectionTests
        , taskTests
        , bytesTests
        , tcoParserPatternTests
        , letFunctionScopingTests
        ]


helloWorldTest : Test
helloWorldTest =
    evalTest_ "\"Hello, World\"" String "Hello, World"


sumTest : Test
sumTest =
    evalTest "2 + 3" "2 + 3" Int 5


fibonacciTest : Test
fibonacciTest =
    evalTest "Fibonacci"
        "let fib n = if n <= 2 then 1 else fib (n - 1) + fib (n - 2) in fib 7"
        Int
        13


recordTest : Test
recordTest =
    evalTest "Record" "{ a = 13, b = 'c'}.b" Char 'c'


customTypeTest : Test
customTypeTest =
    evalTest "Custom type"
        """let
    foo = Just []
in
case foo of
    Nothing -> -1
    Just [ x ] -> 1
    Just [] -> 0
"""
        Int
        0


standardLibraryTest : Test
standardLibraryTest =
    evalTest "Stdlib"
        "List.isEmpty [()]"
        Bool
        False


tailCallTest : Test
tailCallTest =
    slowTest <|
        \i ->
            describe "Tail call"
                [ evalTest "Inline"
                    ("let boom x = if x <= 0 then False else boom (x - 1) in boom " ++ String.fromInt i)
                    Bool
                    False
                , evalTestModule "As module"
                    ("""module TailCall exposing (boom)

boom : Int -> Bool
boom x =
    let
        a = 0
    in
    if x <= 0 then
        False
    else
        boom (x - 1)

main : Bool
main =
    boom """ ++ String.fromInt i)
                    Bool
                    False
                ]


closureTest : Test
closureTest =
    describe "Closures"
        [ evalTest "Simple"
            "let a = 3 in let closed x = a + x in closed 2"
            Int
            5
        , evalTest "Recursive" """let
    closure =
        let
            odd x =
                x < 0 || even (x - 1)
            even x =
                x <= 0 || odd (x - 1)
        in
        odd
in
closure 3""" Bool True
        ]


tooMuchApplyTest : Test
tooMuchApplyTest =
    evalTest "Too much apply"
        "(\\a -> Foo a) 0 1 2"
        identity
    <|
        Custom
            { moduleName = [ "Main" ], name = "Foo" }
            [ Int 0, Int 1, Int 2 ]


mutualRecursionTest : Test
mutualRecursionTest =
    describe "Mutual recursion"
        [ evalTestModule "At the top level"
            """module Test exposing (..)

fib1 n =
    if n <= 2 then
        1
    else
        fib2 (n - 1) + fib2 (n - 2)

fib2 n =
    if n <= 2 then
        1
    else
        fib1 (n - 1) + fib1 (n - 2)

main =
    fib1 7"""
            Int
            13
        , evalTest "Inside a let" """let
    fib1 n =
        if n <= 2 then
            1
        else
            fib2 (n - 1) + fib2 (n - 2)

    fib2 n =
        if n <= 2 then
            1
        else
            fib1 (n - 1) + fib1 (n - 2)
in
fib1 7""" Int 13
        , evalTest "[let] Constant using a function" """let
    a = foo 0
    foo x = x
in
a
""" Int 0
        , evalTest "[let] Constant using a constant before it" """let
    a = 0
    b = a
in
b
""" Int 0
        , evalTest "[let] Constant using a constant after it" """let
    a = b
    b = 0
in
b
""" Int 0
        ]


tuplesTest : Test
tuplesTest =
    evalTest "Tuples"
        """let (a, b) = (2, 3) in let (c, d, e) = (4, 5, 6) in a + b + c + d + e"""
        Int
        20


negationTest : Test
negationTest =
    evalTest_ "-2" Int -2


kernelTest : Test
kernelTest =
    describe "Kernel"
        [ evalTest_ "String.length \"a\"" Int 1
        , evalTest_ "Basics.e" Float e
        ]


joinTest : Test
joinTest =
    let
        list : List Value
        list =
            [ String "0"
            , String "1"
            , String "2"
            ]
    in
    describe "String.join"
        [ evalTest_ """["0","1","2"]""" List list
        , evalTest_ """String.join "." ["0","1","2"]""" String "0.1.2"
        ]


modulesTest : Test
modulesTest =
    evalTest_ "List.sum [ 1, 2, 3 ]" Int 6


higherOrderTest : Test
higherOrderTest =
    evalTest_ "String.map Char.toUpper \"Hello, world!\"" String <|
        String.map Char.toUpper "Hello, world!"


evalTestModule : String -> String -> (a -> Value) -> a -> Test
evalTestModule name expression toValue a =
    test name <|
        \_ ->
            Eval.Module.eval expression (Expression.FunctionOrValue [] "main")
                |> Expect.equal (Ok (toValue a))


shadowingTest : Test
shadowingTest =
    evalTestModule "shadowing in let/in" """module Temp exposing (main)

foo : a -> List a -> List a
foo nodes acc =
    let
        node =
            nodes

        newAcc =
            node :: acc
    in
    newAcc


main : List (List number)
main =
    let
        node =
            [ 0, 1 ]
    in
    foo [ 4, 5 ] [ node ]""" (list (list Int)) [ [ 4, 5 ], [ 0, 1 ] ]


recordUpdateTest : Test
recordUpdateTest =
    describe "Record update"
        [ evalTest "preserves original fields"
            """let rec = { a = 1, b = 2 } in { rec | a = 10 }"""
            Record
            (Dict.fromList [ ( "a", Int 10 ), ( "b", Int 2 ) ])
        , evalTest "updates multiple fields"
            """let rec = { x = 1, y = 2, z = 3 } in { rec | x = 10, z = 30 }"""
            Record
            (Dict.fromList [ ( "x", Int 10 ), ( "y", Int 2 ), ( "z", Int 30 ) ])
        ]


caseBoolPatternTest : Test
caseBoolPatternTest =
    describe "Case matching on Bool constructors"
        [ evalTest "True branch"
            """case True of
    True -> 1
    False -> 0"""
            Int
            1
        , evalTest "False branch"
            """case False of
    True -> 1
    False -> 0"""
            Int
            0
        , evalTest "Bool variable"
            """let x = True in case x of
    True -> "yes"
    False -> "no" """
            String
            "yes"
        ]


kernelFunctionAsArgTest : Test
kernelFunctionAsArgTest =
    describe "Kernel function as higher-order argument"
        [ evalTest "List.map String.fromInt"
            "List.map String.fromInt [1, 2, 3]"
            (list String)
            (List.map String.fromInt [ 1, 2, 3 ])
        , evalTest "List.map negate"
            "List.map negate [1, 2, 3]"
            (list Int)
            (List.map negate [ 1, 2, 3 ])
        , evalTest "List.filterMap String.toInt"
            """List.filterMap String.toInt ["1", "a", "3"]"""
            (list Int)
            (List.filterMap String.toInt [ "1", "a", "3" ])
        , evalTest "Array.map with kernel function"
            "Array.map negate (Array.fromList [1, 2, 3]) |> Array.toList"
            (list Int)
            (Array.toList (Array.map negate (Array.fromList [ 1, 2, 3 ])))
        , evalTest "Array.map with 0-arg kernel wrapper (Char.toUpper)"
            "Array.map Char.toUpper (Array.fromList ['a', 'b', 'c']) |> Array.toList"
            (list Char)
            [ 'A', 'B', 'C' ]
        , evalTest "String.filter with kernel predicate"
            "String.filter Char.isUpper \"Hello World\""
            String
            "HW"
        ]


edgeCaseTests : Test
edgeCaseTests =
    describe "Edge cases"
        [ -- Debug.toString on nested structures
          evalTest "Debug.toString nested Maybe"
            "Debug.toString (Just (Just 1))"
            String
            "Just (Just 1)"
        , evalTest "Debug.toString list of Maybe"
            "Debug.toString [Just 1, Nothing]"
            String
            "[Just 1,Nothing]"
        , evalTest "Debug.toString negative in constructor"
            "Debug.toString (Just -1)"
            String
            "Just (-1)"
        , evalTest "Debug.toString empty list"
            "Debug.toString []"
            String
            "[]"
        , evalTest "Debug.toString tuple"
            "Debug.toString (1, 2)"
            String
            "(1,2)"
        , evalTest "Debug.toString empty record"
            "Debug.toString {}"
            String
            "{}"

        -- Nested destructuring
        , evalTest "nested tuple destructuring"
            "let (a, (b, c)) = (1, (2, 3)) in a + b + c"
            Int
            6
        , evalTest "let record destructuring"
            "let { x, y } = { x = 10, y = 20 } in x + y"
            Int
            30

        -- Wildcard pattern
        , evalTest "wildcard in case"
            "case Just 42 of\n    Nothing -> 0\n    _ -> 1"
            Int
            1

        -- String operations
        , evalTest "String.fromFloat"
            "String.fromFloat 3.14"
            String
            "3.14"
        , evalTest "String.fromInt"
            "String.fromInt 42"
            String
            "42"
        , evalTest "String.toInt valid"
            """String.toInt "123" """
            (TestUtils.maybe Int)
            (Just 123)
        , evalTest "String.toInt invalid"
            """String.toInt "abc" """
            (TestUtils.maybe Int)
            Nothing
        , evalTest "append strings"
            """"hello" ++ " world" """
            String
            "hello world"
        , evalTest "append lists"
            "[1, 2] ++ [3, 4]"
            (list Int)
            [ 1, 2, 3, 4 ]

        -- Composing functions
        , evalTest "function composition >>"
            "(String.fromInt >> String.length) 123"
            Int
            3
        , evalTest "function composition <<"
            "(String.length << String.fromInt) 123"
            Int
            3

        ]


patternMatchTests : Test
patternMatchTests =
    describe "Pattern matching"
        [ -- Single-arg constructor (Just x) — fast path
          evalTest "Just x pattern"
            """case Just 42 of
    Nothing -> 0
    Just x -> x"""
            Int
            42
        , evalTest "Ok x pattern"
            """case Ok "hello" of
    Err _ -> ""
    Ok x -> x"""
            String
            "hello"
        , evalTest "Err e pattern"
            """case Err 99 of
    Ok _ -> 0
    Err e -> e"""
            Int
            99

        -- UnCons pattern (x :: xs)
        , evalTest "uncons pattern"
            """case [1, 2, 3] of
    x :: xs -> x
    [] -> 0"""
            Int
            1
        , evalTest "uncons rest"
            """case [1, 2, 3] of
    x :: xs -> List.length xs
    [] -> 0"""
            Int
            2

        -- Tuple pattern with bindings
        , evalTest "tuple pattern both vars"
            """let (a, b) = (10, 20) in a + b"""
            Int
            30
        , evalTest "triple pattern all vars"
            """let (a, b, c) = (1, 2, 3) in a + b + c"""
            Int
            6

        -- Nested constructor patterns
        , evalTest "nested Just"
            """case Just (Just 7) of
    Just (Just n) -> n
    _ -> 0"""
            Int
            7
        , evalTest "Just with tuple"
            """case Just (1, 2) of
    Just (a, b) -> a + b
    Nothing -> 0"""
            Int
            3

        -- List pattern
        , evalTest "list pattern exact match"
            """case [1, 2, 3] of
    [a, b, c] -> a + b + c
    _ -> 0"""
            Int
            6
        , evalTest "list pattern no match"
            """case [1, 2] of
    [a, b, c] -> a + b + c
    _ -> 0"""
            Int
            0

        -- As pattern
        , evalTest "as pattern"
            """case Just 5 of
    (Just n) as whole -> n
    _ -> 0"""
            Int
            5

        -- Record pattern
        , evalTest "record pattern in case"
            """case { x = 1, y = 2 } of
    { x, y } -> x + y"""
            Int
            3

        -- Multi-arg constructor
        , evalTestModule "multi-arg constructor pattern"
            """module Temp exposing (main)

type Pair = Pair Int Int

main : Int
main =
    case Pair 3 4 of
        Pair a b -> a + b"""
            Int
            7
        ]


lambdaInlineTests : Test
lambdaInlineTests =
    describe "Lambda and inline expression evaluation"
        [ -- Lambda passed as argument (tests evalOrRecurse Lambda inline)
          evalTest "lambda as argument"
            "List.map (\\x -> x + 1) [1, 2, 3]"
            (list Int)
            [ 2, 3, 4 ]

        -- Record access function (tests evalOrRecurse RecordAccessFunction inline)
        , evalTest "record access function"
            "List.map .name [{ name = \"a\" }, { name = \"b\" }]"
            (list String)
            [ "a", "b" ]

        -- Parenthesized expression (tests evalOrRecurse Parenthesized inline)
        , evalTest "parenthesized literal"
            "(42)"
            Int
            42
        , evalTest "parenthesized variable"
            "let x = 7 in (x)"
            Int
            7

        -- Partial application
        , evalTest "partial application 3-arg"
            """let add3 a b c = a + b + c in let add1 = add3 1 in add1 2 3"""
            Int
            6

        -- Negation literals inline (tests evalOrRecurse Negation inline)
        , evalTest "negation int literal"
            "let x = -5 in x"
            Int
            -5
        , evalTest "negation float literal"
            "let x = -3.14 in x + 3.14"
            Float
            0.0

        -- Case with no bindings (tests Dict.isEmpty skip for Environment.with)
        , evalTest "case Nothing branch"
            """case Nothing of
    Nothing -> 42
    Just _ -> 0"""
            Int
            42
        , evalTest "case wildcard no binding"
            """case 7 of
    _ -> 42"""
            Int
            42

        -- Operator with inline args (tests skip callKernel for operators)
        , evalTest "arithmetic operators"
            "1 + 2 * 3 - 4"
            Int
            3
        , evalTest "comparison operators"
            "3 > 2"
            Bool
            True
        , evalTest "string concat operator"
            """"hello" ++ " " ++ "world" """
            String
            "hello world"

        -- Empty list inline (tests evalOrRecurse ListExpr [] inline)
        , evalTest "empty list literal"
            "[]"
            (list Int)
            []

        -- Empty tuple (unit) inline (tests evalOrRecurse TupledExpression [] inline)
        , evalTest "unit from empty tuple"
            "()"
            identity
            Unit
        ]


sortStabilityTests : Test
sortStabilityTests =
    describe "List.sortBy stability"
        [ evalTestModule "sortBy preserves order of equal elements"
            """module Temp exposing (main)

main =
    let
        items =
            [ { name = "alice", group = 2 }
            , { name = "bob", group = 1 }
            , { name = "charlie", group = 2 }
            ]

        sorted = List.sortBy .group items
    in
    List.map .name sorted
"""
            (list String)
            [ "bob", "alice", "charlie" ]
        , evalTestModule "sort stability with identical sort keys"
            """module Temp exposing (main)

main =
    let
        items =
            [ { val = "first", key = 1 }
            , { val = "second", key = 1 }
            , { val = "third", key = 1 }
            ]

        sorted = List.sortBy .key items
    in
    List.map .val sorted
"""
            (list String)
            [ "first", "second", "third" ]
        , evalTestModule "sortWith stability"
            """module Temp exposing (main)

main =
    let
        items = [ (1, "a"), (2, "b"), (1, "c"), (2, "d") ]
        sorted = List.sortWith (\\( k1, _ ) ( k2, _ ) -> compare k1 k2) items
    in
    List.map (\\( _, v ) -> v) sorted
"""
            (list String)
            [ "a", "c", "b", "d" ]

        -- Known elm-syntax parser limitation: negative at start of list literal
        -- needs a space. [-3] fails to parse but [ -3 ] works.
        -- See issues/002-negative-list-literal-parse.md
        , evalTest "negative at start of list literal"
            "List.sum [-3, 5, -1]"
            Int
            1
        ]


debugToStringTests : Test
debugToStringTests =
    describe "Debug.toString"
        [ evalTestModule "Dict shows Dict.fromList format"
            """module Temp exposing (main)
import Dict
main = Debug.toString (Dict.fromList [ ("a", 1), ("b", 2) ])
"""
            String
            """Dict.fromList [("a",1),("b",2)]"""
        , evalTestModule "Set shows Set.fromList format"
            """module Temp exposing (main)
import Set
main = Debug.toString (Set.fromList [ 1, 2, 3 ])
"""
            String
            "Set.fromList [1,2,3]"
        , evalTestModule "empty Dict"
            """module Temp exposing (main)
import Dict
main = Debug.toString (Dict.empty)
"""
            String
            "Dict.fromList []"
        , evalTestModule "empty Set"
            """module Temp exposing (main)
import Set
main = Debug.toString (Set.empty)
"""
            String
            "Set.fromList []"
        ]


parserEdgeCaseTests : Test
parserEdgeCaseTests =
    describe "Parser edge cases"
        [ evalTest "negative in list with space"
            "List.sum [ -3, 5, -1 ]"
            Int
            1
        , evalTest "negative after comma in list"
            "List.sum [5, -3, -1]"
            Int
            1
        ]


stepLimitTests : Test
stepLimitTests =
    describe "step limits"
        [ test "evaluation fails when step limit is exceeded" <|
            \_ ->
                Eval.evalWithMaxSteps (Just 10)
                    "let loop n = if n <= 0 then 0 else loop (n - 1) in loop 100"
                    |> Expect.err
        , test "evaluation succeeds within step limit" <|
            \_ ->
                Eval.evalWithMaxSteps (Just 100000)
                    "let loop n = if n <= 0 then 0 else loop (n - 1) in loop 10"
                    |> Expect.equal (Ok (Int 0))
        , test "no step limit allows large computations" <|
            \_ ->
                Eval.evalWithMaxSteps Nothing
                    "let loop n = if n <= 0 then 0 else loop (n - 1) in loop 100"
                    |> Expect.equal (Ok (Int 0))
        , test "evalWithEnvAndLimit respects step limit" <|
            \_ ->
                let
                    source =
                        """module Temp exposing (main)
main = let loop n = if n <= 0 then 0 else loop (n - 1) in loop 10000"""

                    projectEnv =
                        Eval.Module.buildProjectEnv []
                in
                case projectEnv of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 50)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.err

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "evalWithEnvAndLimit with Nothing allows completion" <|
            \_ ->
                let
                    source =
                        """module Temp exposing (main)
main = let loop n = if n <= 0 then 0 else loop (n - 1) in loop 10"""

                    projectEnv =
                        Eval.Module.buildProjectEnv []
                in
                case projectEnv of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit Nothing
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 0))

                    Err e ->
                        Expect.fail (Debug.toString e)
        ]


{-| Tests for cycle detection — identical-argument loops that are provably
non-terminating in pure Elm. These use no step limit: if the test hangs,
the detection is broken.
-}
accumulatorLoopDetectionTests : Test
accumulatorLoopDetectionTests =
    describe "cycle detection (identical args)"
        [ test "direct recursion with identical args (let binding)" <|
            \_ ->
                -- f(x) calls f(x) — provably infinite in pure Elm.
                Eval.evalWithMaxSteps Nothing
                    """let loop x = loop x
in loop 42"""
                    |> expectInfiniteRecursionError
        , test "direct recursion with identical args (module-level)" <|
            \_ ->
                let
                    source =
                        """module Temp exposing (main)
loop x = loop x
main = loop 42"""

                    projectEnv =
                        Eval.Module.buildProjectEnv []
                in
                case projectEnv of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit Nothing
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> expectInfiniteRecursionError

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "mutual recursion with identical args" <|
            \_ ->
                let
                    source =
                        """module Temp exposing (main)
ping n = pong n
pong n = ping n
main = ping 0"""

                    projectEnv =
                        Eval.Module.buildProjectEnv []
                in
                case projectEnv of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit Nothing
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> expectInfiniteRecursionError

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "legitimate deep recursion must NOT be flagged" <|
            \_ ->
                Eval.evalWithMaxSteps Nothing
                    """let
    mySum acc xs =
        case xs of
            [] -> acc
            x :: rest -> mySum (acc + x) rest
in
mySum 0 [1, 2, 3, 4, 5]"""
                    |> Expect.equal (Ok (Types.Int 15))
        ]


{-| Tests for Category B loops: arguments change every call (so fingerprint
differs), but the function is not making progress toward termination.
These use a step limit as safety timeout, and assert the error is from
cycle detection, not the step limit.
-}
growingArgsLoopDetectionTests : Test
growingArgsLoopDetectionTests =
    describe "cycle detection (growing args — Category B)"
        [ test "accumulator grows, input exhausted — broken foldByBytes" <|
            \_ ->
                -- Models elm-ical mutation: base case removed from foldByBytes.
                -- acc grows every call, chars stays []. Fingerprint changes.
                Eval.evalWithMaxSteps (Just 100000)
                    """let
    fold acc chars =
        case chars of
            _ -> fold (0 :: acc) (List.drop 1 chars)
in
fold [] [1, 2, 3]"""
                    |> expectInfiniteRecursionError
        , test "counter grows without bound (caught by step limit)" <|
            \_ ->
                -- f(0) -> f(1) -> f(2) -> ... No base case.
                -- This is NOT structurally growing (Int size is always 1),
                -- so the Category B size-change check can't detect it.
                -- The step limit catches it as a fallback.
                Eval.evalWithMaxSteps (Just 100000)
                    """let growForever n = growForever (n + 1)
in growForever 0"""
                    |> Expect.err
        , test "accumulator grows, input stays empty" <|
            \_ ->
                -- Both args change, but one only grows and the other is stuck.
                Eval.evalWithMaxSteps (Just 100000)
                    """let
    helper acc remaining =
        case remaining of
            [] -> helper (0 :: acc) []
            c :: rest -> helper (c :: acc) rest
in
helper [] [1, 2, 3]"""
                    |> expectInfiniteRecursionError
        , test "legitimate accumulator recursion terminates — must NOT flag" <|
            \_ ->
                -- Correctly-written: input shrinks each call. Terminates.
                Eval.evalWithMaxSteps (Just 100000)
                    """let
    myLength acc xs =
        case xs of
            [] -> acc
            _ :: rest -> myLength (acc + 1) rest
in
myLength 0 [1, 2, 3, 4, 5]"""
                    |> Expect.equal (Ok (Types.Int 5))
        , test "legitimate fold with growing accumulator — must NOT flag" <|
            \_ ->
                -- acc grows but input shrinks. Terminates normally.
                Eval.evalWithMaxSteps (Just 100000)
                    """let
    rev acc xs =
        case xs of
            [] -> acc
            x :: rest -> rev (x :: acc) rest
in
rev [] [1, 2, 3]"""
                    |> Expect.equal (Ok (Types.List [ Types.Int 3, Types.Int 2, Types.Int 1 ]))
        ]


{-| Assert that evaluation returns an "Infinite recursion" error,
NOT a "Step limit exceeded" error. This proves cycle detection caught the
loop, not the step limit fallback.
-}
expectInfiniteRecursionError : Result Types.Error value -> Expect.Expectation
expectInfiniteRecursionError result =
    case result of
        Err (Types.EvalError { error }) ->
            case error of
                Types.TypeError msg ->
                    if String.contains "Infinite recursion" msg then
                        Expect.pass

                    else
                        Expect.fail ("Expected 'Infinite recursion' TypeError, got: " ++ msg)

                Types.Unsupported msg ->
                    Expect.fail ("Loop was caught by step limit, not cycle detection: " ++ msg)

                other ->
                    Expect.fail ("Unexpected error kind: " ++ Debug.toString other)

        Err (Types.ParsingError _) ->
            Expect.fail "Unexpected parse error"

        Ok _ ->
            Expect.fail "Expected an error but got Ok"


taskTests : Test
taskTests =
    describe "Task module"
        [ evalTestModule "Task.succeed produces a value"
            """module Temp exposing (main)
import Task
main = Task.succeed 42 == Task.succeed 42
"""
            Bool
            True
        , evalTestModule "Task.fail produces a value"
            """module Temp exposing (main)
import Task
main = Task.fail "oops" == Task.fail "oops"
"""
            Bool
            True
        , evalTestModule "Task.succeed and Task.fail are different"
            """module Temp exposing (main)
import Task
main = Task.succeed 1 == Task.fail 1
"""
            Bool
            False
        , evalTestModule "Task.map transforms succeed value"
            """module Temp exposing (main)
import Task
main = Task.map ((+) 1) (Task.succeed 3) == Task.succeed 4
"""
            Bool
            True
        , evalTestModule "Task.andThen chains succeed"
            """module Temp exposing (main)
import Task
main = Task.andThen (\\x -> Task.succeed (x + 1)) (Task.succeed 3) == Task.succeed 4
"""
            Bool
            True
        ]


bytesTests : Test
bytesTests =
    describe "Bytes module"
        [ evalTestModule "encode unsignedInt8 has width 1"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
main = Bytes.width (Bytes.Encode.encode (Bytes.Encode.unsignedInt8 42))
"""
            Int
            1
        , evalTestModule "encode signedInt16 has width 2"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
main = Bytes.width (Bytes.Encode.encode (Bytes.Encode.signedInt16 Bytes.BE 256))
"""
            Int
            2
        , evalTestModule "encode sequence width sums up"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
main = Bytes.width (Bytes.Encode.encode (Bytes.Encode.sequence [ Bytes.Encode.unsignedInt8 1, Bytes.Encode.unsignedInt8 2, Bytes.Encode.unsignedInt32 Bytes.BE 3 ]))
"""
            Int
            6
        , evalTestModule "decode unsignedInt8 round-trip"
            """module Temp exposing (main)
import Bytes.Encode
import Bytes.Decode
main = Bytes.Decode.decode Bytes.Decode.unsignedInt8 (Bytes.Encode.encode (Bytes.Encode.unsignedInt8 42))
"""
            (maybe Int)
            (Just 42)
        , evalTestModule "decode signedInt8 negative"
            """module Temp exposing (main)
import Bytes.Encode
import Bytes.Decode
main = Bytes.Decode.decode Bytes.Decode.signedInt8 (Bytes.Encode.encode (Bytes.Encode.signedInt8 -5))
"""
            (maybe Int)
            (Just -5)
        , evalTestModule "decode unsignedInt16 BE round-trip"
            """module Temp exposing (main)
import Bytes
import Bytes.Encode
import Bytes.Decode
main = Bytes.Decode.decode (Bytes.Decode.unsignedInt16 Bytes.BE) (Bytes.Encode.encode (Bytes.Encode.unsignedInt16 Bytes.BE 1000))
"""
            (maybe Int)
            (Just 1000)
        , evalTestModule "getStringWidth"
            """module Temp exposing (main)
import Bytes.Encode
main = Bytes.Encode.getStringWidth "hello"
"""
            Int
            5
        , evalTestModule "decode fail returns Nothing"
            """module Temp exposing (main)
import Bytes.Encode
import Bytes.Decode
main = Bytes.Decode.decode Bytes.Decode.fail (Bytes.Encode.encode (Bytes.Encode.unsignedInt8 42))
"""
            (maybe Int)
            Nothing
        ]


{-| Tests for the parser-like recursion pattern where a tail-recursive function
is called with UNCHANGED arguments but makes progress via a separate state
channel. This is exactly the pattern used by elm-syntax's infix parser.
-}
tcoParserPatternTests : Test
tcoParserPatternTests =
    describe "TCO parser-like recursion"
        [ evalTestModule "tail-recursive with unchanged closure args (parser pattern)"
            -- parseExpr takes a 'handler' function that doesn't change,
            -- and an offset that decreases. The function IS tail-recursive
            -- (recurses in if/else branch). The handler arg stays the same
            -- across calls — this is the pattern that triggers false cycle
            -- detection in tcoLoop.
            """module Temp exposing (main)

handler : Int -> Int
handler x = x + 1

parseExpr : (Int -> Int) -> Int -> Int
parseExpr f offset =
    if offset <= 0 then
        f 0
    else
        parseExpr f (offset - 1)

main = parseExpr handler 500
"""
            Int
            1
        , test "500+ recursion with only-closure args (parser combinator pattern)" <|
            -- Models infixRight: function takes ONLY closure arguments.
            -- It's called via an outer loop that provides changing state,
            -- but the function itself is called with identical args each time.
            -- The function isn't actually tail-recursive (has work after the
            -- recursive call), but it recurses deeply.
            \_ ->
                Eval.Module.eval
                    """module Temp exposing (main)

type alias Ctx = { remaining : Int, acc : Int }

applyN : (Int -> Int) -> (Int -> Int -> Int) -> Ctx -> Ctx
applyN f g ctx =
    if ctx.remaining <= 0 then
        ctx
    else
        applyN f g { remaining = ctx.remaining - 1, acc = g (f ctx.acc) ctx.acc }

main =
    let result = applyN (\\x -> x + 1) (\\a b -> a + b) { remaining = 600, acc = 0 }
    in result.acc
"""
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.ok
        , evalTestModule "recursion where ONLY closure args and they DON'T change"
            -- This models the exact elm-syntax parser pattern:
            -- `infixRight` takes parser combinator functions as args and
            -- self-calls with the SAME function args. Progress happens via
            -- the return value, not the arguments. The function reads from
            -- a mutable-like state (simulated here via a list that shrinks).
            --
            -- With TCO, tcoLoop checks `newValues == env.values`. Since the
            -- function args (closures) don't change, this would be True if
            -- the env only contains the function args. The key: the env ALSO
            -- contains the local bindings from the function body.
            """module Temp exposing (main)

consume : List Int -> ( Int, List Int )
consume items =
    case items of
        [] -> ( 0, [] )
        x :: rest -> ( x, rest )

parseLoop : (List Int -> ( Int, List Int )) -> List Int -> Int -> Int
parseLoop handler remaining acc =
    case remaining of
        [] -> acc
        _ ->
            let
                ( value, rest ) = handler remaining
            in
            parseLoop handler rest (acc + value)

main = parseLoop consume (List.range 1 300) 0
"""
            Int
            45150
        , test "sequential record-returning functions with recursive sub-call" <|
            -- Models ParserFast.map4: sequences 4 record-returning functions,
            -- the last of which contains a recursive sub-call. If TCO corrupts
            -- the return value, .field access on the result would fail with
            -- "Trying to access a field on a non-record value".
            \_ ->
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnv env
                            [ """module Parser exposing (run)

type Step = Done Int | Loop Int

loop : (Int -> Step) -> Int -> Int
loop stepper state =
    case stepper state of
        Done val -> val
        Loop newState -> loop stepper newState

parse : String -> { value : Int, rest : String }
parse input =
    let
        len = loop (\\n -> if n >= String.length input then Done n else if Char.isDigit (Maybe.withDefault ' ' (List.head (String.toList (String.dropLeft n input)))) then Loop (n + 1) else Done n) 0
    in
    { value = len, rest = String.dropLeft len input }

run : String -> { a : Int, b : Int }
run input =
    let
        r1 = parse input
        r2 = parse r1.rest
    in
    { a = r1.value, b = r2.value }
"""
                            , """module Main exposing (main)

import Parser

main = let result = Parser.run "123abc" in result.a + result.b
"""
                            ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 3))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "parse function application expression via Elm.Parser pattern" <|
            -- Simulates the parser pattern that fails on Debug.log "test" 42.
            -- A map4 function sequences 4 parsers, the 4th of which is recursive.
            -- If TCO corrupts the recursive parser's return value, the record
            -- field access on the map4 result would fail.
            \_ ->
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnv env
                            [ """module Main exposing (main)

type PStep a = Good a Int | Bad

parseA : Int -> PStep String
parseA s = Good "module" (s + 7)

parseB : Int -> PStep String
parseB s = Good "import" (s + 6)

parseC : Int -> PStep (List Int)
parseC s = Good [1, 2] (s + 3)

-- Simulates loopWhileSucceedsHelp pattern: recursive with growing accumulator
loopHelper : (Int -> PStep String) -> List String -> Int -> PStep (List String)
loopHelper parseEl soFar s0 =
    case parseEl s0 of
        Good elResult s1 ->
            loopHelper parseEl (elResult :: soFar) s1
        Bad ->
            Good (List.reverse soFar) s0

simpleExprParser : Int -> PStep String
simpleExprParser s =
    if s < 50 then
        Good ("expr" ++ String.fromInt s) (s + 5)
    else
        Bad

map4 : (a -> b -> c -> d -> e) -> (Int -> PStep a) -> (Int -> PStep b) -> (Int -> PStep c) -> (Int -> PStep d) -> Int -> PStep e
map4 f pa pb pc pd s0 =
    case pa s0 of
        Bad -> Bad
        Good a s1 ->
            case pb s1 of
                Bad -> Bad
                Good b s2 ->
                    case pc s2 of
                        Bad -> Bad
                        Good c s3 ->
                            case pd s3 of
                                Bad -> Bad
                                Good d s4 -> Good (f a b c d) s4

main =
    let
        exprParser = loopHelper simpleExprParser
    in
    case map4 (\\a b c d -> { modu = a, imp = b, decls = c, expr = d }) parseA parseB parseC (\\s -> exprParser [] s) 0 of
        Good r _ -> r.modu
        Bad -> "failed"
"""
                            ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (String "module"))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "tcoTarget leak: non-TCO call inside tcoLoop" <|
            -- Module has tail-recursive `helper` and non-tail-recursive `transform`.
            -- `transform` calls a function ALSO named `helper` (different module).
            -- If tcoTarget leaks through non-TCO calls, Inner.helper would
            -- falsely trigger TailCall for Main.helper's tcoLoop.
            \_ ->
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnv env
                            [ """module Inner exposing (helper)

helper : Int -> { value : Int }
helper x = { value = x * 3 }
"""
                            , """module Main exposing (main)

import Inner

transform : Int -> Int
transform x =
    let r = Inner.helper x
    in r.value

helper : Int -> Int -> Int
helper n acc =
    if n <= 0 then
        acc
    else
        helper (n - 1) (acc + transform n)

main = helper 10 0
"""
                            ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 165))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "tail-recursive function with as-pattern destructuring in args" <|
            -- Models ParserFast.loopWhileSucceedsHelp which has:
            --   loopHelp ((Parser parseElement) as element) soFar ... =
            -- The as-pattern creates BOTH parseElement and element bindings.
            -- If TCO's tcoLoop doesn't preserve destructured bindings, the
            -- body fails when it tries to use parseElement.
            \_ ->
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnv env
                            [ """module Main exposing (main)

type Wrapper a = Wrapper (Int -> a)

loopHelp : Wrapper a -> Int -> (a -> Int -> Int) -> Int -> Int
loopHelp ((Wrapper unwrapped) as wrapper) acc reducer n =
    if n <= 0 then
        acc
    else
        loopHelp wrapper (reducer (unwrapped n) acc) reducer (n - 1)

main = loopHelp (Wrapper (\\x -> x * 2)) 0 (+) 100
"""
                            ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 10100))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "TCO tcoTarget name collision: same function name in two modules" <|
            -- Module Helper has `step : Int -> Int` (non-recursive).
            -- Module Main has tail-recursive `step` that calls Helper.step.
            -- tcoTarget = "step" must NOT match Helper.step, or TailCall
            -- would fire for Helper.step and corrupt the return value.
            \_ ->
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnv env
                            [ """module Helper exposing (step)

step : Int -> Int
step x = x * 10
"""
                            , """module Main exposing (main)

import Helper

step : Int -> Int -> Int
step n acc =
    if n <= 0 then
        acc
    else
        step (n - 1) (acc + Helper.step n)

main = step 5 0
"""
                            ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 150))

                    Err e ->
                        Expect.fail (Debug.toString e)
        ]


{-| Tests for let-function scoping: two let-defined functions with the same
name in different nested scopes must not collide.
-}
letFunctionScopingTests : Test
letFunctionScopingTests =
    describe "Let-function scoping (same name in nested scopes)"
        [ evalTestModule "two raise functions in nested let blocks"
            -- Models the Review.Rule pattern: createProjectVisitor has a `raise`
            -- that creates ProjectVisitor, and createModuleVisitor has a different
            -- `raise` that creates ModuleVisitor. Both are in the same module.
            -- The inner `raise` must shadow the outer in its own scope.
            """module Temp exposing (main)

type Visitor
    = ProjectVisitor Int
    | ModuleVisitor String

createProjectVisitor : Int -> Visitor
createProjectVisitor n =
    let
        raise x = ProjectVisitor x
    in
    raise n

createModuleVisitor : String -> Visitor
createModuleVisitor s =
    let
        raise x = ModuleVisitor x
    in
    raise s

main =
    case ( createProjectVisitor 42, createModuleVisitor "hello" ) of
        ( ProjectVisitor n, ModuleVisitor s ) ->
            String.fromInt n ++ " " ++ s

        _ ->
            "WRONG: got wrong visitor types"
"""
            String
            "42 hello"
        , evalTestModule "nested let blocks with same-named function used in closure"
            -- The closure returned from outerFn captures the outer `process`.
            -- The inner `process` in innerFn must not overwrite it.
            """module Temp exposing (main)

outerFn : Int -> (() -> Int)
outerFn x =
    let
        process n = n * 10
    in
    \\() -> process x

innerFn : Int -> Int
innerFn x =
    let
        process n = n + 1
    in
    process x

main =
    let
        closureFromOuter = outerFn 5
    in
    closureFromOuter () + innerFn 5
"""
            Int
            56
        , evalTestModule "three nested scopes with same let-function name"
            """module Temp exposing (main)

scope1 : Int
scope1 =
    let helper x = x * 2
    in helper 10

scope2 : Int
scope2 =
    let helper x = x + 100
    in helper 10

scope3 : Int
scope3 =
    let helper x = x - 5
    in helper 10

main = scope1 + scope2 + scope3
"""
            Int
            135
        ]
