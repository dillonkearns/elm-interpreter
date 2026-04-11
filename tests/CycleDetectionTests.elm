module CycleDetectionTests exposing (suite)

{-| Tests for false positive cycle detection on parser-like recursion patterns.

The cycle detection fingerprints value structure but returns 0 for all
PartiallyApplied values. Parser combinators are PartiallyApplied closures
that carry position state internally. When a recursive function is called
repeatedly with the same parser combinator arguments (but at different
input positions), the fingerprints are identical and Category A falsely
triggers after 3 calls past depth 200.

Reference: .scratch/bug-infix-parser-infinite-recursion.md
-}

import Elm.Syntax.Expression as Expression
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import Types exposing (Error(..), Value(..))


suite : Test
suite =
    describe "Cycle detection false positives"
        [ functionArgRecursionTests
        , actualInfiniteRecursionTests
        ]


functionArgRecursionTests : Test
functionArgRecursionTests =
    describe "Deep recursion with function arguments should not false-positive"
        [ test "recursive function with closure argument (counter hidden in closure)" <|
            \_ ->
                -- step takes two function arguments. Both are PartiallyApplied
                -- and fingerprint to 0. The counter is inside getN's closure.
                -- Terminates after 250 calls, but cycle detection sees identical
                -- fingerprints after depth 200 and falsely triggers Category A.
                Eval.Module.evalProject
                    [ """module Main exposing (main)

step : (() -> Int) -> (() -> Int -> Int) -> Int
step getN apply =
    if getN () <= 0 then
        0
    else
        step (\\() -> getN () - 1) apply + apply () 1

main = step (\\() -> 250) (\\() n -> n)
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> expectOk
        , test "recursive function with single function argument wrapping state" <|
            \_ ->
                -- Each call wraps f in a new lambda. All are PartiallyApplied → fp 0.
                -- Should terminate when the accumulated count exceeds 250.
                Eval.Module.evalProject
                    [ """module Main exposing (main)

countUp : (() -> Int) -> Int
countUp f =
    if f () >= 250 then
        f ()
    else
        countUp (\\() -> f () + 1)

main = countUp (\\() -> 0)
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> expectOk
        , test "indirect recursion with unchanging function argument (parser-like)" <|
            \_ ->
                -- Models the parser combinator pattern: a function is passed
                -- through multiple levels of indirect recursion. The function
                -- argument doesn't change, but a state argument (hidden in
                -- the recursion structure) tracks progress.
                Eval.Module.evalProject
                    [ """module Main exposing (main)

type alias State = { pos : Int, input : String }

consume : (Char -> Bool) -> State -> State
consume pred state =
    case String.uncons (String.dropLeft state.pos state.input) of
        Just ( c, _ ) ->
            if pred c then
                { state | pos = state.pos + 1 }
            else
                state
        Nothing ->
            state

parseMany : (Char -> Bool) -> State -> List Char -> ( List Char, State )
parseMany pred state acc =
    let
        next = consume pred state
    in
    if next.pos == state.pos then
        ( List.reverse acc, state )
    else
        case String.uncons (String.dropLeft state.pos state.input) of
            Just ( c, _ ) ->
                parseMany pred next (c :: acc)
            Nothing ->
                ( List.reverse acc, state )

main =
    let
        input = String.repeat 300 "a"
        state = { pos = 0, input = input }
        ( result, _ ) = parseMany Char.isAlpha state []
    in
    List.length result
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> expectOk
        , test "List.map2 on 10000-element list does not false-positive as infinite recursion" <|
            \_ ->
                -- `transpose` uses `List.map2 (::)`, and `elm-core`'s `List.map2`
                -- internally loops via `Elm.Kernel.List.go`, which is routed
                -- through `tcoLoop`. The per-value fingerprint used to bucket
                -- all lists of 4+ elements into the same hash, so over thousands
                -- of iterations the TCO cycle detector saw "identical fingerprint
                -- and size" for ~800 consecutive samples and falsely flagged
                -- `go` as infinite recursion. Reproduced by running
                -- `List.Extra.transpose [ List.repeat 10000 1 ]` via the full
                -- test runner pipeline on elmcraft/core-extra's ListTests.
                Eval.Module.evalProject
                    [ """module Main exposing (main)

main =
    List.length (List.map2 (\\a b -> a + b) (List.range 1 10000) (List.range 1 10000))
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 10000))
        , test "higher-order fold with function argument over long list" <|
            \_ ->
                -- List.foldl passes the same function at every step.
                -- With a 300-element list, this recurses 300+ times.
                -- The accumulator (Int) changes, so the combined fingerprint
                -- changes, but this tests that the pattern is safe.
                Eval.Module.evalProject
                    [ """module Main exposing (main)

main = List.foldl (\\x acc -> acc + x) 0 (List.range 1 300)
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Int 45150))
        , test "mutual recursion where all args are functions" <|
            \_ ->
                -- Two functions calling each other, passing function arguments.
                -- All args are PartiallyApplied → fp 0.
                Eval.Module.evalProject
                    [ """module Main exposing (main)

even : (() -> Int) -> Bool
even getN =
    if getN () <= 0 then True
    else odd (\\() -> getN () - 1)

odd : (() -> Int) -> Bool
odd getN =
    if getN () <= 0 then False
    else even (\\() -> getN () - 1)

main = even (\\() -> 250)
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> Expect.equal (Ok (Bool True))
        ]


actualInfiniteRecursionTests : Test
actualInfiniteRecursionTests =
    describe "Actual infinite recursion should still be detected"
        [ test "simple infinite recursion (f x = f x) is caught" <|
            \_ ->
                Eval.Module.evalProject
                    [ """module Main exposing (main)

loop x = loop x

main = loop 0
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> expectInfiniteRecursion
        , test "infinite recursion with unchanged function arg is caught" <|
            \_ ->
                -- The function arg doesn't change AND there's no progress.
                -- This IS a real infinite loop.
                Eval.Module.evalProject
                    [ """module Main exposing (main)

loop f = loop f

main = loop (\\x -> x)
"""
                    ]
                    (Expression.FunctionOrValue [] "main")
                    |> expectInfiniteRecursion
        ]



-- Helpers


expectOk : Result Error Value -> Expect.Expectation
expectOk result =
    case result of
        Ok _ ->
            Expect.pass

        Err (EvalError e) ->
            Expect.fail <|
                "Expected Ok, got error: "
                    ++ Debug.toString e.error
                    ++ " [module: "
                    ++ String.join "." e.currentModule
                    ++ "]"

        Err e ->
            Expect.fail ("Expected Ok, got: " ++ Debug.toString e)


expectInfiniteRecursion : Result Error Value -> Expect.Expectation
expectInfiniteRecursion result =
    case result of
        Err (EvalError e) ->
            case e.error of
                Types.TypeError msg ->
                    if String.contains "Infinite recursion" msg || String.contains "infinite recursion" msg then
                        Expect.pass

                    else
                        Expect.fail ("Expected infinite recursion error, got: " ++ msg)

                _ ->
                    Expect.fail ("Expected TypeError, got: " ++ Debug.toString e.error)

        Err e ->
            Expect.fail ("Expected EvalError, got: " ++ Debug.toString e)

        Ok v ->
            Expect.fail ("Expected error, got Ok: " ++ Debug.toString v)
