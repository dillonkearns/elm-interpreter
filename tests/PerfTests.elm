module PerfTests exposing (suite)

import Elm.Syntax.Expression as Expression
import Eval
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Performance"
        [ listEqualityTests
        , tcoCorrectnessTests
        , tcoProofTests
        ]


{-| Test list equality at various sizes to find where perf degrades.
-}
listEqualityTests : Test
listEqualityTests =
    describe "List equality scaling"
        [ listEqTest 100
        , listEqTest 500
        , listEqTest 1000
        , listEqTest 2000
        ]


listEqTest : Int -> Test
listEqTest n =
    test ("List.range 0 " ++ String.fromInt n ++ " == List.range 0 " ++ String.fromInt n) <|
        \_ ->
            Eval.eval
                ("let size = "
                    ++ String.fromInt n
                    ++ " in List.range 0 size == List.range 0 size"
                )
                |> Expect.equal (Ok (Bool True))


{-| Correctness tests for tail-recursive functions.
-}
tcoCorrectnessTests : Test
tcoCorrectnessTests =
    describe "Tail-call optimization correctness"
        [ test "simple countdown 10000" <|
            \_ ->
                Eval.eval
                    "let countdown n = if n <= 0 then 0 else countdown (n - 1) in countdown 10000"
                    |> Expect.equal (Ok (Int 0))
        , test "accumulator sum 10000" <|
            \_ ->
                Eval.eval
                    "let sum acc n = if n <= 0 then acc else sum (acc + n) (n - 1) in sum 0 10000"
                    |> Expect.equal (Ok (Int 50005000))
        , test "list build via cons 5000" <|
            \_ ->
                Eval.eval
                    "let build acc n = if n <= 0 then acc else build (n :: acc) (n - 1) in List.length (build [] 5000)"
                    |> Expect.equal (Ok (Int 5000))
        , test "countdown 50000" <|
            \_ ->
                Eval.eval
                    "let countdown n = if n <= 0 then 0 else countdown (n - 1) in countdown 50000"
                    |> Expect.equal (Ok (Int 0))
        , test "accumulator build 10000" <|
            \_ ->
                Eval.eval
                    "let build acc n = if n <= 0 then acc else build (n :: acc) (n - 1) in List.length (build [] 10000)"
                    |> Expect.equal (Ok (Int 10000))
        , test "mutual recursion (non-tail) still works" <|
            \_ ->
                Eval.eval
                    "let even n = if n == 0 then True else odd (n - 1)\n    odd n = if n == 0 then False else even (n - 1)\nin even 100"
                    |> Expect.equal (Ok (Bool True))
        ]


{-| TCO PROOF tests: these prove TCO is actually engaging, not just that
results are correct. The technique: use evalWithMaxSteps with a step limit
that is generous enough for TCO (which uses ~0 trampoline steps per iteration
since tcoLoop calls evalExpression directly) but far too tight for the
non-TCO trampoline path (which uses ~5-10 trampoline steps per iteration).

If TCO is NOT engaging, these tests FAIL with "Step limit exceeded".
If TCO IS engaging, the step budget is sufficient and they pass.
-}
tcoProofTests : Test
tcoProofTests =
    describe "TCO proof (step-budget bounded)"
        [ test "module-level countdown 10000 within 5000 steps" <|
            -- Without TCO: ~60-80k trampoline steps for 10000 iterations
            -- With TCO: tcoLoop bypasses the trampoline entirely, ~0 steps
            \_ ->
                let
                    source =
                        "module T exposing (main)\ncountdown n = if n <= 0 then 0 else countdown (n - 1)\nmain = countdown 10000"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 5000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 0))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "module-level accumulator sum 5000 within 5000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmySum acc n = if n <= 0 then acc else mySum (acc + n) (n - 1)\nmain = mySum 0 5000"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 5000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 12502500))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "module-level list build 3000 within 5000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nbuild acc n = if n <= 0 then acc else build (n :: acc) (n - 1)\nmain = List.length (build [] 3000)"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 5000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 3000))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "case-based tail recursion within step budget" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nloop xs acc = case xs of\n  [] -> acc\n  x :: rest -> loop rest (acc + x)\nmain = loop [1,2,3,4,5,6,7,8,9,10] 0"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 5000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 55))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "non-tail-recursive List.map still works (not broken by TCO)" <|
            \_ ->
                Eval.eval
                    "List.map (\\x -> x * 2) [1, 2, 3, 4, 5]"
                    |> Expect.equal (Ok (List [ Int 2, Int 4, Int 6, Int 8, Int 10 ]))
        , test "non-tail-recursive fib still works" <|
            \_ ->
                Eval.eval
                    "let fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10"
                    |> Expect.equal (Ok (Int 55))
        ]
