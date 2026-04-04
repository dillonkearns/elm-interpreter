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
        , test "countdown 10000" <|
            \_ ->
                Eval.eval
                    "let countdown n = if n <= 0 then 0 else countdown (n - 1) in countdown 10000"
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
        [ test "module-level countdown 100000 within 110000 steps" <|
            -- Without TCO: ~200k+ trampoline steps for 100k iterations
            -- With TCO: 100k tcoLoop iterations + small startup
            \_ ->
                let
                    source =
                        "module T exposing (main)\ncountdown n = if n <= 0 then 0 else countdown (n - 1)\nmain = countdown 100000"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 110000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 0))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "module-level accumulator sum 10000 within 55000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmySum acc n = if n <= 0 then acc else mySum (acc + n) (n - 1)\nmain = mySum 0 10000"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 55000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 50005000))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "module-level list build 30000 within 35000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nbuild acc n = if n <= 0 then acc else build (n :: acc) (n - 1)\nmain = List.length (build [] 30000)"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 35000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 30000))

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

        -- === Let-defined self-recursion (TCO proof) ===
        , test "let-defined countdown 100000 within 110000 steps" <|
            -- Without TCO: needs ~200k+ trampoline steps (2+ per iteration)
            -- With TCO: needs 100k tcoLoop iterations + small trampoline startup
            -- 110000 proves TCO is engaging (impossible with 2+ steps/iter)
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = let countdown n = if n <= 0 then 0 else countdown (n - 1) in countdown 100000"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 110000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 0))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "let-defined accumulator 10000 within 55000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = let mySum acc n = if n <= 0 then acc else mySum (acc + n) (n - 1) in mySum 0 10000"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 55000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 50005000))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "let non-tail-recursive fib still correct" <|
            \_ ->
                Eval.evalWithMaxSteps (Just 10000)
                    "let fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10"
                    |> Expect.equal (Ok (Int 55))

        -- === Mutual recursion (correctness, not TCO proof) ===
        -- Mutual recursion doesn't get TCO yet (needs call-group detection).
        -- These verify correctness at moderate scale.
        , test "mutual recursion isEven/isOdd correctness (even)" <|
            \_ ->
                Eval.eval
                    "let isEven n = if n == 0 then True else isOdd (n - 1)\n    isOdd n = if n == 0 then False else isEven (n - 1)\nin isEven 100"
                    |> Expect.equal (Ok (Bool True))
        , test "mutual recursion isEven/isOdd correctness (odd)" <|
            \_ ->
                Eval.eval
                    "let isEven n = if n == 0 then True else isOdd (n - 1)\n    isOdd n = if n == 0 then False else isEven (n - 1)\nin isEven 101"
                    |> Expect.equal (Ok (Bool False))

        -- === TCO with maxSteps=Nothing (via Eval.eval) ===
        , test "Eval.eval list build 10000 (needs TCO for memory)" <|
            -- Eval.eval uses trace=True, maxSteps=Nothing.
            -- Without TCO, building a 10k-element list through the trampoline
            -- would be extremely slow due to memory pressure.
            -- With TCO, tcoLoop keeps memory bounded.
            \_ ->
                Eval.eval
                    "let build acc n = if n <= 0 then List.length acc else build (n :: acc) (n - 1) in build [] 10000"
                    |> Expect.equal (Ok (Int 10000))

        -- === Kernel list operations proof ===
        -- Without kernel: List.map/foldl/filter go through foldr/foldl which
        -- use the trampoline. With kernel: direct host-Elm iteration.
        -- Step budgets are tight enough to prove kernel engagement.
        , test "List.map 10000 within 15000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.length (List.map (\\x -> x + 1) (List.range 0 10000))"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 15000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 10001))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "List.foldl 10000 within 15000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.foldl (+) 0 (List.range 0 10000)"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 15000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 50005000))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "List.filter 10000 within 15000 steps" <|
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.length (List.filter (\\x -> modBy 2 x == 0) (List.range 0 10000))"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 15000)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 5001))

                    Err e ->
                        Expect.fail (Debug.toString e)

        -- === Kernel List.length, List.foldr, List.append proof ===
        -- Step budgets prove kernel engagement (too tight for interpreted path)
        , test "List.length 100000 within 100 steps" <|
            -- List.length uses kernel foldl — uses ~0 trampoline steps for iteration
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.length (List.range 0 100000)"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 100)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 100001))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "List.foldr 50000 within 500 steps" <|
            -- Without kernel foldr: 50k elements via foldrHelper needs ~12500 TCO iterations
            -- With kernel: ~0 trampoline steps
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.foldr (+) 0 (List.range 0 50000)"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 500)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 1250025000))

                    Err e ->
                        Expect.fail (Debug.toString e)
        , test "List.append 50000+50000 within 500 steps" <|
            -- Without kernel: foldr over 50k elements needs many steps
            -- With kernel: direct list concatenation, ~0 trampoline steps
            \_ ->
                let
                    source =
                        "module T exposing (main)\nmain = List.length (List.append (List.range 0 50000) (List.range 50001 100000))"
                in
                case Eval.Module.buildProjectEnv [] of
                    Ok env ->
                        Eval.Module.evalWithEnvAndLimit (Just 500)
                            env
                            [ source ]
                            (Expression.FunctionOrValue [] "main")
                            |> Expect.equal (Ok (Int 100001))

                    Err e ->
                        Expect.fail (Debug.toString e)

        -- === Pipe operators should NOT get TCO (per Elm compiler) ===
        , test "pipe operator is not tail-optimized but still correct" <|
            \_ ->
                Eval.eval
                    "let f n = if n <= 0 then 0 else (n - 1) |> f in f 100"
                    |> Expect.equal (Ok (Int 0))
        ]
