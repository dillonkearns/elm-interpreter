module PerfTests exposing (suite)

import Eval
import Expect
import Test exposing (Test, describe, test)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Performance"
        [ listEqualityTests
        , tcoTests
        ]


{-| Test list equality at various sizes to find where perf degrades.
Each test evaluates: let size = N in List.range 0 size == List.range 0 size
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


{-| TCO tests: tail-recursive functions that would OOM without TCO.
These use custom recursive functions (not List.range) to test the
interpreter's TCO, not kernel shortcuts.
-}
tcoTests : Test
tcoTests =
    describe "Tail-call optimization"
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
