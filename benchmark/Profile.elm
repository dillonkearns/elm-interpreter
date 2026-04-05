port module Profile exposing (main)

{-| Heavy workload for CPU profiling.
Compile: elm make benchmark/Profile.elm --optimize --output=benchmark/profile.js
Profile: node --prof benchmark/run-profile.js
Process: node --prof-process isolate-*.log > profile.txt
-}

import Elm.Syntax.Expression as Expression
import Eval.Module
import Json.Encode as Encode
import Platform
import Types exposing (Value(..))


port reportDone : String -> Cmd msg


main : Program () () ()
main =
    Platform.worker
        { init =
            \_ ->
                let
                    result =
                        runHeavyWorkload ()
                in
                ( (), reportDone result )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


runHeavyWorkload : () -> String
runHeavyWorkload () =
    let
        results =
            [ ( "dict-heavy", dictHeavy () )
            , ( "list-heavy", listHeavy () )
            , ( "string-heavy", stringHeavy () )
            , ( "nested-closures", nestedClosures () )
            , ( "pattern-matching", patternMatching () )
            , ( "recursive-data", recursiveData () )
            ]
    in
    results
        |> List.map
            (\( name, r ) ->
                case r of
                    Ok v ->
                        name ++ ": OK (" ++ v ++ ")"

                    Err e ->
                        name ++ ": ERR (" ++ e ++ ")"
            )
        |> String.join "\n"


eval : String -> Result String String
eval source =
    case Eval.Module.evalProject [ source ] (Expression.FunctionOrValue [] "main") of
        Ok (Int n) ->
            Ok (String.fromInt n)

        Ok (String s) ->
            Ok s

        Ok (Bool b) ->
            Ok
                (if b then
                    "True"

                 else
                    "False"
                )

        Ok _ ->
            Ok "ok"

        Err _ ->
            Err "error"


dictHeavy : () -> Result String String
dictHeavy () =
    eval """module T exposing (main)
import Dict

buildDict n =
    List.foldl (\\i acc -> Dict.insert (String.fromInt i) i acc) Dict.empty (List.range 1 n)

main =
    let
        d = buildDict 3000
        d2 = Dict.map (\\_ v -> v * 2) d
        d3 = Dict.filter (\\_ v -> modBy 3 v == 0) d2
        sum1 = Dict.foldl (\\_ v acc -> acc + v) 0 d3
        d4 = Dict.map (\\k v -> v + String.length k) d
        sum2 = Dict.foldl (\\_ v acc -> acc + v) 0 d4
    in
    sum1 + sum2
"""


listHeavy : () -> Result String String
listHeavy () =
    eval """module T exposing (main)

main =
    let
        xs = List.range 1 10000
        mapped = List.map (\\n -> n * 3 + 1) xs
        filtered = List.filter (\\n -> modBy 7 n /= 0) mapped
        concatMapped = List.concatMap (\\n -> [n, n + 1]) (List.take 5000 filtered)
        indexed = List.indexedMap (\\i x -> i + x) concatMapped
    in
    List.foldl (+) 0 indexed
"""


stringHeavy : () -> Result String String
stringHeavy () =
    eval """module T exposing (main)

main =
    let
        strs = List.map (\\n -> String.fromInt n ++ "-item") (List.range 1 3000)
        joined = String.join ", " strs
        parts = String.split ", " joined
        filtered = List.filter (\\s -> String.contains "5" s) parts
    in
    List.length filtered
"""


nestedClosures : () -> Result String String
nestedClosures () =
    eval """module T exposing (main)

apply f x = f x
compose f g x = f (g x)
flip f a b = f b a

main =
    let
        add n = (\\x -> x + n)
        mul n = (\\x -> x * n)
        pipeline =
            List.map (\\n -> compose (add n) (mul n)) (List.range 1 3000)
        results =
            List.map (\\f -> f 10) pipeline
    in
    List.foldl (+) 0 results
"""


patternMatching : () -> Result String String
patternMatching () =
    eval """module T exposing (main)

type Tree = Leaf Int | Branch Tree Tree

buildTree depth =
    if depth <= 0 then
        Leaf 1
    else
        Branch (buildTree (depth - 1)) (buildTree (depth - 1))

sumTree tree =
    case tree of
        Leaf n -> n
        Branch left right -> sumTree left + sumTree right

main = sumTree (buildTree 12)
"""


recursiveData : () -> Result String String
recursiveData () =
    eval """module T exposing (main)

type Expr
    = Num Int
    | Add Expr Expr
    | Mul Expr Expr

eval expr =
    case expr of
        Num n -> n
        Add a b -> eval a + eval b
        Mul a b -> eval a * eval b

buildExpr depth =
    if depth <= 0 then
        Num 1
    else
        Add (Mul (buildExpr (depth - 1)) (Num 2))
            (Add (buildExpr (depth - 1)) (Num 3))

main = eval (buildExpr 10)
"""
