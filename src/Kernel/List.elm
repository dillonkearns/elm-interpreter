module Kernel.List exposing (sortWith)

{-| Native kernel implementation of List.sortWith using a stable insertion sort.

The generated AST merge sort is not stable because its split function reverses
element order. This native implementation ensures stability by using a monadic
insertion sort that correctly preserves the relative order of equal elements.

-}

import EvalResult
import Rope exposing (Rope)
import Types exposing (Eval, Value)


{-| Stable sort threading Eval through comparisons.
-}
sortWith : (Value -> Eval (Value -> Eval Order)) -> List Value -> Eval (List Value)
sortWith compare xs cfg env =
    insertionSort compare [] xs cfg env


{-| Stable insertion sort threading Eval through comparisons.

Elements are inserted one at a time (left-to-right) from the input list into
the sorted accumulator. For equal elements, the new element is placed after
existing equal elements, preserving original order.

-}
insertionSort : (Value -> Eval (Value -> Eval Order)) -> List Value -> List Value -> Eval (List Value)
insertionSort compare sorted unsorted cfg env =
    case unsorted of
        [] ->
            EvalResult.succeed sorted

        x :: rest ->
            case insert compare x sorted cfg env of
                ( Err e, trees, logs ) ->
                    ( Err e, trees, logs )

                ( Ok newSorted, trees, logs ) ->
                    let
                        ( result, trees2, logs2 ) =
                            insertionSort compare newSorted rest cfg env
                    in
                    ( result
                    , EvalResult.appendRopes trees trees2
                    , EvalResult.appendRopes logs logs2
                    )


{-| Insert a value into a sorted list at the correct position.

  - LT: place x before y (x is smaller)
  - EQ or GT: skip past y and keep looking (preserves stability for EQ)

-}
insert : (Value -> Eval (Value -> Eval Order)) -> Value -> List Value -> Eval (List Value)
insert compare x sorted cfg env =
    case sorted of
        [] ->
            EvalResult.succeed [ x ]

        y :: rest ->
            case compare x cfg env of
                ( Err e, trees, logs ) ->
                    ( Err e, trees, logs )

                ( Ok compareWithY, trees1, logs1 ) ->
                    case compareWithY y cfg env of
                        ( Err e, trees2, logs2 ) ->
                            ( Err e
                            , EvalResult.appendRopes trees1 trees2
                            , EvalResult.appendRopes logs1 logs2
                            )

                        ( Ok ord, trees2, logs2 ) ->
                            let
                                mergedTrees =
                                    EvalResult.appendRopes trees1 trees2

                                mergedLogs =
                                    EvalResult.appendRopes logs1 logs2
                            in
                            case ord of
                                LT ->
                                    -- x < y: x goes before y
                                    ( Ok (x :: y :: rest)
                                    , mergedTrees
                                    , mergedLogs
                                    )

                                _ ->
                                    -- EQ or GT: skip past y, keep looking
                                    let
                                        ( result, trees3, logs3 ) =
                                            insert compare x rest cfg env
                                    in
                                    ( Result.map (\inserted -> y :: inserted) result
                                    , EvalResult.appendRopes mergedTrees trees3
                                    , EvalResult.appendRopes mergedLogs logs3
                                    )
