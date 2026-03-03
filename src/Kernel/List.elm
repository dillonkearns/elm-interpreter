module Kernel.List exposing (sortWith)

{-| Native kernel implementation of List.sortWith using a stable insertion sort.

The generated AST merge sort is not stable because its split function reverses
element order. This native implementation ensures stability by using a monadic
insertion sort that correctly preserves the relative order of equal elements.

-}

import EvalResult
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
            insert compare x sorted cfg env
                |> EvalResult.andThen (\newSorted -> insertionSort compare newSorted rest cfg env)


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
            compare x cfg env
                |> EvalResult.andThen (\compareWithY -> compareWithY y cfg env)
                |> EvalResult.andThen
                    (\ord ->
                        case ord of
                            LT ->
                                -- x < y: x goes before y
                                EvalResult.succeed (x :: y :: rest)

                            _ ->
                                -- EQ or GT: skip past y, keep looking
                                insert compare x rest cfg env
                                    |> EvalResult.map (\inserted -> y :: inserted)
                    )
