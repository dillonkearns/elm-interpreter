module Kernel.List exposing (all, any, append, concatMap, filter, filterMap, foldl, foldr, indexedMap, map, range, sortBy, sortWith)

{-| Native kernel implementations for List sorting.

The generated AST merge sort is not stable because its split function reverses
element order. These native implementations ensure stability by using a monadic
insertion sort that correctly preserves the relative order of equal elements.

-}

import EvalResult
import Kernel.Utils
import Types exposing (Eval, EvalResult(..), Value(..))
import Value


{-| Kernel List.map: applies a function to every element.
Iterates in host Elm, calling the interpreter function per element.
-}
map : (Value -> Eval Value) -> List Value -> Eval (List Value)
map f xs cfg env =
    mapHelp f xs [] cfg env


mapHelp : (Value -> Eval Value) -> List Value -> List Value -> Eval (List Value)
mapHelp f remaining acc cfg env =
    case remaining of
        [] ->
            EvalResult.succeed (List.reverse acc)

        x :: rest ->
            let
                innerCfg =
                    { cfg | tcoTarget = Nothing }
            in
            -- Direct tail call so Elm TCO compiles to a while loop.
            case EvalResult.toResult (f x innerCfg env) of
                Err e ->
                    EvErr e

                Ok mapped ->
                    mapHelp f rest (mapped :: acc) cfg env


{-| Kernel List.foldl: reduces a list from the left.
-}
foldl : (Value -> Eval (Value -> Eval Value)) -> Value -> List Value -> Eval Value
foldl f init xs cfg env =
    foldlHelp f init xs cfg env


foldlHelp : (Value -> Eval (Value -> Eval Value)) -> Value -> List Value -> Eval Value
foldlHelp f acc remaining cfg env =
    case remaining of
        [] ->
            EvalResult.succeed acc

        x :: rest ->
            let
                -- Clear tcoTarget to prevent TailCall signals from escaping
                -- the kernel loop. Without this, a function matching tcoTarget
                -- called inside the fold would fire TailCall, which toResult
                -- would catch as an error and propagate up incorrectly.
                innerCfg =
                    { cfg | tcoTarget = Nothing }
            in
            -- Evaluate f x, then its partial application on acc, with the
            -- recursive call in direct tail position so Elm TCO compiles
            -- this whole body to a while loop. Yields are forwarded as-is
            -- and can still drive the rest of the fold via resume.
            case EvalResult.toResult (f x innerCfg env) of
                Err e ->
                    EvErr e

                Ok g ->
                    case EvalResult.toResult (g acc innerCfg env) of
                        Err e ->
                            EvErr e

                        Ok newAcc ->
                            foldlHelp f newAcc rest cfg env


{-| Kernel List.filter: keeps elements where the predicate returns True.
-}
filter : (Value -> Eval Bool) -> List Value -> Eval (List Value)
filter pred xs cfg env =
    filterHelp pred xs [] cfg env


filterHelp : (Value -> Eval Bool) -> List Value -> List Value -> Eval (List Value)
filterHelp pred remaining acc cfg env =
    let
        innerCfg =
            { cfg | tcoTarget = Nothing }
    in
    case remaining of
        [] ->
            EvalResult.succeed (List.reverse acc)

        x :: rest ->
            case EvalResult.toResult (pred x innerCfg env) of
                Err e ->
                    EvErr e

                Ok True ->
                    filterHelp pred rest (x :: acc) cfg env

                Ok False ->
                    filterHelp pred rest acc cfg env


{-| Direct implementation of List.range. Produces the list without going
through the interpreter's trampoline, avoiding O(N) Env allocations.
-}
range : Int -> Int -> List Value
range lo hi =
    rangeHelp lo hi []


rangeHelp : Int -> Int -> List Value -> List Value
rangeHelp lo hi acc =
    if lo <= hi then
        rangeHelp lo (hi - 1) (Int hi :: acc)

    else
        acc


{-| Kernel List.foldr: reduces a list from the right.
Reverses the list, then folds left. Tail-recursive.
-}
foldr : (Value -> Eval (Value -> Eval Value)) -> Value -> List Value -> Eval Value
foldr f init xs cfg env =
    foldlHelp f init (List.reverse xs) cfg env


{-| Kernel List.append: concatenates two lists.
Pure — no interpreter callback needed.
-}
append : List Value -> List Value -> List Value
append xs ys =
    case ys of
        [] ->
            xs

        _ ->
            List.foldr (::) ys xs


{-| Stable sortBy: wraps the toComparable function into a comparison
and delegates to sortWith.
-}
sortBy : (Value -> Eval Value) -> List Value -> Eval (List Value)
sortBy toComparable xs cfg env =
    let
        compareFunc : Value -> Eval (Value -> Eval Order)
        compareFunc a cfg2 env2 =
            toComparable a cfg2 env2
                |> EvalResult.map
                    (\compA ->
                        \b cfg3 env3 ->
                            toComparable b cfg3 env3
                                |> EvalResult.andThen
                                    (\compB ->
                                        Kernel.Utils.compare compA compB cfg3 env3
                                    )
                    )
    in
    sortWith compareFunc xs cfg env


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


{-| Kernel List.concatMap: map then flatten. Iterates in host Elm.
-}
concatMap : (Value -> Eval (List Value)) -> List Value -> Eval (List Value)
concatMap f xs cfg env =
    concatMapHelp f xs [] cfg env


concatMapHelp : (Value -> Eval (List Value)) -> List Value -> List Value -> Eval (List Value)
concatMapHelp f remaining acc cfg env =
    let
        innerCfg =
            { cfg | tcoTarget = Nothing }
    in
    case remaining of
        [] ->
            EvalResult.succeed (List.reverse acc)

        x :: rest ->
            case f x innerCfg env of
                EvYield tag payload resume ->
                    EvYield tag payload
                        (\v ->
                            case EvalResult.toResult (resume v) of
                                Ok mapped ->
                                    concatMapHelp f rest (List.foldl (::) acc mapped) cfg env

                                Err e ->
                                    EvErr e
                        )

                fxResult ->
                    case EvalResult.toResult fxResult of
                        Ok mapped ->
                            concatMapHelp f rest (List.foldl (::) acc mapped) cfg env

                        Err e ->
                            EvErr e


{-| Kernel List.filterMap: map with Maybe filter. Iterates in host Elm.
-}
filterMap : (Value -> Eval Value) -> List Value -> Eval (List Value)
filterMap f xs cfg env =
    filterMapHelp f xs [] cfg env


filterMapHelp : (Value -> Eval Value) -> List Value -> List Value -> Eval (List Value)
filterMapHelp f remaining acc cfg env =
    let
        innerCfg =
            { cfg | tcoTarget = Nothing }
    in
    case remaining of
        [] ->
            EvalResult.succeed (List.reverse acc)

        x :: rest ->
            case EvalResult.toResult (f x innerCfg env) of
                Ok result ->
                    case result of
                        Custom { name } [ value ] ->
                            if name == "Just" then
                                filterMapHelp f rest (value :: acc) cfg env

                            else
                                filterMapHelp f rest acc cfg env

                        Custom { name } [] ->
                            if name == "Nothing" then
                                filterMapHelp f rest acc cfg env

                            else
                                filterMapHelp f rest acc cfg env

                        _ ->
                            filterMapHelp f rest acc cfg env

                Err e ->
                    EvErr e


{-| Kernel List.indexedMap: map with index. Iterates in host Elm.
-}
indexedMap : (Value -> Eval (Value -> Eval Value)) -> List Value -> Eval (List Value)
indexedMap f xs cfg env =
    indexedMapHelp f 0 xs [] cfg env


indexedMapHelp : (Value -> Eval (Value -> Eval Value)) -> Int -> List Value -> List Value -> Eval (List Value)
indexedMapHelp f index remaining acc cfg env =
    let
        innerCfg =
            { cfg | tcoTarget = Nothing }
    in
    case remaining of
        [] ->
            EvalResult.succeed (List.reverse acc)

        x :: rest ->
            case EvalResult.toResult (f (Int index) innerCfg env) of
                Ok g ->
                    case EvalResult.toResult (g x innerCfg env) of
                        Ok mapped ->
                            indexedMapHelp f (index + 1) rest (mapped :: acc) cfg env

                        Err e ->
                            EvErr e

                Err e ->
                    EvErr e


{-| Kernel List.any: short-circuiting search. Iterates in host Elm.
-}
any : (Value -> Eval Bool) -> List Value -> Eval Bool
any pred xs cfg env =
    let
        innerCfg =
            { cfg | tcoTarget = Nothing }
    in
    case xs of
        [] ->
            EvalResult.succeed False

        x :: rest ->
            case EvalResult.toResult (pred x innerCfg env) of
                Ok True ->
                    EvalResult.succeed True

                Ok False ->
                    any pred rest cfg env

                Err e ->
                    EvErr e


{-| Kernel List.all: short-circuiting check. Iterates in host Elm.
-}
all : (Value -> Eval Bool) -> List Value -> Eval Bool
all pred xs cfg env =
    let
        innerCfg =
            { cfg | tcoTarget = Nothing }
    in
    case xs of
        [] ->
            EvalResult.succeed True

        x :: rest ->
            case EvalResult.toResult (pred x innerCfg env) of
                Ok True ->
                    all pred rest cfg env

                Ok False ->
                    EvalResult.succeed False

                Err e ->
                    EvErr e
