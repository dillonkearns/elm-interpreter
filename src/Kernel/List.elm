module Kernel.List exposing (all, any, append, concatMap, filter, filterMap, foldl, foldr, indexedMap, map, range, sortBy, sortWith)

{-| Native kernel implementations for List sorting.

The generated AST merge sort is not stable because its split function reverses
element order. These native implementations ensure stability by using a monadic
insertion sort that correctly preserves the relative order of equal elements.

-}

import EvalResult
import Kernel.Utils
import Rope
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
                innerCfg : Types.Config
                innerCfg =
                    { cfg | tcoTarget = Nothing }
            in
            -- Fast path: `f x` produces EvOk, keep the TCO'd while loop
            -- alive. Anything else (yields, traces, memo ops, errors) is
            -- handed off to a helper that propagates the effect correctly
            -- and re-enters the main loop on resume.
            case f x innerCfg env of
                EvOk mapped ->
                    mapHelp f rest (mapped :: acc) cfg env

                other ->
                    mapResumeAfterFx f rest acc cfg env other


{-| Continuation used after `f x` returns something other than `EvOk` in
`mapHelp`. Propagates EvYield / EvMemoLookup / EvMemoStore / EvOkTrace /
EvErrTrace / EvErr while threading the in-progress accumulator so the
map can continue once the suspended computation resolves.
-}
mapResumeAfterFx :
    (Value -> Eval Value)
    -> List Value
    -> List Value
    -> Types.Config
    -> Types.Env
    -> EvalResult Value
    -> EvalResult (List Value)
mapResumeAfterFx f rest acc cfg env fxResult =
    case fxResult of
        EvOk mapped ->
            mapHelp f rest (mapped :: acc) cfg env

        EvErr e ->
            EvErr e

        EvOkTrace mapped calls logs ->
            attachTraceList calls logs (mapHelp f rest (mapped :: acc) cfg env)

        EvErrTrace e calls logs ->
            EvErrTrace e calls logs

        EvYield tag payload resume ->
            EvYield tag payload
                (\v -> mapResumeAfterFx f rest acc cfg env (resume v))

        EvMemoLookup payload resume ->
            EvMemoLookup payload
                (\v -> mapResumeAfterFx f rest acc cfg env (resume v))

        EvMemoStore payload next ->
            EvMemoStore payload
                (mapResumeAfterFx f rest acc cfg env next)

        EvOkCoverage mapped coverageSet ->
            EvalResult.mergeCoverageInto coverageSet (mapHelp f rest (mapped :: acc) cfg env)

        EvErrCoverage e coverageSet ->
            EvErrCoverage e coverageSet


{-| Like `attachTrace` but for `EvalResult (List Value)` — used inside
`mapResumeAfterFx` where the result type is a list, not a single value.
-}
attachTraceList : Rope.Rope Types.CallTree -> Rope.Rope String -> EvalResult (List Value) -> EvalResult (List Value)
attachTraceList prevCalls prevLogs result =
    case result of
        EvOk vs ->
            EvOkTrace vs prevCalls prevLogs

        EvErr e ->
            EvErrTrace e prevCalls prevLogs

        EvOkTrace vs calls logs ->
            EvOkTrace vs (Rope.appendTo prevCalls calls) (Rope.appendTo prevLogs logs)

        EvErrTrace e calls logs ->
            EvErrTrace e (Rope.appendTo prevCalls calls) (Rope.appendTo prevLogs logs)

        EvYield tag payload resume ->
            EvYield tag payload (\v -> attachTraceList prevCalls prevLogs (resume v))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\v -> attachTraceList prevCalls prevLogs (resume v))

        EvMemoStore payload next ->
            EvMemoStore payload (attachTraceList prevCalls prevLogs next)

        EvOkCoverage vs _ ->
            EvOkTrace vs prevCalls prevLogs

        EvErrCoverage e _ ->
            EvErrTrace e prevCalls prevLogs


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
                innerCfg : Types.Config
                innerCfg =
                    { cfg | tcoTarget = Nothing }
            in
            -- Fast path: both `f x` and `g acc` produce EvOk, so we can stay
            -- in the same tail-recursive while loop for the whole fold. Elm
            -- TCO only applies when the recursive call is in direct tail
            -- position in a single branch — anything else (yields, traces,
            -- memo operations, errors) is handed off to a helper that
            -- rebuilds the fold via a fresh call to `foldlHelp`, which
            -- preserves yield/memo propagation semantics.
            case f x innerCfg env of
                EvOk g ->
                    case g acc innerCfg env of
                        EvOk newAcc ->
                            foldlHelp f newAcc rest cfg env

                        otherAccResult ->
                            foldlResumeAfterAcc f rest cfg env otherAccResult

                otherFxResult ->
                    foldlResumeAfterFx f acc rest cfg env otherFxResult


{-| Continuation used after `f x` returns something other than `EvOk`.
Handles EvYield, EvOkTrace, EvErrTrace, EvMemoLookup, EvMemoStore and
EvErr by propagating them appropriately; on eventual success, threads the
resulting `g` back into applying it to `acc` and then resuming the fold.
-}
foldlResumeAfterFx :
    (Value -> Eval (Value -> Eval Value))
    -> Value
    -> List Value
    -> Types.Config
    -> Types.Env
    -> EvalResult (Value -> Eval Value)
    -> EvalResult Value
foldlResumeAfterFx f acc rest cfg env fxResult =
    case fxResult of
        EvOk g ->
            let
                innerCfg : Types.Config
                innerCfg =
                    { cfg | tcoTarget = Nothing }
            in
            case g acc innerCfg env of
                EvOk newAcc ->
                    foldlHelp f newAcc rest cfg env

                otherAccResult ->
                    foldlResumeAfterAcc f rest cfg env otherAccResult

        EvErr e ->
            EvErr e

        EvOkTrace g calls logs ->
            let
                innerCfg : Types.Config
                innerCfg =
                    { cfg | tcoTarget = Nothing }
            in
            case g acc innerCfg env of
                EvOk newAcc ->
                    attachTrace calls logs (foldlHelp f newAcc rest cfg env)

                otherAccResult ->
                    attachTrace calls logs (foldlResumeAfterAcc f rest cfg env otherAccResult)

        EvErrTrace e calls logs ->
            EvErrTrace e calls logs

        EvYield tag payload resume ->
            EvYield tag payload
                (\v -> foldlResumeAfterFx f acc rest cfg env (resume v))

        EvMemoLookup payload resume ->
            EvMemoLookup payload
                (\v -> foldlResumeAfterFx f acc rest cfg env (resume v))

        EvMemoStore payload next ->
            EvMemoStore payload
                (foldlResumeAfterFx f acc rest cfg env next)

        EvOkCoverage g _ ->
            let
                innerCfg : Types.Config
                innerCfg =
                    { cfg | tcoTarget = Nothing }
            in
            case g acc innerCfg env of
                EvOk newAcc ->
                    foldlHelp f newAcc rest cfg env

                otherAccResult ->
                    foldlResumeAfterAcc f rest cfg env otherAccResult

        EvErrCoverage e _ ->
            EvErr e


{-| Continuation used after `g acc` returns something other than `EvOk`.
On success we rejoin the TCO'd main loop at `foldlHelp`; otherwise we
propagate yields / memo ops / traces with the continuation needed to
continue folding `rest` once the suspended computation resolves.
-}
foldlResumeAfterAcc :
    (Value -> Eval (Value -> Eval Value))
    -> List Value
    -> Types.Config
    -> Types.Env
    -> EvalResult Value
    -> EvalResult Value
foldlResumeAfterAcc f rest cfg env accResult =
    case accResult of
        EvOk newAcc ->
            foldlHelp f newAcc rest cfg env

        EvErr e ->
            EvErr e

        EvOkTrace newAcc calls logs ->
            attachTrace calls logs (foldlHelp f newAcc rest cfg env)

        EvErrTrace e calls logs ->
            EvErrTrace e calls logs

        EvYield tag payload resume ->
            EvYield tag payload
                (\v -> foldlResumeAfterAcc f rest cfg env (resume v))

        EvMemoLookup payload resume ->
            EvMemoLookup payload
                (\v -> foldlResumeAfterAcc f rest cfg env (resume v))

        EvMemoStore payload next ->
            EvMemoStore payload
                (foldlResumeAfterAcc f rest cfg env next)

        EvOkCoverage newAcc _ ->
            foldlHelp f newAcc rest cfg env

        EvErrCoverage e _ ->
            EvErr e


{-| Prepend accumulated calls/logs to an `EvalResult`, preserving the
outer variant. Used to keep trace information flowing through fold
steps when an intermediate iteration emits a trace.
-}
attachTrace : Rope.Rope Types.CallTree -> Rope.Rope String -> EvalResult Value -> EvalResult Value
attachTrace prevCalls prevLogs result =
    case result of
        EvOk v ->
            EvOkTrace v prevCalls prevLogs

        EvErr e ->
            EvErrTrace e prevCalls prevLogs

        EvOkTrace v calls logs ->
            EvOkTrace v (Rope.appendTo prevCalls calls) (Rope.appendTo prevLogs logs)

        EvErrTrace e calls logs ->
            EvErrTrace e (Rope.appendTo prevCalls calls) (Rope.appendTo prevLogs logs)

        EvYield tag payload resume ->
            EvYield tag payload (\v -> attachTrace prevCalls prevLogs (resume v))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\v -> attachTrace prevCalls prevLogs (resume v))

        EvMemoStore payload next ->
            EvMemoStore payload (attachTrace prevCalls prevLogs next)

        EvOkCoverage v _ ->
            EvOkTrace v prevCalls prevLogs

        EvErrCoverage e _ ->
            EvErrTrace e prevCalls prevLogs


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

Tail-recursive in the `EvOk` happy path so Elm TCO compiles to a `while`
loop — sorting N-element lists uses O(1) JS stack frames rather than O(N).
This matters when the interpreter drives large sorts like
`Fuzz.Float.exponentMapping` (2048 elements): the old continuation-wrapped
form blew the JS stack during normalization-time eval.

-}
insertionSort : (Value -> Eval (Value -> Eval Order)) -> List Value -> List Value -> Eval (List Value)
insertionSort compare sorted unsorted cfg env =
    case unsorted of
        [] ->
            EvalResult.succeed sorted

        x :: rest ->
            case insertHelp compare x [] sorted cfg env of
                EvOk newSorted ->
                    insertionSort compare newSorted rest cfg env

                EvErr e ->
                    EvErr e

                other ->
                    -- Cold path: yields/traces/coverage — fall back to
                    -- andThen so EvalResult helpers handle propagation.
                    other
                        |> EvalResult.andThen
                            (\newSorted -> insertionSort compare newSorted rest cfg env)


{-| Insert a value into a sorted list at the correct position using an
explicit `prefix` accumulator (reversed "already walked past" elements),
so the happy path is directly tail-recursive and Elm can TCO it.

  - LT: place x before y (x is smaller)
  - EQ or GT: skip past y and keep looking (preserves stability for EQ)

-}
insertHelp : (Value -> Eval (Value -> Eval Order)) -> Value -> List Value -> List Value -> Eval (List Value)
insertHelp compare x prefix sorted cfg env =
    case sorted of
        [] ->
            EvalResult.succeed (reverseAppend prefix [ x ])

        y :: rest ->
            case compare x cfg env of
                EvOk compareWithY ->
                    case compareWithY y cfg env of
                        EvOk LT ->
                            EvalResult.succeed (reverseAppend prefix (x :: y :: rest))

                        EvOk _ ->
                            insertHelp compare x (y :: prefix) rest cfg env

                        EvErr e ->
                            EvErr e

                        other ->
                            other
                                |> EvalResult.andThen
                                    (\ord ->
                                        case ord of
                                            LT ->
                                                EvalResult.succeed (reverseAppend prefix (x :: y :: rest))

                                            _ ->
                                                insertHelp compare x (y :: prefix) rest cfg env
                                    )

                EvErr e ->
                    EvErr e

                other ->
                    other
                        |> EvalResult.andThen
                            (\compareWithY ->
                                compareWithY y cfg env
                                    |> EvalResult.andThen
                                        (\ord ->
                                            case ord of
                                                LT ->
                                                    EvalResult.succeed (reverseAppend prefix (x :: y :: rest))

                                                _ ->
                                                    insertHelp compare x (y :: prefix) rest cfg env
                                        )
                            )


{-| `reverseAppend xs ys` is `List.reverse xs ++ ys`, done in one pass.
Used by `insertHelp` to reconstruct the sorted list from a reversed prefix
accumulator plus the remaining tail.
-}
reverseAppend : List a -> List a -> List a
reverseAppend xs ys =
    case xs of
        [] ->
            ys

        x :: rest ->
            reverseAppend rest (x :: ys)


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
