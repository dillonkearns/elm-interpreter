module Eval.NativeDispatch exposing
    ( NativeDispatcher
    , buildRegistry
    , tryDispatch
    , tryDispatchByName
    )

{-| Native core-function dispatchers for the new evaluator's hot path.

Phase 3 iter 3b4 implemented core dispatch via `Value.toExpression` +
synthesized `Application` AST + delegation to the old evaluator. That
works for every core function but pays ~1.4 µs per call of constant
overhead, which dominates the cost of tiny operator-heavy expressions.

This module defines direct `List Value -> ...` dispatchers for the
hottest ~15 core functions so the new evaluator can call them without
the AST round-trip. The dispatchers are registered in a `Dict GlobalId
NativeDispatcher` built once at REnv construction time; `evalR` checks
the registry in its `RApply (RGlobal id)` fast path before falling back
to the old-eval delegation.

**Partial coverage is fine.** Each dispatcher returns a `Maybe Value`
result so it can decline to handle args it doesn't recognize (e.g., the
`Basics.eq` dispatcher handles `Int == Int`, `String == String`, etc.
directly but returns `Nothing` for `Record == Record`, at which point
the caller falls back to the full delegation path). This keeps the
dispatchers simple and lets us target the common cases without having
to re-implement the interpreter's structural equality.

The target list of covered functions (Round 8 profile evidence):

  - `Basics.add`, `sub`, `mul`, `fdiv`, `idiv`, `negate`
  - `Basics.eq`, `neq`, `lt`, `gt`, `le`, `ge` (Int/Float/String/Char/Bool)
  - `Basics.append` (String ++ String, List ++ List)
  - `List.cons` (::)

Anything else falls through to delegation.

-}

import Elm.Syntax.ModuleName exposing (ModuleName)
import Eval.ResolvedIR exposing (GlobalId)
import FastDict
import Types exposing (Value(..))


{-| A native dispatcher takes pre-evaluated argument values and returns
`Just result` if it can handle them, or `Nothing` if the caller should
fall back to the full delegation path.
-}
type alias NativeDispatcher =
    List Value -> Maybe Value


{-| Build the registry. Takes a `(ModuleName, String) -> Maybe GlobalId`
lookup function (typically `FastDict.get ( moduleName, name )
projectEnv.resolved.globalIds`) and returns a Dict keyed by the
resolved `GlobalId`s.

Entries whose `(moduleName, name)` aren't in the project's globalIds
map are silently dropped — the project doesn't reference them, so
there's no id to register against.

-}
buildRegistry :
    (( ModuleName, String ) -> Maybe GlobalId)
    -> FastDict.Dict GlobalId NativeDispatcher
buildRegistry lookupId =
    dispatcherList
        |> List.filterMap
            (\( moduleName, name, dispatcher ) ->
                lookupId ( moduleName, name )
                    |> Maybe.map (\id -> ( id, dispatcher ))
            )
        |> FastDict.fromList


{-| Try to dispatch a core call natively. Returns `Just value` if the
dispatcher handled it, `Nothing` if the caller should fall back to the
full delegation path.
-}
tryDispatch :
    FastDict.Dict GlobalId NativeDispatcher
    -> GlobalId
    -> List Value
    -> Maybe Value
tryDispatch registry id args =
    case FastDict.get id registry of
        Just dispatcher ->
            dispatcher args

        Nothing ->
            Nothing


{-| Old-evaluator entry point: look up a native dispatcher by
`(moduleName, name)` and invoke it. Used from `Eval.Expression`'s
`evalFullyAppliedWithEnv` KernelImpl branch so hot Basics/List calls
in `Application` shape (e.g. `List.foldl (+) 0 xs`) bypass the
`twoWithError` selector/eval-result wrapping on every element.

The linear scan through `dispatcherList` is fine in practice: the
list is 14 entries, only queried when we've already matched
`KernelImpl ["Basics"] _ _` or similar, and is further guarded by
`not cfg.trace` at the call site so interactive debugging still sees
every kernel invocation in the trace output.

-}
tryDispatchByName : ModuleName -> String -> List Value -> Maybe Value
tryDispatchByName moduleName name args =
    tryDispatchByNameInList dispatcherList moduleName name args


tryDispatchByNameInList :
    List ( ModuleName, String, NativeDispatcher )
    -> ModuleName
    -> String
    -> List Value
    -> Maybe Value
tryDispatchByNameInList entries moduleName name args =
    case entries of
        [] ->
            Nothing

        ( entryModuleName, entryName, dispatcher ) :: rest ->
            if entryName == name && entryModuleName == moduleName then
                dispatcher args

            else
                tryDispatchByNameInList rest moduleName name args



-- DISPATCHERS


dispatcherList : List ( ModuleName, String, NativeDispatcher )
dispatcherList =
    [ ( [ "Basics" ], "add", binaryArith (+) (+) )
    , ( [ "Basics" ], "sub", binaryArith (-) (-) )
    , ( [ "Basics" ], "mul", binaryArith (*) (*) )
    , ( [ "Basics" ], "fdiv", floatOnly (/) )
    , ( [ "Basics" ], "idiv", idivDispatcher )
    , ( [ "Basics" ], "negate", negateDispatcher )
    , ( [ "Basics" ], "eq", eqDispatcher )
    , ( [ "Basics" ], "neq", neqDispatcher )
    , ( [ "Basics" ], "lt", compareIntFloat (<) (<) )
    , ( [ "Basics" ], "gt", compareIntFloat (>) (>) )
    , ( [ "Basics" ], "le", compareIntFloat (<=) (<=) )
    , ( [ "Basics" ], "ge", compareIntFloat (>=) (>=) )
    , ( [ "Basics" ], "append", appendDispatcher )
    , ( [ "List" ], "cons", consDispatcher )
    ]


binaryArith : (Int -> Int -> Int) -> (Float -> Float -> Float) -> NativeDispatcher
binaryArith intOp floatOp args =
    case args of
        [ Int a, Int b ] ->
            Just (Int (intOp a b))

        [ Float a, Float b ] ->
            Just (Float (floatOp a b))

        [ Int a, Float b ] ->
            Just (Float (floatOp (toFloat a) b))

        [ Float a, Int b ] ->
            Just (Float (floatOp a (toFloat b)))

        _ ->
            Nothing


floatOnly : (Float -> Float -> Float) -> NativeDispatcher
floatOnly op args =
    case args of
        [ Float a, Float b ] ->
            Just (Float (op a b))

        [ Int a, Float b ] ->
            Just (Float (op (toFloat a) b))

        [ Float a, Int b ] ->
            Just (Float (op a (toFloat b)))

        [ Int a, Int b ] ->
            Just (Float (op (toFloat a) (toFloat b)))

        _ ->
            Nothing


intOnly : (Int -> Int -> Int) -> NativeDispatcher
intOnly op args =
    case args of
        [ Int a, Int b ] ->
            Just (Int (op a b))

        _ ->
            Nothing


idivDispatcher : NativeDispatcher
idivDispatcher args =
    case args of
        [ Int _, Int 0 ] ->
            Nothing

        [ Int a, Int b ] ->
            Just (Int (a // b))

        _ ->
            Nothing


negateDispatcher : NativeDispatcher
negateDispatcher args =
    case args of
        [ Int i ] ->
            Just (Int -i)

        [ Float f ] ->
            Just (Float -f)

        _ ->
            Nothing


compareIntFloat : (Int -> Int -> Bool) -> (Float -> Float -> Bool) -> NativeDispatcher
compareIntFloat intOp floatOp args =
    case args of
        [ Int a, Int b ] ->
            Just (Bool (intOp a b))

        [ Float a, Float b ] ->
            Just (Bool (floatOp a b))

        [ Int a, Float b ] ->
            Just (Bool (floatOp (toFloat a) b))

        [ Float a, Int b ] ->
            Just (Bool (floatOp a (toFloat b)))

        _ ->
            -- Anything else (String <, Custom <, etc.) falls back so
            -- the old evaluator's `Kernel.Utils.cmp` handles it.
            Nothing


eqDispatcher : NativeDispatcher
eqDispatcher args =
    case args of
        [ a, b ] ->
            shallowEqual a b
                |> Maybe.map Bool

        _ ->
            Nothing


neqDispatcher : NativeDispatcher
neqDispatcher args =
    case args of
        [ a, b ] ->
            shallowEqual a b
                |> Maybe.map (not >> Bool)

        _ ->
            Nothing


{-| Shallow equality for common comparable values. Returns `Nothing` for
value kinds that need deep comparison (records, tuples, lists of
non-primitives, Custom ADT constructors), at which point the caller
falls back to the old evaluator's full structural comparison.
-}
shallowEqual : Value -> Value -> Maybe Bool
shallowEqual a b =
    case ( a, b ) of
        ( Int i, Int j ) ->
            Just (i == j)

        ( Float f, Float g ) ->
            Just (f == g)

        ( Int i, Float g ) ->
            Just (toFloat i == g)

        ( Float f, Int j ) ->
            Just (f == toFloat j)

        ( String s, String t ) ->
            Just (s == t)

        ( Char c, Char d ) ->
            Just (c == d)

        ( Bool p, Bool q ) ->
            Just (p == q)

        ( Unit, Unit ) ->
            Just True

        _ ->
            Nothing


appendDispatcher : NativeDispatcher
appendDispatcher args =
    case args of
        [ String s, String t ] ->
            Just (String (s ++ t))

        [ List xs, List ys ] ->
            Just (List (xs ++ ys))

        _ ->
            Nothing


consDispatcher : NativeDispatcher
consDispatcher args =
    case args of
        [ head, List tail ] ->
            Just (List (head :: tail))

        _ ->
            Nothing
