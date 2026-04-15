module Kernel.Utils exposing (append, compare, comparison, equal, equalValues, extractError, innerCompare)

import Array
import Elm.Syntax.ModuleName exposing (ModuleName)
import EvalResult
import FastDict as Dict exposing (Dict)
import Types exposing (Env, Eval, EvalErrorData, Value(..))
import Value exposing (typeError)


append : Value -> Value -> Eval Value
append l r _ env =
    case ( l, r ) of
        ( String ls, String rs ) ->
            EvalResult.succeed <| String (ls ++ rs)

        ( List ll, List rl ) ->
            EvalResult.succeed <| List (ll ++ rl)

        _ ->
            EvalResult.fail <| typeError env <| "Cannot append " ++ Value.toString l ++ " and " ++ Value.toString r


compare : Value -> Value -> Eval Order
compare l r _ env =
    EvalResult.fromResult (innerCompare l r env)


innerCompare : Value -> Value -> Env -> Result EvalErrorData Order
innerCompare l r env =
    let
        inner : comparable -> comparable -> Result EvalErrorData Order
        inner lv rv =
            Ok <| Basics.compare lv rv

        uncomparable : () -> Result EvalErrorData value
        uncomparable () =
            Err <|
                typeError env
                    ("Cannot compare "
                        ++ Value.toString l
                        ++ " and "
                        ++ Value.toString r
                        ++ " because they have different types"
                    )
    in
    case ( l, r ) of
        ( Int lv, Int rv ) ->
            inner lv rv

        ( Int lv, Float rv ) ->
            inner (toFloat lv) rv

        ( Int _, _ ) ->
            uncomparable ()

        ( Float lv, Float rv ) ->
            inner lv rv

        ( Float lv, Int rv ) ->
            inner lv (toFloat rv)

        ( Float _, _ ) ->
            uncomparable ()

        ( String lv, String rv ) ->
            inner lv rv

        ( String _, _ ) ->
            uncomparable ()

        ( Char lv, Char rv ) ->
            inner lv rv

        ( Char _, _ ) ->
            uncomparable ()

        ( Tuple la lb, Tuple ra rb ) ->
            innerCompare la ra env
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            innerCompare lb rb env
                    )

        ( Tuple _ _, _ ) ->
            uncomparable ()

        ( Triple la lb lc, Triple ra rb rc ) ->
            innerCompare la ra env
                |> Result.andThen
                    (\a ->
                        if a /= EQ then
                            Ok a

                        else
                            innerCompare lb rb env
                                |> Result.andThen
                                    (\b ->
                                        if b /= EQ then
                                            Ok b

                                        else
                                            innerCompare lc rc env
                                    )
                    )

        ( Triple _ _ _, _ ) ->
            uncomparable ()

        ( List ll, List rl ) ->
            compareListHelp ll rl env

        ( List _, _ ) ->
            uncomparable ()


        ( Custom lname lvalues, Custom rname rvalues ) ->
            if lname.name /= rname.name then
                -- Special case: Dict/Set nodes with different constructors
                -- (e.g., RBNode vs RBEmpty). Convert to sorted lists for comparison,
                -- mirroring Elm's _Utils_eqHelp which converts Dict/Set to lists.
                if isDictNode lname.name && isDictNode rname.name then
                    innerCompare (dictToSortedList l) (dictToSortedList r) env

                else
                    inner lname.name rname.name

            else
                case lname.name of
                    -- Set_elm_builtin: unwrap to inner Dict and compare
                    "Set_elm_builtin" ->
                        case ( lvalues, rvalues ) of
                            ( [ ldict ], [ rdict ] ) ->
                                innerCompare (dictToSortedList ldict) (dictToSortedList rdict) env

                            _ ->
                                innerCompare (List lvalues) (List rvalues) env

                    -- Dict nodes: convert to sorted (key, value) list and compare
                    "RBNode_elm_builtin" ->
                        innerCompare (dictToSortedList l) (dictToSortedList r) env

                    "RBEmpty_elm_builtin" ->
                        Ok EQ

                    _ ->
                        case ( Value.toArray l, Value.toArray r ) of
                            ( Just la, Just ra ) ->
                                innerCompare (List la) (List ra) env

                            _ ->
                                innerCompare (List lvalues) (List rvalues) env

        ( Custom _ _, _ ) ->
            uncomparable ()

        ( Record ldict, Record rdict ) ->
            let
                toValue : Dict String Value -> Value
                toValue dict =
                    dict
                        |> Dict.toList
                        |> List.map (\( k, v ) -> Tuple (String k) v)
                        |> List
            in
            innerCompare (toValue ldict) (toValue rdict) env

        ( Record _, _ ) ->
            uncomparable ()

        ( JsArray larr, JsArray rarr ) ->
            innerCompare (List <| Array.toList larr) (List <| Array.toList rarr) env

        ( JsArray _, _ ) ->
            uncomparable ()

        ( Bool lb, Bool rb ) ->
            if lb == rb then
                Ok EQ

            else if lb then
                Ok GT

            else
                Ok LT

        ( Bool _, _ ) ->
            uncomparable ()

        ( Unit, Unit ) ->
            Ok EQ

        ( Unit, _ ) ->
            uncomparable ()

        ( PartiallyApplied _ _ _ _ _ _, PartiallyApplied _ _ _ _ _ _ ) ->
            Err <| typeError env "Cannot compare functions"

        ( PartiallyApplied _ _ _ _ _ _, _ ) ->
            uncomparable ()

        ( JsonValue _, _ ) ->
            Err <| typeError env "Cannot compare JSON values"

        ( JsonDecoderValue _, _ ) ->
            Err <| typeError env "Cannot compare JSON decoders"

        ( RegexValue _, _ ) ->
            Err <| typeError env "Cannot compare Regex values"

        ( BytesValue a, BytesValue b ) ->
            if a == b then
                Ok EQ

            else
                -- Bytes don't have a natural ordering, but we can compare by contents
                compareListHelp (List.map Int (Array.toList a)) (List.map Int (Array.toList b)) env

        ( BytesValue _, _ ) ->
            Err <| typeError env "Cannot compare Bytes with non-Bytes"


compareListHelp : List Value -> List Value -> Env -> Result EvalErrorData Order
compareListHelp ll rl env =
    case ( ll, rl ) of
        ( [], [] ) ->
            Ok EQ

        ( [], _ :: _ ) ->
            Ok LT

        ( _ :: _, [] ) ->
            Ok GT

        ( lh :: lt, rh :: rt ) ->
            case innerCompare lh rh env of
                Err e ->
                    Err e

                Ok EQ ->
                    compareListHelp lt rt env

                ok ->
                    ok


{-| Check if a constructor name is a Dict internal node type.
-}
isDictNode : String -> Bool
isDictNode name =
    name == "RBNode_elm_builtin" || name == "RBEmpty_elm_builtin"


{-| Convert a Dict Value (RBNode_elm_builtin/RBEmpty_elm_builtin tree) to a sorted
List of (key, value) Tuple Values. This mirrors Elm's \_Utils\_eqHelp which converts
Dict/Set to lists before comparing, ensuring that structurally different red-black
trees with the same elements compare as equal.
-}
dictToSortedList : Value -> Value
dictToSortedList value =
    List (dictToSortedListHelp value [])


dictToSortedListHelp : Value -> List Value -> List Value
dictToSortedListHelp value acc =
    case value of
        Custom { name } args ->
            case name of
                "RBNode_elm_builtin" ->
                    case args of
                        [ _, key, val, left, right ] ->
                            dictToSortedListHelp left (Tuple key val :: dictToSortedListHelp right acc)

                        _ ->
                            acc

                "RBEmpty_elm_builtin" ->
                    acc

                _ ->
                    acc

        _ ->
            acc


extractError : EvalErrorData -> EvalErrorData
extractError e =
    e


{-| Direct equality check for two values. Used by inlined == operator.
-}
equal : Value -> Value -> Env -> Result EvalErrorData Bool
equal l r env =
    innerCompare l r env
        |> Result.map (\order -> order == EQ)


{-| Zero-allocation equality: returns Bool directly without Result/Order
wrapping. Falls back to innerCompare for complex cases (Dict/Set/Array).
-}
equalValues : Value -> Value -> Bool
equalValues l r =
    case ( l, r ) of
        ( Int lv, Int rv ) ->
            lv == rv

        ( Float lv, Float rv ) ->
            lv == rv

        ( Int lv, Float rv ) ->
            toFloat lv == rv

        ( Float lv, Int rv ) ->
            lv == toFloat rv

        ( String lv, String rv ) ->
            lv == rv

        ( Char lv, Char rv ) ->
            lv == rv

        ( Bool lb, Bool rb ) ->
            lb == rb

        ( Unit, Unit ) ->
            True

        ( Tuple la lb, Tuple ra rb ) ->
            equalValues la ra && equalValues lb rb

        ( Triple la lb lc, Triple ra rb rc ) ->
            equalValues la ra && equalValues lb rb && equalValues lc rc

        ( List ll, List rl ) ->
            equalListHelp ll rl

        ( Custom lname largs, Custom rname rargs ) ->
            if lname.name /= rname.name then
                if isDictNode lname.name && isDictNode rname.name then
                    equalValues (dictToSortedList l) (dictToSortedList r)

                else
                    False

            else
                case lname.name of
                    "Set_elm_builtin" ->
                        case ( largs, rargs ) of
                            ( [ ldict ], [ rdict ] ) ->
                                equalValues (dictToSortedList ldict) (dictToSortedList rdict)

                            _ ->
                                equalListHelp largs rargs

                    "RBNode_elm_builtin" ->
                        equalValues (dictToSortedList l) (dictToSortedList r)

                    "RBEmpty_elm_builtin" ->
                        True

                    _ ->
                        case ( Value.toArray l, Value.toArray r ) of
                            ( Just la, Just ra ) ->
                                equalListHelp la ra

                            _ ->
                                equalListHelp largs rargs

        ( Record ldict, Record rdict ) ->
            Dict.size ldict == Dict.size rdict && Dict.foldl (\k lv ok -> ok && (Dict.get k rdict |> Maybe.map (equalValues lv) |> Maybe.withDefault False)) True ldict

        ( JsArray la, JsArray ra ) ->
            Array.length la == Array.length ra && equalListHelp (Array.toList la) (Array.toList ra)

        ( BytesValue la, BytesValue ra ) ->
            la == ra

        _ ->
            False


equalListHelp : List Value -> List Value -> Bool
equalListHelp ll rl =
    case ( ll, rl ) of
        ( [], [] ) ->
            True

        ( lh :: lt, rh :: rt ) ->
            equalValues lh rh && equalListHelp lt rt

        _ ->
            False


comparison : List Order -> ModuleName -> ( Int, List Value -> Eval Value )
comparison orders _ =
    ( 2
    , \args cfg env ->
        case args of
            [ l, r ] ->
                compare l r cfg env
                    |> EvalResult.map
                        (\result ->
                            Bool (List.member result orders)
                        )

            _ ->
                EvalResult.fail <| typeError env "Comparison needs exactly two arguments"
    )
