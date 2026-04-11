module Kernel.Set exposing
    ( empty
    , emptyValue
    , fromList
    , insert
    , isEmpty
    , member
    , remove
    , size
    , toList
    , union
    )

{-| Native kernel implementations for Set operations.

Sets are represented at runtime as:

    Custom { moduleName = [ "Set" ], name = "Set_elm_builtin" } [ dict ]

where the inner `dict` is the underlying RBTree Dict value. These
kernel functions unwrap the Set constructor, delegate directly to
the host-native Dict kernel, and rewrap the result — bypassing the
interpreted `Set.insert (Set_elm_builtin dict) = ...` step chain.

-}

import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import EvalResult
import Kernel.Dict
import Types exposing (Eval, EvalResult(..), Value(..))
import Value exposing (typeError)


setRef : QualifiedNameRef
setRef =
    { moduleName = [ "Set" ], name = "Set_elm_builtin" }


emptyValue : Value
emptyValue =
    Custom setRef [ Kernel.Dict.emptyValue ]


wrap : Value -> Value
wrap dict =
    Custom setRef [ dict ]


unwrap : Value -> Maybe Value
unwrap value =
    case value of
        Custom ref args ->
            if ref.name == "Set_elm_builtin" then
                case args of
                    [ dict ] ->
                        Just dict

                    _ ->
                        Nothing

            else
                Nothing

        _ ->
            Nothing


empty : Eval Value
empty _ _ =
    EvalResult.succeed emptyValue


isEmpty : Value -> Eval Value
isEmpty set cfg env =
    case unwrap set of
        Just dict ->
            Kernel.Dict.isEmpty dict cfg env

        Nothing ->
            EvalResult.fail (typeError env "Set.isEmpty: expected a Set")


size : Value -> Eval Value
size set cfg env =
    case unwrap set of
        Just dict ->
            Kernel.Dict.size dict cfg env

        Nothing ->
            EvalResult.fail (typeError env "Set.size: expected a Set")


member : Value -> Value -> Eval Value
member key set cfg env =
    case unwrap set of
        Just dict ->
            Kernel.Dict.member key dict cfg env

        Nothing ->
            EvalResult.fail (typeError env "Set.member: expected a Set")


insert : Value -> Value -> Eval Value
insert key set cfg env =
    case unwrap set of
        Just dict ->
            case Kernel.Dict.insert key Unit dict cfg env of
                EvOk newDict ->
                    EvalResult.succeed (wrap newDict)

                EvOkTrace newDict calls logs ->
                    EvOkTrace (wrap newDict) calls logs

                other ->
                    other

        Nothing ->
            EvalResult.fail (typeError env "Set.insert: expected a Set")


remove : Value -> Value -> Eval Value
remove key set cfg env =
    case unwrap set of
        Just dict ->
            case Kernel.Dict.remove key dict cfg env of
                EvOk newDict ->
                    EvalResult.succeed (wrap newDict)

                EvOkTrace newDict calls logs ->
                    EvOkTrace (wrap newDict) calls logs

                other ->
                    other

        Nothing ->
            EvalResult.fail (typeError env "Set.remove: expected a Set")


union : Value -> Value -> Eval Value
union left right cfg env =
    case ( unwrap left, unwrap right ) of
        ( Just lDict, Just rDict ) ->
            case Kernel.Dict.union lDict rDict cfg env of
                EvOk newDict ->
                    EvalResult.succeed (wrap newDict)

                EvOkTrace newDict calls logs ->
                    EvOkTrace (wrap newDict) calls logs

                other ->
                    other

        _ ->
            EvalResult.fail (typeError env "Set.union: expected two Sets")


fromList : Value -> Eval Value
fromList list cfg env =
    case list of
        List items ->
            fromListHelp items Kernel.Dict.emptyValue cfg env

        _ ->
            EvalResult.fail (typeError env "Set.fromList: expected a list")


fromListHelp : List Value -> Value -> Eval Value
fromListHelp items dictAcc cfg env =
    case items of
        [] ->
            EvalResult.succeed (wrap dictAcc)

        item :: rest ->
            case EvalResult.toResult (Kernel.Dict.insert item Unit dictAcc cfg env) of
                Err e ->
                    EvErr e

                Ok next ->
                    fromListHelp rest next cfg env


toList : Value -> Eval Value
toList set cfg env =
    case unwrap set of
        Just dict ->
            case Kernel.Dict.toList dict cfg env of
                EvOk (List pairs) ->
                    EvalResult.succeed (List (List.map unwrapPair pairs))

                EvOkTrace (List pairs) calls logs ->
                    EvOkTrace (List (List.map unwrapPair pairs)) calls logs

                other ->
                    other

        Nothing ->
            EvalResult.fail (typeError env "Set.toList: expected a Set")


unwrapPair : Value -> Value
unwrapPair value =
    case value of
        Tuple k _ ->
            k

        _ ->
            value
