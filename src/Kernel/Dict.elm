module Kernel.Dict exposing
    ( empty
    , emptyValue
    , foldl
    , foldr
    , fromList
    , get
    , insert
    , isEmpty
    , map
    , member
    , remove
    , size
    , toList
    , union
    )

{-| Kernel implementations for Dict operations.

Walks the RBTree structure in host Elm so the whole recursive
insert / get / union / balance loop stays out of the interpreter's
tree-walking evaluator. Key comparison is delegated to the host
`Kernel.Utils.innerCompare` which already has fast paths for
String, Int, Tuple, etc.

Dict values at runtime are:
  - Custom { moduleName = [ "Dict" ], name = "RBEmpty_elm_builtin" } []
  - Custom { moduleName = [ "Dict" ], name = "RBNode_elm_builtin" } [ color, key, value, left, right ]

Color values:
  - Custom { moduleName = [ "Dict" ], name = "Red" } []
  - Custom { moduleName = [ "Dict" ], name = "Black" } []

-}

import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import EvalResult
import Kernel.Utils
import Types exposing (Env, Eval, EvalErrorData, EvalResult(..), Value(..))
import Value exposing (typeError)


rbNodeRef : QualifiedNameRef
rbNodeRef =
    { moduleName = [ "Dict" ], name = "RBNode_elm_builtin" }


rbEmptyRef : QualifiedNameRef
rbEmptyRef =
    { moduleName = [ "Dict" ], name = "RBEmpty_elm_builtin" }


redRef : QualifiedNameRef
redRef =
    { moduleName = [ "Dict" ], name = "Red" }


blackRef : QualifiedNameRef
blackRef =
    { moduleName = [ "Dict" ], name = "Black" }


redValue : Value
redValue =
    Custom redRef []


blackValue : Value
blackValue =
    Custom blackRef []


emptyValue : Value
emptyValue =
    Custom rbEmptyRef []


rbNode : Value -> Value -> Value -> Value -> Value -> Value
rbNode color key val left right =
    Custom rbNodeRef [ color, key, val, left, right ]


isRed : Value -> Bool
isRed value =
    case value of
        Custom ref _ ->
            ref.name == "Red"

        _ ->
            False


isBlack : Value -> Bool
isBlack value =
    case value of
        Custom ref _ ->
            ref.name == "Black"

        _ ->
            True


{-| Kernel Dict.foldl: fold from lowest to highest key.
Tree traversal in host Elm, user function called via interpreter.
-}
foldl : (Value -> Eval (Value -> Eval (Value -> Eval Value))) -> Value -> Value -> Eval Value
foldl func acc dict cfg env =
    let
        innerCfg =
            { cfg | tcoTarget = Nothing }
    in
    foldlHelp func innerCfg acc dict cfg env


foldlHelp : (Value -> Eval (Value -> Eval (Value -> Eval Value))) -> Types.Config -> Value -> Value -> Eval Value
foldlHelp func innerCfg acc dict cfg env =
    case dict of
        Custom { name } args ->
            if name == "RBEmpty_elm_builtin" then
                EvalResult.succeed acc

            else
                -- RBNode_elm_builtin color key value left right
                case args of
                    [ _, key, value, left, right ] ->
                        -- Fold left subtree first
                        case EvalResult.toResult (foldlHelp func innerCfg acc left cfg env) of
                            Err e ->
                                EvErr e

                            Ok leftAcc ->
                                -- Apply: func key value leftAcc
                                case EvalResult.toResult (func key innerCfg env) of
                                    Err e ->
                                        EvErr e

                                    Ok g ->
                                        case EvalResult.toResult (g value innerCfg env) of
                                            Err e ->
                                                EvErr e

                                            Ok h ->
                                                case EvalResult.toResult (h leftAcc innerCfg env) of
                                                    Err e ->
                                                        EvErr e

                                                    Ok newAcc ->
                                                        -- Then fold right subtree
                                                        foldlHelp func innerCfg newAcc right cfg env

                    _ ->
                        EvalResult.succeed acc

        _ ->
            EvalResult.succeed acc


{-| Kernel Dict.foldr: fold from highest to lowest key.
-}
foldr : (Value -> Eval (Value -> Eval (Value -> Eval Value))) -> Value -> Value -> Eval Value
foldr func acc dict cfg env =
    let
        innerCfg =
            { cfg | tcoTarget = Nothing }
    in
    foldrHelp func innerCfg acc dict cfg env


foldrHelp : (Value -> Eval (Value -> Eval (Value -> Eval Value))) -> Types.Config -> Value -> Value -> Eval Value
foldrHelp func innerCfg acc dict cfg env =
    case dict of
        Custom { name } args ->
            if name == "RBEmpty_elm_builtin" then
                EvalResult.succeed acc

            else
                case args of
                    [ _, key, value, left, right ] ->
                        -- Fold right subtree first
                        case EvalResult.toResult (foldrHelp func innerCfg acc right cfg env) of
                            Err e ->
                                EvErr e

                            Ok rightAcc ->
                                -- Apply: func key value rightAcc
                                case EvalResult.toResult (func key innerCfg env) of
                                    Err e ->
                                        EvErr e

                                    Ok g ->
                                        case EvalResult.toResult (g value innerCfg env) of
                                            Err e ->
                                                EvErr e

                                            Ok h ->
                                                case EvalResult.toResult (h rightAcc innerCfg env) of
                                                    Err e ->
                                                        EvErr e

                                                    Ok newAcc ->
                                                        -- Then fold left subtree
                                                        foldrHelp func innerCfg newAcc left cfg env

                    _ ->
                        EvalResult.succeed acc

        _ ->
            EvalResult.succeed acc


{-| Kernel Dict.map: apply function to all values, preserving tree structure.
-}
map : (Value -> Eval (Value -> Eval Value)) -> Value -> Eval Value
map func dict cfg env =
    let
        innerCfg =
            { cfg | tcoTarget = Nothing }
    in
    mapHelp func innerCfg dict cfg env


mapHelp : (Value -> Eval (Value -> Eval Value)) -> Types.Config -> Value -> Eval Value
mapHelp func innerCfg dict cfg env =
    case dict of
        Custom ref args ->
            if ref.name == "RBEmpty_elm_builtin" then
                EvalResult.succeed dict

            else
                case args of
                    [ color, key, value, left, right ] ->
                        -- Map left subtree
                        case EvalResult.toResult (mapHelp func innerCfg left cfg env) of
                            Err e ->
                                EvErr e

                            Ok mappedLeft ->
                                -- Map right subtree
                                case EvalResult.toResult (mapHelp func innerCfg right cfg env) of
                                    Err e ->
                                        EvErr e

                                    Ok mappedRight ->
                                        -- Apply: func key value
                                        case EvalResult.toResult (func key innerCfg env) of
                                            Err e ->
                                                EvErr e

                                            Ok g ->
                                                case EvalResult.toResult (g value innerCfg env) of
                                                    Err e ->
                                                        EvErr e

                                                    Ok mappedValue ->
                                                        EvalResult.succeed
                                                            (Custom ref [ color, key, mappedValue, mappedLeft, mappedRight ])

                    _ ->
                        EvalResult.succeed dict

        _ ->
            EvalResult.succeed dict



-- NATIVE DICT.*: insert / get / member / union / remove / fromList / size / toList / empty / isEmpty
--
-- These walk the RBTree entirely in host Elm, calling innerCompare directly
-- for key ordering instead of round-tripping through the interpreter for each
-- comparison.


empty : Eval Value
empty _ _ =
    EvalResult.succeed emptyValue


isEmpty : Value -> Eval Value
isEmpty dict _ _ =
    case dict of
        Custom ref _ ->
            EvalResult.succeed (Bool (ref.name == "RBEmpty_elm_builtin"))

        _ ->
            EvalResult.succeed (Bool False)


size : Value -> Eval Value
size dict _ _ =
    EvalResult.succeed (Int (sizeHelp 0 dict))


sizeHelp : Int -> Value -> Int
sizeHelp acc dict =
    case dict of
        Custom ref args ->
            if ref.name == "RBEmpty_elm_builtin" then
                acc

            else
                case args of
                    [ _, _, _, left, right ] ->
                        sizeHelp (sizeHelp (acc + 1) right) left

                    _ ->
                        acc

        _ ->
            acc


get : Value -> Value -> Eval Value
get targetKey dict _ env =
    case getHelp targetKey dict env of
        Ok maybeValue ->
            EvalResult.succeed
                (case maybeValue of
                    Just v ->
                        Custom { moduleName = [ "Maybe" ], name = "Just" } [ v ]

                    Nothing ->
                        Custom { moduleName = [ "Maybe" ], name = "Nothing" } []
                )

        Err e ->
            EvErr e


getHelp : Value -> Value -> Env -> Result EvalErrorData (Maybe Value)
getHelp targetKey dict env =
    case dict of
        Custom ref args ->
            if ref.name == "RBEmpty_elm_builtin" then
                Ok Nothing

            else
                case args of
                    [ _, nodeKey, nodeValue, left, right ] ->
                        case Kernel.Utils.innerCompare targetKey nodeKey env of
                            Err e ->
                                Err e

                            Ok LT ->
                                getHelp targetKey left env

                            Ok EQ ->
                                Ok (Just nodeValue)

                            Ok GT ->
                                getHelp targetKey right env

                    _ ->
                        Ok Nothing

        _ ->
            Ok Nothing


member : Value -> Value -> Eval Value
member targetKey dict _ env =
    case getHelp targetKey dict env of
        Ok (Just _) ->
            EvalResult.succeed (Bool True)

        Ok Nothing ->
            EvalResult.succeed (Bool False)

        Err e ->
            EvErr e


insert : Value -> Value -> Value -> Eval Value
insert key value dict _ env =
    case insertHelp key value dict env of
        Err e ->
            EvErr e

        Ok node ->
            case node of
                Custom ref args ->
                    if ref.name == "RBNode_elm_builtin" then
                        case args of
                            [ color, k, v, left, right ] ->
                                if isRed color then
                                    -- Root must be black
                                    EvalResult.succeed (rbNode blackValue k v left right)

                                else
                                    EvalResult.succeed node

                            _ ->
                                EvalResult.succeed node

                    else
                        EvalResult.succeed node

                _ ->
                    EvalResult.succeed node


insertHelp : Value -> Value -> Value -> Env -> Result EvalErrorData Value
insertHelp key value dict env =
    case dict of
        Custom ref args ->
            if ref.name == "RBEmpty_elm_builtin" then
                Ok (rbNode redValue key value emptyValue emptyValue)

            else
                case args of
                    [ nColor, nKey, nValue, nLeft, nRight ] ->
                        case Kernel.Utils.innerCompare key nKey env of
                            Err e ->
                                Err e

                            Ok LT ->
                                case insertHelp key value nLeft env of
                                    Err e ->
                                        Err e

                                    Ok newLeft ->
                                        Ok (balance nColor nKey nValue newLeft nRight)

                            Ok EQ ->
                                Ok (rbNode nColor nKey value nLeft nRight)

                            Ok GT ->
                                case insertHelp key value nRight env of
                                    Err e ->
                                        Err e

                                    Ok newRight ->
                                        Ok (balance nColor nKey nValue nLeft newRight)

                    _ ->
                        Ok dict

        _ ->
            Ok dict


balance : Value -> Value -> Value -> Value -> Value -> Value
balance color key value left right =
    case right of
        Custom rRef rArgs ->
            if rRef.name == "RBNode_elm_builtin" then
                case rArgs of
                    [ rColor, rK, rV, rLeft, rRight ] ->
                        if isRed rColor then
                            case left of
                                Custom lRef lArgs ->
                                    if lRef.name == "RBNode_elm_builtin" then
                                        case lArgs of
                                            [ lColor, lK, lV, lLeft, lRight ] ->
                                                if isRed lColor then
                                                    rbNode
                                                        redValue
                                                        key
                                                        value
                                                        (rbNode blackValue lK lV lLeft lRight)
                                                        (rbNode blackValue rK rV rLeft rRight)

                                                else
                                                    balanceDefault color key value left right rK rV rLeft rRight

                                            _ ->
                                                balanceDefault color key value left right rK rV rLeft rRight

                                    else
                                        balanceDefault color key value left right rK rV rLeft rRight

                                _ ->
                                    balanceDefault color key value left right rK rV rLeft rRight

                        else
                            balanceLeft color key value left right

                    _ ->
                        balanceLeft color key value left right

            else
                balanceLeft color key value left right

        _ ->
            balanceLeft color key value left right


balanceDefault : Value -> Value -> Value -> Value -> Value -> Value -> Value -> Value -> Value -> Value
balanceDefault color key value left _ rK rV rLeft rRight =
    rbNode color rK rV (rbNode redValue key value left rLeft) rRight


balanceLeft : Value -> Value -> Value -> Value -> Value -> Value
balanceLeft color key value left right =
    case left of
        Custom lRef lArgs ->
            if lRef.name == "RBNode_elm_builtin" then
                case lArgs of
                    [ lColor, lK, lV, lLeft, lRight ] ->
                        if isRed lColor then
                            case lLeft of
                                Custom llRef llArgs ->
                                    if llRef.name == "RBNode_elm_builtin" then
                                        case llArgs of
                                            [ llColor, llK, llV, llLeft, llRight ] ->
                                                if isRed llColor then
                                                    rbNode
                                                        redValue
                                                        lK
                                                        lV
                                                        (rbNode blackValue llK llV llLeft llRight)
                                                        (rbNode blackValue key value lRight right)

                                                else
                                                    rbNode color key value left right

                                            _ ->
                                                rbNode color key value left right

                                    else
                                        rbNode color key value left right

                                _ ->
                                    rbNode color key value left right

                        else
                            rbNode color key value left right

                    _ ->
                        rbNode color key value left right

            else
                rbNode color key value left right

        _ ->
            rbNode color key value left right


remove : Value -> Value -> Eval Value
remove targetKey dict _ env =
    case removeHelp targetKey dict env of
        Err e ->
            EvErr e

        Ok node ->
            case node of
                Custom ref args ->
                    if ref.name == "RBNode_elm_builtin" then
                        case args of
                            [ color, k, v, left, right ] ->
                                if isRed color then
                                    EvalResult.succeed (rbNode blackValue k v left right)

                                else
                                    EvalResult.succeed node

                            _ ->
                                EvalResult.succeed node

                    else
                        EvalResult.succeed node

                _ ->
                    EvalResult.succeed node


{-| Simple-but-correct remove: convert to sorted list, drop the key, rebuild.
Not as fast as a full RB-tree remove, but correct and still host-native.
The heavy rules we care about barely call remove, so this trade-off is fine.
-}
removeHelp : Value -> Value -> Env -> Result EvalErrorData Value
removeHelp targetKey dict env =
    case toSortedAssocList dict env of
        Err e ->
            Err e

        Ok pairs ->
            case filterOutKey targetKey pairs env of
                Err e ->
                    Err e

                Ok kept ->
                    Ok (fromSortedAssocList kept)


union : Value -> Value -> Eval Value
union left right cfg env =
    -- Insert all of left's bindings into right; left wins on collisions,
    -- matching elm/core's Dict.union semantics.
    case toSortedAssocList left env of
        Err e ->
            EvErr e

        Ok leftPairs ->
            unionInsertAll leftPairs right cfg env


unionInsertAll : List ( Value, Value ) -> Value -> Eval Value
unionInsertAll pairs acc cfg env =
    case pairs of
        [] ->
            EvalResult.succeed acc

        ( k, v ) :: rest ->
            case insert k v acc cfg env of
                EvOk next ->
                    unionInsertAll rest next cfg env

                other ->
                    other


fromList : Value -> Eval Value
fromList list cfg env =
    case list of
        List items ->
            fromListHelp items emptyValue cfg env

        _ ->
            EvalResult.fail (typeError env "Dict.fromList: expected a list")


fromListHelp : List Value -> Value -> Eval Value
fromListHelp items acc cfg env =
    case items of
        [] ->
            EvalResult.succeed acc

        (Tuple k v) :: rest ->
            case insert k v acc cfg env of
                EvOk next ->
                    fromListHelp rest next cfg env

                other ->
                    other

        _ :: rest ->
            fromListHelp rest acc cfg env


toList : Value -> Eval Value
toList dict _ env =
    case toSortedAssocList dict env of
        Err e ->
            EvErr e

        Ok pairs ->
            EvalResult.succeed (List (List.map (\( k, v ) -> Tuple k v) pairs))


toSortedAssocList : Value -> Env -> Result EvalErrorData (List ( Value, Value ))
toSortedAssocList dict _ =
    Ok (toSortedAssocListHelp dict [])


toSortedAssocListHelp : Value -> List ( Value, Value ) -> List ( Value, Value )
toSortedAssocListHelp dict acc =
    case dict of
        Custom ref args ->
            if ref.name == "RBEmpty_elm_builtin" then
                acc

            else
                case args of
                    [ _, key, value, left, right ] ->
                        toSortedAssocListHelp left (( key, value ) :: toSortedAssocListHelp right acc)

                    _ ->
                        acc

        _ ->
            acc


fromSortedAssocList : List ( Value, Value ) -> Value
fromSortedAssocList pairs =
    List.foldl
        (\( k, v ) acc -> insertRaw k v acc)
        emptyValue
        pairs


insertRaw : Value -> Value -> Value -> Value
insertRaw key value dict =
    case insertRawHelp key value dict of
        Custom ref args ->
            if ref.name == "RBNode_elm_builtin" then
                case args of
                    [ color, k, v, left, right ] ->
                        if isRed color then
                            rbNode blackValue k v left right

                        else
                            Custom ref args

                    _ ->
                        Custom ref args

            else
                Custom ref args

        other ->
            other


insertRawHelp : Value -> Value -> Value -> Value
insertRawHelp key value dict =
    case dict of
        Custom ref args ->
            if ref.name == "RBEmpty_elm_builtin" then
                rbNode redValue key value emptyValue emptyValue

            else
                case args of
                    [ nColor, nKey, nValue, nLeft, nRight ] ->
                        case rawCompare key nKey of
                            LT ->
                                balance nColor nKey nValue (insertRawHelp key value nLeft) nRight

                            EQ ->
                                rbNode nColor nKey value nLeft nRight

                            GT ->
                                balance nColor nKey nValue nLeft (insertRawHelp key value nRight)

                    _ ->
                        dict

        _ ->
            dict


{-| Fast comparison used by insertRaw when we know the key types up front.
We fall back to `compare` on toString for anything exotic.
-}
rawCompare : Value -> Value -> Order
rawCompare l r =
    case ( l, r ) of
        ( String ls, String rs ) ->
            Basics.compare ls rs

        ( Int li, Int ri ) ->
            Basics.compare li ri

        ( Tuple la lb, Tuple ra rb ) ->
            case rawCompare la ra of
                EQ ->
                    rawCompare lb rb

                other ->
                    other

        _ ->
            -- Fallback for uncommon keys.
            Basics.compare (Value.toString l) (Value.toString r)


filterOutKey : Value -> List ( Value, Value ) -> Env -> Result EvalErrorData (List ( Value, Value ))
filterOutKey targetKey pairs env =
    filterOutKeyHelp targetKey pairs [] env


filterOutKeyHelp : Value -> List ( Value, Value ) -> List ( Value, Value ) -> Env -> Result EvalErrorData (List ( Value, Value ))
filterOutKeyHelp targetKey pairs acc env =
    case pairs of
        [] ->
            Ok (List.reverse acc)

        ( k, v ) :: rest ->
            case Kernel.Utils.innerCompare k targetKey env of
                Err e ->
                    Err e

                Ok EQ ->
                    filterOutKeyHelp targetKey rest acc env

                Ok _ ->
                    filterOutKeyHelp targetKey rest (( k, v ) :: acc) env
