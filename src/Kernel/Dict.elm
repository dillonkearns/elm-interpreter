module Kernel.Dict exposing (foldl, foldr, map)

{-| Kernel implementations for Dict higher-order functions.
Walks the RBTree structure in host Elm, calling the interpreter
function only for the user callback per node. This avoids trampoline
overhead for the tree traversal itself.

Dict values at runtime are:
  - Custom { name = "RBEmpty_elm_builtin" } []
  - Custom { name = "RBNode_elm_builtin" } [color, key, value, left, right]
-}

import EvalResult
import Types exposing (Eval, EvalResult(..), Value(..))


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
