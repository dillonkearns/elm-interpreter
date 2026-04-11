module MemoSpec exposing (KeyStrategy(..), MemoSpec, Registry, buildRegistry, emptyRegistry, isEmpty, lookup)

import FastDict as Dict exposing (Dict)
import Set exposing (Set)


type alias Registry =
    Dict String MemoSpec


type alias MemoSpec =
    { id : Int
    , keyStrategy : KeyStrategy
    }


type KeyStrategy
    = StructuralArgs
    | ModuleLookupByRange
    | ModuleLookupByNode


emptyRegistry : Registry
emptyRegistry =
    Dict.empty


isEmpty : Registry -> Bool
isEmpty =
    Dict.isEmpty


lookup : String -> Registry -> Maybe MemoSpec
lookup =
    Dict.get


buildRegistry : Set String -> Registry
buildRegistry qualifiedNames =
    qualifiedNames
        |> Set.toList
        |> List.sort
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( index, qualifiedName ) registry ->
                Dict.insert qualifiedName (specFor index qualifiedName) registry
            )
            Dict.empty


specFor : Int -> String -> MemoSpec
specFor id qualifiedName =
    { id = id
    , keyStrategy = keyStrategyFor qualifiedName
    }


keyStrategyFor : String -> KeyStrategy
keyStrategyFor qualifiedName =
    case qualifiedName of
        "Review.ModuleNameLookupTable.moduleNameAt" ->
            ModuleLookupByRange

        "Review.ModuleNameLookupTable.fullModuleNameAt" ->
            ModuleLookupByRange

        "Review.ModuleNameLookupTable.moduleNameFor" ->
            ModuleLookupByNode

        "Review.ModuleNameLookupTable.fullModuleNameFor" ->
            ModuleLookupByNode

        _ ->
            StructuralArgs
