module MemoRuntime exposing
    ( MemoCache
    , MemoEntry
    , FunctionMemoStats
    , MemoStats
    , decodeMemoCache
    , decodeLookupPayload
    , decodeStorePayload
    , encodeMemoCache
    , disabledMemoStats
    , emptyMemoCache
    , emptyMemoStats
    , entryCount
    , encodeLookupPayload
    , encodeLookupPayloadWithKey
    , encodeStorePayload
    , encodeStorePayloadWithKey
    , formatMemoStats
    , lookupEntries
    , lookupCompactValue
    , lookupTag
    , maybeJust
    , maybeNothing
    , recordFunctionHit
    , recordFunctionLookup
    , recordFunctionMiss
    , recordFunctionStore
    , storeEntry
    , storeCompactValue
    , storeTag
    , topFunctionStats
    )

import FastDict as Dict exposing (Dict)
import Types exposing (Value(..))


type alias MemoEntry =
    { deepFingerprint : Int
    , value : Value
    }


type alias MemoCache =
    { generic : Dict Int (Dict Int (List MemoEntry))
    , compact : Dict Int (Dict Int Value)
    }


type alias MemoStats =
    { enabled : Bool
    , lookups : Int
    , hits : Int
    , misses : Int
    , stores : Int
    , byFunction : Dict String FunctionMemoStats
    }


type alias FunctionMemoStats =
    { lookups : Int
    , hits : Int
    , misses : Int
    , stores : Int
    }


lookupTag : String
lookupTag =
    "__elm_interpreter_memo_lookup"


storeTag : String
storeTag =
    "__elm_interpreter_memo_store"


emptyMemoCache : MemoCache
emptyMemoCache =
    { generic = Dict.empty
    , compact = Dict.empty
    }


emptyMemoStats : MemoStats
emptyMemoStats =
    { enabled = True
    , lookups = 0
    , hits = 0
    , misses = 0
    , stores = 0
    , byFunction = Dict.empty
    }


disabledMemoStats : MemoStats
disabledMemoStats =
    { enabled = False
    , lookups = 0
    , hits = 0
    , misses = 0
    , stores = 0
    , byFunction = Dict.empty
    }


encodeLookupPayload : Types.MemoLookupPayload -> Value
encodeLookupPayload payload =
    Record
        (Dict.fromList
            [ ( "qualifiedName"
              , case payload.qualifiedName of
                    Just qualifiedName ->
                        String qualifiedName

                    Nothing ->
                        Unit
              )
            , ( "specId", Int payload.specId )
            , ( "compactFingerprint"
              , case payload.compactFingerprint of
                    Just compactFingerprint ->
                        Int compactFingerprint

                    Nothing ->
                        Unit
              )
            , ( "args", List (Maybe.withDefault [] payload.args) )
            ]
        )


encodeLookupPayloadWithKey :
    { specId : Int, qualifiedName : Maybe String, shallowFingerprint : Int, deepFingerprint : Int }
    -> Value
encodeLookupPayloadWithKey payload =
    Record
        (Dict.fromList
            [ ( "qualifiedName"
              , case payload.qualifiedName of
                    Just qualifiedName ->
                        String qualifiedName

                    Nothing ->
                        Unit
              )
            , ( "specId", Int payload.specId )
            , ( "compactFingerprint", Unit )
            , ( "shallowFingerprint", Int payload.shallowFingerprint )
            , ( "deepFingerprint", Int payload.deepFingerprint )
            ]
        )


decodeLookupPayload : Value -> Maybe Types.MemoLookupPayload
decodeLookupPayload value =
    case value of
        Record fields ->
            case Dict.get "specId" fields of
                Just (Int specId) ->
                    case
                        ( Dict.get "shallowFingerprint" fields
                        , Dict.get "deepFingerprint" fields
                        )
                    of
                        ( Just (Int shallowFingerprint), Just (Int deepFingerprint) ) ->
                            Just
                                { specId = specId
                                , qualifiedName = decodeQualifiedName fields
                                , compactFingerprint = decodeCompactFingerprint fields
                                , args = Nothing
                                , shallowFingerprint = Just shallowFingerprint
                                , deepFingerprint = Just deepFingerprint
                                }

                        _ ->
                            case Dict.get "args" fields of
                                Just (List args) ->
                                    Just
                                        { specId = specId
                                        , qualifiedName = decodeQualifiedName fields
                                        , compactFingerprint = decodeCompactFingerprint fields
                                        , args = Just args
                                        , shallowFingerprint = Nothing
                                        , deepFingerprint = Nothing
                                        }

                                _ ->
                                    Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


encodeStorePayload : Types.MemoStorePayload -> Value
encodeStorePayload payload =
    Record
        (Dict.fromList
            [ ( "qualifiedName"
              , case payload.qualifiedName of
                    Just qualifiedName ->
                        String qualifiedName

                    Nothing ->
                        Unit
              )
            , ( "specId", Int payload.specId )
            , ( "compactFingerprint"
              , case payload.compactFingerprint of
                    Just compactFingerprint ->
                        Int compactFingerprint

                    Nothing ->
                        Unit
              )
            , ( "args", List (Maybe.withDefault [] payload.args) )
            , ( "value", payload.value )
            ]
        )


encodeStorePayloadWithKey :
    { specId : Int, qualifiedName : Maybe String, shallowFingerprint : Int, deepFingerprint : Int, value : Value }
    -> Value
encodeStorePayloadWithKey payload =
    Record
        (Dict.fromList
            [ ( "qualifiedName"
              , case payload.qualifiedName of
                    Just qualifiedName ->
                        String qualifiedName

                    Nothing ->
                        Unit
              )
            , ( "specId", Int payload.specId )
            , ( "compactFingerprint", Unit )
            , ( "shallowFingerprint", Int payload.shallowFingerprint )
            , ( "deepFingerprint", Int payload.deepFingerprint )
            , ( "value", payload.value )
            ]
        )


decodeStorePayload : Value -> Maybe Types.MemoStorePayload
decodeStorePayload value =
    case value of
        Record fields ->
            case Dict.get "specId" fields of
                Just (Int specId) ->
                    case Dict.get "value" fields of
                        Just cachedValue ->
                            case
                                ( Dict.get "shallowFingerprint" fields
                                , Dict.get "deepFingerprint" fields
                                )
                            of
                                ( Just (Int shallowFingerprint), Just (Int deepFingerprint) ) ->
                                    Just
                                        { specId = specId
                                        , qualifiedName = decodeQualifiedName fields
                                        , compactFingerprint = decodeCompactFingerprint fields
                                        , args = Nothing
                                        , shallowFingerprint = Just shallowFingerprint
                                        , deepFingerprint = Just deepFingerprint
                                        , value = cachedValue
                                        }

                                _ ->
                                    case Dict.get "args" fields of
                                        Just (List args) ->
                                            Just
                                                { specId = specId
                                                , qualifiedName = decodeQualifiedName fields
                                                , compactFingerprint = decodeCompactFingerprint fields
                                                , args = Just args
                                                , shallowFingerprint = Nothing
                                                , deepFingerprint = Nothing
                                                , value = cachedValue
                                                }

                                        _ ->
                                            Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


decodeQualifiedName : Dict String Value -> Maybe String
decodeQualifiedName fields =
    case Dict.get "qualifiedName" fields of
        Just (String qualifiedName) ->
            Just qualifiedName

        _ ->
            Nothing


decodeCompactFingerprint : Dict String Value -> Maybe Int
decodeCompactFingerprint fields =
    case Dict.get "compactFingerprint" fields of
        Just (Int compactFingerprint) ->
            Just compactFingerprint

        _ ->
            Nothing


lookupEntries : Int -> Int -> MemoCache -> Maybe (List MemoEntry)
lookupEntries specId shallowFingerprint memoCache =
    memoCache.generic
        |> Dict.get specId
        |> Maybe.andThen (Dict.get shallowFingerprint)


lookupCompactValue : Int -> Int -> MemoCache -> Maybe Value
lookupCompactValue specId compactFingerprint memoCache =
    memoCache.compact
        |> Dict.get specId
        |> Maybe.andThen (Dict.get compactFingerprint)


storeEntry : Int -> Int -> MemoEntry -> MemoCache -> MemoCache
storeEntry specId shallowFingerprint entry memoCache =
    let
        upsertEntry : Maybe (List MemoEntry) -> List MemoEntry
        upsertEntry maybeEntries =
            case maybeEntries of
                Just entries ->
                    if List.any (\existing -> existing.deepFingerprint == entry.deepFingerprint) entries then
                        List.map
                            (\existing ->
                                if existing.deepFingerprint == entry.deepFingerprint then
                                    entry

                                else
                                    existing
                            )
                            entries

                    else
                        entry :: entries

                Nothing ->
                    [ entry ]

        updatedBuckets =
            case Dict.get specId memoCache.generic of
                Just buckets ->
                    Dict.update shallowFingerprint
                        (\maybeEntries -> Just (upsertEntry maybeEntries))
                        buckets

                Nothing ->
                    Dict.singleton shallowFingerprint [ entry ]
    in
    { memoCache
        | generic = Dict.insert specId updatedBuckets memoCache.generic
    }


storeCompactValue : Int -> Int -> Value -> MemoCache -> MemoCache
storeCompactValue specId compactFingerprint value memoCache =
    let
        updatedBuckets =
            case Dict.get specId memoCache.compact of
                Just buckets ->
                    Dict.insert compactFingerprint value buckets

                Nothing ->
                    Dict.singleton compactFingerprint value
    in
    { memoCache
        | compact = Dict.insert specId updatedBuckets memoCache.compact
    }


maybeJust : Value -> Value
maybeJust value =
    Custom { moduleName = [ "Maybe" ], name = "Just" } [ value ]


maybeNothing : Value
maybeNothing =
    Custom { moduleName = [ "Maybe" ], name = "Nothing" } []


recordFunctionLookup : Maybe String -> MemoStats -> MemoStats
recordFunctionLookup maybeQualifiedName memoStats =
    if memoStats.enabled then
        { memoStats
            | lookups = memoStats.lookups + 1
            , byFunction =
                case maybeQualifiedName of
                    Just qualifiedName ->
                        Dict.update qualifiedName
                            (\maybeStats ->
                                Just
                                    (case maybeStats of
                                        Just stats ->
                                            { stats | lookups = stats.lookups + 1 }

                                        Nothing ->
                                            { lookups = 1, hits = 0, misses = 0, stores = 0 }
                                    )
                            )
                            memoStats.byFunction

                    Nothing ->
                        memoStats.byFunction
        }

    else
        memoStats


recordFunctionHit : Maybe String -> MemoStats -> MemoStats
recordFunctionHit maybeQualifiedName memoStats =
    if memoStats.enabled then
        { memoStats
            | hits = memoStats.hits + 1
            , byFunction =
                case maybeQualifiedName of
                    Just qualifiedName ->
                        Dict.update qualifiedName
                            (\maybeStats ->
                                Just
                                    (case maybeStats of
                                        Just stats ->
                                            { stats | hits = stats.hits + 1 }

                                        Nothing ->
                                            { lookups = 0, hits = 1, misses = 0, stores = 0 }
                                    )
                            )
                            memoStats.byFunction

                    Nothing ->
                        memoStats.byFunction
        }

    else
        memoStats


recordFunctionMiss : Maybe String -> MemoStats -> MemoStats
recordFunctionMiss maybeQualifiedName memoStats =
    if memoStats.enabled then
        { memoStats
            | misses = memoStats.misses + 1
            , byFunction =
                case maybeQualifiedName of
                    Just qualifiedName ->
                        Dict.update qualifiedName
                            (\maybeStats ->
                                Just
                                    (case maybeStats of
                                        Just stats ->
                                            { stats | misses = stats.misses + 1 }

                                        Nothing ->
                                            { lookups = 0, hits = 0, misses = 1, stores = 0 }
                                    )
                            )
                            memoStats.byFunction

                    Nothing ->
                        memoStats.byFunction
        }

    else
        memoStats


recordFunctionStore : Maybe String -> MemoStats -> MemoStats
recordFunctionStore maybeQualifiedName memoStats =
    if memoStats.enabled then
        { memoStats
            | stores = memoStats.stores + 1
            , byFunction =
                case maybeQualifiedName of
                    Just qualifiedName ->
                        Dict.update qualifiedName
                            (\maybeStats ->
                                Just
                                    (case maybeStats of
                                        Just stats ->
                                            { stats | stores = stats.stores + 1 }

                                        Nothing ->
                                            { lookups = 0, hits = 0, misses = 0, stores = 1 }
                                    )
                            )
                            memoStats.byFunction

                    Nothing ->
                        memoStats.byFunction
        }

    else
        memoStats


encodeMemoCache : MemoCache -> Value
encodeMemoCache memoCache =
    Record
        (Dict.fromList
            [ ( "generic", encodeGenericCache memoCache.generic )
            , ( "compact", encodeCompactCache memoCache.compact )
            ]
        )


decodeMemoCache : Value -> Maybe MemoCache
decodeMemoCache value =
    case value of
        Record fields ->
            case ( Dict.get "generic" fields, Dict.get "compact" fields ) of
                ( Just genericValue, Just compactValue ) ->
                    Maybe.map2
                        (\generic compact ->
                            { generic = generic
                            , compact = compact
                            }
                        )
                        (decodeGenericCache genericValue)
                        (decodeCompactCache compactValue)

                _ ->
                    Nothing

        _ ->
            Nothing


encodeGenericCache : Dict Int (Dict Int (List MemoEntry)) -> Value
encodeGenericCache genericCache =
    List
        (genericCache
            |> Dict.toList
            |> List.map
                (\( specId, buckets ) ->
                    Record
                        (Dict.fromList
                            [ ( "specId", Int specId )
                            , ( "buckets"
                              , List
                                    (buckets
                                        |> Dict.toList
                                        |> List.map
                                            (\( shallowFingerprint, entries ) ->
                                                Record
                                                    (Dict.fromList
                                                        [ ( "shallowFingerprint", Int shallowFingerprint )
                                                        , ( "entries"
                                                          , List
                                                                (entries
                                                                    |> List.map
                                                                        (\entry ->
                                                                            Record
                                                                                (Dict.fromList
                                                                                    [ ( "deepFingerprint", Int entry.deepFingerprint )
                                                                                    , ( "value", entry.value )
                                                                                    ]
                                                                                )
                                                                        )
                                                                )
                                                          )
                                                        ]
                                                    )
                                            )
                                    )
                              )
                            ]
                        )
                )
        )


decodeGenericCache : Value -> Maybe (Dict Int (Dict Int (List MemoEntry)))
decodeGenericCache value =
    case value of
        List specEntries ->
            specEntries
                |> List.foldl
                    (\entry maybeCache ->
                        maybeCache
                            |> Maybe.andThen
                                (\cache ->
                                    decodeSpecEntry entry
                                        |> Maybe.map (\( specId, buckets ) -> Dict.insert specId buckets cache)
                                )
                    )
                    (Just Dict.empty)

        _ ->
            Nothing


encodeCompactCache : Dict Int (Dict Int Value) -> Value
encodeCompactCache compactCache =
    List
        (compactCache
            |> Dict.toList
            |> List.map
                (\( specId, buckets ) ->
                    Record
                        (Dict.fromList
                            [ ( "specId", Int specId )
                            , ( "buckets"
                              , List
                                    (buckets
                                        |> Dict.toList
                                        |> List.map
                                            (\( compactFingerprint, cachedValue ) ->
                                                Record
                                                    (Dict.fromList
                                                        [ ( "compactFingerprint", Int compactFingerprint )
                                                        , ( "value", cachedValue )
                                                        ]
                                                    )
                                            )
                                    )
                              )
                            ]
                        )
                )
        )


decodeCompactCache : Value -> Maybe (Dict Int (Dict Int Value))
decodeCompactCache value =
    case value of
        List specEntries ->
            specEntries
                |> List.foldl
                    (\entry maybeCache ->
                        maybeCache
                            |> Maybe.andThen
                                (\cache ->
                                    decodeCompactSpecEntry entry
                                        |> Maybe.map (\( specId, buckets ) -> Dict.insert specId buckets cache)
                                )
                    )
                    (Just Dict.empty)

        _ ->
            Nothing


decodeSpecEntry : Value -> Maybe ( Int, Dict Int (List MemoEntry) )
decodeSpecEntry value =
    case value of
        Record fields ->
            case ( Dict.get "specId" fields, Dict.get "buckets" fields ) of
                ( Just (Int specId), Just (List bucketValues) ) ->
                    bucketValues
                        |> List.foldl
                            (\bucketValue maybeBuckets ->
                                maybeBuckets
                                    |> Maybe.andThen
                                        (\buckets ->
                                            decodeBucket bucketValue
                                                |> Maybe.map (\( shallowFingerprint, entries ) -> Dict.insert shallowFingerprint entries buckets)
                                        )
                            )
                            (Just Dict.empty)
                        |> Maybe.map (\buckets -> ( specId, buckets ))

                _ ->
                    Nothing

        _ ->
            Nothing


decodeBucket : Value -> Maybe ( Int, List MemoEntry )
decodeBucket value =
    case value of
        Record fields ->
            case ( Dict.get "shallowFingerprint" fields, Dict.get "entries" fields ) of
                ( Just (Int shallowFingerprint), Just (List entryValues) ) ->
                    entryValues
                        |> List.foldl
                            (\entryValue maybeEntries ->
                                maybeEntries
                                    |> Maybe.andThen
                                        (\entries ->
                                            decodeEntry entryValue
                                                |> Maybe.map (\entry -> entry :: entries)
                                        )
                            )
                            (Just [])
                        |> Maybe.map (\entries -> ( shallowFingerprint, List.reverse entries ))

                _ ->
                    Nothing

        _ ->
            Nothing


decodeCompactSpecEntry : Value -> Maybe ( Int, Dict Int Value )
decodeCompactSpecEntry value =
    case value of
        Record fields ->
            case ( Dict.get "specId" fields, Dict.get "buckets" fields ) of
                ( Just (Int specId), Just (List bucketValues) ) ->
                    bucketValues
                        |> List.foldl
                            (\bucketValue maybeBuckets ->
                                maybeBuckets
                                    |> Maybe.andThen
                                        (\buckets ->
                                            decodeCompactBucket bucketValue
                                                |> Maybe.map (\( compactFingerprint, cachedValue ) -> Dict.insert compactFingerprint cachedValue buckets)
                                        )
                            )
                            (Just Dict.empty)
                        |> Maybe.map (\buckets -> ( specId, buckets ))

                _ ->
                    Nothing

        _ ->
            Nothing


decodeCompactBucket : Value -> Maybe ( Int, Value )
decodeCompactBucket value =
    case value of
        Record fields ->
            case ( Dict.get "compactFingerprint" fields, Dict.get "value" fields ) of
                ( Just (Int compactFingerprint), Just cachedValue ) ->
                    Just ( compactFingerprint, cachedValue )

                _ ->
                    Nothing

        _ ->
            Nothing


decodeEntry : Value -> Maybe MemoEntry
decodeEntry value =
    case value of
        Record fields ->
            case ( Dict.get "deepFingerprint" fields, Dict.get "value" fields ) of
                ( Just (Int deepFingerprint), Just cachedValue ) ->
                    Just
                        { deepFingerprint = deepFingerprint
                        , value = cachedValue
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


entryCount : MemoCache -> Int
entryCount memoCache =
    genericEntryCount memoCache.generic + compactEntryCount memoCache.compact


genericEntryCount : Dict Int (Dict Int (List MemoEntry)) -> Int
genericEntryCount genericCache =
    Dict.foldl
        (\_ buckets total ->
            total
                + Dict.foldl
                    (\_ entries subtotal ->
                        subtotal + List.length entries
                    )
                    0
                    buckets
        )
        0
        genericCache


compactEntryCount : Dict Int (Dict Int Value) -> Int
compactEntryCount compactCache =
    Dict.foldl
        (\_ buckets total ->
            total + Dict.size buckets
        )
        0
        compactCache


topFunctionStats : Int -> MemoStats -> List ( String, FunctionMemoStats )
topFunctionStats limit memoStats =
    memoStats.byFunction
        |> Dict.toList
        |> List.sortWith compareFunctionStats
        |> List.take limit


compareFunctionStats : ( String, FunctionMemoStats ) -> ( String, FunctionMemoStats ) -> Order
compareFunctionStats ( leftName, leftStats ) ( rightName, rightStats ) =
    case compare rightStats.lookups leftStats.lookups of
        EQ ->
            case compare rightStats.hits leftStats.hits of
                EQ ->
                    case compare rightStats.stores leftStats.stores of
                        EQ ->
                            compare leftName rightName

                        other ->
                            other

                other ->
                    other

        other ->
            other


formatMemoStats : Int -> MemoStats -> String
formatMemoStats memoEntryCount memoStats =
    String.join ", "
        ([ "lookups=" ++ String.fromInt memoStats.lookups
         , "hits=" ++ String.fromInt memoStats.hits
         , "misses=" ++ String.fromInt memoStats.misses
         , "stores=" ++ String.fromInt memoStats.stores
         , "hitRate=" ++ formatHitRate memoStats.hits memoStats.lookups
         , "entries=" ++ String.fromInt memoEntryCount
         ]
            ++ (case topFunctionStats 5 memoStats of
                    [] ->
                        []

                    topStats ->
                        [ "top="
                            ++ (topStats
                                    |> List.map formatFunctionStats
                                    |> String.join "; "
                               )
                        ]
               )
        )


formatFunctionStats : ( String, FunctionMemoStats ) -> String
formatFunctionStats ( qualifiedName, stats ) =
    qualifiedName
        ++ " lookups="
        ++ String.fromInt stats.lookups
        ++ " hits="
        ++ String.fromInt stats.hits
        ++ " misses="
        ++ String.fromInt stats.misses
        ++ " stores="
        ++ String.fromInt stats.stores
        ++ " hitRate="
        ++ formatHitRate stats.hits stats.lookups


formatHitRate : Int -> Int -> String
formatHitRate hits lookups =
    if lookups <= 0 then
        "0%"

    else
        let
            rounded =
                toFloat (round ((toFloat hits / toFloat lookups) * 1000)) / 10
        in
        String.fromFloat rounded ++ "%"
