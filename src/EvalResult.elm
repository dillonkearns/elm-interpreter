module EvalResult exposing (andThen, combine, fail, fromResult, map, map2, mergeCoverageInto, mergeCoverageSets, onValue, succeed, toCoverageSet, toResult, toTriple)

import Rope exposing (Rope)
import Set
import Types exposing (CallTree, EvalErrorData, EvalResult(..))


{-| Append two call tree ropes, short-circuiting when either is empty.
-}
appendRopes : Rope CallTree -> Rope CallTree -> Rope CallTree
appendRopes a b =
    if Rope.isEmpty a then
        b

    else if Rope.isEmpty b then
        a

    else
        Rope.appendTo a b


{-| Append string ropes, short-circuiting when either is empty.
-}
appendLogRopes : Rope String -> Rope String -> Rope String
appendLogRopes a b =
    if Rope.isEmpty a then
        b

    else if Rope.isEmpty b then
        a

    else
        Rope.appendTo a b


{-| Merge two coverage sets.
-}
mergeCoverageSets : Set.Set Int -> Set.Set Int -> Set.Set Int
mergeCoverageSets a b =
    if Set.isEmpty a then
        b

    else if Set.isEmpty b then
        a

    else
        Set.union a b


succeed : a -> EvalResult a
succeed x =
    EvOk x


fail : EvalErrorData -> EvalResult a
fail e =
    EvErr e


fromResult : Result EvalErrorData a -> EvalResult a
fromResult x =
    case x of
        Ok v ->
            EvOk v

        Err e ->
            EvErr e


toResult : EvalResult out -> Result EvalErrorData out
toResult er =
    case er of
        EvOk v ->
            Ok v

        EvErr e ->
            Err e

        EvOkTrace v _ _ ->
            Ok v

        EvErrTrace e _ _ ->
            Err e

        EvYield _ _ _ ->
            -- Yield not handled at this level — framework driver should handle before toResult
            Err { currentModule = [], callStack = [], error = Types.TypeError "Unhandled EvYield in toResult" }

        EvMemoLookup _ _ ->
            Err { currentModule = [], callStack = [], error = Types.TypeError "Unhandled EvMemoLookup in toResult" }

        EvMemoStore _ _ ->
            Err { currentModule = [], callStack = [], error = Types.TypeError "Unhandled EvMemoStore in toResult" }

        EvOkCoverage v _ ->
            Ok v

        EvErrCoverage e _ ->
            Err e


{-| Convert to the legacy triple format. Used at module boundaries
where the caller expects (Result, Rope, Rope).
-}
toTriple : EvalResult out -> ( Result EvalErrorData out, Rope CallTree, Rope String )
toTriple er =
    case er of
        EvOk v ->
            ( Ok v, Rope.empty, Rope.empty )

        EvErr e ->
            ( Err e, Rope.empty, Rope.empty )

        EvOkTrace v t l ->
            ( Ok v, t, l )

        EvErrTrace e t l ->
            ( Err e, t, l )

        EvYield _ _ _ ->
            ( Err { currentModule = [], callStack = [], error = Types.TypeError "Unhandled EvYield" }, Rope.empty, Rope.empty )

        EvMemoLookup _ _ ->
            ( Err { currentModule = [], callStack = [], error = Types.TypeError "Unhandled EvMemoLookup" }, Rope.empty, Rope.empty )

        EvMemoStore _ _ ->
            ( Err { currentModule = [], callStack = [], error = Types.TypeError "Unhandled EvMemoStore" }, Rope.empty, Rope.empty )

        EvOkCoverage v _ ->
            ( Ok v, Rope.empty, Rope.empty )

        EvErrCoverage e _ ->
            ( Err e, Rope.empty, Rope.empty )


{-| Extract the coverage set from an EvalResult. Returns empty set for
non-coverage results.
-}
toCoverageSet : EvalResult out -> ( Result EvalErrorData out, Set.Set Int )
toCoverageSet er =
    case er of
        EvOk v ->
            ( Ok v, Set.empty )

        EvErr e ->
            ( Err e, Set.empty )

        EvOkTrace v _ _ ->
            ( Ok v, Set.empty )

        EvErrTrace e _ _ ->
            ( Err e, Set.empty )

        EvYield _ _ _ ->
            ( Err { currentModule = [], callStack = [], error = Types.TypeError "Unhandled EvYield in toCoverageSet" }, Set.empty )

        EvMemoLookup _ _ ->
            ( Err { currentModule = [], callStack = [], error = Types.TypeError "Unhandled EvMemoLookup in toCoverageSet" }, Set.empty )

        EvMemoStore _ _ ->
            ( Err { currentModule = [], callStack = [], error = Types.TypeError "Unhandled EvMemoStore in toCoverageSet" }, Set.empty )

        EvOkCoverage v s ->
            ( Ok v, s )

        EvErrCoverage e s ->
            ( Err e, s )


map : (a -> out) -> EvalResult a -> EvalResult out
map f er =
    case er of
        EvOk v ->
            EvOk (f v)

        EvErr e ->
            EvErr e

        EvOkTrace v t l ->
            EvOkTrace (f v) t l

        EvErrTrace e t l ->
            EvErrTrace e t l

        EvYield tag payload resume ->
            EvYield tag payload (\v -> map f (resume v))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> map f (resume maybeValue))

        EvMemoStore payload next ->
            EvMemoStore payload (map f next)

        EvOkCoverage v s ->
            EvOkCoverage (f v) s

        EvErrCoverage e s ->
            EvErrCoverage e s


andThen : (a -> EvalResult b) -> EvalResult a -> EvalResult b
andThen f er =
    case er of
        EvOk v ->
            f v

        EvErr e ->
            EvErr e

        EvOkTrace v trees logs ->
            mergeTraceInto trees logs (f v)

        EvErrTrace e trees logs ->
            EvErrTrace e trees logs

        EvYield tag payload resume ->
            EvYield tag payload (\v -> andThen f (resume v))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> andThen f (resume maybeValue))

        EvMemoStore payload next ->
            EvMemoStore payload (andThen f next)

        EvOkCoverage v outerSet ->
            mergeCoverageInto outerSet (f v)

        EvErrCoverage e _ ->
            EvErrCoverage e Set.empty


{-| Merge trace data from an outer evaluation into an inner result.
-}
mergeTraceInto : Rope CallTree -> Rope String -> EvalResult out -> EvalResult out
mergeTraceInto trees logs er =
    case er of
        EvOk v ->
            EvOkTrace v trees logs

        EvErr e ->
            EvErrTrace e trees logs

        EvOkTrace v t l ->
            EvOkTrace v (appendRopes trees t) (appendLogRopes logs l)

        EvErrTrace e t l ->
            EvErrTrace e (appendRopes trees t) (appendLogRopes logs l)

        EvYield tag payload resume ->
            EvYield tag payload (\v -> mergeTraceInto trees logs (resume v))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> mergeTraceInto trees logs (resume maybeValue))

        EvMemoStore payload next ->
            EvMemoStore payload (mergeTraceInto trees logs next)

        EvOkCoverage v s ->
            EvOkCoverage v s

        EvErrCoverage e s ->
            EvErrCoverage e s


{-| Merge coverage data from an outer evaluation into an inner result.
-}
mergeCoverageInto : Set.Set Int -> EvalResult out -> EvalResult out
mergeCoverageInto outerSet er =
    case er of
        EvOk v ->
            EvOkCoverage v outerSet

        EvErr e ->
            EvErrCoverage e outerSet

        EvOkTrace v _ _ ->
            EvOkCoverage v outerSet

        EvErrTrace e _ _ ->
            EvErrCoverage e outerSet

        EvOkCoverage v innerSet ->
            EvOkCoverage v (mergeCoverageSets outerSet innerSet)

        EvErrCoverage e innerSet ->
            EvErrCoverage e (mergeCoverageSets outerSet innerSet)

        EvYield tag payload resume ->
            EvYield tag payload (\v -> mergeCoverageInto outerSet (resume v))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> mergeCoverageInto outerSet (resume maybeValue))

        EvMemoStore payload next ->
            EvMemoStore payload (mergeCoverageInto outerSet next)


map2 : (a -> b -> out) -> EvalResult a -> EvalResult b -> EvalResult out
map2 f a b =
    case a of
        EvOk av ->
            case b of
                EvOk bv ->
                    EvOk (f av bv)

                EvErr e ->
                    EvErr e

                EvOkTrace bv bt bl ->
                    EvOkTrace (f av bv) bt bl

                EvErrTrace e bt bl ->
                    EvErrTrace e bt bl

                EvYield tag payload resume ->
                    EvYield tag payload (\v -> map2 f (EvOk av) (resume v))

                EvMemoLookup payload resume ->
                    EvMemoLookup payload (\maybeValue -> map2 f (EvOk av) (resume maybeValue))

                EvMemoStore payload next ->
                    EvMemoStore payload (map2 f (EvOk av) next)

                EvOkCoverage bv bs ->
                    EvOkCoverage (f av bv) bs

                EvErrCoverage e bs ->
                    EvErrCoverage e bs

        EvErr e ->
            EvErr e

        EvOkTrace av at al ->
            case b of
                EvOk bv ->
                    EvOkTrace (f av bv) at al

                EvErr e ->
                    EvErrTrace e at al

                EvOkTrace bv bt bl ->
                    EvOkTrace (f av bv) (appendRopes at bt) (appendLogRopes al bl)

                EvErrTrace e bt bl ->
                    EvErrTrace e (appendRopes at bt) (appendLogRopes al bl)

                EvOkCoverage bv _ ->
                    EvOkTrace (f av bv) at al

                EvErrCoverage e _ ->
                    EvErrTrace e at al

                EvYield tag payload resume ->
                    EvYield tag payload (\v -> map2 f (EvOkTrace av at al) (resume v))

                EvMemoLookup payload resume ->
                    EvMemoLookup payload (\maybeValue -> map2 f (EvOkTrace av at al) (resume maybeValue))

                EvMemoStore payload next ->
                    EvMemoStore payload (map2 f (EvOkTrace av at al) next)

        EvErrTrace e at al ->
            case b of
                EvOk _ ->
                    EvErrTrace e at al

                EvErr _ ->
                    EvErrTrace e at al

                EvOkTrace _ bt bl ->
                    EvErrTrace e (appendRopes at bt) (appendLogRopes al bl)

                EvErrTrace _ bt bl ->
                    EvErrTrace e (appendRopes at bt) (appendLogRopes al bl)

                EvOkCoverage _ _ ->
                    EvErrTrace e at al

                EvErrCoverage _ _ ->
                    EvErrTrace e at al

                EvYield _ _ _ ->
                    EvErrTrace e at al

                EvMemoLookup _ _ ->
                    EvErrTrace e at al

                EvMemoStore _ _ ->
                    EvErrTrace e at al

        EvOkCoverage av as_ ->
            case b of
                EvOk bv ->
                    EvOkCoverage (f av bv) as_

                EvErr e ->
                    EvErrCoverage e as_

                EvOkTrace bv _ _ ->
                    EvOkCoverage (f av bv) as_

                EvErrTrace e _ _ ->
                    EvErrCoverage e as_

                EvOkCoverage bv bs ->
                    EvOkCoverage (f av bv) (mergeCoverageSets as_ bs)

                EvErrCoverage e bs ->
                    EvErrCoverage e (mergeCoverageSets as_ bs)

                EvYield tag payload resume ->
                    EvYield tag payload (\v -> map2 f (EvOkCoverage av as_) (resume v))

                EvMemoLookup payload resume ->
                    EvMemoLookup payload (\maybeValue -> map2 f (EvOkCoverage av as_) (resume maybeValue))

                EvMemoStore payload next ->
                    EvMemoStore payload (map2 f (EvOkCoverage av as_) next)

        EvErrCoverage e as_ ->
            case b of
                EvOk _ ->
                    EvErrCoverage e as_

                EvErr _ ->
                    EvErrCoverage e as_

                EvOkTrace _ _ _ ->
                    EvErrCoverage e as_

                EvErrTrace _ _ _ ->
                    EvErrCoverage e as_

                EvOkCoverage _ bs ->
                    EvErrCoverage e (mergeCoverageSets as_ bs)

                EvErrCoverage _ bs ->
                    EvErrCoverage e (mergeCoverageSets as_ bs)

                EvYield _ _ _ ->
                    EvErrCoverage e as_

                EvMemoLookup _ _ ->
                    EvErrCoverage e as_

                EvMemoStore _ _ ->
                    EvErrCoverage e as_

        EvYield tag payload resume ->
            EvYield tag payload (\v -> map2 f (resume v) b)

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> map2 f (resume maybeValue) b)

        EvMemoStore payload next ->
            EvMemoStore payload (map2 f next b)


onValue : (a -> Result EvalErrorData out) -> EvalResult a -> EvalResult out
onValue f er =
    case er of
        EvOk v ->
            fromResult (f v)

        EvErr e ->
            EvErr e

        EvOkTrace v t l ->
            case f v of
                Ok w ->
                    EvOkTrace w t l

                Err e ->
                    EvErrTrace e t l

        EvErrTrace e t l ->
            EvErrTrace e t l

        EvYield tag payload resume ->
            EvYield tag payload (\v -> onValue f (resume v))

        EvMemoLookup payload resume ->
            EvMemoLookup payload (\maybeValue -> onValue f (resume maybeValue))

        EvMemoStore payload next ->
            EvMemoStore payload (onValue f next)

        EvOkCoverage v s ->
            case f v of
                Ok w ->
                    EvOkCoverage w s

                Err e ->
                    EvErrCoverage e s

        EvErrCoverage e s ->
            EvErrCoverage e s


combine : List (EvalResult t) -> EvalResult (List t)
combine ls =
    combinePlain ls []


{-| Fast path for combine when no trace/coverage data has been seen.
-}
combinePlain : List (EvalResult t) -> List t -> EvalResult (List t)
combinePlain queue vacc =
    case queue of
        [] ->
            EvOk (List.reverse vacc)

        (EvOk v) :: tail ->
            combinePlain tail (v :: vacc)

        (EvErr e) :: _ ->
            EvErr e

        (EvYield tag payload resume) :: tail ->
            EvYield tag payload (\v -> combinePlain (resume v :: tail) vacc)

        (EvMemoLookup payload resume) :: tail ->
            EvMemoLookup payload (\maybeValue -> combinePlain (resume maybeValue :: tail) vacc)

        (EvMemoStore payload next) :: tail ->
            EvMemoStore payload (combinePlain (next :: tail) vacc)

        (EvOkCoverage _ _) :: _ ->
            -- Switch to coverage path
            combineCoverage queue (List.reverse vacc) Set.empty

        _ ->
            -- Switch to traced path for remaining items
            combineTraced queue (List.reverse vacc) Rope.empty Rope.empty


{-| Traced path for combine when trace data exists.
-}
combineTraced : List (EvalResult t) -> List t -> Rope CallTree -> Rope String -> EvalResult (List t)
combineTraced queue vacc tacc lacc =
    case queue of
        [] ->
            EvOkTrace (List.reverse vacc) tacc lacc

        (EvOk v) :: tail ->
            combineTraced tail (v :: vacc) tacc lacc

        (EvOkTrace v t l) :: tail ->
            combineTraced tail (v :: vacc) (appendRopes tacc t) (appendLogRopes lacc l)

        (EvErr e) :: _ ->
            EvErrTrace e tacc lacc

        (EvErrTrace e t l) :: _ ->
            EvErrTrace e (appendRopes tacc t) (appendLogRopes lacc l)

        (EvYield tag payload resume) :: tail ->
            EvYield tag payload (\v -> combineTraced (resume v :: tail) vacc tacc lacc)

        (EvMemoLookup payload resume) :: tail ->
            EvMemoLookup payload (\maybeValue -> combineTraced (resume maybeValue :: tail) vacc tacc lacc)

        (EvMemoStore payload next) :: tail ->
            EvMemoStore payload (combineTraced (next :: tail) vacc tacc lacc)

        (EvOkCoverage v _) :: tail ->
            combineTraced tail (v :: vacc) tacc lacc

        (EvErrCoverage e _) :: _ ->
            EvErrTrace e tacc lacc


{-| Coverage path for combine — merges Set Int directly.
-}
combineCoverage : List (EvalResult t) -> List t -> Set.Set Int -> EvalResult (List t)
combineCoverage queue vacc sacc =
    case queue of
        [] ->
            EvOkCoverage (List.reverse vacc) sacc

        (EvOk v) :: tail ->
            combineCoverage tail (v :: vacc) sacc

        (EvOkCoverage v s) :: tail ->
            combineCoverage tail (v :: vacc) (mergeCoverageSets sacc s)

        (EvErr e) :: _ ->
            EvErrCoverage e sacc

        (EvErrCoverage e s) :: _ ->
            EvErrCoverage e (mergeCoverageSets sacc s)

        (EvOkTrace v _ _) :: tail ->
            combineCoverage tail (v :: vacc) sacc

        (EvErrTrace e _ _) :: _ ->
            EvErrCoverage e sacc

        (EvYield _ _ _) :: _ ->
            EvErrCoverage { currentModule = [], callStack = [], error = Types.TypeError "Unhandled EvYield in combineCoverage" } sacc

        (EvMemoLookup _ _) :: _ ->
            EvErrCoverage { currentModule = [], callStack = [], error = Types.TypeError "Unhandled EvMemoLookup in combineCoverage" } sacc

        (EvMemoStore _ _) :: _ ->
            EvErrCoverage { currentModule = [], callStack = [], error = Types.TypeError "Unhandled EvMemoStore in combineCoverage" } sacc
