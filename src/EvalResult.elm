module EvalResult exposing (andThen, combine, fail, fromResult, map, map2, onValue, succeed, toResult, toTriple)

import Rope exposing (Rope)
import Types exposing (CallTree, EvalErrorData, EvalErrorKind(..), EvalResult(..))


{-| Append two ropes, short-circuiting when either is empty.
This avoids building deep trees of empty Node wrappers when trace is off.
-}
appendRopes : Rope a -> Rope a -> Rope a
appendRopes a b =
    if Rope.isEmpty a then
        b

    else if Rope.isEmpty b then
        a

    else
        Rope.appendTo a b


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
            EvOkTrace v (appendRopes trees t) (appendRopes logs l)

        EvErrTrace e t l ->
            EvErrTrace e (appendRopes trees t) (appendRopes logs l)

        EvYield tag payload resume ->
            EvYield tag payload (\v -> mergeTraceInto trees logs (resume v))


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

        EvErr e ->
            EvErr e

        EvOkTrace av at al ->
            case b of
                EvOk bv ->
                    EvOkTrace (f av bv) at al

                EvErr e ->
                    EvErrTrace e at al

                EvOkTrace bv bt bl ->
                    EvOkTrace (f av bv) (appendRopes at bt) (appendRopes al bl)

                EvErrTrace e bt bl ->
                    EvErrTrace e (appendRopes at bt) (appendRopes al bl)

                EvYield tag payload resume ->
                    EvYield tag payload (\v -> map2 f (EvOkTrace av at al) (resume v))

        EvErrTrace e at al ->
            case b of
                EvOk _ ->
                    EvErrTrace e at al

                EvErr _ ->
                    EvErrTrace e at al

                EvOkTrace _ bt bl ->
                    EvErrTrace e (appendRopes at bt) (appendRopes al bl)

                EvErrTrace _ bt bl ->
                    EvErrTrace e (appendRopes at bt) (appendRopes al bl)

                EvYield _ _ _ ->
                    EvErrTrace e at al

        EvYield tag payload resume ->
            EvYield tag payload (\v -> map2 f (resume v) b)


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


combine : List (EvalResult t) -> EvalResult (List t)
combine ls =
    combinePlain ls []


{-| Fast path for combine when no trace data has been seen.
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
            combineTraced tail (v :: vacc) (appendRopes tacc t) (appendRopes lacc l)

        (EvErr e) :: _ ->
            EvErrTrace e tacc lacc

        (EvErrTrace e t l) :: _ ->
            EvErrTrace e (appendRopes tacc t) (appendRopes lacc l)

        (EvYield tag payload resume) :: tail ->
            EvYield tag payload (\v -> combineTraced (resume v :: tail) vacc tacc lacc)
