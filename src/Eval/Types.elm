module Eval.Types exposing (combineMap, errorToString, evalErrorToString, failPartial, foldl, foldr, recurseMapThen, recurseThen, succeedPartial)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import EvalResult
import Parser
import Recursion exposing (Rec)
import Rope exposing (Rope)
import Syntax
import Types exposing (Config, Env, Error(..), Eval, EvalErrorData, EvalErrorKind(..), EvalResult(..), PartialResult)


combineMap : (a -> Eval b) -> List a -> Eval (List b)
combineMap f xs cfg env =
    List.foldr
        (\el acc ->
            case acc of
                EvErr _ ->
                    acc

                EvErrTrace _ _ _ ->
                    acc

                _ ->
                    EvalResult.map2 (::)
                        (f el cfg env)
                        acc
        )
        (EvalResult.succeed [])
        xs


foldl : (a -> out -> Eval out) -> out -> List a -> Eval out
foldl f init xs cfg env =
    List.foldl
        (\el acc ->
            case acc of
                EvOk a ->
                    f el a cfg env

                EvOkTrace a _ _ ->
                    EvalResult.andThen (\a2 -> f el a2 cfg env) acc

                _ ->
                    acc
        )
        (EvalResult.succeed init)
        xs


foldr : (a -> out -> Eval out) -> out -> List a -> Eval out
foldr f init xs cfg env =
    List.foldr
        (\el acc ->
            case acc of
                EvOk a ->
                    f el a cfg env

                EvOkTrace a _ _ ->
                    EvalResult.andThen (\a2 -> f el a2 cfg env) acc

                _ ->
                    acc
        )
        (EvalResult.succeed init)
        xs


succeedPartial : v -> PartialResult v
succeedPartial v =
    Recursion.base (EvOk v)


failPartial : EvalErrorData -> PartialResult v
failPartial e =
    Recursion.base (EvErr e)


errorToString : Error -> String
errorToString err =
    case err of
        ParsingError deadEnds ->
            "Parsing error: " ++ Parser.deadEndsToString deadEnds

        EvalError evalError ->
            evalErrorToString evalError


evalErrorToString : EvalErrorData -> String
evalErrorToString { callStack, error } =
    let
        messageWithType : String
        messageWithType =
            case error of
                TypeError message ->
                    "Type error: " ++ message

                Unsupported message ->
                    "Unsupported: " ++ message

                NameError name ->
                    "Name error: " ++ name ++ " not found"

                Todo message ->
                    "Todo: " ++ message

                TailCall _ ->
                    "TailCall (internal TCO signal)"

    in
    messageWithType
        ++ "\nCall stack:\n - "
        ++ String.join "\n - " (List.reverse <| List.map Syntax.qualifiedNameToString callStack)


recurseThen :
    ( Node Expression, Config, Env )
    -> (out -> PartialResult out)
    -> PartialResult out
recurseThen expr f =
    Recursion.recurseThen expr
        (wrapThen f)


wrapThen :
    (value
     -> Rec r t (EvalResult a)
    )
    -> EvalResult value
    -> Rec r t (EvalResult a)
wrapThen f er =
    case er of
        EvOk v ->
            f v

        EvErr e ->
            Recursion.base (EvErr e)

        EvOkTrace v trees logs ->
            f v
                |> Recursion.map (mergeTraceInto trees logs)

        EvErrTrace e trees logs ->
            Recursion.base (EvErrTrace e trees logs)

        EvYield tag payload resume ->
            -- Pass yield through. Resume returns EvalResult value, but we can't
            -- apply f (which returns Rec) inside the resume lambda.
            -- For simple let-binding yields, the let handler catches this first.
            -- This fallback handles yields from operator expressions etc.
            Recursion.base (EvYield tag payload (\rv -> EvalResult.andThen (\_ -> EvErr { currentModule = [], callStack = [], error = Unsupported "EvYield in wrapThen - yield from this code position not yet supported" }) (resume rv)))


{-| Merge trace data from an outer evaluation into an inner result.
-}
mergeTraceInto : Rope Types.CallTree -> Rope String -> EvalResult a -> EvalResult a
mergeTraceInto trees logs er =
    case er of
        EvOk v ->
            EvOkTrace v trees logs

        EvErr e ->
            EvErrTrace e trees logs

        EvOkTrace v ft fl ->
            EvOkTrace v (Rope.appendTo trees ft) (Rope.appendTo fl logs)

        EvErrTrace e ft fl ->
            EvErrTrace e (Rope.appendTo trees ft) (Rope.appendTo fl logs)

        EvYield tag payload resume ->
            EvYield tag payload resume


recurseMapThen :
    ( List (Node Expression), Config, Env )
    -> (List out -> PartialResult out)
    -> PartialResult out
recurseMapThen ( exprs, cfg, env ) f =
    recurseMapPlain (List.reverse exprs) cfg env [] f


{-| Fast path: no trace data seen yet. Unwraps EvOk immediately during fold.
-}
recurseMapPlain : List (Node Expression) -> Config -> Env -> List out -> (List out -> PartialResult out) -> PartialResult out
recurseMapPlain items cfg env vacc f =
    case items of
        [] ->
            f vacc

        item :: rest ->
            Recursion.recurseThen ( item, cfg, env )
                (\result ->
                    case result of
                        EvOk v ->
                            recurseMapPlain rest cfg env (v :: vacc) f

                        EvErr e ->
                            Recursion.base (EvErr e)

                        EvOkTrace v trees logs ->
                            recurseMapTraced rest cfg env (v :: vacc) trees logs f

                        EvErrTrace e trees logs ->
                            Recursion.base (EvErrTrace e trees logs)

                        EvYield tag payload resume ->
                            Recursion.base (EvYield tag payload resume)
                )


{-| Traced path: accumulates trace data alongside values.
-}
recurseMapTraced : List (Node Expression) -> Config -> Env -> List out -> Rope Types.CallTree -> Rope String -> (List out -> PartialResult out) -> PartialResult out
recurseMapTraced items cfg env vacc tacc lacc f =
    case items of
        [] ->
            f vacc
                |> Recursion.map (mergeTraceInto tacc lacc)

        item :: rest ->
            Recursion.recurseThen ( item, cfg, env )
                (\result ->
                    case result of
                        EvOk v ->
                            recurseMapTraced rest cfg env (v :: vacc) tacc lacc f

                        EvErr e ->
                            Recursion.base (EvErrTrace e tacc lacc)

                        EvOkTrace v trees logs ->
                            recurseMapTraced rest cfg env (v :: vacc) (Rope.appendTo tacc trees) (Rope.appendTo lacc logs) f

                        EvErrTrace e trees logs ->
                            Recursion.base (EvErrTrace e (Rope.appendTo tacc trees) (Rope.appendTo lacc logs))

                        EvYield tag payload resume ->
                            Recursion.base (EvYield tag payload resume)
                )
