module Recursion exposing
    ( Rec
    , base, recurse, recurseThen
    , map, andThen
    , runRecursion, resolveRec
    )

{-| Vendored from micahhahn/elm-safe-recursion 2.0.0.
Rec is kept OPAQUE for --optimize performance.
Added resolveRec for yield resume resolution.
-}


type Rec r t a
    = Base a
    | Recurse r (t -> Rec r t a)


base : a -> Rec r t a
base =
    Base


recurse : r -> Rec r t t
recurse r =
    Recurse r base


recurseThen : r -> (t -> Rec r t a) -> Rec r t a
recurseThen =
    Recurse


map : (a -> b) -> Rec r t a -> Rec r t b
map f step =
    case step of
        Base t ->
            Base (f t)

        Recurse r after ->
            Recurse r (after >> map f)


andThen : (a -> Rec r t b) -> Rec r t a -> Rec r t b
andThen next step =
    case step of
        Base t ->
            next t

        Recurse r after ->
            Recurse r (after >> andThen next)


runRecursion : (r -> Rec r t t) -> r -> t
runRecursion project init =
    let
        go step stack =
            case step of
                Base t ->
                    case stack of
                        [] ->
                            t

                        next :: rest ->
                            go (next t) rest

                Recurse r after ->
                    go (project r) (after :: stack)
    in
    go (project init) []


resolveRec : (r -> Rec r t t) -> Rec r t t -> t
resolveRec project rec =
    let
        go step stack =
            case step of
                Base t ->
                    case stack of
                        [] ->
                            t

                        next :: rest ->
                            go (next t) rest

                Recurse r after ->
                    go (project r) (after :: stack)
    in
    go rec []
