module Task exposing
  ( Task(..)
  , succeed, fail
  , map, map2, map3, map4, map5
  , sequence
  , andThen
  , onError, mapError
  )

{-| Minimal Task implementation for the interpreter.

Tasks are represented as a concrete type so they support structural equality.
In the interpreter context, tasks are evaluated eagerly (andThen executes immediately).

# Tasks
@docs Task, succeed, fail

# Chains
@docs andThen, sequence

# Maps
@docs map, map2, map3, map4, map5

# Errors
@docs onError, mapError

-}

import Basics exposing ((|>), (<<))
import List exposing ((::))


{-| A task that may fail with error `x` or succeed with value `a`.
-}
type Task x a
    = Succeed a
    | Fail x


{-| A task that succeeds immediately.
-}
succeed : a -> Task x a
succeed =
    Succeed


{-| A task that fails immediately.
-}
fail : x -> Task x a
fail =
    Fail


{-| Chain together a task and a callback.
-}
andThen : (a -> Task x b) -> Task x a -> Task x b
andThen f task =
    case task of
        Succeed a ->
            f a

        Fail x ->
            Fail x


{-| Recover from a failure in a task.
-}
onError : (x -> Task y a) -> Task x a -> Task y a
onError f task =
    case task of
        Succeed a ->
            Succeed a

        Fail x ->
            f x


{-| Transform a task.
-}
map : (a -> b) -> Task x a -> Task x b
map func taskA =
    taskA
        |> andThen (\a -> succeed (func a))


{-| Put the results of two tasks together.
-}
map2 : (a -> b -> result) -> Task x a -> Task x b -> Task x result
map2 func taskA taskB =
    taskA
        |> andThen (\a -> taskB
        |> andThen (\b -> succeed (func a b)))


{-|-}
map3 : (a -> b -> c -> result) -> Task x a -> Task x b -> Task x c -> Task x result
map3 func taskA taskB taskC =
    taskA
        |> andThen (\a -> taskB
        |> andThen (\b -> taskC
        |> andThen (\c -> succeed (func a b c))))


{-|-}
map4 : (a -> b -> c -> d -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x result
map4 func taskA taskB taskC taskD =
    taskA
        |> andThen (\a -> taskB
        |> andThen (\b -> taskC
        |> andThen (\c -> taskD
        |> andThen (\d -> succeed (func a b c d)))))


{-|-}
map5 : (a -> b -> c -> d -> e -> result) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> Task x result
map5 func taskA taskB taskC taskD taskE =
    taskA
        |> andThen (\a -> taskB
        |> andThen (\b -> taskC
        |> andThen (\c -> taskD
        |> andThen (\d -> taskE
        |> andThen (\e -> succeed (func a b c d e))))))


{-| Start with a list of tasks, and turn them into a single task that returns a list.
-}
sequence : List (Task x a) -> Task x (List a)
sequence tasks =
    List.foldr (map2 (::)) (succeed []) tasks


{-| Transform the error value.
-}
mapError : (x -> y) -> Task x a -> Task y a
mapError convert task =
    task
        |> onError (fail << convert)
