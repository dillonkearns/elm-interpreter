module Eval.DelegateCounter exposing
    ( CollectedDelegateCalls
    , DelegateCallCounter
    , collect
    , empty
    , emit
    , enableOnInterceptsByGlobal
    , enabled
    , merge
    , qualifiedName
    )

import Eval.ResolvedIR as IR
import FastDict as Dict
import Types exposing (EvalResult(..), Intercept(..), Value(..))


type alias DelegateCallCounter =
    { count : Int
    , countsByName : Dict.Dict String Int
    }


type alias CollectedDelegateCalls a =
    { result : EvalResult a
    , delegateCallCounter : DelegateCallCounter
    }


empty : DelegateCallCounter
empty =
    { count = 0
    , countsByName = Dict.empty
    }


merge : DelegateCallCounter -> DelegateCallCounter -> DelegateCallCounter
merge left right =
    { count = left.count + right.count
    , countsByName =
        Dict.foldl
            (\qualified count acc ->
                Dict.update qualified
                    (\maybeExisting ->
                        Just (count + Maybe.withDefault 0 maybeExisting)
                    )
                    acc
            )
            left.countsByName
            right.countsByName
    }


qualifiedName : List String -> String -> String
qualifiedName moduleName name =
    if List.isEmpty moduleName then
        name

    else
        String.join "." moduleName ++ "." ++ name


delegateYieldTag : String
delegateYieldTag =
    "__elm_interpreter_delegate_call__"


emit : List String -> String -> EvalResult a -> EvalResult a
emit moduleName name result =
    EvYield delegateYieldTag
        (String (qualifiedName moduleName name))
        (\_ -> result)


delegateSentinelId : IR.GlobalId
delegateSentinelId =
    -1


enableOnInterceptsByGlobal :
    Dict.Dict IR.GlobalId ( String, Types.Intercept )
    -> Dict.Dict IR.GlobalId ( String, Types.Intercept )
enableOnInterceptsByGlobal interceptsByGlobal =
    Dict.insert delegateSentinelId
        ( "__delegate_counter__"
        , Intercept (\_ _ _ _ -> EvOk Unit)
        )
        interceptsByGlobal


enabled : Dict.Dict IR.GlobalId ( String, Types.Intercept ) -> Bool
enabled interceptsByGlobal =
    Dict.member delegateSentinelId interceptsByGlobal


collect : EvalResult a -> CollectedDelegateCalls a
collect =
    collectHelp empty


collectHelp : DelegateCallCounter -> EvalResult a -> CollectedDelegateCalls a
collectHelp acc result =
    case result of
        EvYield tag payload resume ->
            if tag == delegateYieldTag then
                case payload of
                    String qualified ->
                        collectHelp (record qualified acc) (resume Unit)

                    _ ->
                        { result = result
                        , delegateCallCounter = acc
                        }

            else
                { result = result
                , delegateCallCounter = acc
                }

        EvMemoStore payload next ->
            let
                collectedNext : CollectedDelegateCalls a
                collectedNext =
                    collectHelp acc next
            in
            { result = EvMemoStore payload collectedNext.result
            , delegateCallCounter = collectedNext.delegateCallCounter
            }

        _ ->
            { result = result
            , delegateCallCounter = acc
            }


record : String -> DelegateCallCounter -> DelegateCallCounter
record qualified acc =
    { count = acc.count + 1
    , countsByName =
        Dict.update qualified
            (\maybeExisting ->
                Just (1 + Maybe.withDefault 0 maybeExisting)
            )
            acc.countsByName
    }
