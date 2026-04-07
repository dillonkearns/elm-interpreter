module Kernel.Regex exposing
    ( contains
    , findAtMost
    , fromStringWith
    , infinity
    , never
    , replaceAtMost
    , splitAtMost
    )

import EvalResult
import FastDict as Dict
import Regex
import Rope
import Types exposing (Eval, EvalResult(..), Value(..))
import Value


{-| A regex that never matches anything.
-}
never : Value
never =
    RegexValue Regex.never


{-| The "infinity" constant used for unlimited find/split/replace.
-}
infinity : Value
infinity =
    -- Use a very large number since Elm doesn't have Infinity as an Int
    Int 999999999


{-| fromStringWith : Options -> String -> Maybe Regex
Options is { caseInsensitive : Bool, multiline : Bool }
-}
fromStringWith : Value -> String -> Value
fromStringWith optionsVal pattern =
    let
        caseInsensitive =
            case optionsVal of
                Record fields ->
                    case Dict.get "caseInsensitive" fields of
                        Just (Bool b) ->
                            b

                        _ ->
                            False

                _ ->
                    False

        multiline =
            case optionsVal of
                Record fields ->
                    case Dict.get "multiline" fields of
                        Just (Bool b) ->
                            b

                        _ ->
                            False

                _ ->
                    False

        options =
            { caseInsensitive = caseInsensitive
            , multiline = multiline
            }
    in
    case Regex.fromStringWith options pattern of
        Just regex ->
            Custom { moduleName = [ "Maybe" ], name = "Just" } [ RegexValue regex ]

        Nothing ->
            Custom { moduleName = [ "Maybe" ], name = "Nothing" } []


{-| contains : Regex -> String -> Bool
-}
contains : Value -> String -> Value
contains regexVal str =
    case regexVal of
        RegexValue regex ->
            Bool (Regex.contains regex str)

        _ ->
            Bool False


{-| findAtMost : Int -> Regex -> String -> List Match
Match = { match : String, index : Int, number : Int, submatches : List (Maybe String) }
-}
findAtMost : Int -> Value -> String -> Value
findAtMost n regexVal str =
    case regexVal of
        RegexValue regex ->
            let
                matches =
                    Regex.findAtMost n regex str
            in
            List (List.map matchToValue matches)

        _ ->
            List []


matchToValue : Regex.Match -> Value
matchToValue m =
    Record
        (Dict.fromList
            [ ( "match", String m.match )
            , ( "index", Int m.index )
            , ( "number", Int m.number )
            , ( "submatches"
              , List
                    (List.map
                        (\sub ->
                            case sub of
                                Just s ->
                                    Custom { moduleName = [ "Maybe" ], name = "Just" } [ String s ]

                                Nothing ->
                                    Custom { moduleName = [ "Maybe" ], name = "Nothing" } []
                        )
                        m.submatches
                    )
              )
            ]
        )


{-| splitAtMost : Int -> Regex -> String -> List String
-}
splitAtMost : Int -> Value -> String -> Value
splitAtMost n regexVal str =
    case regexVal of
        RegexValue regex ->
            List (List.map String (Regex.splitAtMost n regex str))

        _ ->
            List [ String str ]


{-| replaceAtMost : Int -> Regex -> (Match -> String) -> String -> String
The replacer function needs to be applied via the interpreter.
-}
replaceAtMost : Int -> Value -> (Value -> Eval Value) -> String -> Eval String
replaceAtMost n regexVal replacerFn str cfg env =
    case regexVal of
        RegexValue regex ->
            let
                matches =
                    Regex.findAtMost n regex str

                result =
                    replaceHelp matches replacerFn str 0 "" cfg env
            in
            result

        _ ->
            EvalResult.succeed str


{-| Build the replaced string by iterating through matches.
-}
replaceHelp : List Regex.Match -> (Value -> Eval Value) -> String -> Int -> String -> Types.Config -> Types.Env -> Types.EvalResult String
replaceHelp matches replacerFn original offset acc cfg env =
    case matches of
        [] ->
            EvalResult.succeed (acc ++ String.dropLeft offset original)

        m :: rest ->
            let
                before =
                    String.slice offset m.index original

                matchVal =
                    matchToValue m
            in
            case replacerFn matchVal cfg env of
                EvOk (String replacement) ->
                    replaceHelp rest replacerFn original (m.index + String.length m.match) (acc ++ before ++ replacement) cfg env

                EvOk other ->
                    -- Try to coerce to string
                    EvalResult.fail (Value.typeError env ("Regex.replace: replacer must return a String, got: " ++ Value.toString other))

                EvErr e ->
                    EvErr e

                EvOkTrace (String replacement) traces logs ->
                    case replaceHelp rest replacerFn original (m.index + String.length m.match) (acc ++ before ++ replacement) cfg env of
                        EvOkTrace v t2 l2 ->
                            EvOkTrace v (Rope.appendTo traces t2) (Rope.appendTo logs l2)

                        EvOk v ->
                            EvOkTrace v traces logs

                        other ->
                            other

                EvOkTrace other _ _ ->
                    EvalResult.fail (Value.typeError env ("Regex.replace: replacer must return a String, got: " ++ Value.toString other))

                EvErrTrace e t l ->
                    EvErrTrace e t l

                EvYield tag payload _ ->
                    EvYield tag payload (\_ -> EvalResult.succeed original)

                EvMemoLookup payload _ ->
                    EvMemoLookup payload (\_ -> EvalResult.succeed original)

                EvMemoStore payload _ ->
                    EvMemoStore payload (EvalResult.succeed original)
