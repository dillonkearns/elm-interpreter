module CoreTests.Regex exposing (suite)

import Elm.Syntax.Expression as Expression
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import Types exposing (Value(..))


suite : Test
suite =
    describe "Regex"
        [ fromStringTests
        , containsTests
        , findTests
        , splitTests
        , replaceTests
        , neverTests
        ]


evalRegexModule : String -> String -> Value -> Test
evalRegexModule name source expected =
    test name <|
        \_ ->
            case Eval.Module.eval source (Expression.FunctionOrValue [] "main") of
                Ok value ->
                    value |> Expect.equal expected

                Err e ->
                    Expect.fail (Debug.toString e)


just : Value -> Value
just v =
    Custom { moduleName = [ "Maybe" ], name = "Just" } [ v ]


fromStringTests : Test
fromStringTests =
    describe "fromString"
        [ evalRegexModule "valid regex returns Just"
            """module Test exposing (main)

import Regex

main =
    case Regex.fromString "[a-z]+" of
        Just _ -> "ok"
        Nothing -> "fail"
"""
            (String "ok")
        , evalRegexModule "invalid regex returns Nothing"
            """module Test exposing (main)

import Regex

main =
    case Regex.fromString "[" of
        Just _ -> "ok"
        Nothing -> "fail"
"""
            (String "fail")
        ]


containsTests : Test
containsTests =
    describe "contains"
        [ evalRegexModule "contains matches"
            """module Test exposing (main)

import Regex

main =
    case Regex.fromString "[0-9]" of
        Just regex -> Regex.contains regex "abc123"
        Nothing -> False
"""
            (Bool True)
        , evalRegexModule "contains does not match"
            """module Test exposing (main)

import Regex

main =
    case Regex.fromString "[0-9]" of
        Just regex -> Regex.contains regex "abcxyz"
        Nothing -> False
"""
            (Bool False)
        ]


findTests : Test
findTests =
    describe "find"
        [ evalRegexModule "find matches"
            """module Test exposing (main)

import Regex

main =
    case Regex.fromString "[0-9]+" of
        Just regex ->
            Regex.find regex "abc123def456"
                |> List.map .match
        Nothing -> []
"""
            (List [ String "123", String "456" ])
        , evalRegexModule "find no matches"
            """module Test exposing (main)

import Regex

main =
    case Regex.fromString "[0-9]+" of
        Just regex ->
            Regex.find regex "abcdef"
                |> List.map .match
        Nothing -> []
"""
            (List [])
        , evalRegexModule "find with submatches"
            """module Test exposing (main)

import Regex

main =
    case Regex.fromString "(\\\\d+)-(\\\\d+)" of
        Just regex ->
            case Regex.find regex "12-34" of
                [ m ] -> m.submatches
                _ -> []
        Nothing -> []
"""
            (List [ just (String "12"), just (String "34") ])
        ]


splitTests : Test
splitTests =
    describe "split"
        [ evalRegexModule "split by comma"
            """module Test exposing (main)

import Regex

main =
    case Regex.fromString " *, *" of
        Just regex -> Regex.split regex "tom, 99, 90, 85"
        Nothing -> []
"""
            (List [ String "tom", String "99", String "90", String "85" ])
        , evalRegexModule "split no matches"
            """module Test exposing (main)

import Regex

main =
    case Regex.fromString ";" of
        Just regex -> Regex.split regex "no semicolons here"
        Nothing -> []
"""
            (List [ String "no semicolons here" ])
        ]


replaceTests : Test
replaceTests =
    describe "replace"
        [ evalRegexModule "replace vowels"
            """module Test exposing (main)

import Regex

main =
    case Regex.fromString "[aeiou]" of
        Just regex ->
            Regex.replace regex (\\_ -> "") "The quick brown fox"
        Nothing -> ""
"""
            (String "Th qck brwn fx")
        , evalRegexModule "replace using match"
            """module Test exposing (main)

import Regex

main =
    case Regex.fromString "\\\\w+" of
        Just regex ->
            Regex.replace regex (\\m -> String.reverse m.match) "abc def"
        Nothing -> ""
"""
            (String "cba fed")
        ]


neverTests : Test
neverTests =
    describe "never"
        [ evalRegexModule "never does not match"
            """module Test exposing (main)

import Regex

main =
    Regex.contains Regex.never "anything"
"""
            (Bool False)
        , evalRegexModule "never split returns original"
            """module Test exposing (main)

import Regex

main =
    Regex.split Regex.never "hello"
"""
            (List [ String "hello" ])
        ]
