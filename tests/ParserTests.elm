module ParserTests exposing (suite)

import Elm.Syntax.Expression as Expression
import Eval.Module
import Expect
import Test exposing (Test, describe, test)
import Types exposing (Value(..))


suite : Test
suite =
    describe "elm/parser"
        [ basicTests
        , intTests
        , keywordAndSymbolTests
        , chompTests
        , findSubStringTests
        , integrationTests
        , floatTests
        , numberTests
        ]


basicTests : Test
basicTests =
    describe "Basic parser operations"
        [ parserTest "Parser.end on empty string"
            """module Test exposing (main)

import Parser

main =
    Parser.run Parser.end ""
"""
            (ok Unit)
        , parserTest "Parser.succeed value"
            """module Test exposing (main)

import Parser

main =
    Parser.run (Parser.succeed 42) ""
"""
            (ok (Int 42))
        ]


intTests : Test
intTests =
    describe "Parser.int"
        [ parserTest "simple integer"
            """module Test exposing (main)

import Parser

main =
    Parser.run Parser.int "123"
"""
            (ok (Int 123))
        , parserTest "single digit"
            """module Test exposing (main)

import Parser

main =
    Parser.run Parser.int "0"
"""
            (ok (Int 0))
        , parserTest "large number"
            """module Test exposing (main)

import Parser

main =
    Parser.run Parser.int "999999"
"""
            (ok (Int 999999))
        , parserTest "int fails on non-digit"
            """module Test exposing (main)

import Parser

main =
    case Parser.run Parser.int "abc" of
        Err _ -> "error"
        Ok _ -> "ok"
"""
            (String "error")
        , parserTest "int fails on empty"
            """module Test exposing (main)

import Parser

main =
    case Parser.run Parser.int "" of
        Err _ -> "error"
        Ok _ -> "ok"
"""
            (String "error")
        ]


floatTests : Test
floatTests =
    describe "Parser.float"
        [ parserTest "simple float"
            """module Test exposing (main)

import Parser

main =
    Parser.run Parser.float "3.14"
"""
            (ok (Float 3.14))
        , parserTest "float accepts integer-looking strings"
            """module Test exposing (main)

import Parser

main =
    Parser.run Parser.float "123"
"""
            (ok (Float 123))
        , parserTest "float fails on non-number"
            """module Test exposing (main)

import Parser

main =
    case Parser.run Parser.float "abc" of
        Err _ -> "error"
        Ok _ -> "ok"
"""
            (String "error")
        ]


numberTests : Test
numberTests =
    describe "Parser.number"
        [ parserTest "hex number"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.number
            { int = Just identity
            , hex = Just identity
            , octal = Nothing
            , binary = Nothing
            , float = Nothing
            }
        )
        "0xFF"
"""
            (ok (Int 255))
        , parserTest "octal number"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.number
            { int = Just identity
            , hex = Nothing
            , octal = Just identity
            , binary = Nothing
            , float = Nothing
            }
        )
        "0o17"
"""
            (ok (Int 15))
        , parserTest "binary number"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.number
            { int = Just identity
            , hex = Nothing
            , octal = Nothing
            , binary = Just identity
            , float = Nothing
            }
        )
        "0b1010"
"""
            (ok (Int 10))
        ]


chompTests : Test
chompTests =
    describe "chomp operations (isSubChar)"
        [ parserTest "chompIf success"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.chompIf Char.isDigit)
        "5abc"
"""
            (ok Unit)
        , parserTest "chompIf failure"
            """module Test exposing (main)

import Parser

main =
    case Parser.run (Parser.chompIf Char.isDigit) "abc" of
        Err _ -> "error"
        Ok _ -> "ok"
"""
            (String "error")
        , parserTest "chompWhile alpha"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.getChompedString (Parser.chompWhile Char.isAlpha))
        "abc123"
"""
            (ok (String "abc"))
        , parserTest "chompWhile empty match"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.getChompedString (Parser.chompWhile Char.isAlpha))
        "123abc"
"""
            (ok (String ""))
        , parserTest "chompWhile digits"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.getChompedString (Parser.chompWhile Char.isDigit))
        "12345xyz"
"""
            (ok (String "12345"))
        , parserTest "pipeline with |="
            """module Test exposing (main)

import Parser exposing ((|=))

main =
    Parser.run
        (Parser.succeed identity |= Parser.int)
        "42"
"""
            (ok (Int 42))
        , parserTest "pipeline with |= and |."
            """module Test exposing (main)

import Parser exposing ((|=), (|.))

main =
    Parser.run
        (Parser.succeed identity
            |. Parser.spaces
            |= Parser.int
        )
        "  42"
"""
            (ok (Int 42))
        , parserTest "two values with pipeline operators"
            """module Test exposing (main)

import Parser exposing ((|=), (|.))

main =
    Parser.run
        (Parser.succeed Tuple.pair
            |= Parser.int
            |. Parser.symbol ","
            |= Parser.int
        )
        "3,4"
"""
            (ok (Tuple (Int 3) (Int 4)))
        , parserTest "map over parser"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.map (\\n -> n * 2) Parser.int)
        "21"
"""
            (ok (Int 42))
        , parserTest "andThen"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.int |> Parser.andThen (\\n -> Parser.succeed (n + 1)))
        "41"
"""
            (ok (Int 42))
        ]


integrationTests : Test
integrationTests =
    describe "Integration tests"
        [ parserTest "pipeline with PA.keeper"
            """module Test exposing (main)

import Parser
import Parser.Advanced as PA

main =
    Parser.run
        (PA.keeper (PA.succeed identity) Parser.int)
        "42"
"""
            (ok (Int 42))
        , parserTest "pipeline with PA.keeper and PA.ignorer"
            """module Test exposing (main)

import Parser
import Parser.Advanced as PA

main =
    Parser.run
        (PA.keeper
            (PA.ignorer (PA.succeed identity) Parser.spaces)
            Parser.int
        )
        "  42"
"""
            (ok (Int 42))
        , parserTest "two values with pipeline"
            """module Test exposing (main)

import Parser
import Parser.Advanced as PA

main =
    Parser.run
        (PA.keeper
            (PA.keeper
                (PA.ignorer
                    (PA.succeed Tuple.pair)
                    Parser.spaces
                )
                Parser.int
            )
            (PA.keeper
                (PA.ignorer (PA.succeed identity) (Parser.symbol ","))
                Parser.int
            )
        )
        "  3,4"
"""
            (ok (Tuple (Int 3) (Int 4)))
        , parserTest "oneOf with first match"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.oneOf
            [ Parser.map (\\_ -> "keyword") (Parser.keyword "true")
            , Parser.map (\\_ -> "other") (Parser.keyword "false")
            ]
        )
        "true"
"""
            (ok (String "keyword"))
        , parserTest "oneOf with second match"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.oneOf
            [ Parser.map (\\_ -> "keyword") (Parser.keyword "true")
            , Parser.map (\\_ -> "other") (Parser.keyword "false")
            ]
        )
        "false"
"""
            (ok (String "other"))
        , parserTest "getPosition"
            """module Test exposing (main)

import Parser
import Parser.Advanced as PA

main =
    Parser.run
        (PA.keeper
            (PA.ignorer (PA.succeed identity) (Parser.keyword "hi"))
            Parser.getPosition
        )
        "hi"
"""
            (ok (Tuple (Int 1) (Int 3)))
        , parserTest "getOffset"
            """module Test exposing (main)

import Parser
import Parser.Advanced as PA

main =
    Parser.run
        (PA.keeper
            (PA.ignorer (PA.succeed identity) (Parser.keyword "hi"))
            Parser.getOffset
        )
        "hi"
"""
            (ok (Int 2))
        , parserTest "backtrackable"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.oneOf
            [ Parser.backtrackable (Parser.map (\\_ -> "int") Parser.int)
            , Parser.map (\\_ -> "kw") (Parser.keyword "abc")
            ]
        )
        "abc"
"""
            (ok (String "kw"))
        , parserTest "loop counting"
            """module Test exposing (main)

import Parser
import Parser.Advanced as PA

count : Parser.Parser Int
count =
    Parser.loop 0 (\\n ->
        Parser.oneOf
            [ Parser.map (\\_ -> Parser.Loop (n + 1)) (Parser.chompIf Char.isAlpha)
            , PA.succeed (Parser.Done n)
            ]
    )

main =
    Parser.run count "abc123"
"""
            (ok (Int 3))
        , parserTest "lazy parser"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.lazy (\\_ -> Parser.int))
        "42"
"""
            (ok (Int 42))
        , parserTest "variable parser"
            """module Test exposing (main)

import Parser
import Set

main =
    Parser.run
        (Parser.variable
            { start = Char.isLower
            , inner = \\c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList ["let", "in", "if"]
            }
        )
        "myVar123"
"""
            (ok (String "myVar123"))
        , parserTest "variable parser rejects reserved"
            """module Test exposing (main)

import Parser
import Set

main =
    case Parser.run
        (Parser.variable
            { start = Char.isLower
            , inner = \\c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList ["let", "in", "if"]
            }
        )
        "let"
    of
        Err _ -> "reserved"
        Ok _ -> "ok"
"""
            (String "reserved")
        ]


findSubStringTests : Test
findSubStringTests =
    describe "findSubString (chompUntil, chompUntilEndOr)"
        [ parserTest "chompUntil"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.getChompedString (Parser.chompUntil ","))
        "hello,world"
"""
            (ok (String "hello"))
        , parserTest "chompUntilEndOr stops at marker"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.getChompedString (Parser.chompUntilEndOr "end"))
        "start middle end more"
"""
            (ok (String "start middle "))
        , parserTest "chompUntilEndOr reaches end"
            """module Test exposing (main)

import Parser

main =
    Parser.run
        (Parser.getChompedString (Parser.chompUntilEndOr "end"))
        "no marker here"
"""
            (ok (String "no marker here"))
        ]


keywordAndSymbolTests : Test
keywordAndSymbolTests =
    describe "Parser.keyword and Parser.symbol"
        [ parserTest "keyword match"
            """module Test exposing (main)

import Parser

main =
    Parser.run (Parser.keyword "let") "let"
"""
            (ok Unit)
        , parserTest "keyword no match"
            """module Test exposing (main)

import Parser

main =
    case Parser.run (Parser.keyword "let") "var" of
        Err _ -> "error"
        Ok _ -> "ok"
"""
            (String "error")
        , parserTest "symbol match"
            """module Test exposing (main)

import Parser

main =
    Parser.run (Parser.symbol "+") "+"
"""
            (ok Unit)
        , parserTest "symbol no match"
            """module Test exposing (main)

import Parser

main =
    case Parser.run (Parser.symbol "+") "-" of
        Err _ -> "error"
        Ok _ -> "ok"
"""
            (String "error")
        , parserTest "keyword followed by alphanumeric fails"
            """module Test exposing (main)

import Parser

main =
    case Parser.run (Parser.keyword "let") "letter" of
        Err _ -> "error"
        Ok _ -> "ok"
"""
            (String "error")
        , parserTest "token match"
            """module Test exposing (main)

import Parser

main =
    Parser.run (Parser.token "hello") "hello world"
"""
            (ok Unit)
        ]


parserTest : String -> String -> Value -> Test
parserTest name source expected =
    test name <|
        \_ ->
            case Eval.Module.eval source (Expression.FunctionOrValue [] "main") of
                Ok value ->
                    value |> Expect.equal expected

                Err e ->
                    Expect.fail (Debug.toString e)


ok : Value -> Value
ok value =
    Custom { moduleName = [ "Result" ], name = "Ok" } [ value ]
