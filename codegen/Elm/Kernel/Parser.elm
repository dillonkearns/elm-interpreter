module Elm.Kernel.Parser exposing
    ( chompBase10
    , consumeBase
    , consumeBase16
    , findSubString
    , isAsciiCode
    , isSubChar
    , isSubString
    )

import Bitwise


isSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
isSubString smallString offset row col bigString =
    let
        smallLength =
            String.length smallString
    in
    if offset + smallLength > String.length bigString then
        ( -1, row, col )

    else
        isSubStringHelp smallString 0 smallLength offset row col bigString


isSubStringHelp : String -> Int -> Int -> Int -> Int -> Int -> String -> ( Int, Int, Int )
isSubStringHelp small i smallLen offset row col big =
    if i >= smallLen then
        ( offset, row, col )

    else if String.slice (offset + i - i + i) (offset + i - i + i + 1) big /= String.slice i (i + 1) small then
        ( -1, row, col )

    else
        let
            code =
                big
                    |> String.slice offset (offset + 1)
                    |> String.toList
                    |> List.head
                    |> Maybe.map Char.toCode
                    |> Maybe.withDefault 0
        in
        if code == 0x0A then
            isSubStringHelp small (i + 1) smallLen (offset + 1) (row + 1) 1 big

        else if Bitwise.and code 0xF800 == 0xD800 then
            isSubStringHelp small (i + 2) smallLen (offset + 2) row (col + 1) big

        else
            isSubStringHelp small (i + 1) smallLen (offset + 1) row (col + 1) big


isSubChar : (Char -> Bool) -> Int -> String -> Int
isSubChar predicate offset string =
    if String.length string <= offset then
        -1

    else
        case String.slice offset (offset + 1) string |> String.toList |> List.head of
            Nothing ->
                -1

            Just c ->
                let
                    code =
                        Char.toCode c
                in
                if Bitwise.and code 0xF800 == 0xD800 then
                    let
                        fullChar =
                            String.slice offset (offset + 2) string
                                |> String.toList
                                |> List.head
                                |> Maybe.withDefault c
                    in
                    if predicate fullChar then
                        offset + 2

                    else
                        -1

                else if predicate c then
                    if code == 0x0A then
                        -2

                    else
                        offset + 1

                else
                    -1


isAsciiCode : Int -> Int -> String -> Bool
isAsciiCode code offset string =
    case String.slice offset (offset + 1) string |> String.toList |> List.head of
        Just c ->
            Char.toCode c == code

        Nothing ->
            False


chompBase10 : Int -> String -> Int
chompBase10 offset string =
    case String.slice offset (offset + 1) string |> String.toList |> List.head of
        Just c ->
            let
                code =
                    Char.toCode c
            in
            if code >= 0x30 && code <= 0x39 then
                chompBase10 (offset + 1) string

            else
                offset

        Nothing ->
            offset


consumeBase : Int -> Int -> String -> ( Int, Int )
consumeBase base offset string =
    consumeBaseHelp base offset 0 string


consumeBaseHelp : Int -> Int -> Int -> String -> ( Int, Int )
consumeBaseHelp base offset total string =
    case String.slice offset (offset + 1) string |> String.toList |> List.head of
        Just c ->
            let
                digit =
                    Char.toCode c - 0x30
            in
            if digit >= 0 && digit < base then
                consumeBaseHelp base (offset + 1) (base * total + digit) string

            else
                ( offset, total )

        Nothing ->
            ( offset, total )


consumeBase16 : Int -> String -> ( Int, Int )
consumeBase16 offset string =
    consumeBase16Help offset 0 string


consumeBase16Help : Int -> Int -> String -> ( Int, Int )
consumeBase16Help offset total string =
    case String.slice offset (offset + 1) string |> String.toList |> List.head of
        Just c ->
            let
                code =
                    Char.toCode c
            in
            if code >= 0x30 && code <= 0x39 then
                consumeBase16Help (offset + 1) (16 * total + code - 0x30) string

            else if code >= 0x41 && code <= 0x46 then
                consumeBase16Help (offset + 1) (16 * total + code - 55) string

            else if code >= 0x61 && code <= 0x66 then
                consumeBase16Help (offset + 1) (16 * total + code - 87) string

            else
                ( offset, total )

        Nothing ->
            ( offset, total )


findSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
findSubString smallString offset row col bigString =
    let
        newOffset =
            indexOf smallString offset bigString

        target =
            if newOffset < 0 then
                String.length bigString

            else
                newOffset + String.length smallString
    in
    let
        ( finalRow, finalCol ) =
            advancePosition offset row col target bigString
    in
    ( newOffset, finalRow, finalCol )


indexOf : String -> Int -> String -> Int
indexOf small offset big =
    let
        smallLen =
            String.length small

        bigLen =
            String.length big
    in
    if offset + smallLen > bigLen then
        -1

    else if String.slice offset (offset + smallLen) big == small then
        offset

    else
        indexOf small (offset + 1) big


advancePosition : Int -> Int -> Int -> Int -> String -> ( Int, Int )
advancePosition offset row col target string =
    if offset >= target then
        ( row, col )

    else
        case String.slice offset (offset + 1) string |> String.toList |> List.head of
            Just c ->
                let
                    code =
                        Char.toCode c
                in
                if code == 0x0A then
                    advancePosition (offset + 1) (row + 1) 1 target string

                else if Bitwise.and code 0xF800 == 0xD800 then
                    advancePosition (offset + 2) row (col + 1) target string

                else
                    advancePosition (offset + 1) row (col + 1) target string

            Nothing ->
                ( row, col )
