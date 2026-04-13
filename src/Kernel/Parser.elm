module Kernel.Parser exposing
    ( chompBase10
    , consumeBase
    , consumeBase16
    , findSubString
    , isAsciiCode
    , isSubChar
    , isSubString
    )

import Bitwise


charCodeAt : Int -> String -> Maybe Int
charCodeAt offset str =
    str
        |> String.slice offset (offset + 1)
        |> String.uncons
        |> Maybe.map (Tuple.first >> Char.toCode)


isAsciiCode : Int -> Int -> String -> Bool
isAsciiCode code offset string =
    charCodeAt offset string == Just code


chompBase10 : Int -> String -> Int
chompBase10 offset string =
    case charCodeAt offset string of
        Just code ->
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
    case charCodeAt offset string of
        Just code ->
            let
                digit =
                    code - 0x30
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
    case charCodeAt offset string of
        Just code ->
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


isSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
isSubString smallString offset row col bigString =
    let
        smallLength =
            String.length smallString
    in
    if String.slice offset (offset + smallLength) bigString == smallString then
        isSubStringAdvance offset (offset + smallLength) row col bigString

    else
        ( -1, row, col )


isSubStringAdvance : Int -> Int -> Int -> Int -> String -> ( Int, Int, Int )
isSubStringAdvance pos endPos row col bigString =
    if pos >= endPos then
        ( endPos, row, col )

    else
        case charCodeAt pos bigString of
            Just code ->
                if code == 0x0A then
                    isSubStringAdvance (pos + 1) endPos (row + 1) 1 bigString

                else if Bitwise.and code 0xF800 == 0xD800 then
                    isSubStringAdvance (pos + 2) endPos row (col + 1) bigString

                else
                    isSubStringAdvance (pos + 1) endPos row (col + 1) bigString

            Nothing ->
                ( endPos, row, col )


findSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
findSubString smallString offset row col bigString =
    let
        newOffset =
            indexOfFrom smallString offset bigString

        target =
            if newOffset < 0 then
                String.length bigString

            else
                newOffset + String.length smallString

        ( finalRow, finalCol ) =
            advancePosition offset row col target bigString
    in
    ( newOffset, finalRow, finalCol )


indexOfFrom : String -> Int -> String -> Int
indexOfFrom small offset big =
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
        indexOfFrom small (offset + 1) big


advancePosition : Int -> Int -> Int -> Int -> String -> ( Int, Int )
advancePosition offset row col target string =
    if offset >= target then
        ( row, col )

    else
        case charCodeAt offset string of
            Just code ->
                if code == 0x0A then
                    advancePosition (offset + 1) (row + 1) 1 target string

                else if Bitwise.and code 0xF800 == 0xD800 then
                    advancePosition (offset + 2) row (col + 1) target string

                else
                    advancePosition (offset + 1) row (col + 1) target string

            Nothing ->
                ( row, col )


{-| isSubChar needs to be called from Kernel.elm with evalFunction,
so the actual implementation is a stub here. The real logic is in Kernel.elm's
custom entry function since it needs to call back into the evaluator.
-}
isSubChar : ()
isSubChar =
    ()
