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
    if smallLength == 0 then
        ( offset, row, col )

    else if String.startsWith smallString (String.dropLeft offset bigString) then
        let
            endPos =
                offset + smallLength

            ( newRow, newCol ) =
                advancePosition offset row col endPos bigString
        in
        ( endPos, newRow, newCol )

    else
        ( -1, row, col )


isSubStringAdvance : Int -> Int -> Int -> Int -> String -> ( Int, Int, Int )
isSubStringAdvance pos endPos row col bigString =
    let
        ( newRow, newCol ) =
            advancePosition pos row col endPos bigString
    in
    ( endPos, newRow, newCol )


findSubString : String -> Int -> Int -> Int -> String -> ( Int, Int, Int )
findSubString smallString offset row col bigString =
    let
        newOffset =
            findOffset smallString offset bigString

        target =
            if newOffset < 0 then
                String.length bigString

            else
                newOffset + String.length smallString

        ( finalRow, finalCol ) =
            advancePosition offset row col target bigString
    in
    ( newOffset, finalRow, finalCol )


findOffset : String -> Int -> String -> Int
findOffset small offset big =
    if String.isEmpty small then
        offset

    else
        String.dropLeft offset big
            |> String.indexes small
            |> List.head
            |> Maybe.map ((+) offset)
            |> Maybe.withDefault -1


advancePosition : Int -> Int -> Int -> Int -> String -> ( Int, Int )
advancePosition offset row col target string =
    if offset >= target then
        ( row, col )

    else
        let
            chunk =
                String.slice offset target string
        in
        if String.contains "\n" chunk then
            let
                parts =
                    String.split "\n" chunk
            in
            case List.reverse parts of
                [] ->
                    ( row, col )

                lastLine :: previousLines ->
                    ( row + List.length previousLines
                    , String.length lastLine + 1
                    )

        else
            ( row, col + String.length chunk )


{-| isSubChar needs to be called from Kernel.elm with evalFunction,
so the actual implementation is a stub here. The real logic is in Kernel.elm's
custom entry function since it needs to call back into the evaluator.
-}
isSubChar : ()
isSubChar =
    ()
