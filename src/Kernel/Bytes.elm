module Kernel.Bytes exposing (decode, decodeFailure, encode, encodeKernel, getHostEndianness, getStringWidth, readBytesChunk, readF32, readF64, readI16, readI32, readI8, readString, readU16, readU32, readU8, width)

import Array exposing (Array)
import Bitwise
import EvalResult
import Types exposing (Eval, EvalResult(..), Value(..))
import Value


{-| Get the byte width of a Bytes value.
-}
width : Array Int -> Int
width =
    Array.length


{-| Calculate the UTF-8 byte width of a string.
-}
getStringWidth : String -> Int
getStringWidth str =
    String.foldl
        (\char acc ->
            let
                code =
                    Char.toCode char
            in
            if code < 0x80 then
                acc + 1

            else if code < 0x0800 then
                acc + 2

            else if code < 0x00010000 then
                acc + 3

            else
                acc + 4
        )
        0
        str


{-| Encode an Encoder value into Bytes.
The Encoder is a Custom type with constructors: I8, I16, I32, U8, U16, U32, F32, F64, Seq, Utf8, Bytes.
-}
encode : Value -> Array Int
encode encoder =
    let
        result =
            encodeHelper encoder []
    in
    Array.fromList (List.reverse result)


encodeHelper : Value -> List Int -> List Int
encodeHelper encoder acc =
    case encoder of
        -- I8 Int
        Custom { name } [ Int n ] ->
            case name of
                "I8" ->
                    signedToUnsigned 8 n :: acc

                "U8" ->
                    Bitwise.and n 0xFF :: acc

                _ ->
                    acc

        -- I16/U16 Endianness Int
        Custom { name } [ endianness, Int n ] ->
            case name of
                "I16" ->
                    writeInt16 (isLE endianness) (signedToUnsigned 16 n) acc

                "U16" ->
                    writeInt16 (isLE endianness) n acc

                "I32" ->
                    writeInt32 (isLE endianness) (signedToUnsigned 32 n) acc

                "U32" ->
                    writeInt32 (isLE endianness) n acc

                "F32" ->
                    -- F32 stores a Float, not an Int; this case shouldn't match
                    acc

                "F64" ->
                    acc

                _ ->
                    acc

        -- F32/F64 Endianness Float
        Custom { name } [ endianness, Float f ] ->
            case name of
                "F32" ->
                    writeFloat32 (isLE endianness) f acc

                "F64" ->
                    writeFloat64 (isLE endianness) f acc

                _ ->
                    acc

        -- Seq Int (List Encoder)
        Custom { name } [ Int _, List encoders ] ->
            if name == "Seq" then
                List.foldl (\e a -> encodeHelper e a) acc encoders

            else
                acc

        -- Utf8 Int String
        Custom { name } [ Int _, String s ] ->
            if name == "Utf8" then
                encodeUtf8 s acc

            else
                acc

        -- Bytes Bytes (embedding existing bytes)
        Custom { name } [ BytesValue arr ] ->
            if name == "Bytes" then
                -- Prepend bytes in reverse (since acc is reversed)
                Array.foldr (\b a -> b :: a) acc arr

            else
                acc

        _ ->
            acc


isLE : Value -> Bool
isLE v =
    case v of
        Custom { name } [] ->
            name == "LE"

        _ ->
            False


signedToUnsigned : Int -> Int -> Int
signedToUnsigned bits n =
    if n < 0 then
        n + Bitwise.shiftLeftBy bits 1

    else
        n


writeInt16 : Bool -> Int -> List Int -> List Int
writeInt16 le n acc =
    let
        b0 =
            Bitwise.and (Bitwise.shiftRightBy 8 n) 0xFF

        b1 =
            Bitwise.and n 0xFF
    in
    if le then
        b0 :: b1 :: acc

    else
        b1 :: b0 :: acc


writeInt32 : Bool -> Int -> List Int -> List Int
writeInt32 le n acc =
    let
        b0 =
            Bitwise.and (Bitwise.shiftRightBy 24 n) 0xFF

        b1 =
            Bitwise.and (Bitwise.shiftRightBy 16 n) 0xFF

        b2 =
            Bitwise.and (Bitwise.shiftRightBy 8 n) 0xFF

        b3 =
            Bitwise.and n 0xFF
    in
    if le then
        b0 :: b1 :: b2 :: b3 :: acc

    else
        b3 :: b2 :: b1 :: b0 :: acc


writeFloat32 : Bool -> Float -> List Int -> List Int
writeFloat32 le _ acc =
    -- TODO: proper IEEE 754 float32 encoding
    -- For now, write 4 zero bytes as placeholder
    if le then
        0 :: 0 :: 0 :: 0 :: acc

    else
        0 :: 0 :: 0 :: 0 :: acc


writeFloat64 : Bool -> Float -> List Int -> List Int
writeFloat64 le _ acc =
    -- TODO: proper IEEE 754 float64 encoding
    -- For now, write 8 zero bytes as placeholder
    0 :: 0 :: 0 :: 0 :: 0 :: 0 :: 0 :: 0 :: acc


encodeUtf8 : String -> List Int -> List Int
encodeUtf8 str acc =
    String.foldl
        (\char a ->
            let
                code =
                    Char.toCode char
            in
            if code < 0x80 then
                code :: a

            else if code < 0x0800 then
                Bitwise.and (Bitwise.or 0x80 code) 0xBF
                    :: Bitwise.or 0xC0 (Bitwise.shiftRightBy 6 code)
                    :: a

            else if code < 0x00010000 then
                Bitwise.and (Bitwise.or 0x80 code) 0xBF
                    :: Bitwise.and (Bitwise.or 0x80 (Bitwise.shiftRightBy 6 code)) 0xBF
                    :: Bitwise.or 0xE0 (Bitwise.shiftRightBy 12 code)
                    :: a

            else
                Bitwise.and (Bitwise.or 0x80 code) 0xBF
                    :: Bitwise.and (Bitwise.or 0x80 (Bitwise.shiftRightBy 6 code)) 0xBF
                    :: Bitwise.and (Bitwise.or 0x80 (Bitwise.shiftRightBy 12 code)) 0xBF
                    :: Bitwise.or 0xF0 (Bitwise.shiftRightBy 18 code)
                    :: a
        )
        acc
        str


{-| Decode bytes using a decoder function.
The decoder is a function Value that takes (Bytes, Int) and returns (Int, Value).
Returns Just value if successful, Nothing if the decoder fails (offset out of bounds).
-}
decode : (Value -> Eval (Value -> Eval Value)) -> Value -> Eval Value
decode decoderFn bytesVal cfg env =
    -- Call decoderFn with bytesVal
    case decoderFn bytesVal cfg env of
        EvOk partialFn ->
            -- Call the resulting function with offset 0
            case partialFn (Int 0) cfg env of
                EvOk (Tuple (Int newOffset) result) ->
                    case bytesVal of
                        BytesValue arr ->
                            if newOffset >= 0 && newOffset <= Array.length arr then
                                EvalResult.succeed
                                    (Custom { moduleName = [ "Maybe" ], name = "Just" } [ result ])

                            else
                                EvalResult.succeed
                                    (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])

                        _ ->
                            EvalResult.succeed
                                (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])

                EvOk _ ->
                    EvalResult.succeed
                        (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])

                EvErr e ->
                    EvalResult.succeed
                        (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])

                EvOkTrace v _ _ ->
                    -- Handle traced variant - extract and recurse
                    case v of
                        Tuple (Int newOffset) result ->
                            case bytesVal of
                                BytesValue arr ->
                                    if newOffset >= 0 && newOffset <= Array.length arr then
                                        EvalResult.succeed
                                            (Custom { moduleName = [ "Maybe" ], name = "Just" } [ result ])

                                    else
                                        EvalResult.succeed
                                            (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])

                                _ ->
                                    EvalResult.succeed
                                        (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])

                        _ ->
                            EvalResult.succeed
                                (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])

                EvErrTrace _ _ _ ->
                    EvalResult.succeed
                        (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])

        EvErr _ ->
            EvalResult.succeed
                (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])

        EvOkTrace partialFn _ _ ->
            case partialFn (Int 0) cfg env of
                EvOk (Tuple (Int newOffset) result) ->
                    case bytesVal of
                        BytesValue arr ->
                            if newOffset >= 0 && newOffset <= Array.length arr then
                                EvalResult.succeed
                                    (Custom { moduleName = [ "Maybe" ], name = "Just" } [ result ])

                            else
                                EvalResult.succeed
                                    (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])

                        _ ->
                            EvalResult.succeed
                                (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])

                _ ->
                    EvalResult.succeed
                        (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])

        EvErrTrace _ _ _ ->
            EvalResult.succeed
                (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])


{-| Wrapper for `encode` that works with the kernel registration's `oneWithError` pattern.
-}
encodeKernel : Value -> Eval (Array Int)
encodeKernel encoderVal _ _ =
    EvalResult.succeed (encode encoderVal)


{-| Read an unsigned 8-bit integer from bytes at an offset.
Returns (newOffset, value) as a Tuple.
-}
readU8 : Array Int -> Int -> Value
readU8 arr offset =
    case Array.get offset arr of
        Just b ->
            Tuple (Int (offset + 1)) (Int b)

        Nothing ->
            Tuple (Int -1) Unit


{-| Read a signed 8-bit integer from bytes at an offset.
-}
readI8 : Array Int -> Int -> Value
readI8 arr offset =
    case Array.get offset arr of
        Just b ->
            let
                signed =
                    if b >= 128 then
                        b - 256

                    else
                        b
            in
            Tuple (Int (offset + 1)) (Int signed)

        Nothing ->
            Tuple (Int -1) Unit


{-| Read an unsigned 16-bit integer.
-}
readU16 : Bool -> Array Int -> Int -> Value
readU16 le arr offset =
    case ( Array.get offset arr, Array.get (offset + 1) arr ) of
        ( Just b0, Just b1 ) ->
            let
                value =
                    if le then
                        b0 + Bitwise.shiftLeftBy 8 b1

                    else
                        Bitwise.shiftLeftBy 8 b0 + b1
            in
            Tuple (Int (offset + 2)) (Int value)

        _ ->
            Tuple (Int -1) Unit


{-| Read a signed 16-bit integer.
-}
readI16 : Bool -> Array Int -> Int -> Value
readI16 le arr offset =
    case readU16 le arr offset of
        Tuple off (Int v) ->
            let
                signed =
                    if v >= 32768 then
                        v - 65536

                    else
                        v
            in
            Tuple off (Int signed)

        other ->
            other


{-| Read an unsigned 32-bit integer.
-}
readU32 : Bool -> Array Int -> Int -> Value
readU32 le arr offset =
    case ( Array.get offset arr, Array.get (offset + 1) arr ) of
        ( Just b0, Just b1 ) ->
            case ( Array.get (offset + 2) arr, Array.get (offset + 3) arr ) of
                ( Just b2, Just b3 ) ->
                    let
                        value =
                            if le then
                                b0 + Bitwise.shiftLeftBy 8 b1 + Bitwise.shiftLeftBy 16 b2 + Bitwise.shiftLeftBy 24 b3

                            else
                                Bitwise.shiftLeftBy 24 b0 + Bitwise.shiftLeftBy 16 b1 + Bitwise.shiftLeftBy 8 b2 + b3
                    in
                    Tuple (Int (offset + 4)) (Int value)

                _ ->
                    Tuple (Int -1) Unit

        _ ->
            Tuple (Int -1) Unit


{-| Read a signed 32-bit integer.
-}
readI32 : Bool -> Array Int -> Int -> Value
readI32 le arr offset =
    -- For 32-bit signed, the unsigned value works since Elm ints are 32-bit signed
    readU32 le arr offset


{-| Read a 32-bit float (stub - returns 0.0).
-}
readF32 : Bool -> Array Int -> Int -> Value
readF32 _ arr offset =
    if offset + 4 <= Array.length arr then
        -- TODO: proper IEEE 754 decoding
        Tuple (Int (offset + 4)) (Float 0.0)

    else
        Tuple (Int -1) Unit


{-| Read a 64-bit float (stub - returns 0.0).
-}
readF64 : Bool -> Array Int -> Int -> Value
readF64 _ arr offset =
    if offset + 8 <= Array.length arr then
        -- TODO: proper IEEE 754 decoding
        Tuple (Int (offset + 8)) (Float 0.0)

    else
        Tuple (Int -1) Unit


{-| Read a chunk of bytes.
-}
readBytesChunk : Int -> Array Int -> Int -> Value
readBytesChunk n arr offset =
    if offset + n <= Array.length arr then
        Tuple (Int (offset + n)) (BytesValue (Array.slice offset (offset + n) arr))

    else
        Tuple (Int -1) Unit


{-| Read a UTF-8 string of n bytes.
-}
readString : Int -> Array Int -> Int -> Value
readString n arr offset =
    if offset + n <= Array.length arr then
        let
            byteList =
                Array.toList (Array.slice offset (offset + n) arr)

            str =
                decodeUtf8 byteList
        in
        Tuple (Int (offset + n)) (String str)

    else
        Tuple (Int -1) Unit


decodeUtf8 : List Int -> String
decodeUtf8 bytes =
    decodeUtf8Helper bytes []
        |> List.reverse
        |> String.fromList


decodeUtf8Helper : List Int -> List Char -> List Char
decodeUtf8Helper bytes acc =
    case bytes of
        [] ->
            acc

        b0 :: rest ->
            if b0 < 0x80 then
                decodeUtf8Helper rest (Char.fromCode b0 :: acc)

            else if Bitwise.and b0 0xE0 == 0xC0 then
                case rest of
                    b1 :: rest2 ->
                        let
                            code =
                                Bitwise.or
                                    (Bitwise.shiftLeftBy 6 (Bitwise.and b0 0x1F))
                                    (Bitwise.and b1 0x3F)
                        in
                        decodeUtf8Helper rest2 (Char.fromCode code :: acc)

                    _ ->
                        acc

            else if Bitwise.and b0 0xF0 == 0xE0 then
                case rest of
                    b1 :: b2 :: rest2 ->
                        let
                            code =
                                Bitwise.or
                                    (Bitwise.shiftLeftBy 12 (Bitwise.and b0 0x0F))
                                    (Bitwise.or
                                        (Bitwise.shiftLeftBy 6 (Bitwise.and b1 0x3F))
                                        (Bitwise.and b2 0x3F)
                                    )
                        in
                        decodeUtf8Helper rest2 (Char.fromCode code :: acc)

                    _ ->
                        acc

            else
                case rest of
                    b1 :: b2 :: b3 :: rest2 ->
                        let
                            code =
                                Bitwise.or
                                    (Bitwise.shiftLeftBy 18 (Bitwise.and b0 0x07))
                                    (Bitwise.or
                                        (Bitwise.shiftLeftBy 12 (Bitwise.and b1 0x3F))
                                        (Bitwise.or
                                            (Bitwise.shiftLeftBy 6 (Bitwise.and b2 0x3F))
                                            (Bitwise.and b3 0x3F)
                                        )
                                    )
                        in
                        decodeUtf8Helper rest2 (Char.fromCode code :: acc)

                    _ ->
                        acc


{-| Decode failure - returns an invalid offset to signal failure.
-}
decodeFailure : Array Int -> Int -> Value
decodeFailure _ _ =
    Tuple (Int -1) Unit


{-| Get host endianness. Returns LE (little-endian) for the interpreter.
-}
getHostEndianness : Value -> Value -> Eval Value
getHostEndianness le _ _ _ =
    -- Return Task.succeed LE for the interpreter
    EvalResult.succeed (Custom { moduleName = [ "Task" ], name = "Succeed" } [ le ])
