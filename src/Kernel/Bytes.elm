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
                    writeInt32 (isLE endianness) n acc

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
writeFloat32 le f acc =
    let
        -- Encode as float64 first, then truncate precision
        -- Float32: 1 sign, 8 exponent (bias 127), 23 mantissa
        bs =
            float64ToBytes f

        -- Convert float64 to float32 bytes
        -- This is lossy but correct for round-tripping
        f32Bytes =
            float64BytesToFloat32Bytes bs
    in
    writeByteList le f32Bytes acc


writeFloat64 : Bool -> Float -> List Int -> List Int
writeFloat64 le f acc =
    let
        bs =
            float64ToBytes f
    in
    writeByteList le bs acc


writeByteList : Bool -> List Int -> List Int -> List Int
writeByteList le bs acc =
    if le then
        -- LE: preserve order in reversed acc (final reversal makes it LE)
        List.foldr (\b a -> b :: a) acc bs

    else
        -- BE: reverse into acc (final reversal restores BE order)
        List.foldl (\b a -> b :: a) acc bs


{-| Encode a Float as 8 bytes in IEEE 754 double precision (big-endian).
-}
float64ToBytes : Float -> List Int
float64ToBytes f =
    if isNaN f then
        [ 0x7F, 0xF8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]

    else if isInfinite f then
        if f > 0 then
            [ 0x7F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]

        else
            [ 0xFF, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]

    else if f == 0 then
        if 1 / f < 0 then
            -- negative zero
            [ 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]

        else
            [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]

    else
        let
            sign =
                if f < 0 then
                    1

                else
                    0

            absF =
                abs f

            -- Find exponent: 2^exp <= absF < 2^(exp+1)
            exp =
                floor (logBase 2 absF)

            -- Bias the exponent (IEEE 754 double: bias = 1023)
            biasedExp =
                exp + 1023

            -- Mantissa: absF / 2^exp - 1.0 (remove implicit leading 1)
            mantissa =
                absF / (2 ^ toFloat exp) - 1.0

            -- Scale mantissa to 52-bit integer
            -- Split into high 20 bits and low 32 bits
            mantissaScaled =
                mantissa * 4503599627370496

            -- 2^52
            mantissaInt =
                round mantissaScaled

            mantissaHigh =
                mantissaInt // 4294967296

            -- div by 2^32
            mantissaLow =
                modBy 4294967296 mantissaInt

            -- Byte 0: sign(1) + exp(7 high bits)
            b0 =
                Bitwise.or (Bitwise.shiftLeftBy 7 sign) (Bitwise.shiftRightBy 4 biasedExp)

            -- Byte 1: exp(4 low bits) + mantissa(4 high bits)
            b1 =
                Bitwise.or
                    (Bitwise.shiftLeftBy 4 (Bitwise.and biasedExp 0x0F))
                    (Bitwise.and (Bitwise.shiftRightBy 16 mantissaHigh) 0x0F)

            b2 =
                Bitwise.and (Bitwise.shiftRightBy 8 mantissaHigh) 0xFF

            b3 =
                Bitwise.and mantissaHigh 0xFF

            b4 =
                Bitwise.and (Bitwise.shiftRightBy 24 mantissaLow) 0xFF

            b5 =
                Bitwise.and (Bitwise.shiftRightBy 16 mantissaLow) 0xFF

            b6 =
                Bitwise.and (Bitwise.shiftRightBy 8 mantissaLow) 0xFF

            b7 =
                Bitwise.and mantissaLow 0xFF
        in
        [ Bitwise.and b0 0xFF, Bitwise.and b1 0xFF, b2, b3, b4, b5, b6, b7 ]


{-| Convert float64 bytes (8) to float32 bytes (4).
-}
float64BytesToFloat32Bytes : List Int -> List Int
float64BytesToFloat32Bytes f64 =
    case f64 of
        [ b0, b1, b2, b3, _, _, _, _ ] ->
            let
                -- Extract float64 components
                sign =
                    Bitwise.shiftRightBy 7 b0

                exp64 =
                    Bitwise.or
                        (Bitwise.shiftLeftBy 4 (Bitwise.and b0 0x7F))
                        (Bitwise.shiftRightBy 4 b1)

                -- Float32: bias 127, float64: bias 1023. Adjust: exp32 = exp64 - 1023 + 127
                exp32 =
                    exp64 - 896

                -- Take top 23 bits of mantissa from float64's 52 bits
                -- Float64 mantissa starts at bit 4 of b1
                mantissaHigh =
                    Bitwise.and b1 0x0F

                fb0 =
                    Bitwise.or (Bitwise.shiftLeftBy 7 sign)
                        (Bitwise.and (Bitwise.shiftRightBy 1 exp32) 0x7F)

                fb1 =
                    Bitwise.or (Bitwise.shiftLeftBy 7 (Bitwise.and exp32 0x01))
                        (Bitwise.or (Bitwise.shiftLeftBy 3 mantissaHigh) (Bitwise.shiftRightBy 5 b2))

                fb2 =
                    Bitwise.or (Bitwise.shiftLeftBy 3 (Bitwise.and b2 0x1F)) (Bitwise.shiftRightBy 5 b3)

                fb3 =
                    Bitwise.or (Bitwise.shiftLeftBy 3 (Bitwise.and b3 0x1F)) 0
            in
            if exp64 == 0 then
                -- Zero or denormalized
                [ Bitwise.shiftLeftBy 7 sign, 0, 0, 0 ]

            else if exp64 == 0x07FF then
                -- Inf or NaN
                [ Bitwise.or (Bitwise.shiftLeftBy 7 sign) 0x7F, 0x80, 0, 0 ]

            else if exp32 <= 0 then
                -- Underflow to zero
                [ Bitwise.shiftLeftBy 7 sign, 0, 0, 0 ]

            else if exp32 >= 0xFF then
                -- Overflow to infinity
                [ Bitwise.or (Bitwise.shiftLeftBy 7 sign) 0x7F, 0x80, 0, 0 ]

            else
                [ Bitwise.and fb0 0xFF, Bitwise.and fb1 0xFF, Bitwise.and fb2 0xFF, Bitwise.and fb3 0xFF ]

        _ ->
            [ 0, 0, 0, 0 ]


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

                EvYield tag payload resume ->
                    EvYield tag payload resume

                EvMemoLookup payload resume ->
                    EvMemoLookup payload resume

                EvMemoStore payload next ->
                    EvMemoStore payload next

                EvOkCoverage v _ ->
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

                EvErrCoverage _ _ ->
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

        EvYield tag payload _ ->
            EvYield tag payload (\_ -> EvalResult.succeed (Custom { moduleName = [ "Maybe" ], name = "Nothing" } []))

        EvMemoLookup payload _ ->
            EvMemoLookup payload (\_ -> EvalResult.succeed (Custom { moduleName = [ "Maybe" ], name = "Nothing" } []))

        EvMemoStore payload _ ->
            EvMemoStore payload (EvalResult.succeed (Custom { moduleName = [ "Maybe" ], name = "Nothing" } []))

        EvOkCoverage partialFn _ ->
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

        EvErrCoverage _ _ ->
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


{-| Read a 32-bit float.
-}
readF32 : Bool -> Array Int -> Int -> Value
readF32 le arr offset =
    if offset + 4 <= Array.length arr then
        let
            bs =
                readBytesOrdered le arr offset 4

            f =
                bytesToFloat32 bs
        in
        Tuple (Int (offset + 4)) (Float f)

    else
        Tuple (Int -1) Unit


{-| Read a 64-bit float.
-}
readF64 : Bool -> Array Int -> Int -> Value
readF64 le arr offset =
    if offset + 8 <= Array.length arr then
        let
            bs =
                readBytesOrdered le arr offset 8

            f =
                bytesToFloat64 bs
        in
        Tuple (Int (offset + 8)) (Float f)

    else
        Tuple (Int -1) Unit


{-| Read n bytes from array, reordering for endianness.
Returns bytes in big-endian order regardless of input endianness.
-}
readBytesOrdered : Bool -> Array Int -> Int -> Int -> List Int
readBytesOrdered le arr offset n =
    let
        bs =
            List.range offset (offset + n - 1)
                |> List.filterMap (\i -> Array.get i arr)
    in
    if le then
        List.reverse bs

    else
        bs


{-| Decode 8 big-endian bytes into an IEEE 754 double.
-}
bytesToFloat64 : List Int -> Float
bytesToFloat64 bs =
    case bs of
        [ b0, b1, b2, b3, b4, b5, b6, b7 ] ->
            let
                sign =
                    Bitwise.shiftRightBy 7 b0

                exp =
                    Bitwise.or
                        (Bitwise.shiftLeftBy 4 (Bitwise.and b0 0x7F))
                        (Bitwise.shiftRightBy 4 b1)

                -- Reconstruct mantissa from bytes
                mantissaHigh =
                    Bitwise.or
                        (Bitwise.shiftLeftBy 16 (Bitwise.and b1 0x0F))
                        (Bitwise.or (Bitwise.shiftLeftBy 8 b2) b3)

                mantissaLow =
                    Bitwise.or
                        (Bitwise.shiftLeftBy 24 b4)
                        (Bitwise.or (Bitwise.shiftLeftBy 16 b5)
                            (Bitwise.or (Bitwise.shiftLeftBy 8 b6) b7)
                        )

                mantissaFloat =
                    toFloat mantissaHigh * 4294967296 + toFloat (Bitwise.and mantissaLow 0x7FFFFFFF) + (if Bitwise.and mantissaLow 0x80000000 /= 0 then 2147483648 else 0)

                signMul =
                    if sign == 1 then
                        -1.0

                    else
                        1.0
            in
            if exp == 0 && mantissaHigh == 0 && mantissaLow == 0 then
                signMul * 0.0

            else if exp == 0x07FF then
                if mantissaHigh == 0 && mantissaLow == 0 then
                    signMul * (1 / 0)

                else
                    0 / 0

            else if exp == 0 then
                -- Denormalized
                signMul * (mantissaFloat / 4503599627370496) * (2 ^ -1022)

            else
                -- Normalized: (-1)^sign * 2^(exp-1023) * (1 + mantissa/2^52)
                signMul * (2 ^ toFloat (exp - 1023)) * (1.0 + mantissaFloat / 4503599627370496)

        _ ->
            0.0


{-| Decode 4 big-endian bytes into an IEEE 754 single-precision float.
-}
bytesToFloat32 : List Int -> Float
bytesToFloat32 bs =
    case bs of
        [ b0, b1, b2, b3 ] ->
            let
                sign =
                    Bitwise.shiftRightBy 7 b0

                exp =
                    Bitwise.or
                        (Bitwise.shiftLeftBy 1 (Bitwise.and b0 0x7F))
                        (Bitwise.shiftRightBy 7 b1)

                mantissa =
                    Bitwise.or
                        (Bitwise.shiftLeftBy 16 (Bitwise.and b1 0x7F))
                        (Bitwise.or (Bitwise.shiftLeftBy 8 b2) b3)

                mantissaFloat =
                    toFloat mantissa

                signMul =
                    if sign == 1 then
                        -1.0

                    else
                        1.0
            in
            if exp == 0 && mantissa == 0 then
                signMul * 0.0

            else if exp == 0xFF then
                if mantissa == 0 then
                    signMul * (1 / 0)

                else
                    0 / 0

            else if exp == 0 then
                -- Denormalized
                signMul * (mantissaFloat / 8388608) * (2 ^ -126)

            else
                -- Normalized: (-1)^sign * 2^(exp-127) * (1 + mantissa/2^23)
                signMul * (2 ^ toFloat (exp - 127)) * (1.0 + mantissaFloat / 8388608)

        _ ->
            0.0


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
