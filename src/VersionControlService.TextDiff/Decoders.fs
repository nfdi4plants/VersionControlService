namespace VersionControlService.TextDiff

[<RequireQualifiedAccess>]
type TextEncoding =
    | Utf8
    | Utf16LE
    | Utf16BE
    | Utf32LE
    | Utf32BE
    | Windows1252

type DecodeError = { Offset: int64; Reason: string }

type DecoderState = {
    Encoding: TextEncoding
    AbsoluteOffset: int64
    PendingStart: int64
    PendingValue: int
    PendingCount: int
    ExpectedCount: int
    PendingHighSurrogate: int
    PendingHighStart: int64
    PendingHighEnd: int64
}

module Decoders =
    let name = function
        | TextEncoding.Utf8 -> "utf-8"
        | TextEncoding.Utf16LE -> "utf-16le"
        | TextEncoding.Utf16BE -> "utf-16be"
        | TextEncoding.Utf32LE -> "utf-32le"
        | TextEncoding.Utf32BE -> "utf-32be"
        | TextEncoding.Windows1252 -> "windows-1252"

    let tryParseName (value: string) =
        if isNull value then None
        else
            match value.Trim().ToLowerInvariant() with
            | "utf-8" | "utf8" -> Some TextEncoding.Utf8
            | "utf-16le" -> Some TextEncoding.Utf16LE
            | "utf-16be" -> Some TextEncoding.Utf16BE
            | "utf-32le" -> Some TextEncoding.Utf32LE
            | "utf-32be" -> Some TextEncoding.Utf32BE
            | "windows-1252" | "cp1252" -> Some TextEncoding.Windows1252
            | _ -> None

    let createAt encoding absoluteOffset = {
        Encoding = encoding
        AbsoluteOffset = absoluteOffset
        PendingStart = absoluteOffset
        PendingValue = 0
        PendingCount = 0
        ExpectedCount = 0
        PendingHighSurrogate = 0
        PendingHighStart = 0L
        PendingHighEnd = 0L
    }

    let create encoding = createAt encoding 0L

    let private fail offset reason = Error { Offset = offset; Reason = reason }

    let private emitScalar sink start finish scalar =
        if scalar <= 0xFFFF then
            sink start finish scalar
        else
            let value = scalar - 0x10000
            sink start finish (0xD800 + (value >>> 10))
            sink start finish (0xDC00 + (value &&& 0x3FF))

    let windows1252 = [|
        0x0000; 0x0001; 0x0002; 0x0003; 0x0004; 0x0005; 0x0006; 0x0007
        0x0008; 0x0009; 0x000A; 0x000B; 0x000C; 0x000D; 0x000E; 0x000F
        0x0010; 0x0011; 0x0012; 0x0013; 0x0014; 0x0015; 0x0016; 0x0017
        0x0018; 0x0019; 0x001A; 0x001B; 0x001C; 0x001D; 0x001E; 0x001F
        0x0020; 0x0021; 0x0022; 0x0023; 0x0024; 0x0025; 0x0026; 0x0027
        0x0028; 0x0029; 0x002A; 0x002B; 0x002C; 0x002D; 0x002E; 0x002F
        0x0030; 0x0031; 0x0032; 0x0033; 0x0034; 0x0035; 0x0036; 0x0037
        0x0038; 0x0039; 0x003A; 0x003B; 0x003C; 0x003D; 0x003E; 0x003F
        0x0040; 0x0041; 0x0042; 0x0043; 0x0044; 0x0045; 0x0046; 0x0047
        0x0048; 0x0049; 0x004A; 0x004B; 0x004C; 0x004D; 0x004E; 0x004F
        0x0050; 0x0051; 0x0052; 0x0053; 0x0054; 0x0055; 0x0056; 0x0057
        0x0058; 0x0059; 0x005A; 0x005B; 0x005C; 0x005D; 0x005E; 0x005F
        0x0060; 0x0061; 0x0062; 0x0063; 0x0064; 0x0065; 0x0066; 0x0067
        0x0068; 0x0069; 0x006A; 0x006B; 0x006C; 0x006D; 0x006E; 0x006F
        0x0070; 0x0071; 0x0072; 0x0073; 0x0074; 0x0075; 0x0076; 0x0077
        0x0078; 0x0079; 0x007A; 0x007B; 0x007C; 0x007D; 0x007E; 0x007F
        0x20AC; -1;    0x201A; 0x0192; 0x201E; 0x2026; 0x2020; 0x2021
        0x02C6; 0x2030; 0x0160; 0x2039; 0x0152; -1;    0x017D; -1
        -1;    0x2018; 0x2019; 0x201C; 0x201D; 0x2022; 0x2013; 0x2014
        0x02DC; 0x2122; 0x0161; 0x203A; 0x0153; -1;    0x017E; 0x0178
        0x00A0; 0x00A1; 0x00A2; 0x00A3; 0x00A4; 0x00A5; 0x00A6; 0x00A7
        0x00A8; 0x00A9; 0x00AA; 0x00AB; 0x00AC; 0x00AD; 0x00AE; 0x00AF
        0x00B0; 0x00B1; 0x00B2; 0x00B3; 0x00B4; 0x00B5; 0x00B6; 0x00B7
        0x00B8; 0x00B9; 0x00BA; 0x00BB; 0x00BC; 0x00BD; 0x00BE; 0x00BF
        0x00C0; 0x00C1; 0x00C2; 0x00C3; 0x00C4; 0x00C5; 0x00C6; 0x00C7
        0x00C8; 0x00C9; 0x00CA; 0x00CB; 0x00CC; 0x00CD; 0x00CE; 0x00CF
        0x00D0; 0x00D1; 0x00D2; 0x00D3; 0x00D4; 0x00D5; 0x00D6; 0x00D7
        0x00D8; 0x00D9; 0x00DA; 0x00DB; 0x00DC; 0x00DD; 0x00DE; 0x00DF
        0x00E0; 0x00E1; 0x00E2; 0x00E3; 0x00E4; 0x00E5; 0x00E6; 0x00E7
        0x00E8; 0x00E9; 0x00EA; 0x00EB; 0x00EC; 0x00ED; 0x00EE; 0x00EF
        0x00F0; 0x00F1; 0x00F2; 0x00F3; 0x00F4; 0x00F5; 0x00F6; 0x00F7
        0x00F8; 0x00F9; 0x00FA; 0x00FB; 0x00FC; 0x00FD; 0x00FE; 0x00FF
    |]

    let inline utf8ExpectedCount value =
        if value >= 0xC2 && value <= 0xDF then 2
        elif value >= 0xE0 && value <= 0xEF then 3
        elif value >= 0xF0 && value <= 0xF4 then 4
        else 0

    let inline isUtf8Continuation value = value >= 0x80 && value <= 0xBF

    let inline isValidUtf8Scalar expectedCount scalar =
        not (
            (expectedCount = 3 && scalar < 0x800)
            || (expectedCount = 4 && scalar < 0x10000)
            || (scalar >= 0xD800 && scalar <= 0xDFFF)
            || scalar > 0x10FFFF
        )

    let inline isHighSurrogate unit = unit >= 0xD800 && unit <= 0xDBFF

    let inline isLowSurrogate unit = unit >= 0xDC00 && unit <= 0xDFFF

    let inline isValidUtf32Bytes first second third fourth littleEndian =
        if littleEndian then
            let scalar = first ||| (second <<< 8) ||| (third <<< 16)
            fourth = 0 && third <= 0x10 && not (scalar >= 0xD800 && scalar <= 0xDFFF)
        else
            let scalar = (second <<< 16) ||| (third <<< 8) ||| fourth
            first = 0 && second <= 0x10 && not (scalar >= 0xD800 && scalar <= 0xDFFF)

    let inline windows1252Scalar value = windows1252[value]

    let decode (initial: DecoderState) (bytes: byte[]) offset count (sink: int64 -> int64 -> int -> unit) =
        if isNull bytes then nullArg (nameof bytes)
        if offset < 0 || count < 0 || offset > bytes.Length - count then invalidArg (nameof offset) "The byte range is outside the input buffer."

        let encoding = initial.Encoding
        let mutable absoluteOffset = initial.AbsoluteOffset
        let mutable pendingStart = initial.PendingStart
        let mutable pendingValue = initial.PendingValue
        let mutable pendingCount = initial.PendingCount
        let mutable expectedCount = initial.ExpectedCount
        let mutable pendingHigh = initial.PendingHighSurrogate
        let mutable pendingHighStart = initial.PendingHighStart
        let mutable pendingHighEnd = initial.PendingHighEnd
        let mutable index = offset
        let finishIndex = offset + count
        let mutable error: DecodeError option = None

        let acceptUtf16Unit unit start finish =
            if pendingHigh <> 0 then
                if isLowSurrogate unit then
                    sink pendingHighStart pendingHighEnd pendingHigh
                    sink start finish unit
                    pendingHigh <- 0
                    pendingHighStart <- 0L
                    pendingHighEnd <- 0L
                else
                    error <- Some { Offset = pendingHighStart; Reason = "Unpaired UTF-16 high surrogate." }
            elif isHighSurrogate unit then
                pendingHigh <- unit
                pendingHighStart <- start
                pendingHighEnd <- finish
            elif isLowSurrogate unit then
                error <- Some { Offset = start; Reason = "Unpaired UTF-16 low surrogate." }
            else
                sink start finish unit

        while index < finishIndex && error.IsNone do
            let currentOffset = absoluteOffset
            let value = int bytes[index]
            match encoding with
            | TextEncoding.Windows1252 ->
                let scalar = windows1252Scalar value
                if scalar < 0 then error <- Some { Offset = currentOffset; Reason = "Undefined Windows-1252 byte." }
                else
                    sink currentOffset (currentOffset + 1L) scalar
                    absoluteOffset <- currentOffset + 1L
                    index <- index + 1
            | TextEncoding.Utf8 ->
                if pendingCount = 0 then
                    if value <= 0x7F then
                        sink currentOffset (currentOffset + 1L) value
                        absoluteOffset <- currentOffset + 1L
                        index <- index + 1
                    else
                        let expected = utf8ExpectedCount value
                        if expected = 0 then error <- Some { Offset = currentOffset; Reason = "Invalid UTF-8 leading byte." }
                        else
                            absoluteOffset <- currentOffset + 1L
                            pendingStart <- currentOffset
                            pendingValue <- value &&& (if expected = 2 then 0x1F elif expected = 3 then 0x0F else 0x07)
                            pendingCount <- 1
                            expectedCount <- expected
                            index <- index + 1
                elif not (isUtf8Continuation value) then
                    error <- Some { Offset = pendingStart; Reason = "Invalid UTF-8 continuation byte." }
                else
                    let nextValue = (pendingValue <<< 6) ||| (value &&& 0x3F)
                    let nextCount = pendingCount + 1
                    let nextOffset = currentOffset + 1L
                    index <- index + 1
                    if nextCount = expectedCount then
                        let scalar = nextValue
                        let isInvalid = not (isValidUtf8Scalar expectedCount scalar)
                        if isInvalid then
                            absoluteOffset <- nextOffset
                            error <- Some { Offset = pendingStart; Reason = "Invalid UTF-8 scalar value." }
                        else
                            emitScalar sink pendingStart nextOffset scalar
                            absoluteOffset <- nextOffset
                            pendingStart <- nextOffset
                            pendingValue <- 0
                            pendingCount <- 0
                            expectedCount <- 0
                    else
                        absoluteOffset <- nextOffset
                        pendingValue <- nextValue
                        pendingCount <- nextCount
            | (TextEncoding.Utf16LE | TextEncoding.Utf16BE) as encoding ->
                let start = if pendingCount = 0 then currentOffset else pendingStart
                let partial =
                    if encoding = TextEncoding.Utf16LE then pendingValue ||| (value <<< (pendingCount * 8))
                    else (pendingValue <<< 8) ||| value
                let nextCount = pendingCount + 1
                let nextOffset = currentOffset + 1L
                index <- index + 1
                if nextCount = 2 then
                    absoluteOffset <- nextOffset
                    pendingStart <- nextOffset
                    pendingValue <- 0
                    pendingCount <- 0
                    expectedCount <- 0
                    acceptUtf16Unit partial start nextOffset
                else
                    absoluteOffset <- nextOffset
                    pendingStart <- start
                    pendingValue <- partial
                    pendingCount <- nextCount
                    expectedCount <- 2
            | (TextEncoding.Utf32LE | TextEncoding.Utf32BE) as encoding ->
                let start = if pendingCount = 0 then currentOffset else pendingStart
                let shift = pendingCount * 8
                let partial =
                    if encoding = TextEncoding.Utf32LE then int ((uint32 pendingValue) ||| (uint32 value <<< shift))
                    else int ((uint32 pendingValue <<< 8) ||| uint32 value)
                let nextCount = pendingCount + 1
                let nextOffset = currentOffset + 1L
                index <- index + 1
                if nextCount = 4 then
                    let scalarBits = uint32 partial
                    absoluteOffset <- nextOffset
                    pendingStart <- nextOffset
                    pendingValue <- 0
                    pendingCount <- 0
                    expectedCount <- 0
                    let littleEndian = encoding = TextEncoding.Utf32LE
                    let first, second, third, fourth =
                        if littleEndian then
                            int (scalarBits &&& 0xFFu),
                            int ((scalarBits >>> 8) &&& 0xFFu),
                            int ((scalarBits >>> 16) &&& 0xFFu),
                            int ((scalarBits >>> 24) &&& 0xFFu)
                        else
                            int ((scalarBits >>> 24) &&& 0xFFu),
                            int ((scalarBits >>> 16) &&& 0xFFu),
                            int ((scalarBits >>> 8) &&& 0xFFu),
                            int (scalarBits &&& 0xFFu)
                    if not (isValidUtf32Bytes first second third fourth littleEndian) then
                        error <- Some { Offset = start; Reason = "Invalid UTF-32 scalar value." }
                    else
                        let scalar = int scalarBits
                        emitScalar sink start nextOffset scalar
                else
                    absoluteOffset <- nextOffset
                    pendingStart <- start
                    pendingValue <- partial
                    pendingCount <- nextCount
                    expectedCount <- 4

        let finalState = {
            Encoding = encoding
            AbsoluteOffset = absoluteOffset
            PendingStart = pendingStart
            PendingValue = pendingValue
            PendingCount = pendingCount
            ExpectedCount = expectedCount
            PendingHighSurrogate = pendingHigh
            PendingHighStart = pendingHighStart
            PendingHighEnd = pendingHighEnd
        }

        match error with
        | Some value -> finalState, Error value
        | None -> finalState, Ok (index - offset)

    let flush (state: DecoderState) =
        if state.PendingCount > 0 then fail state.PendingStart "Incomplete encoded character."
        elif state.PendingHighSurrogate <> 0 then fail state.PendingHighStart "Unpaired UTF-16 high surrogate."
        else Ok state
