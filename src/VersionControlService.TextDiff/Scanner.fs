namespace VersionControlService.TextDiff

open System
open VersionControlService.Abstractions

type ScannerEvidence = {
    Offset: int64
    Kind: string
    WindowStart: int64
}

type ScannedLine = {
    StartOffset: int64
    EndOffset: int64
    Ending: LineEnding
    KeyLo: uint32
    KeyHi: uint32
    Utf16Length: int64
    Text: string option
}

type ScannerStatus =
    | InputConsumed
    | QuantumReached
    | BudgetReached
    | EndOfInput
    | DecodeFailure

type ScannerStep = {
    Consumed: int
    Status: ScannerStatus
    Error: DecodeError option
}

/// Scalar and control counts for one 64 KiB observation window. Start is an absolute byte offset.
type ObservationWindow = {
    Start: float
    mutable Bytes: int
    mutable Scalars: int
    mutable Controls: int
    mutable FirstControl: int
}

/// Line ending codes stored in LineBatch.Endings.
module LineEndingCode =
    [<Literal>]
    let NoEnding = 0

    [<Literal>]
    let LF = 1

    [<Literal>]
    let CRLF = 2

    [<Literal>]
    let CR = 3

    let toLineEnding (code: int) =
        match code with
        | 1 -> LineEnding.LF
        | 2 -> LineEnding.CRLF
        | 3 -> LineEnding.CR
        | _ -> LineEnding.NoEnding

/// Scanned lines stored column by column in typed arrays, so the scanner allocates nothing per line.
/// Offsets and UTF-16 lengths are floats, which are exact below 2^53. Keys are the two signed words of
/// the FNV-1a 64 hash. The scanner hands a full batch to its consumer and clears it afterwards, so a
/// consumer copies whatever it keeps.
type LineBatch(capacity: int) =
    do if capacity <= 0 then invalidArg (nameof capacity) "The batch capacity must be positive."
    let startOffsets = Array.zeroCreate<float> capacity
    let endOffsets = Array.zeroCreate<float> capacity
    let keysLo = Array.zeroCreate<int> capacity
    let keysHi = Array.zeroCreate<int> capacity
    let utf16Lengths = Array.zeroCreate<float> capacity
    let endings = Array.zeroCreate<byte> capacity
    let texts = Array.zeroCreate<string> capacity
    let mutable count = 0
    let mutable pushed = 0.0

    new() = LineBatch(4_096)

    member _.Capacity = capacity

    member _.Count
        with get () = count
        and set value =
            if value < 0 || value > capacity then invalidArg (nameof value) "The line count is outside the batch."
            count <- value

    /// Lines pushed since the batch was created, including lines already handed out and cleared.
    member _.Pushed = pushed

    member _.StartOffsets = startOffsets
    member _.EndOffsets = endOffsets
    member _.KeysLo = keysLo
    member _.KeysHi = keysHi
    member _.Utf16Lengths = utf16Lengths
    member _.Endings = endings

    /// Retained line text, or null when the scanner keeps no text.
    member _.Texts = texts

    member _.Push(startOffset: float, endOffset: float, ending: int, keyLo: int, keyHi: int, utf16Length: float, text: string) =
        if count = capacity then invalidOp "The line batch is full."
        Native.writeFloat startOffsets count startOffset
        Native.writeFloat endOffsets count endOffset
        Native.writeInt keysLo count keyLo
        Native.writeInt keysHi count keyHi
        Native.writeFloat utf16Lengths count utf16Length
        Native.writeByte endings count ending
        texts[count] <- text
        count <- count + 1
        pushed <- pushed + 1.0

    /// Converts one stored line to the public record.
    member _.Line(index: int) : ScannedLine =
        if index < 0 || index >= count then invalidArg (nameof index) "The line index is outside the batch."
        {
            StartOffset = int64 (Native.readFloat startOffsets index)
            EndOffset = int64 (Native.readFloat endOffsets index)
            Ending = LineEndingCode.toLineEnding (Native.readByte endings index)
            KeyLo = uint32 (Native.readInt keysLo index)
            KeyHi = uint32 (Native.readInt keysHi index)
            Utf16Length = int64 (Native.readFloat utf16Lengths index)
            Text = Option.ofObj texts[index]
        }

/// Resumable scanner state. Absolute offsets are floats so that the loops avoid 64-bit integer
/// arithmetic, which compiles to BigInt in JavaScript. A copy made with Scanner.copyState is a checkpoint.
type ScannerState = {
    Encoding: TextEncoding
    StartOffset: int64
    mutable NextOffset: float
    mutable ValidatedBytes: float
    mutable PendingValue: int
    mutable PendingCount: int
    mutable ExpectedCount: int
    mutable PendingStart: float
    mutable PendingHigh: int
    mutable PendingHighStart: float
    mutable PendingHighEnd: float
    mutable LineStart: float
    mutable LineLengthUtf16: float
    mutable LineHasText: bool
    mutable KeyLo: int
    mutable KeyHi: int
    RetainLimit: int option
    Retained: uint16[]
    mutable RetainedCount: int
    mutable RetainStopped: bool
    mutable PendingCR: bool
    mutable PendingCREnd: float
    mutable CurrentWindow: ObservationWindow
    mutable PreviousWindow: ObservationWindow option
    mutable IsComplete: bool
}

module Scanner =
    [<Literal>]
    let ObservationWindowBytes = 65_536L

    [<Literal>]
    let SmallFinalWindowBytes = 4_096

    [<Literal>]
    let private WindowByteCount = 65_536

    [<Literal>]
    let private SegmentBytes = 4_096

    let windowStartOf (offset: float) = Math.Floor(offset / 65_536.0) * 65_536.0

    /// 1 for U+0001..U+0008, U+000B, U+000E..U+001F and U+007F. NUL, tab, LF, form feed and CR are 0.
    let controlFlags =
        let table = Array.zeroCreate<byte> 256
        for value in 1..8 do
            table[value] <- 1uy
        table[0x0B] <- 1uy
        for value in 0x0E..0x1F do
            table[value] <- 1uy
        table[0x7F] <- 1uy
        table

    let newWindow (start: float) (bytes: int) = {
        Start = start
        Bytes = bytes
        Scalars = 0
        Controls = 0
        FirstControl = -1
    }

    let create encoding startOffset retainLimit =
        if startOffset < 0L then invalidArg (nameof startOffset) "The starting byte offset cannot be negative."
        match retainLimit with
        | Some value when value < 0 -> invalidArg (nameof retainLimit) "The retained text limit cannot be negative."
        | _ -> ()

        let start = float startOffset
        let windowStart = windowStartOf start
        let retained =
            match retainLimit with
            | Some value -> Array.zeroCreate<uint16> value
            | None -> Array.zeroCreate<uint16> 0

        {
            Encoding = encoding
            StartOffset = startOffset
            NextOffset = start
            ValidatedBytes = 0.0
            PendingValue = 0
            PendingCount = 0
            ExpectedCount = 0
            PendingStart = start
            PendingHigh = 0
            PendingHighStart = 0.0
            PendingHighEnd = 0.0
            LineStart = start
            LineLengthUtf16 = 0.0
            LineHasText = false
            KeyLo = Hash.OffsetLo
            KeyHi = Hash.OffsetHi
            RetainLimit = retainLimit
            Retained = retained
            RetainedCount = 0
            RetainStopped = false
            PendingCR = false
            PendingCREnd = 0.0
            CurrentWindow = newWindow windowStart (int (start - windowStart))
            PreviousWindow = None
            IsComplete = false
        }

    let private copyWindow window = {
        Start = window.Start
        Bytes = window.Bytes
        Scalars = window.Scalars
        Controls = window.Controls
        FirstControl = window.FirstControl
    }

    let copyState (state: ScannerState) = {
        state with
            Retained = Array.copy state.Retained
            CurrentWindow = copyWindow state.CurrentWindow
            PreviousWindow = state.PreviousWindow |> Option.map copyWindow
    }

    /// Emits control-ratio evidence when more than one percent of a finished window's scalars are controls.
    let finalizeWindow (window: ObservationWindow) emitEvidence =
        if window.Scalars > 0 && window.Controls * 100 > window.Scalars then
            if window.FirstControl >= 0 then
                emitEvidence {
                    Offset = int64 (window.Start + float window.FirstControl)
                    Kind = "control ratio"
                    WindowStart = int64 window.Start
                }

    let private settlePrevious (state: ScannerState) emitEvidence =
        match state.PreviousWindow with
        | Some previous ->
            finalizeWindow previous emitEvidence
            state.PreviousWindow <- None
        | None -> ()

    let private shiftWindow (state: ScannerState) emitEvidence =
        settlePrevious state emitEvidence
        let previous = state.CurrentWindow
        state.PreviousWindow <- Some previous
        state.CurrentWindow <- newWindow (previous.Start + 65_536.0) 0

    let private advanceWindowsBy (state: ScannerState) count emitEvidence =
        if count > 0 then
            let byteCount = state.CurrentWindow.Bytes + count
            if byteCount >= WindowByteCount then
                state.CurrentWindow.Bytes <- WindowByteCount
                shiftWindow state emitEvidence
            else
                state.CurrentWindow.Bytes <- byteCount
                if state.CurrentWindow.Bytes >= SmallFinalWindowBytes then
                    settlePrevious state emitEvidence

    let inline private retainLimitOf (state: ScannerState) =
        match state.RetainLimit with
        | Some value -> value
        | None -> -1

    let private resetLine (state: ScannerState) (start: float) =
        state.LineStart <- start
        state.LineLengthUtf16 <- 0.0
        state.LineHasText <- false
        state.KeyLo <- Hash.OffsetLo
        state.KeyHi <- Hash.OffsetHi
        state.RetainedCount <- 0
        state.RetainStopped <- false

    let private emitLine (state: ScannerState) (batch: LineBatch) (onLines: LineBatch -> unit) (endOffset: float) (ending: int) =
        if batch.Count = batch.Capacity then
            onLines batch
            batch.Count <- 0
        let text =
            match state.RetainLimit with
            | Some _ -> Native.utf16Decode state.Retained state.RetainedCount
            | None -> null
        batch.Push(state.LineStart, endOffset, ending, state.KeyLo, state.KeyHi, state.LineLengthUtf16, text)
        resetLine state endOffset

    let private closePendingCR (state: ScannerState) batch onLines =
        if state.PendingCR then
            state.PendingCR <- false
            emitLine state batch onLines state.PendingCREnd LineEndingCode.CR

    /// Keeps whole scalars only. Once a scalar does not fit, the rest of the line is dropped as well.
    let inline private retainUnits (state: ScannerState) (limit: int) (units: int) (first: int) (second: int) =
        if not state.RetainStopped then
            if state.RetainedCount + units <= limit then
                Native.writeUnit state.Retained state.RetainedCount first
                if units = 2 then Native.writeUnit state.Retained (state.RetainedCount + 1) second
                state.RetainedCount <- state.RetainedCount + units
            else
                state.RetainStopped <- true

    let inline private retainScalar (state: ScannerState) (limit: int) (scalar: int) =
        if scalar <= 0xFFFF then
            retainUnits state limit 1 scalar 0
        else
            let value = scalar - 0x10000
            retainUnits state limit 2 (0xD800 + (value >>> 10)) (0xDC00 + (value &&& 0x3FF))

    /// Retains `count` ASCII characters read from data[first], data[first + step], ...
    let private retainAscii (state: ScannerState) (limit: int) (data: byte[]) (first: int) (count: int) (step: int) =
        if not state.RetainStopped then
            let room = limit - state.RetainedCount
            let taken = if count < room then count else room
            let retained = state.Retained
            let mutable target = state.RetainedCount
            let mutable source = first
            for _ in 1..taken do
                Native.writeUnit retained target (Native.readByte data source)
                target <- target + 1
                source <- source + step
            state.RetainedCount <- target
            if taken < count then state.RetainStopped <- true

    let private hashByteInto (state: ScannerState) (value: int) =
        let mixed = state.KeyLo ^^^ value
        state.KeyHi <- Hash.mixHi state.KeyHi mixed
        state.KeyLo <- Hash.mixLo mixed

    /// Handles one decoded scalar using the line state stored in `state`. The loops call it for line
    /// endings, controls, NUL and the less common scalars, and handle printable text inline.
    let private acceptScalar
        (state: ScannerState)
        (batch: LineBatch)
        (onLines: LineBatch -> unit)
        (emitEvidence: ScannerEvidence -> unit)
        (limit: int)
        (scalar: int)
        (start: float)
        (finish: float)
        =
        // A scalar belongs to the window of its first byte, which is the previous window when a
        // sequence started just before the boundary.
        let window =
            if start < state.CurrentWindow.Start then
                match state.PreviousWindow with
                | Some previous -> previous
                | None -> state.CurrentWindow
            else
                state.CurrentWindow
        window.Scalars <- window.Scalars + 1
        if scalar < 0x80 then
            if Native.readByte controlFlags scalar <> 0 then
                window.Controls <- window.Controls + 1
                if window.FirstControl < 0 then window.FirstControl <- int (start - window.Start)
            elif scalar = 0 then
                emitEvidence {
                    Offset = int64 start
                    Kind = "nul"
                    WindowStart = int64 window.Start
                }

        if scalar = 0x0A then
            if state.PendingCR then
                state.PendingCR <- false
                emitLine state batch onLines finish LineEndingCode.CRLF
            else
                emitLine state batch onLines finish LineEndingCode.LF
        else
            closePendingCR state batch onLines
            if scalar = 0x0D then
                state.PendingCR <- true
                state.PendingCREnd <- finish
            else
                state.LineHasText <- true
                Hash.forEachUtf8Byte scalar (fun value -> hashByteInto state value)
                state.LineLengthUtf16 <- state.LineLengthUtf16 + (if scalar > 0xFFFF then 2.0 else 1.0)
                if limit >= 0 then retainScalar state limit scalar

    let private finishSegment (state: ScannerState) (origin: float) (index: int) (error: DecodeError option) =
        let nextOffset = origin + float index
        state.NextOffset <- nextOffset
        if error.IsNone then state.ValidatedBytes <- nextOffset - float state.StartOffset
        if state.PendingCount = 0 then state.PendingStart <- nextOffset
        if state.PendingHigh = 0 then
            state.PendingHighStart <- nextOffset
            state.PendingHighEnd <- nextOffset

    // Each encoding has its own loop. The line hash lives in the locals lo and hi and is written to the
    // state around every call that reads or resets it. `origin` is the absolute offset of data[0].

    let private scanUtf8 (state: ScannerState) batch onLines emitEvidence (data: byte[]) (offset: int) (count: int) =
        let origin = state.NextOffset - float offset
        let stop = offset + count
        let limit = retainLimitOf state
        let mutable lo = state.KeyLo
        let mutable hi = state.KeyHi
        let mutable scalars = 0
        let mutable pendingValue = state.PendingValue
        let mutable pendingCount = state.PendingCount
        let mutable expectedCount = state.ExpectedCount
        let mutable index = offset
        let mutable error: DecodeError option = None

        while index < stop && error.IsNone do
            let value = Native.readByte data index
            if pendingCount > 0 then
                // Continues a sequence that the segment or chunk boundary cut, or that the inline path rejected.
                if not (Decoders.isUtf8Continuation value) then
                    error <- Some { Offset = int64 state.PendingStart; Reason = "Invalid UTF-8 continuation byte." }
                else
                    let nextValue = (pendingValue <<< 6) ||| (value &&& 0x3F)
                    let nextCount = pendingCount + 1
                    index <- index + 1
                    if nextCount < expectedCount then
                        pendingValue <- nextValue
                        pendingCount <- nextCount
                    elif not (Decoders.isValidUtf8Scalar expectedCount nextValue) then
                        error <- Some { Offset = int64 state.PendingStart; Reason = "Invalid UTF-8 scalar value." }
                    else
                        pendingValue <- 0
                        pendingCount <- 0
                        expectedCount <- 0
                        state.KeyLo <- lo
                        state.KeyHi <- hi
                        acceptScalar state batch onLines emitEvidence limit nextValue state.PendingStart (origin + float index)
                        lo <- state.KeyLo
                        hi <- state.KeyHi
            elif value >= 0x20 && value < 0x7F then
                if state.PendingCR then
                    state.KeyLo <- lo
                    state.KeyHi <- hi
                    closePendingCR state batch onLines
                    lo <- state.KeyLo
                    hi <- state.KeyHi
                let runStart = index
                let mutable current = value
                while current >= 0x20 && current < 0x7F do
                    let mixed = lo ^^^ current
                    hi <- Hash.mixHi hi mixed
                    lo <- Hash.mixLo mixed
                    index <- index + 1
                    current <- if index < stop then Native.readByte data index else 0
                let length = index - runStart
                scalars <- scalars + length
                state.LineHasText <- true
                state.LineLengthUtf16 <- state.LineLengthUtf16 + float length
                if limit >= 0 then retainAscii state limit data runStart length 1
            elif value < 0x80 then
                state.KeyLo <- lo
                state.KeyHi <- hi
                acceptScalar state batch onLines emitEvidence limit value (origin + float index) (origin + float (index + 1))
                lo <- state.KeyLo
                hi <- state.KeyHi
                index <- index + 1
            else
                let expected = Decoders.utf8ExpectedCount value
                if expected = 0 then
                    error <- Some { Offset = int64 (origin + float index); Reason = "Invalid UTF-8 leading byte." }
                else
                    let leadBits = value &&& (if expected = 2 then 0x1F elif expected = 3 then 0x0F else 0x07)
                    let mutable scalar = leadBits
                    let mutable decoded = 1
                    if index + expected <= stop then
                        while decoded < expected && Decoders.isUtf8Continuation (Native.readByte data (index + decoded)) do
                            scalar <- (scalar <<< 6) ||| (Native.readByte data (index + decoded) &&& 0x3F)
                            decoded <- decoded + 1
                    if decoded = expected && Decoders.isValidUtf8Scalar expected scalar then
                        if state.PendingCR then
                            state.KeyLo <- lo
                            state.KeyHi <- hi
                            closePendingCR state batch onLines
                            lo <- state.KeyLo
                            hi <- state.KeyHi
                        // Valid UTF-8 is already the canonical encoding, so the raw bytes are hashed.
                        let finish = index + expected
                        while index < finish do
                            let mixed = lo ^^^ Native.readByte data index
                            hi <- Hash.mixHi hi mixed
                            lo <- Hash.mixLo mixed
                            index <- index + 1
                        scalars <- scalars + 1
                        state.LineHasText <- true
                        state.LineLengthUtf16 <- state.LineLengthUtf16 + (if scalar > 0xFFFF then 2.0 else 1.0)
                        if limit >= 0 then retainScalar state limit scalar
                    else
                        // The sequence is cut by the segment end or is invalid. The continuation branch
                        // takes it byte by byte, which reports errors exactly as for a split sequence.
                        state.PendingStart <- origin + float index
                        pendingValue <- leadBits
                        pendingCount <- 1
                        expectedCount <- expected
                        index <- index + 1

        state.KeyLo <- lo
        state.KeyHi <- hi
        state.PendingValue <- pendingValue
        state.PendingCount <- pendingCount
        state.ExpectedCount <- expectedCount
        state.CurrentWindow.Scalars <- state.CurrentWindow.Scalars + scalars
        finishSegment state origin index error
        index - offset, error

    let private scanWindows1252 (state: ScannerState) batch onLines emitEvidence (data: byte[]) (offset: int) (count: int) =
        let origin = state.NextOffset - float offset
        let stop = offset + count
        let limit = retainLimitOf state
        let mutable lo = state.KeyLo
        let mutable hi = state.KeyHi
        let mutable scalars = 0
        let mutable index = offset
        let mutable error: DecodeError option = None

        while index < stop && error.IsNone do
            let value = Native.readByte data index
            if value >= 0x20 && value < 0x7F then
                if state.PendingCR then
                    state.KeyLo <- lo
                    state.KeyHi <- hi
                    closePendingCR state batch onLines
                    lo <- state.KeyLo
                    hi <- state.KeyHi
                let runStart = index
                let mutable current = value
                while current >= 0x20 && current < 0x7F do
                    let mixed = lo ^^^ current
                    hi <- Hash.mixHi hi mixed
                    lo <- Hash.mixLo mixed
                    index <- index + 1
                    current <- if index < stop then Native.readByte data index else 0
                let length = index - runStart
                scalars <- scalars + length
                state.LineHasText <- true
                state.LineLengthUtf16 <- state.LineLengthUtf16 + float length
                if limit >= 0 then retainAscii state limit data runStart length 1
            else
                let scalar = Decoders.windows1252Scalar value
                if scalar < 0 then
                    error <- Some { Offset = int64 (origin + float index); Reason = "Undefined Windows-1252 byte." }
                else
                    state.KeyLo <- lo
                    state.KeyHi <- hi
                    acceptScalar state batch onLines emitEvidence limit scalar (origin + float index) (origin + float (index + 1))
                    lo <- state.KeyLo
                    hi <- state.KeyHi
                    index <- index + 1

        state.KeyLo <- lo
        state.KeyHi <- hi
        state.CurrentWindow.Scalars <- state.CurrentWindow.Scalars + scalars
        finishSegment state origin index error
        index - offset, error

    let private scanUtf16 (state: ScannerState) batch onLines emitEvidence (data: byte[]) (offset: int) (count: int) =
        let littleEndian =
            match state.Encoding with
            | TextEncoding.Utf16LE -> true
            | _ -> false
        let lowByte = if littleEndian then 0 else 1
        let origin = state.NextOffset - float offset
        let stop = offset + count
        let limit = retainLimitOf state
        let mutable lo = state.KeyLo
        let mutable hi = state.KeyHi
        let mutable scalars = 0
        let mutable pendingValue = state.PendingValue
        let mutable pendingCount = state.PendingCount
        let mutable expectedCount = state.ExpectedCount
        let mutable pendingHigh = state.PendingHigh
        let mutable index = offset
        let mutable error: DecodeError option = None

        while index < stop && error.IsNone do
            // A whole unit outside the surrogate range with nothing pending takes the inline paths.
            let mutable codeUnit = -1
            if pendingCount = 0 && pendingHigh = 0 && index + 1 < stop then
                let first = Native.readByte data index
                let second = Native.readByte data (index + 1)
                let value = if littleEndian then first ||| (second <<< 8) else (first <<< 8) ||| second
                if value < 0xD800 || value > 0xDFFF then codeUnit <- value

            if codeUnit >= 0x20 && codeUnit < 0x7F then
                if state.PendingCR then
                    state.KeyLo <- lo
                    state.KeyHi <- hi
                    closePendingCR state batch onLines
                    lo <- state.KeyLo
                    hi <- state.KeyHi
                let runStart = index
                let mutable current = codeUnit
                while current >= 0x20 && current < 0x7F do
                    let mixed = lo ^^^ current
                    hi <- Hash.mixHi hi mixed
                    lo <- Hash.mixLo mixed
                    index <- index + 2
                    current <-
                        if index + 1 >= stop then 0
                        elif littleEndian then Native.readByte data index ||| (Native.readByte data (index + 1) <<< 8)
                        else (Native.readByte data index <<< 8) ||| Native.readByte data (index + 1)
                let length = (index - runStart) >>> 1
                scalars <- scalars + length
                state.LineHasText <- true
                state.LineLengthUtf16 <- state.LineLengthUtf16 + float length
                if limit >= 0 then retainAscii state limit data (runStart + lowByte) length 2
            elif codeUnit >= 0 && codeUnit < 0x80 then
                state.KeyLo <- lo
                state.KeyHi <- hi
                acceptScalar state batch onLines emitEvidence limit codeUnit (origin + float index) (origin + float (index + 2))
                lo <- state.KeyLo
                hi <- state.KeyHi
                index <- index + 2
            elif codeUnit >= 0x80 then
                if state.PendingCR then
                    state.KeyLo <- lo
                    state.KeyHi <- hi
                    closePendingCR state batch onLines
                    lo <- state.KeyLo
                    hi <- state.KeyHi
                // The key hashes the UTF-8 form of the scalar, two or three bytes for a BMP unit.
                if codeUnit < 0x800 then
                    let mixed = lo ^^^ (0xC0 ||| (codeUnit >>> 6))
                    hi <- Hash.mixHi hi mixed
                    lo <- Hash.mixLo mixed
                    let mixed = lo ^^^ (0x80 ||| (codeUnit &&& 0x3F))
                    hi <- Hash.mixHi hi mixed
                    lo <- Hash.mixLo mixed
                else
                    let mixed = lo ^^^ (0xE0 ||| (codeUnit >>> 12))
                    hi <- Hash.mixHi hi mixed
                    lo <- Hash.mixLo mixed
                    let mixed = lo ^^^ (0x80 ||| ((codeUnit >>> 6) &&& 0x3F))
                    hi <- Hash.mixHi hi mixed
                    lo <- Hash.mixLo mixed
                    let mixed = lo ^^^ (0x80 ||| (codeUnit &&& 0x3F))
                    hi <- Hash.mixHi hi mixed
                    lo <- Hash.mixLo mixed
                scalars <- scalars + 1
                state.LineHasText <- true
                state.LineLengthUtf16 <- state.LineLengthUtf16 + 1.0
                if limit >= 0 then retainUnits state limit 1 codeUnit 0
                index <- index + 2
            else
                // Byte by byte for units split by a boundary and for surrogates.
                let value = Native.readByte data index
                let unitStart = if pendingCount = 0 then origin + float index else state.PendingStart
                let partial =
                    if littleEndian then pendingValue ||| (value <<< (pendingCount * 8))
                    else (pendingValue <<< 8) ||| value
                index <- index + 1
                if pendingCount = 0 then
                    state.PendingStart <- unitStart
                    pendingValue <- partial
                    pendingCount <- 1
                    expectedCount <- 2
                else
                    pendingValue <- 0
                    pendingCount <- 0
                    expectedCount <- 0
                    let unitEnd = origin + float index
                    if pendingHigh <> 0 then
                        if Decoders.isLowSurrogate partial then
                            let scalar = 0x10000 + ((pendingHigh - 0xD800) <<< 10) + partial - 0xDC00
                            pendingHigh <- 0
                            state.KeyLo <- lo
                            state.KeyHi <- hi
                            acceptScalar state batch onLines emitEvidence limit scalar state.PendingHighStart unitEnd
                            lo <- state.KeyLo
                            hi <- state.KeyHi
                        else
                            error <- Some { Offset = int64 state.PendingHighStart; Reason = "Unpaired UTF-16 high surrogate." }
                    elif Decoders.isHighSurrogate partial then
                        pendingHigh <- partial
                        state.PendingHighStart <- unitStart
                        state.PendingHighEnd <- unitEnd
                    elif Decoders.isLowSurrogate partial then
                        error <- Some { Offset = int64 unitStart; Reason = "Unpaired UTF-16 low surrogate." }
                    else
                        state.KeyLo <- lo
                        state.KeyHi <- hi
                        acceptScalar state batch onLines emitEvidence limit partial unitStart unitEnd
                        lo <- state.KeyLo
                        hi <- state.KeyHi

        state.KeyLo <- lo
        state.KeyHi <- hi
        state.PendingValue <- pendingValue
        state.PendingCount <- pendingCount
        state.ExpectedCount <- expectedCount
        state.PendingHigh <- pendingHigh
        state.CurrentWindow.Scalars <- state.CurrentWindow.Scalars + scalars
        finishSegment state origin index error
        index - offset, error

    let private scanUtf32 (state: ScannerState) batch onLines emitEvidence (data: byte[]) (offset: int) (count: int) =
        let littleEndian =
            match state.Encoding with
            | TextEncoding.Utf32LE -> true
            | _ -> false
        let origin = state.NextOffset - float offset
        let stop = offset + count
        let limit = retainLimitOf state
        let mutable lo = state.KeyLo
        let mutable hi = state.KeyHi
        let mutable scalars = 0
        let mutable pendingValue = state.PendingValue
        let mutable pendingCount = state.PendingCount
        let mutable expectedCount = state.ExpectedCount
        let mutable index = offset
        let mutable error: DecodeError option = None

        while index < stop && error.IsNone do
            if pendingCount = 0 && index + 3 < stop then
                let first = Native.readByte data index
                let second = Native.readByte data (index + 1)
                let third = Native.readByte data (index + 2)
                let fourth = Native.readByte data (index + 3)
                let unitStart = origin + float index
                index <- index + 4
                if not (Decoders.isValidUtf32Bytes first second third fourth littleEndian) then
                    error <- Some { Offset = int64 unitStart; Reason = "Invalid UTF-32 scalar value." }
                else
                    let scalar =
                        if littleEndian then first ||| (second <<< 8) ||| (third <<< 16)
                        else (second <<< 16) ||| (third <<< 8) ||| fourth
                    if scalar >= 0x20 && scalar < 0x7F then
                        if state.PendingCR then
                            state.KeyLo <- lo
                            state.KeyHi <- hi
                            closePendingCR state batch onLines
                            lo <- state.KeyLo
                            hi <- state.KeyHi
                        let mixed = lo ^^^ scalar
                        hi <- Hash.mixHi hi mixed
                        lo <- Hash.mixLo mixed
                        scalars <- scalars + 1
                        state.LineHasText <- true
                        state.LineLengthUtf16 <- state.LineLengthUtf16 + 1.0
                        if limit >= 0 then retainUnits state limit 1 scalar 0
                    else
                        state.KeyLo <- lo
                        state.KeyHi <- hi
                        acceptScalar state batch onLines emitEvidence limit scalar unitStart (origin + float index)
                        lo <- state.KeyLo
                        hi <- state.KeyHi
            else
                // Byte by byte for a unit that a segment or chunk boundary splits.
                let value = Native.readByte data index
                if pendingCount = 0 then state.PendingStart <- origin + float index
                if pendingCount < 3 then
                    pendingValue <-
                        if littleEndian then pendingValue ||| (value <<< (pendingCount * 8))
                        else (pendingValue <<< 8) ||| value
                    pendingCount <- pendingCount + 1
                    expectedCount <- 4
                    index <- index + 1
                else
                    let first = if littleEndian then pendingValue &&& 0xFF else (pendingValue >>> 16) &&& 0xFF
                    let second = (pendingValue >>> 8) &&& 0xFF
                    let third = if littleEndian then (pendingValue >>> 16) &&& 0xFF else pendingValue &&& 0xFF
                    index <- index + 1
                    pendingCount <- 0
                    expectedCount <- 0
                    pendingValue <- 0
                    if not (Decoders.isValidUtf32Bytes first second third value littleEndian) then
                        error <- Some { Offset = int64 state.PendingStart; Reason = "Invalid UTF-32 scalar value." }
                    else
                        let scalar =
                            if littleEndian then first ||| (second <<< 8) ||| (third <<< 16)
                            else (second <<< 16) ||| (third <<< 8) ||| value
                        state.KeyLo <- lo
                        state.KeyHi <- hi
                        acceptScalar state batch onLines emitEvidence limit scalar state.PendingStart (origin + float index)
                        lo <- state.KeyLo
                        hi <- state.KeyHi

        state.KeyLo <- lo
        state.KeyHi <- hi
        state.PendingValue <- pendingValue
        state.PendingCount <- pendingCount
        state.ExpectedCount <- expectedCount
        state.CurrentWindow.Scalars <- state.CurrentWindow.Scalars + scalars
        finishSegment state origin index error
        index - offset, error

    let private scanSegment (state: ScannerState) batch onLines emitEvidence (data: byte[]) (offset: int) (count: int) =
        match state.Encoding with
        | TextEncoding.Utf8 -> scanUtf8 state batch onLines emitEvidence data offset count
        | TextEncoding.Utf16LE
        | TextEncoding.Utf16BE -> scanUtf16 state batch onLines emitEvidence data offset count
        | TextEncoding.Utf32LE
        | TextEncoding.Utf32BE -> scanUtf32 state batch onLines emitEvidence data offset count
        | TextEncoding.Windows1252 -> scanWindows1252 state batch onLines emitEvidence data offset count

    let private decoderState (state: ScannerState) : DecoderState = {
        Encoding = state.Encoding
        AbsoluteOffset = int64 state.NextOffset
        PendingStart = int64 state.PendingStart
        PendingValue = state.PendingValue
        PendingCount = state.PendingCount
        ExpectedCount = state.ExpectedCount
        PendingHighSurrogate = state.PendingHigh
        PendingHighStart = int64 state.PendingHighStart
        PendingHighEnd = int64 state.PendingHighEnd
    }

    let private finishWindows (state: ScannerState) emitEvidence =
        let current = state.CurrentWindow
        match state.PreviousWindow with
        | Some previous when current.Bytes > 0 && current.Bytes < SmallFinalWindowBytes ->
            let firstControl =
                if previous.FirstControl >= 0 then previous.FirstControl
                elif current.FirstControl >= 0 then WindowByteCount + current.FirstControl
                else -1
            let merged = {
                Start = previous.Start
                Bytes = previous.Bytes + current.Bytes
                Scalars = previous.Scalars + current.Scalars
                Controls = previous.Controls + current.Controls
                FirstControl = firstControl
            }
            finalizeWindow merged emitEvidence
            state.PreviousWindow <- None
        | Some previous ->
            finalizeWindow previous emitEvidence
            finalizeWindow current emitEvidence
            state.PreviousWindow <- None
        | None -> finalizeWindow current emitEvidence

    /// Scans bytes[offset .. offset + count - 1]. Lines go into `batch`. When the batch is full, and once
    /// more before returning, the scanner calls `onLines` and then clears the batch.
    let scanChunk
        (state: ScannerState)
        (bytes: byte[])
        offset
        count
        endOfSource
        (meter: Meter)
        (batch: LineBatch)
        (onLines: LineBatch -> unit)
        (emitEvidence: ScannerEvidence -> unit)
        =
        if isNull bytes then nullArg (nameof bytes)
        if isNull (box batch) then nullArg (nameof batch)
        if offset < 0 || count < 0 || offset > bytes.Length - count then
            invalidArg (nameof offset) "The byte range is outside the input buffer."
        if state.IsComplete then invalidOp "The scanner has already reached the end of its source."

        let mutable consumed = 0
        let mutable error: DecodeError option = None
        let mutable status = InputConsumed
        let mutable stopped = false
        let mutable checkedMeter = false

        while consumed < count && not stopped && error.IsNone do
            if not checkedMeter then
                checkedMeter <- true
                if Meter.overBudget meter then
                    status <- BudgetReached
                    stopped <- true
                elif Meter.quantumDue meter then
                    status <- QuantumReached
                    stopped <- true
            if not stopped then
                let windowRemaining = WindowByteCount - state.CurrentWindow.Bytes
                let segmentCount = min SegmentBytes (min (count - consumed) windowRemaining)
                let linesBefore = batch.Pushed
                let actual, segmentError = scanSegment state batch onLines emitEvidence bytes (offset + consumed) segmentCount
                // One unit per line plus one per 4096 bytes, charged once per segment.
                Meter.charge meter (int (batch.Pushed - linesBefore))
                if actual > 0 then
                    Meter.chargeBytes meter actual
                    advanceWindowsBy state actual emitEvidence
                    consumed <- consumed + actual
                match segmentError with
                | Some decodeError ->
                    state.ValidatedBytes <- max state.ValidatedBytes (float decodeError.Offset - float state.StartOffset)
                    error <- Some decodeError
                | None ->
                    if Meter.overBudget meter then
                        status <- BudgetReached
                        stopped <- true
                    elif Meter.quantumDue meter then
                        status <- QuantumReached
                        stopped <- true

        match error with
        | Some _ -> status <- DecodeFailure
        | None when endOfSource && consumed = count && not stopped ->
            match Decoders.flush (decoderState state) with
            | Error decodeError ->
                error <- Some decodeError
                state.ValidatedBytes <- float decodeError.Offset - float state.StartOffset
                status <- DecodeFailure
            | Ok _ ->
                let linesBefore = batch.Pushed
                if state.PendingCR then
                    state.PendingCR <- false
                    emitLine state batch onLines state.PendingCREnd LineEndingCode.CR
                if state.LineHasText then
                    emitLine state batch onLines state.NextOffset LineEndingCode.NoEnding
                Meter.charge meter (int (batch.Pushed - linesBefore))
                finishWindows state emitEvidence
                state.IsComplete <- true
                status <- EndOfInput
        | None when consumed = count && not stopped -> status <- InputConsumed
        | None -> ()

        if batch.Count > 0 then
            onLines batch
            batch.Count <- 0

        { Consumed = consumed; Status = status; Error = error }
