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

type ObservationWindow = {
    Start: int64
    mutable Bytes: int
    mutable Scalars: int
    mutable Controls: int
    mutable FirstControl: int
}

type ScannerState = {
    Encoding: TextEncoding
    StartOffset: int64
    mutable Decoder: DecoderState
    mutable NextOffset: int64
    mutable ValidatedBytes: int64
    mutable LineStart: int64
    mutable LineLengthUtf16: int64
    mutable LineLengthUtf16Delta: int
    mutable LineHasText: bool
    mutable LineHash: Hash64
    RetainLimit: int option
    Retained: char[]
    mutable RetainedCount: int
    mutable RetainStopped: bool
    mutable PendingCR: bool
    mutable PendingCREnd: int64
    mutable PendingBytesBase: int64
    mutable PendingBytesIndex: int
    mutable PendingBytesWindowIndex: int
    mutable PendingBytesWindowIsPrevious: bool
    mutable PendingHighBase: int64
    mutable PendingHighIndex: int
    mutable PendingHighEndBase: int64
    mutable PendingHighEndIndex: int
    mutable PendingHighWindowIndex: int
    mutable PendingHighWindowIsPrevious: bool
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
    let private ObservationWindowByteCount = 65_536

    let private noEnding = LineEnding.NoEnding
    let private lfEnding = LineEnding.LF
    let private crlfEnding = LineEnding.CRLF
    let private crEnding = LineEnding.CR

    let private windowStart offset = offset / ObservationWindowBytes * ObservationWindowBytes

    let private newWindow start bytes = {
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

        let start = windowStart startOffset
        let retained =
            match retainLimit with
            | Some value -> Array.zeroCreate<char> value
            | None -> Array.empty

        {
            Encoding = encoding
            StartOffset = startOffset
            Decoder = Decoders.createAt encoding startOffset
            NextOffset = startOffset
            ValidatedBytes = 0L
            LineStart = startOffset
            LineLengthUtf16 = 0L
            LineLengthUtf16Delta = 0
            LineHasText = false
            LineHash = Hash.create ()
            RetainLimit = retainLimit
            Retained = retained
            RetainedCount = 0
            RetainStopped = false
            PendingCR = false
            PendingCREnd = 0L
            PendingBytesBase = startOffset
            PendingBytesIndex = 0
            PendingBytesWindowIndex = 0
            PendingBytesWindowIsPrevious = false
            PendingHighBase = startOffset
            PendingHighIndex = 0
            PendingHighEndBase = startOffset
            PendingHighEndIndex = 0
            PendingHighWindowIndex = 0
            PendingHighWindowIsPrevious = false
            CurrentWindow = newWindow start (int (startOffset - start))
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
            LineHash = { Lo = state.LineHash.Lo; Hi = state.LineHash.Hi }
            Retained = Array.copy state.Retained
            CurrentWindow = copyWindow state.CurrentWindow
            PreviousWindow = state.PreviousWindow |> Option.map copyWindow
    }

    let private isControl scalar =
        (scalar >= 0x01 && scalar <= 0x08)
        || scalar = 0x0B
        || (scalar >= 0x0E && scalar <= 0x1F)
        || scalar = 0x7F

    let private finalizeWindow (window: ObservationWindow) emitEvidence =
        if window.Scalars > 0 && window.Controls * 100 > window.Scalars then
            if window.FirstControl >= 0 then
                emitEvidence {
                    Offset = window.Start + int64 window.FirstControl
                    Kind = "control ratio"
                    WindowStart = window.Start
                }

    let private settlePrevious (state: ScannerState) emitEvidence =
        match state.PreviousWindow with
        | Some previous ->
            finalizeWindow previous emitEvidence
            state.PreviousWindow <- None
        | None -> ()

    let private shiftWindow (state: ScannerState) emitEvidence =
        settlePrevious state emitEvidence
        if state.Decoder.PendingCount > 0 then
            state.PendingBytesWindowIsPrevious <- true
        if state.Decoder.PendingHighSurrogate <> 0 then
            state.PendingHighWindowIsPrevious <- true
        let previous = state.CurrentWindow
        let nextStart = previous.Start + ObservationWindowBytes
        state.PreviousWindow <- Some previous
        state.CurrentWindow <- newWindow nextStart 0

    let private advanceWindowsBy (state: ScannerState) count emitEvidence =
        if count > 0 then
            let byteCount = state.CurrentWindow.Bytes + count
            if byteCount >= ObservationWindowByteCount then
                state.CurrentWindow.Bytes <- ObservationWindowByteCount
                shiftWindow state emitEvidence
            else
                state.CurrentWindow.Bytes <- byteCount
                if state.CurrentWindow.Bytes >= SmallFinalWindowBytes then
                    settlePrevious state emitEvidence

    let private windowForScalar (state: ScannerState) isPrevious =
        if isPrevious then
            match state.PreviousWindow with
            | Some previous -> previous
            | None -> state.CurrentWindow
        else state.CurrentWindow

    let private recordScalar (window: ObservationWindow) windowIndex scalar emitEvidence =
        window.Scalars <- window.Scalars + 1
        if isControl scalar then
            window.Controls <- window.Controls + 1
            if window.FirstControl < 0 then window.FirstControl <- windowIndex
        if scalar = 0 then
            emitEvidence {
                Offset = window.Start + int64 windowIndex
                Kind = "nul"
                WindowStart = window.Start
            }

    let inline private addRetained (state: ScannerState) units (first: char) (second: char option) =
        match state.RetainLimit with
        | None -> ()
        | Some limit ->
            if not state.RetainStopped then
                if state.RetainedCount + units <= limit then
                    state.Retained[state.RetainedCount] <- first
                    state.RetainedCount <- state.RetainedCount + 1
                    match second with
                    | Some value ->
                        state.Retained[state.RetainedCount] <- value
                        state.RetainedCount <- state.RetainedCount + 1
                    | None -> ()
                else
                    state.RetainStopped <- true

    let inline private lineText (state: ScannerState) =
        match state.RetainLimit with
        | None -> None
        | Some _ -> Some(String(state.Retained, 0, state.RetainedCount))

    let private emitLine (state: ScannerState) endOffset ending meter onLine =
        state.LineLengthUtf16 <- state.LineLengthUtf16 + int64 state.LineLengthUtf16Delta
        state.LineLengthUtf16Delta <- 0
        onLine {
            StartOffset = state.LineStart
            EndOffset = endOffset
            Ending = ending
            KeyLo = state.LineHash.Lo
            KeyHi = state.LineHash.Hi
            Utf16Length = state.LineLengthUtf16
            Text = lineText state
        }
        Meter.charge meter 1
        state.LineStart <- endOffset
        state.LineLengthUtf16 <- 0L
        state.LineHasText <- false
        Hash.reset state.LineHash
        state.RetainedCount <- 0
        state.RetainStopped <- false

    let inline private addTextScalar (state: ScannerState) scalar =
        state.LineHasText <- true
        Hash.addCodeUnit state.LineHash scalar
        if scalar <= 0xFFFF then
            state.LineLengthUtf16Delta <- state.LineLengthUtf16Delta + 1
            addRetained state 1 (char scalar) None
        else
            let value = scalar - 0x10000
            let high = char (0xD800 + (value >>> 10))
            let low = char (0xDC00 + (value &&& 0x3FF))
            state.LineLengthUtf16Delta <- state.LineLengthUtf16Delta + 2
            addRetained state 2 high (Some low)

    let inline private addAsciiText (state: ScannerState) value =
        state.LineHasText <- true
        Hash.addByte state.LineHash (byte value)
        state.LineLengthUtf16Delta <- state.LineLengthUtf16Delta + 1
        match state.RetainLimit with
        | Some limit when not state.RetainStopped ->
            if state.RetainedCount < limit then
                state.Retained[state.RetainedCount] <- char value
                state.RetainedCount <- state.RetainedCount + 1
            else
                state.RetainStopped <- true
        | _ -> ()

    let private processLineScalar state segmentBase finishIndex scalar meter onLine =
        let mutable processCurrent = true
        if state.PendingCR then
            if scalar = 0x0A then
                state.PendingCR <- false
                emitLine state (segmentBase + int64 finishIndex) crlfEnding meter onLine
                processCurrent <- false
            else
                let crEnd = state.PendingCREnd
                state.PendingCR <- false
                emitLine state crEnd crEnding meter onLine

        if processCurrent then
            if scalar = 0x0D then
                state.PendingCR <- true
                state.PendingCREnd <- segmentBase + int64 finishIndex
            elif scalar = 0x0A then
                emitLine state (segmentBase + int64 finishIndex) lfEnding meter onLine
            else
                addTextScalar state scalar

    let private processAscii state segmentBase window windowIndex finishIndex value meter onLine emitEvidence =
        recordScalar window windowIndex value emitEvidence
        let mutable processCurrent = true
        if state.PendingCR then
            if value = 0x0A then
                state.PendingCR <- false
                emitLine state (segmentBase + int64 finishIndex) crlfEnding meter onLine
                processCurrent <- false
            else
                let crEnd = state.PendingCREnd
                state.PendingCR <- false
                emitLine state crEnd crEnding meter onLine

        if processCurrent then
            if value = 0x0D then
                state.PendingCR <- true
                state.PendingCREnd <- segmentBase + int64 finishIndex
            elif value = 0x0A then
                emitLine state (segmentBase + int64 finishIndex) lfEnding meter onLine
            else
                addAsciiText state value

    let private processDecodedScalar state segmentBase window windowIndex finishIndex scalar meter onLine emitEvidence =
        if scalar <= 0x7F then
            recordScalar window windowIndex scalar emitEvidence
            let mutable processCurrent = true
            if state.PendingCR then
                if scalar = 0x0A then
                    state.PendingCR <- false
                    emitLine state (segmentBase + int64 finishIndex) crlfEnding meter onLine
                    processCurrent <- false
                else
                    let crEnd = state.PendingCREnd
                    state.PendingCR <- false
                    emitLine state crEnd crEnding meter onLine

            if processCurrent then
                if scalar = 0x0D then
                    state.PendingCR <- true
                    state.PendingCREnd <- segmentBase + int64 finishIndex
                elif scalar = 0x0A then
                    emitLine state (segmentBase + int64 finishIndex) lfEnding meter onLine
                else
                    addAsciiText state scalar
        else
            recordScalar window windowIndex scalar emitEvidence
            processLineScalar state segmentBase finishIndex scalar meter onLine

    let private scanSegment (state: ScannerState) (bytes: byte[]) offset count meter onLine emitEvidence =
        let segmentBase = state.NextOffset
        let windowByteBase = state.CurrentWindow.Bytes
        let utf16LittleEndian =
            match state.Encoding with
            | TextEncoding.Utf16LE -> true
            | _ -> false
        let utf32LittleEndian =
            match state.Encoding with
            | TextEncoding.Utf32LE -> true
            | _ -> false
        let initialDecoder = state.Decoder
        let mutable pendingValue = initialDecoder.PendingValue
        let mutable pendingCount = initialDecoder.PendingCount
        let mutable expectedCount = initialDecoder.ExpectedCount
        let mutable pendingBytesBase = state.PendingBytesBase
        let mutable pendingBytesIndex = state.PendingBytesIndex
        let mutable pendingBytesWindowIndex = state.PendingBytesWindowIndex
        let mutable pendingBytesWindowIsPrevious = state.PendingBytesWindowIsPrevious
        let mutable pendingHigh = initialDecoder.PendingHighSurrogate
        let mutable pendingHighBase = state.PendingHighBase
        let mutable pendingHighIndex = state.PendingHighIndex
        let mutable pendingHighEndBase = state.PendingHighEndBase
        let mutable pendingHighEndIndex = state.PendingHighEndIndex
        let mutable pendingHighWindowIndex = state.PendingHighWindowIndex
        let mutable pendingHighWindowIsPrevious = state.PendingHighWindowIsPrevious
        let mutable index = 0
        let mutable error: DecodeError option = None

        while index < count && error.IsNone do
            let value = int bytes[offset + index]
            match state.Encoding with
            | TextEncoding.Windows1252 ->
                let scalar = Decoders.windows1252Scalar value
                if scalar < 0 then
                    error <- Some { Offset = segmentBase + int64 index; Reason = "Undefined Windows-1252 byte." }
                else
                    let windowIndex = windowByteBase + index
                    processDecodedScalar state segmentBase state.CurrentWindow windowIndex (index + 1) scalar meter onLine emitEvidence
                    index <- index + 1
            | TextEncoding.Utf8 ->
                if pendingCount = 0 then
                    if value <= 0x7F then
                        let windowIndex = windowByteBase + index
                        processAscii state segmentBase state.CurrentWindow windowIndex (index + 1) value meter onLine emitEvidence
                        index <- index + 1
                    else
                        let expected = Decoders.utf8ExpectedCount value
                        if expected = 0 then
                            error <- Some { Offset = segmentBase + int64 index; Reason = "Invalid UTF-8 leading byte." }
                        else
                            pendingBytesBase <- segmentBase
                            pendingBytesIndex <- index
                            pendingBytesWindowIndex <- windowByteBase + index
                            pendingBytesWindowIsPrevious <- false
                            pendingValue <- value &&& (if expected = 2 then 0x1F elif expected = 3 then 0x0F else 0x07)
                            pendingCount <- 1
                            expectedCount <- expected
                            index <- index + 1
                elif not (Decoders.isUtf8Continuation value) then
                    error <- Some {
                        Offset = pendingBytesBase + int64 pendingBytesIndex
                        Reason = "Invalid UTF-8 continuation byte."
                    }
                else
                    let nextValue = (pendingValue <<< 6) ||| (value &&& 0x3F)
                    let nextCount = pendingCount + 1
                    index <- index + 1
                    if nextCount = expectedCount then
                        if not (Decoders.isValidUtf8Scalar expectedCount nextValue) then
                            error <- Some {
                                Offset = pendingBytesBase + int64 pendingBytesIndex
                                Reason = "Invalid UTF-8 scalar value."
                            }
                        else
                            let window = windowForScalar state pendingBytesWindowIsPrevious
                            processDecodedScalar state segmentBase window pendingBytesWindowIndex index nextValue meter onLine emitEvidence
                            pendingValue <- 0
                            pendingCount <- 0
                            expectedCount <- 0
                            pendingBytesIndex <- -1
                            pendingBytesWindowIndex <- -1
                            pendingBytesWindowIsPrevious <- false
                    else
                        pendingValue <- nextValue
                        pendingCount <- nextCount
            | TextEncoding.Utf16LE
            | TextEncoding.Utf16BE ->
                let unitStartBase = if pendingCount = 0 then segmentBase else pendingBytesBase
                let unitStartIndex = if pendingCount = 0 then index else pendingBytesIndex
                let unitWindowIndex = if pendingCount = 0 then windowByteBase + index else pendingBytesWindowIndex
                let unitWindowIsPrevious = pendingCount > 0 && pendingBytesWindowIsPrevious
                let partial =
                    if utf16LittleEndian then pendingValue ||| (value <<< (pendingCount * 8))
                    else (pendingValue <<< 8) ||| value
                let nextCount = pendingCount + 1
                index <- index + 1
                if nextCount = 1 then
                    pendingBytesBase <- unitStartBase
                    pendingBytesIndex <- unitStartIndex
                    pendingBytesWindowIndex <- unitWindowIndex
                    pendingBytesWindowIsPrevious <- unitWindowIsPrevious
                    pendingValue <- partial
                    pendingCount <- 1
                    expectedCount <- 2
                else
                    pendingValue <- 0
                    pendingCount <- 0
                    expectedCount <- 0
                    pendingBytesIndex <- -1
                    pendingBytesWindowIndex <- -1
                    pendingBytesWindowIsPrevious <- false
                    if pendingHigh <> 0 then
                        if Decoders.isLowSurrogate partial then
                            let scalar = 0x10000 + ((pendingHigh - 0xD800) <<< 10) + partial - 0xDC00
                            let window = windowForScalar state pendingHighWindowIsPrevious
                            processDecodedScalar state segmentBase window pendingHighWindowIndex index scalar meter onLine emitEvidence
                            pendingHigh <- 0
                            pendingHighBase <- segmentBase
                            pendingHighIndex <- index
                            pendingHighEndBase <- segmentBase
                            pendingHighEndIndex <- index
                            pendingHighWindowIndex <- -1
                            pendingHighWindowIsPrevious <- false
                        else
                            error <- Some {
                                Offset = pendingHighBase + int64 pendingHighIndex
                                Reason = "Unpaired UTF-16 high surrogate."
                            }
                    elif Decoders.isHighSurrogate partial then
                        pendingHigh <- partial
                        pendingHighBase <- unitStartBase
                        pendingHighIndex <- unitStartIndex
                        pendingHighEndBase <- segmentBase
                        pendingHighEndIndex <- index
                        pendingHighWindowIndex <- unitWindowIndex
                        pendingHighWindowIsPrevious <- unitWindowIsPrevious
                    elif Decoders.isLowSurrogate partial then
                        error <- Some {
                            Offset = unitStartBase + int64 unitStartIndex
                            Reason = "Unpaired UTF-16 low surrogate."
                        }
                    else
                        let window = windowForScalar state unitWindowIsPrevious
                        processDecodedScalar state segmentBase window unitWindowIndex index partial meter onLine emitEvidence
            | TextEncoding.Utf32LE
            | TextEncoding.Utf32BE ->
                let unitStartBase = if pendingCount = 0 then segmentBase else pendingBytesBase
                let unitStartIndex = if pendingCount = 0 then index else pendingBytesIndex
                let unitWindowIndex = if pendingCount = 0 then windowByteBase + index else pendingBytesWindowIndex
                let unitWindowIsPrevious = pendingCount > 0 && pendingBytesWindowIsPrevious
                if pendingCount < 3 then
                    let partial =
                        if utf32LittleEndian then pendingValue ||| (value <<< (pendingCount * 8))
                        else (pendingValue <<< 8) ||| value
                    if pendingCount = 0 then
                        pendingBytesBase <- unitStartBase
                        pendingBytesIndex <- unitStartIndex
                        pendingBytesWindowIndex <- unitWindowIndex
                        pendingBytesWindowIsPrevious <- false
                    pendingValue <- partial
                    pendingCount <- pendingCount + 1
                    expectedCount <- 4
                    index <- index + 1
                else
                    let first, second, third, fourth =
                        if utf32LittleEndian then
                            pendingValue &&& 0xFF,
                            (pendingValue >>> 8) &&& 0xFF,
                            (pendingValue >>> 16) &&& 0xFF,
                            value
                        else
                            (pendingValue >>> 16) &&& 0xFF,
                            (pendingValue >>> 8) &&& 0xFF,
                            pendingValue &&& 0xFF,
                            value
                    index <- index + 1
                    pendingCount <- 0
                    expectedCount <- 0
                    pendingValue <- 0
                    pendingBytesIndex <- -1
                    pendingBytesWindowIndex <- -1
                    pendingBytesWindowIsPrevious <- false
                    if not (Decoders.isValidUtf32Bytes first second third fourth utf32LittleEndian) then
                        error <- Some {
                            Offset = unitStartBase + int64 unitStartIndex
                            Reason = "Invalid UTF-32 scalar value."
                        }
                    else
                        let scalar =
                            if utf32LittleEndian then first ||| (second <<< 8) ||| (third <<< 16)
                            else (second <<< 16) ||| (third <<< 8) ||| fourth
                        let window = windowForScalar state unitWindowIsPrevious
                        processDecodedScalar state segmentBase window unitWindowIndex index scalar meter onLine emitEvidence

        let nextOffset = segmentBase + int64 index
        state.NextOffset <- nextOffset
        if error.IsNone then
            state.ValidatedBytes <- nextOffset - state.StartOffset
        let pendingStart =
            if pendingCount > 0 then pendingBytesBase + int64 pendingBytesIndex
            else nextOffset
        let pendingHighStart =
            if pendingHigh <> 0 then pendingHighBase + int64 pendingHighIndex
            else nextOffset
        let pendingHighEnd =
            if pendingHigh <> 0 then pendingHighEndBase + int64 pendingHighEndIndex
            else nextOffset
        state.Decoder <- {
            initialDecoder with
                AbsoluteOffset = nextOffset
                PendingStart = pendingStart
                PendingValue = pendingValue
                PendingCount = pendingCount
                ExpectedCount = expectedCount
                PendingHighSurrogate = pendingHigh
                PendingHighStart = pendingHighStart
                PendingHighEnd = pendingHighEnd
        }
        state.PendingBytesBase <- pendingBytesBase
        state.PendingBytesIndex <- pendingBytesIndex
        state.PendingBytesWindowIndex <- pendingBytesWindowIndex
        state.PendingBytesWindowIsPrevious <- pendingBytesWindowIsPrevious
        state.PendingHighBase <- pendingHighBase
        state.PendingHighIndex <- pendingHighIndex
        state.PendingHighEndBase <- pendingHighEndBase
        state.PendingHighEndIndex <- pendingHighEndIndex
        state.PendingHighWindowIndex <- pendingHighWindowIndex
        state.PendingHighWindowIsPrevious <- pendingHighWindowIsPrevious
        index, error

    let private finishWindows (state: ScannerState) emitEvidence =
        let current = state.CurrentWindow
        match state.PreviousWindow with
        | Some previous when current.Bytes > 0 && current.Bytes < SmallFinalWindowBytes ->
            let firstControl =
                if previous.FirstControl >= 0 then previous.FirstControl
                elif current.FirstControl >= 0 then ObservationWindowByteCount + current.FirstControl
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

    let scanChunk (state: ScannerState) (bytes: byte[]) offset count endOfSource (meter: Meter) onLine emitEvidence =
        if isNull bytes then nullArg (nameof bytes)
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
                let windowRemaining = ObservationWindowByteCount - state.CurrentWindow.Bytes
                let segmentCount = min 4096 (min (count - consumed) windowRemaining)
                let actual, segmentError =
                    scanSegment state bytes (offset + consumed) segmentCount meter onLine emitEvidence
                if actual > 0 then
                    Meter.chargeBytes meter actual
                    advanceWindowsBy state actual emitEvidence
                    state.LineLengthUtf16 <- state.LineLengthUtf16 + int64 state.LineLengthUtf16Delta
                    state.LineLengthUtf16Delta <- 0
                    consumed <- consumed + actual
                match segmentError with
                | Some decodeError ->
                    state.ValidatedBytes <- max state.ValidatedBytes (decodeError.Offset - state.StartOffset)
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
            match Decoders.flush state.Decoder with
            | Error decodeError ->
                error <- Some decodeError
                state.ValidatedBytes <- decodeError.Offset - state.StartOffset
                status <- DecodeFailure
            | Ok decoder ->
                state.Decoder <- decoder
                if state.PendingCR then
                    state.PendingCR <- false
                    emitLine state state.PendingCREnd crEnding meter onLine
                if state.LineHasText then
                    emitLine state state.NextOffset noEnding meter onLine
                finishWindows state emitEvidence
                state.IsComplete <- true
                status <- EndOfInput
        | None when consumed = count && not stopped -> status <- InputConsumed
        | None when consumed = count -> ()
        | None -> ()

        { Consumed = consumed; Status = status; Error = error }
