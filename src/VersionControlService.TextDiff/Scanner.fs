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
    KeyHash: Hash64
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
    mutable Bytes: int64
    mutable Scalars: int64
    mutable Controls: int64
    mutable FirstControl: int64 option
}

type ScannerState = {
    Encoding: TextEncoding
    StartOffset: int64
    mutable Decoder: DecoderState
    mutable NextOffset: int64
    mutable ValidatedBytes: int64
    mutable LineStart: int64
    mutable LineLengthUtf16: int64
    mutable LineHasText: bool
    mutable LineHash: Hash64
    RetainLimit: int option
    Retained: char[]
    mutable RetainedCount: int
    mutable RetainStopped: bool
    mutable PendingCR: bool
    mutable PendingCRStart: int64
    mutable PendingCREnd: int64
    mutable PendingHigh: int
    mutable PendingHighStart: int64
    mutable CurrentWindow: ObservationWindow
    mutable PreviousWindow: ObservationWindow option
    mutable IsComplete: bool
}

module Scanner =
    [<Literal>]
    let ObservationWindowBytes = 65_536L

    [<Literal>]
    let SmallFinalWindowBytes = 4_096L

    let private windowStart offset = offset / ObservationWindowBytes * ObservationWindowBytes

    let private newWindow start bytes = {
        Start = start
        Bytes = bytes
        Scalars = 0L
        Controls = 0L
        FirstControl = None
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
            LineHasText = false
            LineHash = Hash.create ()
            RetainLimit = retainLimit
            Retained = retained
            RetainedCount = 0
            RetainStopped = false
            PendingCR = false
            PendingCRStart = 0L
            PendingCREnd = 0L
            PendingHigh = 0
            PendingHighStart = 0L
            CurrentWindow = newWindow start (startOffset - start)
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
        if window.Scalars > 0L && window.Controls * 100L > window.Scalars then
            match window.FirstControl with
            | Some offset -> emitEvidence { Offset = offset; Kind = "control ratio"; WindowStart = window.Start }
            | None -> ()

    let private settlePrevious (state: ScannerState) emitEvidence =
        match state.PreviousWindow with
        | Some previous ->
            finalizeWindow previous emitEvidence
            state.PreviousWindow <- None
        | None -> ()

    let private shiftWindow (state: ScannerState) emitEvidence =
        settlePrevious state emitEvidence
        let previous = state.CurrentWindow
        let nextStart = previous.Start + ObservationWindowBytes
        state.PreviousWindow <- Some previous
        state.CurrentWindow <- newWindow nextStart 0L

    let private advanceWindowsTo (state: ScannerState) endOffset emitEvidence =
        let mutable done' = false

        while not done' do
            let windowEnd = state.CurrentWindow.Start + ObservationWindowBytes

            if endOffset >= windowEnd then
                state.CurrentWindow.Bytes <- ObservationWindowBytes
                shiftWindow state emitEvidence
            else
                state.CurrentWindow.Bytes <- max state.CurrentWindow.Bytes (max 0L (endOffset - state.CurrentWindow.Start))

                if state.CurrentWindow.Bytes >= SmallFinalWindowBytes then
                    settlePrevious state emitEvidence

                done' <- true

    let private addRetained (state: ScannerState) (units: int) (first: char) (second: char option) =
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

    let private lineText (state: ScannerState) =
        match state.RetainLimit with
        | None -> None
        | Some _ -> Some(String(state.Retained, 0, state.RetainedCount))

    let private emitLine (state: ScannerState) endOffset ending meter onLine =
        onLine {
            StartOffset = state.LineStart
            EndOffset = endOffset
            Ending = ending
            KeyHash = { Lo = state.LineHash.Lo; Hi = state.LineHash.Hi }
            Utf16Length = state.LineLengthUtf16
            Text = lineText state
        }
        Meter.charge meter 1
        state.LineStart <- endOffset
        state.LineLengthUtf16 <- 0L
        state.LineHasText <- false
        state.LineHash <- Hash.create ()
        state.RetainedCount <- 0
        state.RetainStopped <- false

    let private recordScalar (state: ScannerState) start finish scalar emitEvidence =
        advanceWindowsTo state start emitEvidence
        state.CurrentWindow.Scalars <- state.CurrentWindow.Scalars + 1L
        state.CurrentWindow.Bytes <- max state.CurrentWindow.Bytes (min ObservationWindowBytes (finish - state.CurrentWindow.Start))

        if isControl scalar then
            state.CurrentWindow.Controls <- state.CurrentWindow.Controls + 1L

            if state.CurrentWindow.FirstControl.IsNone then
                state.CurrentWindow.FirstControl <- Some start

        if scalar = 0 then
            emitEvidence { Offset = start; Kind = "nul"; WindowStart = windowStart start }

    let private addTextScalar (state: ScannerState) scalar =
        state.LineHasText <- true
        Hash.addCodeUnit state.LineHash scalar

        if scalar <= 0xFFFF then
            state.LineLengthUtf16 <- state.LineLengthUtf16 + 1L
            addRetained state 1 (char scalar) None
        else
            let value = scalar - 0x10000
            let high = char (0xD800 + (value >>> 10))
            let low = char (0xDC00 + (value &&& 0x3FF))
            state.LineLengthUtf16 <- state.LineLengthUtf16 + 2L
            addRetained state 2 high (Some low)

    let rec private processLineScalar state start finish scalar meter onLine =
        if state.PendingCR then
            if scalar = 0x0A then
                state.PendingCR <- false
                emitLine state finish LineEnding.CRLF meter onLine
            else
                let crEnd = state.PendingCREnd
                state.PendingCR <- false
                emitLine state crEnd LineEnding.CR meter onLine
                processLineScalar state start finish scalar meter onLine
        elif scalar = 0x0D then
            state.PendingCR <- true
            state.PendingCRStart <- start
            state.PendingCREnd <- finish
        elif scalar = 0x0A then
            emitLine state finish LineEnding.LF meter onLine
        else
            addTextScalar state scalar

    let private processScalar state start finish scalar meter onLine emitEvidence =
        recordScalar state start finish scalar emitEvidence
        processLineScalar state start finish scalar meter onLine

    let private processCodeUnit state start finish unit meter onLine emitEvidence =
        if state.PendingHigh <> 0 then
            if unit >= 0xDC00 && unit <= 0xDFFF then
                let scalar = 0x10000 + ((state.PendingHigh - 0xD800) <<< 10) + unit - 0xDC00
                let scalarStart = state.PendingHighStart
                state.PendingHigh <- 0
                processScalar state scalarStart finish scalar meter onLine emitEvidence
            else
                state.PendingHigh <- 0
                processScalar state start finish unit meter onLine emitEvidence
        elif unit >= 0xD800 && unit <= 0xDBFF then
            state.PendingHigh <- unit
            state.PendingHighStart <- start
        else
            processScalar state start finish unit meter onLine emitEvidence

    let private finishWindows (state: ScannerState) emitEvidence =
        advanceWindowsTo state state.NextOffset emitEvidence

        let current = state.CurrentWindow

        match state.PreviousWindow with
        | Some previous when current.Bytes > 0L && current.Bytes < SmallFinalWindowBytes ->
            let merged = {
                Start = previous.Start
                Bytes = previous.Bytes + current.Bytes
                Scalars = previous.Scalars + current.Scalars
                Controls = previous.Controls + current.Controls
                FirstControl =
                    match previous.FirstControl, current.FirstControl with
                    | Some left, Some right -> Some(min left right)
                    | Some left, None -> Some left
                    | None, Some right -> Some right
                    | None, None -> None
            }
            finalizeWindow merged emitEvidence
            state.PreviousWindow <- None
        | Some previous ->
            finalizeWindow previous emitEvidence
            finalizeWindow current emitEvidence
            state.PreviousWindow <- None
        | None -> finalizeWindow current emitEvidence

    let private scanAsciiPrefix state (bytes: byte[]) offset count meter onLine emitEvidence =
        let mutable consumed = 0
        let mutable stopped = false

        while consumed < count && not stopped do
            advanceWindowsTo state state.NextOffset emitEvidence
            let windowRemaining = state.CurrentWindow.Start + ObservationWindowBytes - state.NextOffset
            let segmentCount = min (count - consumed) (int windowRemaining)
            let startOffset = state.NextOffset
            let mutable index = 0

            while index < segmentCount && bytes[offset + consumed + index] < 0x80uy do
                let value = int bytes[offset + consumed + index]
                let absolute = startOffset + int64 index
                let window = state.CurrentWindow
                window.Scalars <- window.Scalars + 1L

                if isControl value then
                    window.Controls <- window.Controls + 1L

                    if window.FirstControl.IsNone then
                        window.FirstControl <- Some absolute

                if value = 0 then
                    emitEvidence { Offset = absolute; Kind = "nul"; WindowStart = window.Start }

                let mutable continueValue = true

                if state.PendingCR then
                    state.PendingCR <- false

                    if value = 0x0A then
                        emitLine state (absolute + 1L) LineEnding.CRLF meter onLine
                        continueValue <- false
                    else
                        emitLine state state.PendingCREnd LineEnding.CR meter onLine

                if continueValue then
                    if value = 0x0D then
                        state.PendingCR <- true
                        state.PendingCRStart <- absolute
                        state.PendingCREnd <- absolute + 1L
                    elif value = 0x0A then
                        emitLine state (absolute + 1L) LineEnding.LF meter onLine
                    else
                        state.LineHasText <- true
                        state.LineLengthUtf16 <- state.LineLengthUtf16 + 1L
                        Hash.addByte state.LineHash (byte value)

                        match state.RetainLimit with
                        | Some limit when not state.RetainStopped ->
                            if state.RetainedCount < limit then
                                state.Retained[state.RetainedCount] <- char value
                                state.RetainedCount <- state.RetainedCount + 1
                            else
                                state.RetainStopped <- true
                        | _ -> ()

                index <- index + 1

            if index = 0 then
                stopped <- true
            else
                state.NextOffset <- startOffset + int64 index
                state.ValidatedBytes <- state.NextOffset - state.StartOffset
                state.Decoder <- { state.Decoder with AbsoluteOffset = state.NextOffset }
                Meter.chargeBytes meter index
                consumed <- consumed + index
                advanceWindowsTo state state.NextOffset emitEvidence

                if index < segmentCount || Meter.overBudget meter || Meter.quantumDue meter then
                    stopped <- true

        consumed

    let scanChunk (state: ScannerState) (bytes: byte[]) offset count endOfSource (meter: Meter) onLine emitEvidence =
        if isNull bytes then nullArg (nameof bytes)
        if offset < 0 || count < 0 || offset > bytes.Length - count then
            invalidArg (nameof offset) "The byte range is outside the input buffer."
        if state.IsComplete then invalidOp "The scanner has already reached the end of its source."

        let mutable consumed = 0
        let mutable error: DecodeError option = None
        let mutable status = InputConsumed
        let mutable stopped = false

        while consumed < count && not stopped && error.IsNone do
            if Meter.overBudget meter then
                status <- BudgetReached
                stopped <- true
            elif Meter.quantumDue meter then
                status <- QuantumReached
                stopped <- true
            else
                let chunkCount = min 4096 (count - consumed)
                let canScanAscii =
                    state.Encoding = TextEncoding.Utf8
                    && state.Decoder.PendingCount = 0
                    && state.Decoder.PendingHighSurrogate = 0
                    && state.PendingHigh = 0

                let asciiConsumed =
                    if canScanAscii then scanAsciiPrefix state bytes (offset + consumed) chunkCount meter onLine emitEvidence
                    else 0

                consumed <- consumed + asciiConsumed

                if Meter.overBudget meter then
                    status <- BudgetReached
                    stopped <- true
                elif Meter.quantumDue meter then
                    status <- QuantumReached
                    stopped <- true
                elif asciiConsumed < chunkCount then
                    let inputOffset = offset + consumed
                    let remaining = chunkCount - asciiConsumed
                    let sink start finish unit = processCodeUnit state start finish unit meter onLine emitEvidence
                    let decoder, result = Decoders.decode state.Decoder bytes inputOffset remaining sink
                    state.Decoder <- decoder

                    match result with
                    | Ok actual ->
                        consumed <- consumed + actual
                        state.NextOffset <- decoder.AbsoluteOffset
                        state.ValidatedBytes <- state.NextOffset - state.StartOffset
                        Meter.chargeBytes meter actual
                        advanceWindowsTo state state.NextOffset emitEvidence
                    | Error decodeError ->
                        let actual = int (decoder.AbsoluteOffset - state.NextOffset)
                        if actual > 0 then Meter.chargeBytes meter actual
                        state.NextOffset <- decoder.AbsoluteOffset
                        state.ValidatedBytes <- max state.ValidatedBytes (decodeError.Offset - state.StartOffset)
                        consumed <- consumed + max 0 actual
                        advanceWindowsTo state state.NextOffset emitEvidence
                        error <- Some decodeError

                if error.IsNone then
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
                if state.PendingHigh <> 0 then
                    error <- Some { Offset = state.PendingHighStart; Reason = "Unpaired decoded high surrogate." }
                    status <- DecodeFailure
                else
                    if state.PendingCR then
                        state.PendingCR <- false
                        emitLine state state.PendingCREnd LineEnding.CR meter onLine

                    if state.LineHasText then
                        emitLine state state.NextOffset LineEnding.NoEnding meter onLine

                    finishWindows state emitEvidence
                    state.IsComplete <- true
                    status <- EndOfInput
        | None when consumed = count && not stopped -> status <- InputConsumed
        | None when consumed = count -> ()
        | None -> ()

        { Consumed = consumed; Status = status; Error = error }

    let validatedBytes (state: ScannerState) = state.ValidatedBytes
