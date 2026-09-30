namespace VersionControlService.TextDiff

open System

/// The equal prefix of two byte ranges that share an encoding and both start on a scalar boundary at
/// the same absolute offset, with its line count and evidence. No line keys are computed for it.
type CommonRunResult = {
    /// Byte length of the prefix. It ends on a scalar boundary. After a mismatch it ends after the last
    /// line ending before the first differing byte. After a validation error it ends at the error.
    Length: int
    Mismatch: bool
    DifferenceOffset: int option
    /// Lines that end inside the prefix, including a CR line carried in from the previous run.
    Lines: int
    /// Absolute offset after the last line ending inside the prefix, which is where the open line
    /// starts. It is the prefix start when no line ends inside the prefix.
    LastLineStart: int64
    /// True when the prefix ends with a CR whose ending depends on the byte after it.
    PendingCR: bool
    /// NUL evidence, and control-ratio evidence for the windows that the prefix settles by itself,
    /// grouped by window in offset order.
    Evidence: ScannerEvidence[]
    /// Counts for the windows at the prefix edges. Their ratio depends on bytes outside the prefix,
    /// so the caller adds them to its own window counts. Bytes counts only the bytes of the prefix.
    Windows: ObservationWindow[]
    Error: DecodeError option
}

module CommonRun =
    [<Literal>]
    let MaxRunBytes = 8_388_608

    [<Literal>]
    let private BlockBytes = 4_096

    let private firstDifference (left: byte[]) (leftOffset: int) (right: byte[]) (rightOffset: int) (count: int) =
        if Native.bytesEqual left leftOffset right rightOffset count then
            count
        else
            let mutable block = 0
            while block + BlockBytes <= count
                  && Native.bytesEqual left (leftOffset + block) right (rightOffset + block) BlockBytes do
                block <- block + BlockBytes
            let mutable index = block
            while index < count && Native.readByte left (leftOffset + index) = Native.readByte right (rightOffset + index) do
                index <- index + 1
            index

    let private unitBytes (encoding: TextEncoding) =
        match encoding with
        | TextEncoding.Utf16LE
        | TextEncoding.Utf16BE -> 2
        | TextEncoding.Utf32LE
        | TextEncoding.Utf32BE -> 4
        | _ -> 1

    let private isLittleEndian (encoding: TextEncoding) =
        match encoding with
        | TextEncoding.Utf16LE
        | TextEncoding.Utf32LE -> true
        | _ -> false

    /// Reads a UTF-16 or UTF-32 code unit. UTF-32 units are only read after validation, so they fit in 21 bits.
    let inline private readCodeUnit (size: int) (littleEndian: bool) (data: byte[]) (index: int) =
        if size = 2 then
            if littleEndian then Native.readByte data index ||| (Native.readByte data (index + 1) <<< 8)
            else (Native.readByte data index <<< 8) ||| Native.readByte data (index + 1)
        elif littleEndian then
            Native.readByte data index ||| (Native.readByte data (index + 1) <<< 8) ||| (Native.readByte data (index + 2) <<< 16)
        else
            (Native.readByte data (index + 1) <<< 16) ||| (Native.readByte data (index + 2) <<< 8) ||| Native.readByte data (index + 3)

    /// Shortens a length so that it does not end inside an encoded scalar.
    let private trimToBoundary (encoding: TextEncoding) (data: byte[]) (offset: int) (length: int) =
        match encoding with
        | TextEncoding.Utf8 ->
            let mutable result = length
            let mutable back = 1
            let mutable searching = true
            while searching && back <= 3 && back <= length do
                let value = Native.readByte data (offset + length - back)
                if Decoders.isUtf8Continuation value then
                    back <- back + 1
                else
                    searching <- false
                    if value >= 0x80 && Decoders.utf8ExpectedCount value > back then result <- length - back
            result
        | TextEncoding.Utf16LE
        | TextEncoding.Utf16BE ->
            let even = length &&& ~~~1
            if even >= 2 && Decoders.isHighSurrogate (readCodeUnit 2 (isLittleEndian encoding) data (offset + even - 2)) then
                even - 2
            else
                even
        | TextEncoding.Utf32LE
        | TextEncoding.Utf32BE -> length - length % 4
        | TextEncoding.Windows1252 -> length

    let private isValid (encoding: TextEncoding) (data: byte[]) (length: int) =
        match encoding with
        | TextEncoding.Utf8 -> Native.isUtf8 data 0 length
        | TextEncoding.Windows1252 ->
            Native.indexOfByte data 0x81 0 length < 0
            && Native.indexOfByte data 0x8D 0 length < 0
            && Native.indexOfByte data 0x8F 0 length < 0
            && Native.indexOfByte data 0x90 0 length < 0
            && Native.indexOfByte data 0x9D 0 length < 0
        | TextEncoding.Utf16LE
        | TextEncoding.Utf16BE ->
            let littleEndian = isLittleEndian encoding
            let mutable valid = true
            let mutable high = false
            let mutable index = 0
            while valid && index < length do
                let value = readCodeUnit 2 littleEndian data index
                if high then
                    if Decoders.isLowSurrogate value then high <- false else valid <- false
                elif Decoders.isHighSurrogate value then
                    high <- true
                elif Decoders.isLowSurrogate value then
                    valid <- false
                index <- index + 2
            valid && not high
        | TextEncoding.Utf32LE
        | TextEncoding.Utf32BE ->
            let littleEndian = isLittleEndian encoding
            let mutable valid = true
            let mutable index = 0
            while valid && index < length do
                valid <-
                    Decoders.isValidUtf32Bytes
                        (Native.readByte data index)
                        (Native.readByte data (index + 1))
                        (Native.readByte data (index + 2))
                        (Native.readByte data (index + 3))
                        littleEndian
                index <- index + 4
            valid

    /// Finds the first error with the strict decoders, so the offsets and reasons match the scanner.
    let private firstError (encoding: TextEncoding) (position: int64) (data: byte[]) (length: int) =
        let initial = Decoders.createAt encoding position
        match Decoders.decode initial data 0 length (fun _ _ _ -> ()) with
        | _, Error error -> Some error
        | decoded, Ok _ ->
            match Decoders.flush decoded with
            | Error error -> Some error
            | Ok _ -> None

    /// Counts line endings in a byte encoding. Returns the line count, the start of the open line and the pending-CR flag.
    let private countByteLines (data: byte[]) (length: int) (pendingCR: bool) =
        let mutable lines = 0
        let mutable lastStart = 0
        let mutable pending = pendingCR
        let mutable searchFrom = 0
        if pending && length > 0 then
            pending <- false
            lines <- 1
            if Native.readByte data 0 = 0x0A then
                lastStart <- 1
                searchFrom <- 1
        let mutable found = Native.indexOfByte data 0x0A searchFrom length
        while found >= 0 do
            lines <- lines + 1
            lastStart <- found + 1
            found <- Native.indexOfByte data 0x0A (found + 1) length
        let mutable carriage = Native.indexOfByte data 0x0D 0 length
        while carriage >= 0 do
            let next = carriage + 1
            if next = length then
                pending <- true
            elif Native.readByte data next <> 0x0A then
                lines <- lines + 1
                if next > lastStart then lastStart <- next
            carriage <- Native.indexOfByte data 0x0D next length
        lines, lastStart, pending

    /// Counts line endings in UTF-16 or UTF-32 unit by unit.
    let private countUnitLines (size: int) (littleEndian: bool) (data: byte[]) (length: int) (pendingCR: bool) =
        let mutable lines = 0
        let mutable lastStart = 0
        let mutable pending = pendingCR
        let mutable index = 0
        while index < length do
            let value = readCodeUnit size littleEndian data index
            if value = 0x0A then
                // An LF ends one line whether or not a CR came before it.
                lines <- lines + 1
                lastStart <- index + size
                pending <- false
            else
                if pending then
                    lines <- lines + 1
                    lastStart <- index
                    pending <- false
                if value = 0x0D then pending <- true
            index <- index + size
        lines, lastStart, pending

    /// Creates one observation per window that bytes [0, length) touch, with byte counts only.
    let private touchedWindows (position: float) (length: int) =
        let windows = ResizeArray<ObservationWindow>()
        let finish = position + float length
        let mutable start = Scanner.windowStartOf position
        while start < finish do
            let first = max start position
            let last = min (start + 65_536.0) finish
            windows.Add(Scanner.newWindow start (int (last - first)))
            start <- start + 65_536.0
        windows.ToArray()

    /// Counts controls in bytes [first, last) of a byte encoding with a table lookup behind a range test.
    let private countByteControls (data: byte[]) (first: int) (last: int) (windowOrigin: int) (window: ObservationWindow) =
        let flags = Scanner.controlFlags
        let mutable controls = 0
        let mutable firstControl = window.FirstControl
        for index in first .. last - 1 do
            let value = Native.readByte data index
            if value < 0x20 || value = 0x7F then
                if Native.readByte flags value <> 0 then
                    controls <- controls + 1
                    if firstControl < 0 then firstControl <- index - windowOrigin
        window.Controls <- window.Controls + controls
        window.FirstControl <- firstControl

    let private countUtf8Scalars (data: byte[]) (first: int) (last: int) =
        let mutable scalars = 0
        for index in first .. last - 1 do
            if not (Decoders.isUtf8Continuation (Native.readByte data index)) then scalars <- scalars + 1
        scalars

    /// Compares left[leftOffset ..] with right[rightOffset ..] over at most `count` bytes, capped at
    /// MaxRunBytes. `position` is the absolute offset of both starts and `pendingCR` is the flag that
    /// the previous run or the scanner left open.
    let find
        (encoding: TextEncoding)
        (position: int64)
        (pendingCR: bool)
        (left: byte[])
        (leftOffset: int)
        (right: byte[])
        (rightOffset: int)
        (count: int)
        : CommonRunResult =
        if isNull left then nullArg (nameof left)
        if isNull right then nullArg (nameof right)
        if position < 0L then invalidArg (nameof position) "The position cannot be negative."
        if count < 0 || leftOffset < 0 || leftOffset > left.Length - count then
            invalidArg (nameof leftOffset) "The left byte range is outside its buffer."
        if rightOffset < 0 || rightOffset > right.Length - count then
            invalidArg (nameof rightOffset) "The right byte range is outside its buffer."

        let start = float position
        let limit = min count MaxRunBytes
        let difference = firstDifference left leftOffset right rightOffset limit
        let mismatch = difference < limit
        let candidate = trimToBoundary encoding left leftOffset difference
        let data = Native.view left leftOffset candidate

        let validEnd, error =
            if isValid encoding data candidate then
                candidate, None
            else
                match firstError encoding position data candidate with
                | Some decodeError -> int (float decodeError.Offset - start), Some decodeError
                | None -> candidate, None

        let size = unitBytes encoding
        let lines, lastStart, pendingOut =
            if size = 1 then countByteLines data validEnd pendingCR
            else countUnitLines size (isLittleEndian encoding) data validEnd pendingCR

        // After a mismatch the open line may differ, so the prefix stops after the last line ending.
        let length, pendingOut =
            if error.IsNone && mismatch then lastStart, false else validEnd, pendingOut

        if length = 0 then
            {
                Length = 0
                Mismatch = mismatch
                DifferenceOffset = if mismatch then Some difference else None
                Lines = 0
                LastLineStart = position
                PendingCR = pendingCR
                Evidence = Array.empty
                Windows = Array.empty
                Error = error
            }
        else
            let windows = touchedWindows start length
            let keep = Array.create windows.Length false
            let lastIndex = windows.Length - 1
            if windows[0].Start < start then keep[0] <- true
            keep[lastIndex] <- true
            if lastIndex > 0 && windows[lastIndex].Bytes < Scanner.SmallFinalWindowBytes then keep[lastIndex - 1] <- true

            let nulOffsets = ResizeArray<float>()
            if size = 1 then
                let mutable found = Native.indexOfByte data 0 0 length
                while found >= 0 do
                    nulOffsets.Add(start + float found)
                    found <- Native.indexOfByte data 0 (found + 1) length
                for windowIndex in 0 .. lastIndex do
                    let window = windows[windowIndex]
                    let windowOrigin = int (window.Start - start)
                    let first = max 0 windowOrigin
                    let last = first + window.Bytes
                    countByteControls data first last windowOrigin window
                    if encoding = TextEncoding.Windows1252 then
                        window.Scalars <- window.Bytes
                    elif keep[windowIndex] || window.Controls > 0 then
                        // A settled window without controls cannot produce evidence, so its scalars are not needed.
                        window.Scalars <- countUtf8Scalars data first last
            else
                let littleEndian = isLittleEndian encoding
                let mutable windowIndex = 0
                let mutable index = 0
                while index < length do
                    let absolute = start + float index
                    while absolute >= windows[windowIndex].Start + 65_536.0 do
                        windowIndex <- windowIndex + 1
                    let window = windows[windowIndex]
                    let value = readCodeUnit size littleEndian data index
                    window.Scalars <- window.Scalars + 1
                    if value < 0x80 then
                        if Native.readByte Scanner.controlFlags value <> 0 then
                            window.Controls <- window.Controls + 1
                            if window.FirstControl < 0 then window.FirstControl <- int (absolute - window.Start)
                        elif value = 0 then
                            nulOffsets.Add absolute
                    // A surrogate pair is one scalar, counted in the window of its high surrogate.
                    index <- index + (if size = 2 && Decoders.isHighSurrogate value then 4 else size)

            let evidence = ResizeArray<ScannerEvidence>()
            let kept = ResizeArray<ObservationWindow>()
            let mutable nulIndex = 0
            for windowIndex in 0 .. lastIndex do
                let window = windows[windowIndex]
                let windowEnd = window.Start + 65_536.0
                while nulIndex < nulOffsets.Count && nulOffsets[nulIndex] < windowEnd do
                    evidence.Add {
                        Offset = int64 nulOffsets[nulIndex]
                        Kind = "nul"
                        WindowStart = int64 window.Start
                    }
                    nulIndex <- nulIndex + 1
                if keep[windowIndex] then kept.Add window
                else Scanner.finalizeWindow window evidence.Add

            {
                Length = length
                Mismatch = mismatch
                DifferenceOffset = if mismatch then Some difference else None
                Lines = lines
                LastLineStart = int64 (start + float lastStart)
                PendingCR = pendingOut
                Evidence = evidence.ToArray()
                Windows = kept.ToArray()
                Error = error
            }
