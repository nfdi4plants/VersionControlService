namespace VersionControlService.TextDiff

open System
open VersionControlService.Abstractions

type ClassificationSample = {
    BufferOffset: int64
    Bytes: byte[]
    SampleOffset: int
    SampleLength: int
}

type ClassificationResult =
    | Classified of TextEncoding * hasBom: bool * bomLength: int
    | Candidates of EncodingCandidate[]
    | BinaryEvidence of string

module Classification =
    type private Bom = { Encoding: TextEncoding; Length: int; Bytes: byte[] }
    type private Evaluation = {
        Encoding: TextEncoding
        Error: string option
        ControlEvidence: string option
    }

    /// The decoded text of one candidate, built only when two candidates must be compared or previewed.
    type private DecodedText = {
        Units: uint16[]
        Count: int
        FirstSampleCount: int
    }

    let private utf8Bom = [| 0xEFuy; 0xBBuy; 0xBFuy |]
    let private utf16LeBom = [| 0xFFuy; 0xFEuy |]
    let private utf16BeBom = [| 0xFEuy; 0xFFuy |]
    let private utf32LeBom = [| 0xFFuy; 0xFEuy; 0x00uy; 0x00uy |]
    let private utf32BeBom = [| 0x00uy; 0x00uy; 0xFEuy; 0xFFuy |]
    let private hdf5Signature = [| 0x89uy; 0x48uy; 0x44uy; 0x46uy; 0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy |]

    let private properStart (sample: ClassificationSample) =
        max 0 (min sample.Bytes.Length sample.SampleOffset)

    let private properLength (sample: ClassificationSample) =
        max 0 (min (sample.Bytes.Length - properStart sample) sample.SampleLength)

    let private tryByteAt (samples: ClassificationSample[]) absolute =
        let mutable result = None
        let mutable i = 0
        while result.IsNone && i < samples.Length do
            let sample = samples[i]
            let index = absolute - sample.BufferOffset
            if index >= 0L && index < int64 sample.Bytes.Length then result <- Some sample.Bytes[int index]
            i <- i + 1
        result

    let private hasBytesAt samples offset (expected: byte[]) =
        let mutable matched = true
        let mutable i = 0
        while matched && i < expected.Length do
            matched <- tryByteAt samples (offset + int64 i) = Some expected[i]
            i <- i + 1
        matched

    let private readU32Le samples offset =
        match tryByteAt samples offset, tryByteAt samples (offset + 1L), tryByteAt samples (offset + 2L), tryByteAt samples (offset + 3L) with
        | Some a, Some b, Some c, Some d -> Some(uint32 a ||| (uint32 b <<< 8) ||| (uint32 c <<< 16) ||| (uint32 d <<< 24))
        | _ -> None

    let private readU32Be samples offset =
        match tryByteAt samples offset, tryByteAt samples (offset + 1L), tryByteAt samples (offset + 2L), tryByteAt samples (offset + 3L) with
        | Some a, Some b, Some c, Some d -> Some((uint32 a <<< 24) ||| (uint32 b <<< 16) ||| (uint32 c <<< 8) ||| uint32 d)
        | _ -> None

    let private detectBom samples =
        let choices =
            [| { Encoding = TextEncoding.Utf32LE; Length = 4; Bytes = utf32LeBom }
               { Encoding = TextEncoding.Utf32BE; Length = 4; Bytes = utf32BeBom }
               { Encoding = TextEncoding.Utf16LE; Length = 2; Bytes = utf16LeBom }
               { Encoding = TextEncoding.Utf16BE; Length = 2; Bytes = utf16BeBom }
               { Encoding = TextEncoding.Utf8; Length = 3; Bytes = utf8Bom } |]
        choices |> Array.tryFind (fun item -> hasBytesAt samples 0L item.Bytes)

    let private binarySignature sourceLength samples =
        let starts bytes = hasBytesAt samples 0L bytes
        let ascii text = text |> Seq.map (fun c -> byte c) |> Seq.toArray
        let pdf = ascii "%PDF-"
        let zip = starts [| 0x50uy; 0x4Buy; 0x03uy; 0x04uy |]
        let zipVersion = tryByteAt samples 4L |> Option.map int
        if zip && (zipVersion |> Option.exists (fun v -> v >= 10 && v <= 63)) then Some "ZIP archive"
        elif starts pdf &&
             (tryByteAt samples 5L |> Option.exists (fun b -> b >= 0x30uy && b <= 0x39uy)) &&
             tryByteAt samples 6L = Some 0x2Euy &&
             (tryByteAt samples 7L |> Option.exists (fun b -> b >= 0x30uy && b <= 0x39uy)) then Some "PDF document"
        elif starts [| 0x89uy; 0x50uy; 0x4Euy; 0x47uy; 0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy |] then Some "PNG image"
        elif starts [| 0xFFuy; 0xD8uy; 0xFFuy |] then Some "JPEG image"
        elif starts (ascii "GIF87a") || starts (ascii "GIF89a") then Some "GIF image"
        elif starts [| 0x49uy; 0x49uy; 0x2Auy; 0x00uy |] || starts [| 0x4Duy; 0x4Duy; 0x00uy; 0x2Auy |] then Some "TIFF image"
        elif starts (ascii "BM") &&
             (readU32Le samples 2L |> Option.exists (fun size -> size >= 26u && int64 size <= sourceLength)) then Some "BMP image"
        elif starts [| 0x1Fuy; 0x8Buy; 0x08uy |] && (tryByteAt samples 3L |> Option.exists (fun flags -> (flags &&& 0xE0uy) = 0uy)) then Some "gzip stream"
        elif starts [| 0x42uy; 0x5Auy; 0x68uy |] && (tryByteAt samples 3L |> Option.exists (fun level -> level >= 0x31uy && level <= 0x39uy)) then Some "bzip2 stream"
        elif starts [| 0xFDuy; 0x37uy; 0x7Auy; 0x58uy; 0x5Auy; 0x00uy |] then Some "xz stream"
        elif starts [| 0x28uy; 0xB5uy; 0x2Fuy; 0xFDuy |] then Some "zstd stream"
        elif starts [| 0x37uy; 0x7Auy; 0xBCuy; 0xAFuy; 0x27uy; 0x1Cuy |] then Some "7z archive"
        elif starts [| 0x7Fuy; 0x45uy; 0x4Cuy; 0x46uy |] && (tryByteAt samples 4L |> Option.exists (fun c -> c = 1uy || c = 2uy)) then Some "ELF executable"
        elif starts (ascii "MZ") &&
             (match readU32Le samples 0x3CL with
              | Some offset when offset <= uint32 Int32.MaxValue && int64 offset + 4L <= sourceLength ->
                  hasBytesAt samples (int64 offset) (ascii "PE\000\000")
              | _ -> false) then Some "PE executable"
        elif starts [| 0xFEuy; 0xEDuy; 0xFAuy; 0xCEuy |] || starts [| 0xFEuy; 0xEDuy; 0xFAuy; 0xCFuy |] ||
             starts [| 0xCEuy; 0xFAuy; 0xEDuy; 0xFEuy |] || starts [| 0xCFuy; 0xFAuy; 0xEDuy; 0xFEuy |] then
            let littleEndian = starts [| 0xCEuy; 0xFAuy; 0xEDuy; 0xFEuy |] || starts [| 0xCFuy; 0xFAuy; 0xEDuy; 0xFEuy |]
            let commandCount = if littleEndian then readU32Le samples 16L else readU32Be samples 16L
            if sourceLength >= 28L && (commandCount |> Option.exists (fun count -> count <= 100000u)) then Some "Mach-O executable"
            else None
        elif starts [| 0xCAuy; 0xFEuy; 0xBAuy; 0xBEuy |] &&
             (readU32Be samples 4L |> Option.exists (fun count -> count >= 1u && count <= 4096u)) then Some "Mach-O universal binary"
        else
            let rec findHdf offset =
                if offset + int64 hdf5Signature.Length > 65536L || offset + int64 hdf5Signature.Length > sourceLength then None
                elif hasBytesAt samples offset hdf5Signature then Some "HDF5 file"
                else findHdf (if offset = 0L then 512L else offset * 2L)
            findHdf 0L

    let private adjustedBounds sourceLength encoding (sample: ClassificationSample) =
        let first = properStart sample
        let finish = first + properLength sample
        let absoluteFirst = sample.BufferOffset + int64 first
        let absoluteFinish = sample.BufferOffset + int64 finish
        let mutable start = first
        let mutable stop = finish
        match encoding with
        | TextEncoding.Utf8 ->
            if absoluteFirst > 0L then
                while start < stop && (sample.Bytes[start] &&& 0xC0uy) = 0x80uy do start <- start + 1
            if absoluteFinish < sourceLength && stop > start then
                let mutable lead = stop - 1
                while lead >= start && (sample.Bytes[lead] &&& 0xC0uy) = 0x80uy do lead <- lead - 1
                if lead >= start then
                    let b = int sample.Bytes[lead]
                    let width = if b < 0x80 then 1 elif b < 0xE0 then 2 elif b < 0xF0 then 3 elif b < 0xF8 then 4 else 1
                    if stop - lead < width then stop <- lead
        | TextEncoding.Utf16LE | TextEncoding.Utf16BE ->
            if (absoluteFirst &&& 1L) <> 0L then start <- min stop (start + 1)
            if absoluteFirst > 0L && start + 1 < stop then
                let a, b = int sample.Bytes[start], int sample.Bytes[start + 1]
                let unit = if encoding = TextEncoding.Utf16LE then a ||| (b <<< 8) else (a <<< 8) ||| b
                if unit >= 0xDC00 && unit <= 0xDFFF then start <- start + 2
            if absoluteFinish < sourceLength then
                let usable = (stop - start) &&& (~~~1)
                stop <- start + usable
                if stop - start >= 2 then
                    let i = stop - 2
                    let unit = if encoding = TextEncoding.Utf16LE then int sample.Bytes[i] ||| (int sample.Bytes[i + 1] <<< 8) else (int sample.Bytes[i] <<< 8) ||| int sample.Bytes[i + 1]
                    if unit >= 0xD800 && unit <= 0xDBFF then stop <- i
        | TextEncoding.Utf32LE | TextEncoding.Utf32BE ->
            let misalignment = int (absoluteFirst &&& 3L)
            start <- min stop (start + ((4 - misalignment) &&& 3))
            let aligned = (stop - start) / 4 * 4
            if absoluteFinish < sourceLength then stop <- start + aligned
        | TextEncoding.Windows1252 -> ()
        start, max start stop

    let private windowStart sourceLength offset =
        let window = 65536L
        let remainder = sourceLength % window
        if sourceLength < 4096L then 0L
        elif remainder > 0L && remainder < 4096L && offset >= sourceLength - (window + remainder) then max 0L (sourceLength - (window + remainder))
        else (offset / window) * window

    /// The end of the window that starts at `key`, or the maximum value when it is the last one.
    let private windowEnd sourceLength key =
        if windowStart sourceLength (key + 65536L) = key then Int64.MaxValue else key + 65536L

    // Scalar values 0 to 127 map to 2 for NUL, 1 for the other control characters and 0 for the rest.
    let private controlKinds =
        Array.init 128 (fun value ->
            if value = 0 then 2uy
            elif (value >= 1 && value <= 8) || value = 11 || (value >= 14 && value <= 31) || value = 127 then 1uy
            else 0uy)

    let private controlKind (value: int) =
        if value < 128 then Native.readByte controlKinds value else 0

    let private clampIndex (value: int64) =
        if value < -1L then -1
        elif value > int64 Int32.MaxValue then Int32.MaxValue
        else int value

    /// Counts scalars per window and remembers the first NUL and the first window with too many controls.
    /// Scalars arrive in increasing offset order, so a high-water mark counts each source scalar once even
    /// when samples overlap. Offsets inside a sample are relative to the sample buffer, which keeps the hot
    /// loops on 32-bit integers.
    type private Tally(sourceLength: int64) =
        let mutable baseOffset = 0L
        let mutable highWater = -1L
        let mutable highIndex = -1
        let mutable highIndexAtBegin = -1
        let mutable windowKey = 0L
        let mutable windowEndOffset = 0L
        let mutable windowEndIndex = -1
        let mutable scalars = 0
        let mutable controls = 0
        let mutable controlKey = -1L
        let mutable nulAt = 0L
        let mutable hasNul = false

        let closeWindow () =
            if scalars > 0 && controls * 100 > scalars && controlKey < 0L then controlKey <- windowKey

        member _.Begin(sampleOffset: int64) =
            baseOffset <- sampleOffset
            highIndex <- clampIndex (highWater - sampleOffset)
            highIndexAtBegin <- highIndex
            windowEndIndex <-
                if windowEndOffset = Int64.MaxValue then Int32.MaxValue
                else clampIndex (windowEndOffset - sampleOffset)

        /// Records the scalar that starts at byte `index` of the sample. `kind` comes from `controlKind`.
        member _.Scalar(index: int, kind: int) =
            if index > highIndex then
                highIndex <- index
                if index >= windowEndIndex then
                    closeWindow ()
                    windowKey <- windowStart sourceLength (baseOffset + int64 index)
                    windowEndOffset <- windowEnd sourceLength windowKey
                    windowEndIndex <-
                        if windowEndOffset = Int64.MaxValue then Int32.MaxValue
                        else clampIndex (windowEndOffset - baseOffset)
                    scalars <- 0
                    controls <- 0
                scalars <- scalars + 1
                if kind = 1 then controls <- controls + 1
                elif kind = 2 && not hasNul then
                    hasNul <- true
                    nulAt <- baseOffset + int64 index

        member _.End() =
            if highIndex > highIndexAtBegin then highWater <- baseOffset + int64 highIndex

        member _.HasNul = hasNul

        member _.Evidence() =
            closeWindow ()
            if hasNul then Some(sprintf "NUL character at byte %d" nulAt)
            elif controlKey >= 0L then Some(sprintf "control ratio above 1%% in window at byte %d" controlKey)
            else None

    let private failure encoding (offset: int64) =
        Some(sprintf "invalid %s sequence at byte %d" (Decoders.name encoding) offset)

    /// Counts a range that is known to be valid UTF-8. Every lead byte and ASCII byte starts one scalar.
    let private countUtf8 (tally: Tally) (bytes: byte[]) start stop =
        for i = start to stop - 1 do
            let value = Native.readByte bytes i
            if value < 0x80 then tally.Scalar(i, controlKind value)
            elif value >= 0xC0 then tally.Scalar(i, 0)

    /// Returns the byte index of the first undefined byte, or -1.
    let private countWindows1252 (tally: Tally) (bytes: byte[]) start stop =
        let mutable index = start
        let mutable failedAt = -1
        while failedAt < 0 && index < stop do
            let value = Native.readByte bytes index
            if value < 0x80 then
                tally.Scalar(index, controlKind value)
                index <- index + 1
            elif value <= 0x9F && Native.readInt Decoders.windows1252 value < 0 then failedAt <- index
            else
                tally.Scalar(index, 0)
                index <- index + 1
        failedAt

    /// Returns the byte index where the decoder reports its error, or -1. A high surrogate counts once its
    /// low surrogate arrives, so an unfinished pair at the end of a sample is not counted.
    let private countUtf16 (tally: Tally) (bytes: byte[]) start stop littleEndian endsAtEof =
        let mutable index = start
        let mutable failedAt = -1
        let mutable highAt = -1
        while failedAt < 0 && index + 1 < stop do
            let first = Native.readByte bytes index
            let second = Native.readByte bytes (index + 1)
            let unit = if littleEndian then first ||| (second <<< 8) else (first <<< 8) ||| second
            if highAt >= 0 then
                if unit >= 0xDC00 && unit <= 0xDFFF then
                    tally.Scalar(highAt, 0)
                    highAt <- -1
                else failedAt <- highAt
            elif unit >= 0xD800 && unit <= 0xDBFF then highAt <- index
            elif unit >= 0xDC00 && unit <= 0xDFFF then failedAt <- index
            else tally.Scalar(index, controlKind unit)
            index <- index + 2
        if failedAt < 0 && endsAtEof then
            if index < stop then failedAt <- index
            elif highAt >= 0 then failedAt <- highAt
        failedAt

    /// Runs the strict decoder for the encodings and inputs without a loop of their own, so that partial
    /// counts and error offsets stay exactly those of the decoder.
    let private countWithDecoder (tally: Tally) encoding (sample: ClassificationSample) start stop endsAtEof =
        let baseOffset = sample.BufferOffset
        let mutable pendingHigh = 0
        let mutable pendingIndex = 0
        let sink (startOffset: int64) (_endOffset: int64) (codeUnit: int) =
            let index = int (startOffset - baseOffset)
            if pendingHigh <> 0 && codeUnit >= 0xDC00 && codeUnit <= 0xDFFF then
                pendingHigh <- 0
                tally.Scalar(pendingIndex, 0)
            elif codeUnit >= 0xD800 && codeUnit <= 0xDBFF then
                pendingHigh <- codeUnit
                pendingIndex <- index
            else tally.Scalar(index, controlKind codeUnit)
        let state = Decoders.createAt encoding (baseOffset + int64 start)
        let updated, decoded = Decoders.decode state sample.Bytes start (stop - start) sink
        match decoded with
        | Error error -> failure encoding error.Offset
        | Ok _ ->
            if endsAtEof then
                match Decoders.flush updated with
                | Error error -> failure encoding error.Offset
                | Ok _ -> None
            else None

    let private countSample (tally: Tally) encoding (sample: ClassificationSample) start stop endsAtEof =
        tally.Begin sample.BufferOffset
        let indexFailure failedAt =
            if failedAt >= 0 then failure encoding (sample.BufferOffset + int64 failedAt) else None
        let result =
            match encoding with
            | TextEncoding.Windows1252 -> indexFailure (countWindows1252 tally sample.Bytes start stop)
            | TextEncoding.Utf16LE -> indexFailure (countUtf16 tally sample.Bytes start stop true endsAtEof)
            | TextEncoding.Utf16BE -> indexFailure (countUtf16 tally sample.Bytes start stop false endsAtEof)
            | TextEncoding.Utf8 when Native.isUtf8 sample.Bytes start (stop - start) ->
                countUtf8 tally sample.Bytes start stop
                None
            | _ -> countWithDecoder tally encoding sample start stop endsAtEof
        tally.End()
        result

    let private orderedSamples samples =
        samples |> Array.sortBy (fun s -> s.BufferOffset + int64 (properStart s))

    let private sampleRange sourceLength encoding bomLength (sample: ClassificationSample) =
        let mutable start, stop = adjustedBounds sourceLength encoding sample
        if sample.BufferOffset + int64 start = 0L && bomLength > 0 then start <- min stop (start + bomLength)
        start, stop

    /// Walks the samples in order. With `stopAtNul` it skips the remaining samples once a NUL is counted.
    /// Only callers that treat any NUL evidence as decisive may ask for it, because the error is then incomplete.
    let private evaluate sourceLength samples encoding bomLength stopAtNul =
        let ordered = orderedSamples samples
        let tally = Tally(sourceLength)
        let mutable decodeFailure = None
        let mutable sampleIndex = 0
        while decodeFailure.IsNone && not (stopAtNul && tally.HasNul) && sampleIndex < ordered.Length do
            let sample = ordered[sampleIndex]
            let start, stop = sampleRange sourceLength encoding bomLength sample
            if stop > start then
                let endsAtEof = sample.BufferOffset + int64 stop = sourceLength
                decodeFailure <- countSample tally encoding sample start stop endsAtEof
            sampleIndex <- sampleIndex + 1
        { Encoding = encoding; Error = decodeFailure; ControlEvidence = tally.Evidence() }

    /// Writes the code units of a range that is known to be valid UTF-8 and returns the new unit count.
    let private writeUtf8Units (bytes: byte[]) start stop (units: uint16[]) count =
        let mutable index = start
        let mutable next = count
        while index < stop do
            let lead = Native.readByte bytes index
            if lead < 0x80 then
                Native.writeUnit units next lead
                next <- next + 1
                index <- index + 1
            elif lead < 0xE0 then
                Native.writeUnit units next (((lead &&& 0x1F) <<< 6) ||| (Native.readByte bytes (index + 1) &&& 0x3F))
                next <- next + 1
                index <- index + 2
            elif lead < 0xF0 then
                let scalar =
                    ((lead &&& 0x0F) <<< 12)
                    ||| ((Native.readByte bytes (index + 1) &&& 0x3F) <<< 6)
                    ||| (Native.readByte bytes (index + 2) &&& 0x3F)
                Native.writeUnit units next scalar
                next <- next + 1
                index <- index + 3
            else
                let scalar =
                    ((lead &&& 0x07) <<< 18)
                    ||| ((Native.readByte bytes (index + 1) &&& 0x3F) <<< 12)
                    ||| ((Native.readByte bytes (index + 2) &&& 0x3F) <<< 6)
                    ||| (Native.readByte bytes (index + 3) &&& 0x3F)
                let offset = scalar - 0x10000
                Native.writeUnit units next (0xD800 + (offset >>> 10))
                Native.writeUnit units (next + 1) (0xDC00 + (offset &&& 0x3FF))
                next <- next + 2
                index <- index + 4
        next

    let private writeWindows1252Units (bytes: byte[]) start stop (units: uint16[]) count =
        let mutable next = count
        for index = start to stop - 1 do
            Native.writeUnit units next (Native.readInt Decoders.windows1252 (Native.readByte bytes index))
            next <- next + 1
        next

    let private writeUtf16Units (bytes: byte[]) start stop littleEndian (units: uint16[]) count =
        let mutable index = start
        let mutable next = count
        while index + 1 < stop do
            let first = Native.readByte bytes index
            let second = Native.readByte bytes (index + 1)
            Native.writeUnit units next (if littleEndian then first ||| (second <<< 8) else (first <<< 8) ||| second)
            next <- next + 1
            index <- index + 2
        next

    /// Decodes every sample the way `evaluate` walks them, for an encoding that evaluated without an error.
    let private decodedText sourceLength samples encoding =
        let ordered = orderedSamples samples
        let ranges = ordered |> Array.map (sampleRange sourceLength encoding 0)
        let capacity = ranges |> Array.sumBy (fun (start, stop) -> stop - start)
        let units = Array.zeroCreate<uint16> capacity
        let mutable count = 0
        let mutable firstSampleCount = 0
        for sampleIndex = 0 to ordered.Length - 1 do
            let start, stop = ranges[sampleIndex]
            let bytes = ordered[sampleIndex].Bytes
            if stop > start then
                count <-
                    match encoding with
                    | TextEncoding.Utf8 -> writeUtf8Units bytes start stop units count
                    | TextEncoding.Windows1252 -> writeWindows1252Units bytes start stop units count
                    | TextEncoding.Utf16LE -> writeUtf16Units bytes start stop true units count
                    | TextEncoding.Utf16BE -> writeUtf16Units bytes start stop false units count
                    | _ -> count
            if sampleIndex = 0 then firstSampleCount <- count
        { Units = units; Count = count; FirstSampleCount = firstSampleCount }

    let private sameText (left: DecodedText) (right: DecodedText) =
        let mutable equal = left.Count = right.Count
        let mutable index = 0
        while equal && index < left.Count do
            equal <- Native.readUnit left.Units index = Native.readUnit right.Units index
            index <- index + 1
        equal

    /// The first up to 2048 units of the first sample, without splitting a surrogate pair.
    let private previewOf (text: DecodedText) =
        let taken = min text.FirstSampleCount 2049
        let length =
            if taken <= 2048 then taken
            elif Decoders.isHighSurrogate (Native.readUnit text.Units 2047) then 2047
            else 2048
        Native.utf16Decode text.Units length

    let private parityCandidate samples oddOffsets =
        let mutable zeros = 0
        let mutable parityZeros = 0
        let mutable total = 0
        for sample in samples do
            let first = properStart sample
            let finish = first + properLength sample
            let baseOdd = int (sample.BufferOffset &&& 1L)
            let bytes = sample.Bytes
            total <- total + (finish - first)
            // A native search skips the whole sample when it has no zero, which is the case for most text.
            let firstZero = Native.indexOfByte bytes 0 first finish
            if firstZero >= 0 then
                for i = firstZero to finish - 1 do
                    if Native.readByte bytes i = 0 then
                        zeros <- zeros + 1
                        if (((baseOdd + i) &&& 1) = 1) = oddOffsets then parityZeros <- parityZeros + 1
        total > 0 && zeros * 10 >= total && parityZeros * 10 >= zeros * 9

    let classifyWithChoice sourceLength samples encoding =
        match detectBom samples with
        | Some bom ->
            let evaluation = evaluate sourceLength samples bom.Encoding bom.Length false
            match evaluation.Error, evaluation.ControlEvidence with
            | Some error, _ -> BinaryEvidence error
            | None, Some evidence -> BinaryEvidence evidence
            | _ -> Classified(bom.Encoding, true, bom.Length)
        | _ ->
            match binarySignature sourceLength samples with
            | Some evidence -> BinaryEvidence evidence
            | None ->
                let evaluation = evaluate sourceLength samples encoding 0 false
                match evaluation.Error, evaluation.ControlEvidence with
                | Some error, _ -> BinaryEvidence error
                | None, Some evidence -> BinaryEvidence evidence
                | _ -> Classified(encoding, false, 0)

    let classify sourceLength samples =
        match detectBom samples with
        | Some bom -> classifyWithChoice sourceLength samples bom.Encoding
        | None ->
            match binarySignature sourceLength samples with
            | Some evidence -> BinaryEvidence evidence
            | None ->
                let accepted = ResizeArray<Evaluation>()
                let consider (value: Evaluation) =
                    if value.Error.IsNone && value.ControlEvidence.IsNone then accepted.Add value
                if parityCandidate samples true then consider (evaluate sourceLength samples TextEncoding.Utf16LE 0 true)
                if parityCandidate samples false then consider (evaluate sourceLength samples TextEncoding.Utf16BE 0 true)
                let utf8 = evaluate sourceLength samples TextEncoding.Utf8 0 true
                consider utf8
                let windows1252 = evaluate sourceLength samples TextEncoding.Windows1252 0 true
                consider windows1252
                if accepted.Count = 0 then
                    match utf8.Error, utf8.ControlEvidence, windows1252.Error, windows1252.ControlEvidence with
                    | _, Some evidence, _, _ | _, _, _, Some evidence -> BinaryEvidence evidence
                    | Some error, _, _, _ -> BinaryEvidence error
                    | _, _, Some error, _ -> BinaryEvidence error
                    | _ -> BinaryEvidence "no supported text encoding"
                elif accepted.Count = 1 then Classified(accepted[0].Encoding, false, 0)
                else
                    let distinct = ResizeArray<Evaluation>()
                    let texts = ResizeArray<DecodedText>()
                    for evaluation in accepted do
                        let text = decodedText sourceLength samples evaluation.Encoding
                        if not (texts |> Seq.exists (fun existing -> sameText existing text)) then
                            distinct.Add evaluation
                            texts.Add text
                    if distinct.Count = 1 then Classified(distinct[0].Encoding, false, 0)
                    else
                        let candidates =
                            Array.init distinct.Count (fun index ->
                                ({ Encoding = Decoders.name distinct[index].Encoding; Preview = previewOf texts[index] }: EncodingCandidate))
                        Candidates candidates

    let hdf5OffsetsBeyondPrefix sourceLength =
        let offsets = ResizeArray<int64>()
        let mutable offset = 512L
        while offset <= 65536L do offset <- offset * 2L
        while offset < sourceLength do
            offsets.Add offset
            if offset > Int64.MaxValue / 2L then offset <- sourceLength else offset <- offset * 2L
        offsets.ToArray()
