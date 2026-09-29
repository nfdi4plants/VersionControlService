namespace VersionControlService.TextDiff

open System
open System.Collections.Generic
open System.Text
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
    type private WindowCount = { mutable Scalars: int; mutable Controls: int }
    type private Evaluation = {
        Encoding: TextEncoding
        Text: string
        Preview: string
        Error: string option
        ControlEvidence: string option
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

    let private evaluate sourceLength samples encoding bomLength =
        let ordered = samples |> Array.sortBy (fun s -> s.BufferOffset + int64 (properStart s))
        let fullText = StringBuilder()
        let firstPreview = StringBuilder()
        let windows = Dictionary<int64, WindowCount>()
        let countedScalarStarts = HashSet<int64>()
        let mutable nulEvidence = None
        let mutable decodeFailure = None
        for sampleIndex = 0 to ordered.Length - 1 do
            let sample = ordered[sampleIndex]
            let mutable start, stop = adjustedBounds sourceLength encoding sample
            if sample.BufferOffset + int64 start = 0L && bomLength > 0 then start <- min stop (start + bomLength)
            if stop > start && decodeFailure.IsNone then
                let text = StringBuilder()
                let mutable pendingHigh = 0
                let mutable pendingStart = 0L
                let sink startOffset _endOffset codeUnit =
                    text.Append(char codeUnit) |> ignore
                    fullText.Append(char codeUnit) |> ignore
                    if sampleIndex = 0 && firstPreview.Length < 2049 then firstPreview.Append(char codeUnit) |> ignore
                    let scalar, scalarStart =
                        if pendingHigh <> 0 && codeUnit >= 0xDC00 && codeUnit <= 0xDFFF then
                            let value = 0x10000 + ((pendingHigh - 0xD800) <<< 10) + codeUnit - 0xDC00
                            pendingHigh <- 0
                            value, pendingStart
                        elif codeUnit >= 0xD800 && codeUnit <= 0xDBFF then
                            pendingHigh <- codeUnit
                            pendingStart <- startOffset
                            -1, startOffset
                        else codeUnit, startOffset
                    if scalar >= 0 && countedScalarStarts.Add scalarStart then
                        let key = windowStart sourceLength scalarStart
                        let count =
                            match windows.TryGetValue key with
                            | true, found -> found
                            | _ ->
                                let fresh = { Scalars = 0; Controls = 0 }
                                windows.Add(key, fresh)
                                fresh
                        count.Scalars <- count.Scalars + 1
                        if scalar = 0 && nulEvidence.IsNone then nulEvidence <- Some(sprintf "NUL character at byte %d" scalarStart)
                        elif (scalar >= 1 && scalar <= 8) || scalar = 11 || (scalar >= 14 && scalar <= 31) || scalar = 127 then count.Controls <- count.Controls + 1
                let state = Decoders.createAt encoding (sample.BufferOffset + int64 start)
                let updated, decoded = Decoders.decode state sample.Bytes start (stop - start) sink
                match decoded with
                | Error error -> decodeFailure <- Some(sprintf "invalid %s sequence at byte %d" (Decoders.name encoding) error.Offset)
                | Ok _ ->
                    let endsAtEof = sample.BufferOffset + int64 stop = sourceLength
                    if endsAtEof then
                        match Decoders.flush updated with
                        | Error error -> decodeFailure <- Some(sprintf "invalid %s sequence at byte %d" (Decoders.name encoding) error.Offset)
                        | Ok _ -> ()
        let control =
            windows
            |> Seq.tryPick (fun pair ->
                let count = pair.Value
                if count.Scalars > 0 && count.Controls * 100 > count.Scalars then Some(sprintf "control ratio above 1%% in window at byte %d" pair.Key) else None)
        let preview =
            let value = firstPreview.ToString()
            if value.Length <= 2048 then value
            elif value.Length > 2048 && int value[2047] >= 0xD800 && int value[2047] <= 0xDBFF then value.Substring(0, 2047)
            else value.Substring(0, 2048)
        { Encoding = encoding; Text = fullText.ToString(); Preview = preview; Error = decodeFailure; ControlEvidence = nulEvidence |> Option.orElse control }

    let private parityCandidate samples oddOffsets =
        let mutable zeros = 0
        let mutable parityZeros = 0
        let mutable total = 0
        for sample in samples do
            let first = properStart sample
            let finish = first + properLength sample
            for i = first to finish - 1 do
                total <- total + 1
                if sample.Bytes[i] = 0uy then
                    zeros <- zeros + 1
                    let absolute = sample.BufferOffset + int64 i
                    if ((absolute &&& 1L) = 1L) = oddOffsets then parityZeros <- parityZeros + 1
        total > 0 && zeros * 10 >= total && parityZeros * 10 >= zeros * 9

    let classifyWithChoice sourceLength samples encoding =
        match detectBom samples with
        | Some bom ->
            let evaluation = evaluate sourceLength samples bom.Encoding bom.Length
            match evaluation.Error, evaluation.ControlEvidence with
            | Some error, _ -> BinaryEvidence error
            | None, Some evidence -> BinaryEvidence evidence
            | _ -> Classified(bom.Encoding, true, bom.Length)
        | _ ->
            match binarySignature sourceLength samples with
            | Some evidence -> BinaryEvidence evidence
            | None ->
                let evaluation = evaluate sourceLength samples encoding 0
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
                let eligible = ResizeArray<TextEncoding>()
                if parityCandidate samples true then eligible.Add TextEncoding.Utf16LE
                if parityCandidate samples false then eligible.Add TextEncoding.Utf16BE
                let evaluations = ResizeArray<Evaluation>()
                for encoding in eligible do
                    let value = evaluate sourceLength samples encoding 0
                    if value.Error.IsNone && value.ControlEvidence.IsNone then evaluations.Add value
                let utf8 = evaluate sourceLength samples TextEncoding.Utf8 0
                if utf8.Error.IsNone && utf8.ControlEvidence.IsNone then evaluations.Add utf8
                let windows1252 = evaluate sourceLength samples TextEncoding.Windows1252 0
                if windows1252.Error.IsNone && windows1252.ControlEvidence.IsNone then evaluations.Add windows1252
                if evaluations.Count = 0 then
                    let utf8 = evaluate sourceLength samples TextEncoding.Utf8 0
                    let cp = evaluate sourceLength samples TextEncoding.Windows1252 0
                    match utf8.Error, utf8.ControlEvidence, cp.Error, cp.ControlEvidence with
                    | _, Some evidence, _, _ | _, _, _, Some evidence -> BinaryEvidence evidence
                    | Some error, _, _, _ -> BinaryEvidence error
                    | _, _, Some error, _ -> BinaryEvidence error
                    | _ -> BinaryEvidence "no supported text encoding"
                else
                    let distinct = ResizeArray<Evaluation>()
                    for evaluation in evaluations do
                        if not (distinct |> Seq.exists (fun existing -> String.Equals(existing.Text, evaluation.Text, StringComparison.Ordinal))) then distinct.Add evaluation
                    if distinct.Count = 1 then Classified(distinct[0].Encoding, false, 0)
                    else
                        let candidates =
                            distinct
                            |> Seq.map (fun evaluation -> { Encoding = Decoders.name evaluation.Encoding; Preview = evaluation.Preview })
                            |> Seq.toArray
                        Candidates candidates

    let hdf5OffsetsBeyondPrefix sourceLength =
        let offsets = ResizeArray<int64>()
        let mutable offset = 512L
        while offset <= 65536L do offset <- offset * 2L
        while offset < sourceLength do
            offsets.Add offset
            if offset > Int64.MaxValue / 2L then offset <- sourceLength else offset <- offset * 2L
        offsets.ToArray()
