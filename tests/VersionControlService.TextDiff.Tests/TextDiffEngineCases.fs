namespace VersionControlService.TextDiff.Tests

open System
open System.Text
open VersionControlService.Abstractions
open VersionControlService.TextDiff

module Check =
    let equal expected actual message =
        if not (Unchecked.equals expected actual) then
            failwith $"{message} Expected {expected}, got {actual}."

    let true' value message =
        if not value then failwith message

    let sequence (expected: 'T seq) (actual: 'T seq) message =
        let left = Seq.toArray expected
        let right = Seq.toArray actual
        if left.Length <> right.Length || not (Array.forall2 Unchecked.equals left right) then
            failwith message

module TextDiffEngineCases =
    let private bytes (values: int seq) = values |> Seq.map byte |> Seq.toArray

    let private ascii (value: string) = value |> Seq.map (fun character -> byte character) |> Seq.toArray

    let private createMeter () =
        let clock = ManualClock 0.0
        let limits = { Limits.defaults with RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
        Meter.create (clock :> IClock) limits

    let private scan (encoding: TextEncoding) (retainLimit: int option) (chunks: byte[][]) =
        let state = Scanner.create encoding 0L retainLimit
        let lines = ResizeArray<ScannedLine>()
        let evidence = ResizeArray<ScannerEvidence>()
        let meter = createMeter ()

        for index = 0 to chunks.Length - 1 do
            let chunk = chunks[index]
            let result = Scanner.scanChunk state chunk 0 chunk.Length (index = chunks.Length - 1) meter lines.Add evidence.Add
            Check.equal chunk.Length result.Consumed "The scanner consumed each input chunk."
            match result.Error with
            | Some error -> failwith $"The scanner rejected byte {error.Offset}: {error.Reason}"
            | None -> ()

        lines.ToArray(), evidence.ToArray(), state, meter

    let private hashText (text: string) =
        let hash = Hash.create ()
        let mutable index = 0

        while index < text.Length do
            let first = int text[index]

            if first >= 0xD800 && first <= 0xDBFF && index + 1 < text.Length then
                let second = int text[index + 1]
                let scalar = 0x10000 + ((first - 0xD800) <<< 10) + second - 0xDC00
                Hash.addCodeUnit hash scalar
                index <- index + 2
            else
                Hash.addCodeUnit hash first
                index <- index + 1

        Hash.toString hash

    let private sample (data: byte[]) = {
        BufferOffset = 0L
        Bytes = data
        SampleOffset = 0
        SampleLength = data.Length
    }

    let private classify (data: byte[]) = Classification.classify (int64 data.Length) [| sample data |]

    let private expectClassified (expected: ClassificationResult) (encoding: TextEncoding) (hasBom: bool) (bomLength: int) =
        match expected with
        | Classified(actual, actualBom, actualLength) ->
            Check.equal encoding actual "The classifier selected the encoding."
            Check.equal hasBom actualBom "The classifier reported the BOM state."
            Check.equal bomLength actualLength "The classifier reported the BOM length."
        | result -> failwith $"Expected a classified source, got {result}."

    let private expectBinary (expected: string) result =
        match result with
        | BinaryEvidence evidence ->
            if not (evidence.Contains expected) then failwith $"Expected evidence containing {expected}, got {evidence}."
        | other -> failwith $"Expected binary evidence, got {other}."

    let private decodeSplit (encoding: TextEncoding) (data: byte[]) (expectedUnits: int[]) =
        for split = 0 to data.Length do
            let units = ResizeArray<int>()
            let sink _ _ unit = units.Add unit
            let initial = Decoders.create encoding
            let afterFirst, firstResult = Decoders.decode initial data 0 split sink

            match firstResult with
            | Error error -> failwith $"The first split failed at byte {error.Offset}."
            | Ok consumed -> Check.equal split consumed "The decoder consumed the first part."

            let afterSecond, secondResult = Decoders.decode afterFirst data split (data.Length - split) sink

            match secondResult with
            | Error error -> failwith $"The second split failed at byte {error.Offset}."
            | Ok consumed -> Check.equal (data.Length - split) consumed "The decoder consumed the second part."

            match Decoders.flush afterSecond with
            | Error error -> failwith $"The decoder flush failed at byte {error.Offset}."
            | Ok _ -> ()

            Check.sequence expectedUnits (units.ToArray()) "Split decoding returned the same UTF-16 units."

    let private decoderError (encoding: TextEncoding) (data: byte[]) (flushAtEnd: bool) (expectedOffset: int64) =
        let state = Decoders.create encoding
        let _, decoded = Decoders.decode state data 0 data.Length (fun _ _ _ -> ())

        match decoded with
        | Error error -> Check.equal expectedOffset error.Offset "The decoder reported the invalid sequence start."
        | Ok _ when flushAtEnd ->
            match Decoders.flush (fst (Decoders.decode state data 0 data.Length (fun _ _ _ -> ()))) with
            | Error error -> Check.equal expectedOffset error.Offset "The decoder reported the incomplete sequence start."
            | Ok _ -> failwith "The decoder accepted an incomplete sequence."
        | Ok _ -> failwith "The decoder accepted invalid input."

    let cases: (string * (unit -> Async<unit>)) list = [
        "FNV-1a hashes canonical line text", fun () -> async {
            let vectors = [|
                "", "cbf29ce484222325"
                "a", "af63dc4c8601ec8c"
                "foobar", "85944171f73967e8"
                "hello", "a430d84680aabd0b"
                "Hello, world!", "38d1334144987bf4"
                "line", "bf4ba5ad694f5907"
                "line\r", "5ce38fa9f1d44bfe"
                "line\n", "5ce392a9f1d45117"
                "line\r\n", "aaf911c5ebbd0f9c"
                "é", "0ac21707b7181e01"
                "€", "5646581b8855166b"
                "😀", "feff073875020288"
                "café", "48e8823acfa40d89"
                "Swate", "a8857a08e41ed6ed"
                "diff", "c9fcc6675752105a"
                "hash-key", "3923cace9efc3313"
                "UTF-8", "e14a1ad0ff308d43"
                "Κόσμε", "4871b6675515ef7f"
                "𐍈", "7d764638bba86358"
                "\u0000", "af63bd4c8601b7df"
            |]

            for text, expected in vectors do
                let actual = hashText text
                Check.equal expected actual $"Hash vector for {text}."

            return ()
        }
        "strict decoders resume at every byte boundary", fun () -> async {
            let text = "A€😀Z"
            let units = text.ToCharArray() |> Array.map int
            decodeSplit TextEncoding.Utf8 (bytes [ 0x41; 0xE2; 0x82; 0xAC; 0xF0; 0x9F; 0x98; 0x80; 0x5A ]) units
            decodeSplit TextEncoding.Utf16LE (bytes [ 0x41; 0; 0xAC; 0x20; 0x3D; 0xD8; 0; 0xDE; 0x5A; 0 ]) units
            decodeSplit TextEncoding.Utf16BE (bytes [ 0; 0x41; 0x20; 0xAC; 0xD8; 0x3D; 0xDE; 0; 0; 0x5A ]) units
            decodeSplit TextEncoding.Utf32LE (bytes [ 0x41; 0; 0; 0; 0xAC; 0x20; 0; 0; 0; 0xF6; 1; 0; 0x5A; 0; 0; 0 ]) units
            decodeSplit TextEncoding.Utf32BE (bytes [ 0; 0; 0; 0x41; 0; 0; 0x20; 0xAC; 0; 1; 0xF6; 0; 0; 0; 0; 0x5A ]) units
            decodeSplit TextEncoding.Windows1252 (bytes [ 0x41; 0xE9; 0x5A ]) [| 0x41; 0xE9; 0x5A |]
            return ()
        }
        "UTF-8 rejects overlong values, surrogates, out-of-range values, and stray continuations", fun () -> async {
            decoderError TextEncoding.Utf8 (bytes [ 0xC0; 0xAF ]) false 0L
            decoderError TextEncoding.Utf8 (bytes [ 0xED; 0xA0; 0x80 ]) false 0L
            decoderError TextEncoding.Utf8 (bytes [ 0xF4; 0x90; 0x80; 0x80 ]) false 0L
            decoderError TextEncoding.Utf8 (bytes [ 0x80 ]) false 0L
            return ()
        }
        "UTF-8 flush rejects a truncated sequence", fun () -> async {
            decoderError TextEncoding.Utf8 (bytes [ 0xE2; 0x82 ]) true 0L
            return ()
        }
        "UTF-16 rejects unpaired surrogates and odd final bytes", fun () -> async {
            decoderError TextEncoding.Utf16LE (bytes [ 0x00; 0xD8 ]) true 0L
            decoderError TextEncoding.Utf16BE (bytes [ 0xDC; 0x00 ]) false 0L
            decoderError TextEncoding.Utf16LE (bytes [ 0x41 ]) true 0L
            return ()
        }
        "UTF-32 rejects invalid scalars and partial units", fun () -> async {
            decoderError TextEncoding.Utf32LE (bytes [ 0; 0xD8; 0; 0 ]) false 0L
            decoderError TextEncoding.Utf32BE (bytes [ 0; 0x11; 0; 0 ]) false 0L
            decoderError TextEncoding.Utf32LE (bytes [ 0x41; 0; 0 ]) true 0L
            return ()
        }
        "Windows-1252 rejects all undefined bytes", fun () -> async {
            for value in [ 0x81; 0x8D; 0x8F; 0x90; 0x9D ] do
                decoderError TextEncoding.Windows1252 (bytes [ value ]) false 0L
            return ()
        }
        "decoder failures retain the absolute invalid sequence offset", fun () -> async {
            decoderError TextEncoding.Utf8 (bytes [ 0x41; 0xE2; 0x28 ]) false 1L
            decoderError TextEncoding.Utf16LE (bytes [ 0x41; 0; 0; 0xDC ]) false 2L
            decoderError TextEncoding.Utf32BE (bytes [ 0; 0; 0xD8; 0 ]) false 0L
            return ()
        }
        "memory sources distinguish growing data from proven EOF", fun () -> async {
            let source = MemoryByteSource(bytes [ 1; 2; 3 ], growing = true) :> IByteSource
            let buffer = Array.zeroCreate<byte> 3
            Check.equal (Some 3L) source.KnownLength "A growing source retains its expected length."
            Check.equal 0L (source.AvailableLength()) "A new growing source exposes no bytes."
            Check.true' (not (source.IsComplete())) "A growing source has not reached EOF."
            let! waiting = source.ReadAt 0L buffer 0 3
            Check.equal ReadOutcome.NotYetAvailable waiting "An empty growing source waits for bytes."
            (source :?> MemoryByteSource).AdvanceTo 2L
            let! available = source.ReadAt 0L buffer 0 3
            Check.equal (ReadOutcome.Bytes 2) available "The readable prefix is returned."
            Check.equal [| 1uy; 2uy |] buffer[0..1] "The prefix bytes are copied."
            (source :?> MemoryByteSource).AdvanceTo 3L
            Check.true' (source.IsComplete()) "The complete expected length proves EOF."
            let! eof = source.ReadAt 3L buffer 0 1
            Check.equal ReadOutcome.EndOfSource eof "EndOfSource follows producer completion."
            return ()
        }
        "memory temporary storage supports append, positional writes, reads, and disposal", fun () -> async {
            let store = MemoryTempStore() :> ITempStore
            let first = bytes [ 1; 2; 3 ]
            let second = bytes [ 8; 9 ]
            let! start = store.Append first 0 first.Length
            Check.equal 0L start "The first append starts at zero."
            do! store.WriteAt 1L second 0 second.Length
            Check.equal 3L (store.Length()) "Positional writes extend the store length."
            let output = Array.zeroCreate<byte> 3
            let! read = store.ReadAt 0L output 0 output.Length
            Check.equal 3 read "The store reads the requested available range."
            Check.equal (bytes [ 1; 8; 9 ]) output "The positional write replaced the selected bytes."
            do! store.Dispose()
            Check.equal 0L (store.Length()) "Disposal releases in-memory storage."
            return ()
        }
        "the work meter carries byte remainder and enforces time quanta", fun () -> async {
            let clock = ManualClock 5.0
            let meter = Meter.create (clock :> IClock) { MaxUnits = 2; RequestMs = 400.0; QuantumMs = 10.0 }
            Meter.chargeBytes meter 2_048
            Check.equal 0 meter.Units "Half a byte unit stays in the remainder."
            Meter.chargeBytes meter 2_048
            Check.equal 1 meter.Units "Small byte charges add up to one unit."
            clock.Advance 10.0
            Check.true' (Meter.quantumDue meter) "The quantum expires at its end time."
            clock.Advance 390.0
            Check.true' (Meter.overBudget meter) "The request deadline ends the work budget."
            return ()
        }
        "line endings cover LF, CRLF, CR, and a final unterminated line", fun () -> async {
            let lines, _, _, _ = scan TextEncoding.Utf8 (Some 32) [| ascii "a\nb\r\nc\rd" |]
            Check.sequence [| LineEnding.LF; LineEnding.CRLF; LineEnding.CR; LineEnding.NoEnding |] (lines |> Array.map _.Ending) "The scanner preserved each line ending."
            Check.sequence [| "a"; "b"; "c"; "d" |] (lines |> Array.choose _.Text) "The scanner excluded endings from line text."
            Check.equal 4 lines.Length "The scanner emitted four lines."
            return ()
        }
        "CRLF is recognized when its bytes arrive in separate chunks", fun () -> async {
            let lines, _, _, _ = scan TextEncoding.Utf8 (Some 8) [| ascii "first\r"; ascii "\nsecond" |]
            Check.equal 2 lines.Length "The split CRLF produced one boundary."
            Check.equal LineEnding.CRLF lines[0].Ending "The split terminator is CRLF."
            Check.equal "first" lines[0].Text.Value "The first line text is intact."
            Check.equal LineEnding.NoEnding lines[1].Ending "The second line has no terminator."
            return ()
        }
        "empty input produces no line", fun () -> async {
            let lines, _, _, _ = scan TextEncoding.Utf8 (Some 8) [| Array.empty |]
            Check.equal 0 lines.Length "The empty source has no lines."
            return ()
        }
        "equal text hashes match across encodings", fun () -> async {
            let utf8 = bytes [ 0x61; 0xE2; 0x82; 0xAC; 0xF0; 0x9F; 0x98; 0x80 ]
            let utf16 = bytes [ 0x61; 0; 0xAC; 0x20; 0x3D; 0xD8; 0; 0xDE ]
            let utf32 = bytes [ 0x61; 0; 0; 0; 0xAC; 0x20; 0; 0; 0; 0xF6; 1; 0 ]
            let one encoding data = scan encoding None [| data |] |> fun (lines, _, _, _) -> lines[0].KeyHash |> Hash.toString
            Check.equal (one TextEncoding.Utf8 utf8) (one TextEncoding.Utf16LE utf16) "UTF-16 text uses the same canonical key."
            Check.equal (one TextEncoding.Utf8 utf8) (one TextEncoding.Utf32LE utf32) "UTF-32 text uses the same canonical key."
            return ()
        }
        "control evidence uses a strict one percent threshold", fun () -> async {
            let above = Array.create 100 0x61uy
            above[0] <- 0x01uy
            above[99] <- 0x1Fuy
            let below = Array.create 200 0x61uy
            below[0] <- 0x01uy
            let _, aboveEvidence, _, _ = scan TextEncoding.Utf8 None [| above |]
            let _, belowEvidence, _, _ = scan TextEncoding.Utf8 None [| below |]
            Check.true' (aboveEvidence |> Array.exists (fun item -> item.Kind = "control ratio")) "The control ratio above one percent is evidence."
            Check.true' (belowEvidence |> Array.forall (fun item -> item.Kind <> "control ratio")) "The control ratio below one percent is allowed."
            return ()
        }
        "a short final observation window merges into the preceding window", fun () -> async {
            let first = Array.create 65_536 0x61uy
            for index = 0 to 659 do first[index] <- 0x01uy
            let tail = Array.create 3_000 0x61uy
            let _, evidence, _, _ = scan TextEncoding.Utf8 None [| first; tail |]
            Check.true' (evidence |> Array.forall (fun item -> item.Kind <> "control ratio")) "The merged ratio stays below one percent."
            return ()
        }
        "decoded NUL is evidence and UTF-16 zero bytes are ordinary text bytes", fun () -> async {
            let _, nulEvidence, _, _ = scan TextEncoding.Utf16LE None [| bytes [ 0; 0 ] |]
            let _, utf16Evidence, _, _ = scan TextEncoding.Utf16LE None [| bytes [ 0x41; 0; 0x0A; 0 ] |]
            Check.true' (nulEvidence |> Array.exists (fun item -> item.Kind = "nul" && item.Offset = 0L)) "Decoded U+0000 is evidence."
            Check.true' (utf16Evidence |> Array.isEmpty) "UTF-16 encoding zero bytes do not create evidence."
            return ()
        }
        "a copied scanner state resumes inside a long line", fun () -> async {
            let prefix = Array.create 8_192 0x61uy
            let suffix = Array.append (Array.create 8_192 0x62uy) (ascii "\n")
            let state = Scanner.create TextEncoding.Utf8 0L (Some 4)
            let meter = createMeter ()
            let ignoredLine _ = ()
            let ignoredEvidence _ = ()
            let first = Scanner.scanChunk state prefix 0 prefix.Length false meter ignoredLine ignoredEvidence
            Check.equal prefix.Length first.Consumed "The prefix was scanned."
            let resumed = Scanner.copyState state
            let originalLines = ResizeArray<ScannedLine>()
            let resumedLines = ResizeArray<ScannedLine>()
            let originalEnd = Scanner.scanChunk state suffix 0 suffix.Length true meter originalLines.Add ignoredEvidence
            let resumedEnd = Scanner.scanChunk resumed suffix 0 suffix.Length true meter resumedLines.Add ignoredEvidence
            Check.equal EndOfInput originalEnd.Status "The original state reached EOF."
            Check.equal EndOfInput resumedEnd.Status "The copied state reached EOF."
            Check.equal (Hash.toString originalLines[0].KeyHash) (Hash.toString resumedLines[0].KeyHash) "The copied scanner preserved its partial hash."
            Check.equal originalLines[0].Utf16Length resumedLines[0].Utf16Length "The copied scanner preserved its line length."
            Check.equal (Some "aaaa") originalLines[0].Text "The retained prefix is bounded."
            return ()
        }
        "the scanner suspends at its work budget with line state intact", fun () -> async {
            let state = Scanner.create TextEncoding.Utf8 0L (Some 8)
            let clock = ManualClock 0.0
            let limited = Meter.create (clock :> IClock) { MaxUnits = 1; RequestMs = 1000.0; QuantumMs = 1000.0 }
            let input = Array.append (Array.create 8_192 0x61uy) (ascii "\n")
            let lines = ResizeArray<ScannedLine>()
            let first = Scanner.scanChunk state input 0 input.Length true limited lines.Add (fun _ -> ())
            Check.equal 4096 first.Consumed "The scanner stops after charging one unit."
            Check.equal BudgetReached first.Status "The scanner reports budget exhaustion."
            let resumedMeter = createMeter ()
            let second = Scanner.scanChunk state input first.Consumed (input.Length - first.Consumed) true resumedMeter lines.Add (fun _ -> ())
            Check.equal EndOfInput second.Status "The scanner resumes through EOF."
            Check.equal 1 lines.Count "The resumed scan emits its line once."
            Check.equal 8192L lines[0].Utf16Length "The resumed line length includes both chunks."
            return ()
        }
        "all supported BOMs select their encodings", fun () -> async {
            let examples = [|
                bytes [ 0xEF; 0xBB; 0xBF; 0x41 ], TextEncoding.Utf8, 3
                bytes [ 0xFF; 0xFE; 0x41; 0 ], TextEncoding.Utf16LE, 2
                bytes [ 0xFE; 0xFF; 0; 0x41 ], TextEncoding.Utf16BE, 2
                bytes [ 0xFF; 0xFE; 0; 0; 0x41; 0; 0; 0 ], TextEncoding.Utf32LE, 4
                bytes [ 0; 0; 0xFE; 0xFF; 0; 0; 0; 0x41 ], TextEncoding.Utf32BE, 4
            |]
            for data, encoding, length in examples do
                expectClassified (classify data) encoding true length
            expectBinary "byte 2" (classify (bytes [ 0xFF; 0xFE; 0; 0xD8 ]))
            return ()
        }
        "content signatures identify binary formats", fun () -> async {
            let bmp = Array.create 26 0uy
            bmp[0] <- 0x42uy
            bmp[1] <- 0x4Duy
            bmp[2] <- 26uy
            let examples = [|
                "ZIP", bytes [ 0x50; 0x4B; 3; 4; 20; 0 ]
                "PDF", ascii "%PDF-1.7"
                "PNG", bytes [ 0x89; 0x50; 0x4E; 0x47; 13; 10; 26; 10 ]
                "JPEG", bytes [ 0xFF; 0xD8; 0xFF ]
                "GIF", ascii "GIF89a"
                "TIFF", bytes [ 0x49; 0x49; 42; 0 ]
                "BMP", bmp
                "gzip", bytes [ 0x1F; 0x8B; 8; 0 ]
                "bzip2", ascii "BZh9"
                "xz", bytes [ 0xFD; 0x37; 0x7A; 0x58; 0x5A; 0 ]
                "zstd", bytes [ 0x28; 0xB5; 0x2F; 0xFD ]
                "7z", bytes [ 0x37; 0x7A; 0xBC; 0xAF; 0x27; 0x1C ]
            |]
            for formatName, data in examples do expectBinary formatName (classify data)
            let hdf = Array.create 520 0uy
            Array.blit (bytes [ 0x89; 0x48; 0x44; 0x46; 13; 10; 26; 10 ]) 0 hdf 512 8
            expectBinary "HDF5" (classify hdf)
            let beyond = Classification.hdf5OffsetsBeyondPrefix (2L * 1024L * 1024L)
            Check.sequence [| 131_072L; 262_144L; 524_288L; 1_048_576L |] beyond "Later HDF5 offsets grow by powers of two."
            return ()
        }
        "PE, ELF, and Mach-O checks use their structural fields", fun () -> async {
            let pe = Array.create 68 0uy
            pe[0] <- 0x4Duy
            pe[1] <- 0x5Auy
            pe[60] <- 64uy
            Array.blit (ascii "PE\000\000") 0 pe 64 4
            expectBinary "PE" (classify pe)
            expectBinary "ELF" (classify (bytes [ 0x7F; 0x45; 0x4C; 0x46; 2 ]))
            let macho = Array.create 28 0uy
            Array.blit (bytes [ 0xFE; 0xED; 0xFA; 0xCF ]) 0 macho 0 4
            macho[19] <- 1uy
            expectBinary "Mach-O" (classify macho)
            let fat = bytes [ 0xCA; 0xFE; 0xBA; 0xBE; 0; 0; 0; 2 ]
            expectBinary "Mach-O" (classify fat)
            return ()
        }
        "UTF-16 parity and the ASCII shortcut select stable encodings", fun () -> async {
            expectClassified (classify (bytes [ 0x41; 0; 0x42; 0 ])) TextEncoding.Utf16LE false 0
            expectClassified (classify (bytes [ 0; 0x41; 0; 0x42 ])) TextEncoding.Utf16BE false 0
            expectClassified (classify (ascii "plain ascii")) TextEncoding.Utf8 false 0
            return ()
        }
        "valid UTF-8 and Windows-1252 can require a choice", fun () -> async {
            match classify (bytes [ 0xC3; 0xA9 ]) with
            | Candidates values ->
                Check.sequence [| "utf-8"; "windows-1252" |] (values |> Array.map _.Encoding) "Both materially different candidates are returned."
                Check.true' (values |> Array.forall (fun item -> item.Preview.Length <= 2048)) "Candidate previews stay bounded."
            | result -> failwith $"Expected encoding candidates, got {result}."
            return ()
        }
        "artificial sample cuts are skipped and a real EOF cut is an error", fun () -> async {
            let artificial = {
                BufferOffset = 0L
                Bytes = bytes [ 0xE2; 0x82; 0xAC; 0x41 ]
                SampleOffset = 1
                SampleLength = 2
            }
            expectClassified (Classification.classifyWithChoice 4L [| artificial |] TextEncoding.Utf8) TextEncoding.Utf8 false 0
            expectBinary "byte 0" (Classification.classifyWithChoice 2L [| sample (bytes [ 0xE2; 0x82 ]) |] TextEncoding.Utf8)
            return ()
        }
        "a chosen encoding is still checked for binary evidence", fun () -> async {
            expectBinary "NUL" (Classification.classifyWithChoice 2L [| sample (bytes [ 0x41; 0 ]) |] TextEncoding.Utf8)
            return ()
        }
        "allocation reservations enforce caps and release leases", fun () -> async {
            let ledger = Ledger()
            Check.true' (ledger.TryReserve(AllocationCategory.PreviousWindows, 32L * 1024L * 1024L)) "A full previous-side window budget fits."
            Check.true' (not (ledger.TryReserve(AllocationCategory.PreviousWindows, 1L))) "Previous-side windows stop at their cap."
            Check.true' (ledger.TryReserve(AllocationCategory.CurrentWindows, 32L * 1024L * 1024L)) "Current-side windows have a separate cap."
            Check.true' (ledger.TryReserve(AllocationCategory.SampledIndexes, 54L * 1024L * 1024L)) "Indexes and bucket heads share their cap."
            Check.true' (not (ledger.TryReserve(AllocationCategory.SampledIndexes, 1L))) "The sampled index cap is enforced."
            use lease = ledger.TryLease(AllocationCategory.ResponseData, 8L * 1024L * 1024L) |> Option.defaultWith (fun () -> failwith "The response lease fits.")
            Check.equal (8L * 1024L * 1024L) (ledger.Used AllocationCategory.ResponseData) "The lease reserves response memory."
            lease.Dispose()
            Check.equal 0L (ledger.Used AllocationCategory.ResponseData) "Disposal releases the response lease."
            let categories = [|
                AllocationCategory.ChunkScratch, 16L
                AllocationCategory.AlignmentScratch, 8L
                AllocationCategory.ResidentCheckpoints, 1L
                AllocationCategory.RetainedBlobs, 2L
            |]
            for category, capMb in categories do
                let cap = capMb * 1024L * 1024L
                Check.true' (ledger.TryReserve(category, cap)) $"The {category} cap fits."
                Check.true' (not (ledger.TryReserve(category, 1L))) $"The {category} cap rejects extra bytes."
                ledger.Release(category, cap)

            let pool = new ChunkBufferPool(ledger)
            let chunk = pool.Rent() |> Option.defaultWith (fun () -> failwith "The first chunk lease fits.")
            Check.equal (8L * 1024L * 1024L) (ledger.Used AllocationCategory.ChunkScratch) "An active chunk holds eight MiB."
            (chunk :> IDisposable).Dispose()
            Check.equal (8L * 1024L * 1024L) (ledger.Used AllocationCategory.ChunkScratch) "A cached chunk stays reserved."
            pool.Dispose()
            Check.equal 0L (ledger.Used AllocationCategory.ChunkScratch) "Pool disposal releases cached chunk memory."
            return ()
        }
    ]
