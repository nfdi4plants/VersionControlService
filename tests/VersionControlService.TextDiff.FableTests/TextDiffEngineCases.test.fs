namespace VersionControlService.TextDiff.FableTests

open Fable.Core
open VersionControlService.TextDiff
open VersionControlService.TextDiff.Tests

module TextDiffEngineCasesTests =
    let private benchmarkSize = 64 * 1024 * 1024

    let private createMeter () =
        let clock = ManualClock 0.0
        Meter.create
            (clock :> IClock)
            { Limits.defaults with MaxUnits = 2_147_483_647; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }

    /// Runs `work` three times and reports the best rate of the last two runs in MB/s, so that JIT
    /// warm-up does not count against the measured code.
    let private bestRate (size: int) (work: unit -> 'T) =
        let mutable best = 0.0
        let mutable result = work ()
        for _ in 1..2 do
            let started = BrowserClock.nowMs()
            result <- work ()
            let elapsed = max 1.0 (BrowserClock.nowMs() - started)
            best <- max best (float size / 1_000_000.0 * 1_000.0 / elapsed)
        result, best

    let private scanThroughput encoding retainLimit (data: byte[]) =
        let batch = LineBatch()
        bestRate data.Length (fun () ->
            let state = Scanner.create encoding 0L retainLimit
            Scanner.scanChunk state data 0 data.Length true (createMeter ()) batch ignore ignore)

    let private commonRunThroughput encoding (left: byte[]) (right: byte[]) =
        bestRate left.Length (fun () ->
            let mutable position = 0
            let mutable pendingCR = false
            let mutable lines = 0

            while position < left.Length do
                let run = CommonRun.find encoding (int64 position) pendingCR left position right position (left.Length - position)
                if run.Length = 0 then failwith "The common run did not advance."
                position <- position + run.Length
                pendingCR <- run.PendingCR
                lines <- lines + run.Lines

            lines)

    let private asciiLines size =
        let data = Array.create size 0x61uy
        let mutable lineEnd = 39

        while lineEnd < size do
            data[lineEnd] <- 0x0Auy
            lineEnd <- lineEnd + 40

        data

    let private repeatWithAsciiTail size (pattern: byte[]) =
        let data = Array.zeroCreate<byte> size
        let mutable offset = 0

        while offset + pattern.Length <= size do
            Array.blit pattern 0 data offset pattern.Length
            offset <- offset + pattern.Length

        while offset < size do
            data[offset] <- 0x61uy
            offset <- offset + 1

        data

    let private repeatBytes size (pattern: byte[]) =
        let data = Array.zeroCreate<byte> size
        let mutable offset = 0

        while offset < size do
            let copied = min pattern.Length (size - offset)
            Array.blit pattern 0 data offset copied
            offset <- offset + copied

        data

    let private utf8Data () =
        let cycle =
            Array.concat [
                Array.create 14 0x61uy
                [| 0xC3uy; 0xA9uy; 0xC3uy; 0xA9uy; 0xC3uy; 0xA9uy |]
                [| 0xE2uy; 0x82uy; 0xACuy; 0xE2uy; 0x82uy; 0xACuy; 0xE2uy; 0x82uy; 0xACuy |]
            ]
        let pattern = Array.zeroCreate<byte> (cycle.Length * 16)

        for repeat = 0 to 15 do
            Array.blit cycle 0 pattern (repeat * cycle.Length) cycle.Length

        repeatWithAsciiTail benchmarkSize pattern

    let private utf16LeData () =
        let units =
            Array.concat [
                Array.create 14 0x0061
                [| 0x00E9; 0x00E9; 0x00E9 |]
                [| 0x20AC; 0x20AC; 0x20AC |]
            ]
        let pattern = Array.zeroCreate<byte> (units.Length * 2)

        for index = 0 to units.Length - 1 do
            let value = units[index]
            pattern[index * 2] <- byte value
            pattern[index * 2 + 1] <- byte (value >>> 8)

        repeatBytes benchmarkSize pattern

    let private reportScan label encoding retainLimit (data: byte[]) =
        let result, rate = scanThroughput encoding retainLimit data
        Vitest.log ($"{label}: {rate:F1} MB/s")
        if result.Status <> EndOfInput then failwith $"{label} did not finish the 64 MiB input."
        Async.StartAsPromise(async.Return())

    let private reportCommonRun label (data: byte[]) =
        let _, rate = commonRunThroughput TextEncoding.Utf8 data (Array.copy data)
        Vitest.log ($"{label}: {rate:F1} MB/s")
        Async.StartAsPromise(async.Return())

    Vitest.describe (
        "Text diff engine shared cases",
        fun () ->
            for name, run in TextDiffEngineCases.cases do
                Vitest.it(name, fun () -> Async.StartAsPromise(run ()))
    )

    Vitest.it (
        "scans a 64 MiB ASCII buffer with 40-byte lines",
        fun () -> reportScan "ASCII scanner throughput" TextEncoding.Utf8 None (asciiLines benchmarkSize)
    )

    Vitest.it (
        "scans a 64 MiB UTF-8 buffer with mixed character widths",
        fun () -> reportScan "UTF-8 scanner throughput" TextEncoding.Utf8 None (utf8Data ())
    )

    Vitest.it (
        "scans a 64 MiB UTF-16 LE buffer",
        fun () -> reportScan "UTF-16 LE scanner throughput" TextEncoding.Utf16LE None (utf16LeData ())
    )

    Vitest.it (
        "scans a 64 MiB ASCII buffer with 40-byte lines and retained text",
        fun () -> reportScan "ASCII scanner throughput with retained text" TextEncoding.Utf8 (Some 4096) (asciiLines benchmarkSize)
    )

    Vitest.it (
        "finds the common run of two identical 64 MiB ASCII buffers",
        fun () -> reportCommonRun "ASCII common run throughput" (asciiLines benchmarkSize)
    )

    Vitest.it (
        "finds the common run of two identical 64 MiB mixed UTF-8 buffers",
        fun () -> reportCommonRun "UTF-8 common run throughput" (utf8Data ())
    )
