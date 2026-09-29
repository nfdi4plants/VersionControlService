namespace VersionControlService.TextDiff.FableTests

open Fable.Core
open VersionControlService.TextDiff
open VersionControlService.TextDiff.Tests

module TextDiffEngineCasesTests =
    Vitest.describe (
        "Text diff engine shared cases",
        fun () ->
            for name, run in TextDiffEngineCases.cases do
                Vitest.it(name, fun () -> Async.StartAsPromise(run ()))
    )

    Vitest.it (
        "scans a 64 MiB ASCII buffer",
        fun () ->
            let size = 64 * 1024 * 1024
            let data = Array.create size 0x61uy
            let state = Scanner.create TextEncoding.Utf8 0L None
            let clock = ManualClock 0.0
            let meter =
                Meter.create
                    (clock :> IClock)
                    { Limits.defaults with RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let started = BrowserClock.nowMs()
            let result = Scanner.scanChunk state data 0 data.Length true meter (fun _ -> ()) (fun _ -> ())
            let elapsed = max 1.0 (BrowserClock.nowMs() - started)
            let rate = float size / 1_000_000.0 * 1_000.0 / elapsed
            Vitest.log ($"Scanner throughput: {rate:F1} MB/s")
            if result.Status <> EndOfInput then failwith "The scanner did not finish the 64 MiB input."
            Async.StartAsPromise (async.Return ())
    )
