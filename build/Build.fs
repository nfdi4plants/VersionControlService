open System
open System.IO

let private usage =
    [
        "Usage: build <target>"
        ""
        "  test run                                    Compile the suite with Fable and run it in Vitest"
        "  test watch                                  The same suite, recompiled and rerun on every change"
        "  test focused <test-file> <filter>           One compiled test file, selecting tests by name"
        "  test consumer                               The Fable consumer suite over the abstractions"
        "  test lakefs                                 The live lakeFS matrix against a pinned container"
        "  pack --version=<v> --output=<dir>           Pack the five coordinated packages into a local feed"
        "  verify packages --feed=<dir> --version=<v>  Check a local feed against the expected package graph"
        "  package-consumer [--version=<v>] [--temp=<dir>]"
        "                                              Pack, verify, compile and bundle the one-reference consumer"
    ]
    |> String.concat Environment.NewLine

/// Targets report what went wrong on one line; a stack trace would say nothing more.
let private target (action: unit -> unit) =
    try
        action ()
        0
    with ex ->
        printRedfn "%s" ex.Message
        1

let private asExitCode (result: Async<Result<unit, exn>>) =
    match result |> Async.RunSynchronously with
    | Ok() -> 0
    | Error _ -> 1

[<EntryPoint>]
let main args =
    // Only the target tokens are lowercased. Versions, paths and test filters are passed
    // through as they were typed.
    let raw = args |> Array.toList
    let argv = raw |> List.map _.ToLower()

    match argv with
    | "test" :: "run" :: _ -> Test.Run.all () |> asExitCode
    | "test" :: "consumer" :: _ -> Test.Run.fableConsumer () |> asExitCode
    | "test" :: "watch" :: _ ->
        Test.Watch()
        0
    | "test" :: "focused" :: _ ->
        match raw with
        | _ :: _ :: testFile :: filter :: _ -> target (fun () -> Test.Focused testFile filter)
        | _ ->
            printRedfn "Usage: build test focused <test-file> <filter>"
            1
    | "test" :: "lakefs" :: _ -> target LakeFs.Live
    | "pack" :: _ ->
        target (fun () ->
            let version = raw |> flagValue "--version"
            let output = raw |> flagValue "--output"

            Pack.Local version output |> ignore
        )
    | "verify" :: "packages" :: _ ->
        target (fun () ->
            let feed = raw |> flagValue "--feed"
            let version = raw |> flagValue "--version"

            Verify.PackageGraph feed version
        )
    | "package-consumer" :: _ ->
        target (fun () ->
            // A unique version per run keeps the local feed apart from anything cached.
            let version =
                raw
                |> tryFlagValue "--version"
                |> Option.defaultWith (fun () ->
                    let stamp = DateTime.UtcNow.ToString "yyyyMMddHHmmss"
                    $"0.0.0-local.{stamp}"
                )

            let workingRoot = raw |> tryFlagValue "--temp" |> Option.defaultWith Path.GetTempPath
            let feed = Path.Combine(workingRoot, "version-control-service-feed")
            let cache = Path.Combine(workingRoot, "version-control-service-cache")
            let output = Path.Combine(workingRoot, "version-control-service-consumer-output")

            Pack.Local version feed |> ignore
            Verify.PackageGraph feed version
            Consumer.Compile version feed cache output
            Consumer.BundleWithOldestFable version feed cache output
        )
    | [] ->
        Console.WriteLine usage
        0
    | _ ->
        printRedfn "No valid target provided."
        Console.WriteLine usage
        1
