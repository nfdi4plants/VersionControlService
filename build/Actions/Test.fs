[<RequireQualifiedAccessAttribute>]
module Test

open ProjectInfo
open System.IO

let private cleanGeneratedOutput (testProjectPath: string) =
    let outputPath = Path.Combine(testProjectPath, "output")

    if Directory.Exists outputPath then
        Directory.Delete(outputPath, true)

/// Fable starts the Vitest runner itself, so "npx" is spelled plainly in these arguments.
let private suiteArgs = [
    "fable"
    "-o"
    "output"
    "-s"
    "--run"
    "npx"
    "vitest"
    "run"
]

let Watch () =
    cleanGeneratedOutput ProjectPaths.testsPath

    [
        runAsync
            "tests"
            "dotnet"
            [
                "fable"
                "watch"
                "-o"
                "output"
                "-s"
                "--run"
                "npx"
                "vitest"
            ]
            ProjectPaths.testsPath
    ]
    |> runParallel

/// Compiles the suite and runs a single test file, selecting tests by name. The output is
/// kept between runs because a focused run is meant to be repeated quickly.
let Focused (testFile: string) (filter: string) =
    run "dotnet" [ "fable"; "-o"; "output"; "-s" ] ProjectPaths.testsPath

    let outputFile = Path.Combine("output", testFile)

    if not (File.Exists(Path.Combine(ProjectPaths.testsPath, outputFile))) then
        failwithf "Focused test output does not exist: %s" outputFile

    run
        npx
        [
            "vitest"
            "run"
            outputFile
            "-t"
            filter
            "--reporter=verbose"
        ]
        ProjectPaths.testsPath

module Run =

    let all () =
        cleanGeneratedOutput ProjectPaths.testsPath
        runAsync "tests" "dotnet" suiteArgs ProjectPaths.testsPath

    /// The same suite with its output captured, so the live lakeFS matrix can also fail on
    /// a profile that reports a skip.
    let allCaptured () =
        cleanGeneratedOutput ProjectPaths.testsPath
        runCaptured "tests" "dotnet" suiteArgs ProjectPaths.testsPath

    /// The consumer suite reaches the abstractions through Fable only, which keeps the
    /// portable surface honest.
    let fableConsumer () =
        cleanGeneratedOutput ProjectPaths.fableConsumerTestsPath
        runAsync "consumer" "dotnet" suiteArgs ProjectPaths.fableConsumerTestsPath
