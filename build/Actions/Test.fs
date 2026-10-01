[<RequireQualifiedAccessAttribute>]
module Test

open ProjectInfo
open System.IO

let private cleanGeneratedOutput (testProjectPath: string) =
    let outputPath = Path.Combine(testProjectPath, "output")

    if Directory.Exists outputPath then
        Directory.Delete(outputPath, true)

let TextDiff () =
    let projectPath = Path.GetFullPath "tests/VersionControlService.TextDiff.FableTests"
    cleanGeneratedOutput projectPath
    run "dotnet" [ "fable"; "-o"; "output"; "-s" ] projectPath
    run npx [ "vitest"; "run"; "output/TextDiffEngineCases.test.js"; "--configLoader"; "native" ] projectPath

/// The compiler version Swate builds the library with. `dotnet tool exec` downloads it on first use
/// and leaves the pinned tool of this repository alone.
let private swateFableVersion = "5.0.0-alpha.21"

/// Compiles the text-diff engine cases and the three worker test files with the compiler Swate uses and
/// runs them in Vitest. The suites write to their usual output folders, which the Vitest configuration requires.
let TextDiffSwateFable () =
    let compile projectPath =
        cleanGeneratedOutput projectPath

        run
            "dotnet"
            [ "tool"; "exec"; "fable"; "--version"; swateFableVersion; "--yes"; "--"; "-o"; "output"; "-s" ]
            projectPath

    let engineProject = Path.GetFullPath "tests/VersionControlService.TextDiff.FableTests"
    compile engineProject
    run npx [ "vitest"; "run"; "output/TextDiffEngineCases.test.js"; "--configLoader"; "native" ] engineProject

    compile ProjectPaths.testsPath

    run
        npx
        [
            "vitest"
            "run"
            "output/TextDiffEndToEnd.test.js"
            "output/TextDiffWorkerThread.test.js"
            "output/TextDiffPool.test.js"
            "--configLoader"
            "native"
        ]
        ProjectPaths.testsPath

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
