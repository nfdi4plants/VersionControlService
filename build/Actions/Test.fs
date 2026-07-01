[<RequireQualifiedAccessAttribute>]
module Test

open ProjectInfo
open System.IO

let private cleanGeneratedOutput (testProjectPath: string) =
    let outputPath = Path.Combine(testProjectPath, "output")

    if Directory.Exists outputPath then
        Directory.Delete(outputPath, true)

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

module Run =
    let all =
        cleanGeneratedOutput ProjectPaths.testsPath

        runAsync
            "tests"
            "dotnet"
            [
                "fable"
                "-o"
                "output"
                "-s"
                "--run"
                "npx"
                "vitest"
                "run"
            ]
            ProjectPaths.testsPath
