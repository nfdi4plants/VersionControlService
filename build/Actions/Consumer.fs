[<RequireQualifiedAccessAttribute>]
module Consumer

open System
open ProjectInfo

/// Restores the one-reference consumer against a local feed and compiles it with Fable,
/// which is what proves the umbrella package alone carries the whole graph to a consumer.
let Compile (version: string) (feed: string) (cache: string) (output: string) =
    run
        "dotnet"
        [
            "restore"
            ProjectPaths.packageConsumerProject
            // nuget.org has to be listed first: with a directory as the first source,
            // dotnet/sdk#51514 turns every later source into a local path on Windows.
            "--source"
            "https://api.nuget.org/v3/index.json"
            "--source"
            feed
            "--packages"
            cache
            "--no-cache"
            $"-p:VersionControlServicePackageVersion={version}"
        ]
        ProjectPaths.repositoryRoot

    // The environment variable is deliberate: Fable 5.5 does not accept arbitrary -p:
    // arguments, so the version cannot travel the same way it does through restore.
    Environment.SetEnvironmentVariable("VersionControlServicePackageVersion", version)

    run
        "dotnet"
        [
            "fable"
            ProjectPaths.packageConsumerProject
            "--noRestore"
            "--noCache"
            "-o"
            output
            "-s"
        ]
        ProjectPaths.repositoryRoot

    printGreenfn "Compiled the one-reference consumer against %s %s" project version
