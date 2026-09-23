[<RequireQualifiedAccessAttribute>]
module Consumer

open System
open System.IO
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

/// Compiles the consumer with the oldest Fable a consumer uses, bundles it to CommonJS and loads it,
/// so a fable-library member that Fable lacks or a namespace import the interop breaks stops the target.
let BundleWithOldestFable (version: string) (feed: string) (cache: string) (output: string) =
    let oldestOutput = output + "-fable-5.0.0-alpha.21"

    if Directory.Exists oldestOutput then
        Directory.Delete(oldestOutput, true)

    // Swate compiles with Fable 5.0.0-alpha.21. The library's own Fable 5.5.0 tool ships a
    // fable-library with members the older version lacks. This Fable restores the project
    // again while it cracks it, and MSBuild reads these variables as properties, so that
    // restore finds the local feed and the cache the first restore filled.
    Environment.SetEnvironmentVariable("RestoreAdditionalProjectSources", feed)
    Environment.SetEnvironmentVariable("RestorePackagesPath", cache)

    run
        "dotnet"
        [
            "tool"
            "execute"
            "fable@5.0.0-alpha.21"
            "--allow-roll-forward"
            "--"
            ProjectPaths.packageConsumerProject
            "--noRestore"
            "--noCache"
            "-o"
            oldestOutput
        ]
        ProjectPaths.repositoryRoot

    let entry = Path.Combine(oldestOutput, "Program.js")

    if not (File.Exists entry) then
        failwithf "Fable output folder %s does not contain Program.js" oldestOutput

    let bundle = Path.Combine(oldestOutput, "consumer.bundle.cjs")
    Environment.SetEnvironmentVariable("VCS_CONSUMER_BUNDLE_INPUT", entry)
    Environment.SetEnvironmentVariable("VCS_CONSUMER_BUNDLE_OUTPUT", bundle)

    let rollupConfig =
        Path.Combine(ProjectPaths.repositoryRoot, "tests/VersionControlService.PackageConsumer/rollup.config.mjs")

    run npx [ "rollup"; "--config"; rollupConfig ] ProjectPaths.repositoryRoot

    // The bundle lives outside the repository, so node finds the external npm packages
    // (simple-git) only through NODE_PATH.
    Environment.SetEnvironmentVariable("NODE_PATH", Path.Combine(ProjectPaths.repositoryRoot, "node_modules"))
    run "node" [ bundle ] ProjectPaths.repositoryRoot

    printGreenfn "Bundled and loaded the consumer compiled with Fable 5.0.0-alpha.21"
