module ProjectInfo

open System.IO

module ProjectPaths =

    /// Every target is invoked from the repository root, the way build.cmd and build.sh do.
    let repositoryRoot = Path.GetFullPath "."

    let solution = Path.GetFullPath "VersionControlService.slnx"

    let abstractionsProject =
        Path.GetFullPath "src/VersionControlService.Abstractions/VersionControlService.Abstractions.fsproj"

    let runtimeNodeProject =
        Path.GetFullPath "src/VersionControlService.Runtime.Node/VersionControlService.Runtime.Node.fsproj"

    let gitProject =
        Path.GetFullPath "src/VersionControlService.Git/VersionControlService.Git.fsproj"

    let lakeFsProject =
        Path.GetFullPath "src/VersionControlService.LakeFs/VersionControlService.LakeFs.fsproj"

    let umbrellaProject =
        Path.GetFullPath "src/VersionControlService/VersionControlService.fsproj"

    let abstractionsTestsProject =
        Path.GetFullPath "tests/VersionControlService.Abstractions.Tests/VersionControlService.Abstractions.Tests.fsproj"

    let packageConsumerProject =
        Path.GetFullPath "tests/VersionControlService.PackageConsumer/VersionControlService.PackageConsumer.fsproj"

    /// The Fable suites are compiled from their project directory, so they are paths.
    let testsPath = Path.GetFullPath "tests/VersionControlService.Tests"

    let fableConsumerTestsPath =
        Path.GetFullPath "tests/VersionControlService.FableConsumer.Tests"

let gitOwner = "nfdi4plants"
let project = "VersionControlService"
let projectRepo = $"https://github.com/{gitOwner}/{project}"

let packageAuthors = "Caroline Ott"
let packageLicense = "MIT"

/// Source Link writes this prefix into the portable PDBs of the symbol packages.
let sourceLinkPrefix = $"raw.githubusercontent.com/{gitOwner}/{project}/"

/// The five coordinated packages, in the order they are packed. Dependency order keeps
/// the umbrella last, and the graph verification expects exactly these ids.
module Packages =

    let abstractions = "VersionControlService.Abstractions"
    let runtimeNode = "VersionControlService.Runtime.Node"
    let git = "VersionControlService.Git"
    let lakeFs = "VersionControlService.LakeFs"
    let umbrella = "VersionControlService"

    let projects = [
        abstractions, ProjectPaths.abstractionsProject
        runtimeNode, ProjectPaths.runtimeNodeProject
        git, ProjectPaths.gitProject
        lakeFs, ProjectPaths.lakeFsProject
        umbrella, ProjectPaths.umbrellaProject
    ]

    let all = projects |> List.map fst

    /// The umbrella ships metadata only; the other four carry assemblies and symbols.
    let implementations = all |> List.filter (fun id -> id <> umbrella)

    let internalDependencies =
        Map [
            abstractions, []
            runtimeNode, [ abstractions ]
            git, [ abstractions; runtimeNode ]
            lakeFs, [ abstractions; runtimeNode ]
            umbrella, [ abstractions; runtimeNode; git; lakeFs ]
        ]

/// The live lakeFS matrix runs against this pinned image; docs/conformance-profiles.md
/// names the same tag.
module LakeFsIntegration =

    let image = "treeverse/lakefs:1.83.0"
    let port = 8000
    let endpoint = $"http://127.0.0.1:{port}"
    let healthcheckUrl = $"{endpoint}/api/v1/healthcheck"
    let accessKeyId = "integration-access"
    let secretAccessKey = "integration-secret"
    let encryptionKey = "local-test-encryption-key"
    let installationUser = "integration"
