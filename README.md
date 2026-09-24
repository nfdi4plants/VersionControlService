# VersionControlService

VersionControlService defines portable version-control workspace contracts for Fable and .NET. The built-in Git and lakeFS providers target Fable/Node and implement the same `ProviderFactory` and `WorkspaceSession` contracts. Hosts compose the providers they need and keep application concerns such as settings, UI state, and credential storage outside the library.

## Install

The packages are on nuget.org:

```console
dotnet add package VersionControlService
```

The umbrella package is dependency-only and carries the public abstractions, the Node runtime, the Git provider, and the lakeFS provider at one coordinated version. An external provider that does not need the built-in implementations can reference `VersionControlService.Abstractions` alone.

The Git and lakeFS providers run on Fable and Node. They need the `simple-git` npm package, and the Git provider needs git 2.38 or newer on the path. Git LFS is optional and only the large-object services use it. lakeFS needs no local tool. [Consuming the library](docs/consuming.md) lists what to install.

## Quick start

This example clones a Git repository, commits the changed files, pulls and pushes. Git calls those steps clone, commit, pull and push. The abstraction calls them `Clone`, `CreateRevision`, `Update` and `Publish`.

Every operation takes an `OperationContext` and returns `Async<OperationResult<'T>>`. The `valueOf` helper below unwraps a result and throws on anything other than `Succeeded`, which is enough for a first run. A real host reads the failure category and recovery action, as [reading a result](docs/consuming.md#reading-a-result) shows. Save the example as `Program.fs` in a console project named `QuickStart.fsproj` that references the package.

```fsharp
module QuickStart

open VersionControlService.Abstractions

module Git = VersionControlService.Git.GitWorkspaceSession

/// Returns the payload and throws on a failure or a partial success.
let valueOf (result: OperationResult<'T>) : 'T =
    match result with
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded(_, failure)
    | Failed failure -> failwith $"{failure.Code}: {failure.Message}"

let factory = Git.createFactory Git.GitSessionHooks.none

let location: RepositoryLocation = {
    ProviderId = ProviderId.tryCreate WellKnownProviderIds.Git |> Result.defaultWith invalidOp
    DisplayName = None
    ProviderLocation = "https://github.com/example/study-archive.git"
    ConnectionProfileId = None
}

let quickStart (targetPath: string) =
    async {
        let context = OperationContext.detached "quick-start"

        // Clone into a missing or empty directory. The binding is what a host stores
        // and passes to Open the next time.
        let! cloned =
            factory.Clone
                {
                    Location = location
                    TargetPath = targetPath
                    TargetRef = None
                    MaterializeAllObjects = false
                }
                context

        let! opened = factory.Open (valueOf cloned) context
        let session = valueOf opened

        let readVersion () =
            async {
                let! status = session.Core.GetStatus context
                return (valueOf status).WorkspaceVersion
            }

        // Edit files under targetPath, then read the status.
        let! status = session.Core.GetStatus context
        let status = valueOf status

        // Commit exactly the changed paths. The version guards against a workspace
        // that changed since the status was read.
        if status.Changes.Length > 0 then
            let! revision =
                session.Core.CreateRevision
                    {
                        Message = "Update results"
                        Paths = status.Changes |> Array.map (fun change -> change.Path)
                        ExpectedWorkspaceVersion = status.WorkspaceVersion
                    }
                    context

            printfn "Created revision %s" (RevisionId.value (valueOf revision))

        match session.Synchronization with
        | Some sync ->
            // Pull. Every mutation moves the version, so read it again each time.
            let! version = readVersion ()
            let! updated = sync.Update { ExpectedWorkspaceVersion = version } context
            printfn "After update: %A" (valueOf updated).Relationship

            // Push.
            let! version = readVersion ()

            let! published =
                sync.Publish
                    {
                        ExpectedWorkspaceVersion = version
                        ExpectedTargetRevision = None
                    }
                    context

            printfn "After publish: %A" (valueOf published).Relationship
        | None -> printfn "This provider has no synchronization service."

        do! session.Close()
    }

[<EntryPoint>]
let main _ =
    quickStart "study-archive" |> Async.StartImmediate
    0
```

Each mutation carries the `WorkspaceVersion` of the status the caller last read. A workspace that changed in between rejects the call with a `Concurrency` failure. The Git provider does not return the new version after a mutation, so the example reads the status again before each one. `Synchronization` is optional on a session, which is why the example matches on it.

`Synchronization.Synchronize` runs the pull and the push as one operation. It refuses when the pull would overwrite local changes or open a conflict session, and [synchronizing](docs/consuming.md#synchronizing) shows the refusals a host handles.

`Git.createFactory` uses anonymous credentials. Public HTTPS remotes and local paths work with them, and SSH works through the user's SSH agent. `CreateRevision` takes the author from the repository's `user.name` and `user.email` and fails with `identity_missing` when git has none. `Git.createFactoryWithCredentials` takes a credential strategy for tokens, and `Git.createFactoryWithCredentialsAndIdentity` also takes an identity strategy. [Credentials](docs/consuming.md#credentials) describes both.

### Run it

Fable compiles the program to JavaScript and Node runs it. Start the workflow with `Async.StartImmediate`, as the entry point above does, or with `Async.StartAsPromise` from Fable.Core when the caller wants a promise. The Node runtime loads Node modules with `require`, which Node does not define inside an ES module, so bundle the Fable output as CommonJS before you run it:

```console
npm install simple-git
npm install --save-dev rollup
dotnet new tool-manifest
dotnet tool install fable --version 5.5.0
dotnet tool run fable QuickStart.fsproj --outDir output
npx rollup output/Program.js --file output/app.cjs --format cjs
node output/app.cjs
```

Fable names each output file after its source file, so `Program.fs` becomes `output/Program.js`.

### Git terms and their names in the abstraction

| Git | VersionControlService |
|---|---|
| `git clone` | `ProviderFactory.Clone` |
| `git init` | `ProviderFactory.Initialize` |
| `git remote add origin` | `ProviderFactory.Bind` |
| `git status` | `Core.GetStatus` |
| `git add` and `git commit` | `Core.CreateRevision` |
| `git branch` | `Core.ListRefs` and `Core.CreateRef` |
| `git checkout <branch>` | `Core.PreflightSwitchRef`, then `Core.SwitchRef` |
| `git restore` | `Core.RestorePaths` |
| `git diff --stat` | `Core.GetDiffSummary` |
| `git fetch` | `Synchronization.Refresh` |
| `git pull` | `Synchronization.Update` |
| `git push` | `Synchronization.Publish` |
| `git pull`, then `git push` | `Synchronization.Synchronize` |

### lakeFS

A lakeFS session answers the same calls. Only the factory and the location change. `LakeFsWorkspaceSession.createFactory` takes a `LakeFsProviderOptions` record and a credential strategy. `StateRoot` is a directory outside every workspace where the provider keeps its state, and `PathCaseSensitivity` matches the host's filesystem.

```fsharp
module LakeFs = VersionControlService.LakeFs.LakeFsWorkspaceSession
module LakeFsCredentials = VersionControlService.LakeFs.LakeFsCredentials

let lakeFsFactory =
    LakeFs.createFactory
        {
            StateRoot = "/var/lib/my-app/lakefs-state"
            PathCaseSensitivity = CaseSensitive
        }
        (LakeFsCredentials.fixedConnection {
            Endpoint = "http://localhost:8000"
            AccessKeyId = accessKeyId
            SecretAccessKey = secretAccessKey
        })
```

The host supplies `accessKeyId` and `secretAccessKey`. The location then uses `WellKnownProviderIds.LakeFs` and a `ProviderLocation` such as `lakefs://study-archive/main`.

## Compose providers

Factories are ordinary values. Create them with host-owned configuration, then build an immutable catalog at the application composition root.

```fsharp
open VersionControlService.Abstractions

module Git = VersionControlService.Git.GitWorkspaceSession
module LakeFsCredentials = VersionControlService.LakeFs.LakeFsCredentials
module LakeFsOptions = VersionControlService.LakeFs.LakeFsProviderOptions
module LakeFs = VersionControlService.LakeFs.LakeFsWorkspaceSession
module Resolver = VersionControlService.Abstractions.ProviderResolver

let gitFactory = Git.createFactory Git.GitSessionHooks.none

let lakeFsOptions: LakeFsOptions.LakeFsProviderOptions = {
    StateRoot = applicationStateDirectory
    PathCaseSensitivity = CaseInsensitive
}

let lakeFsFactory =
    LakeFs.createFactory lakeFsOptions LakeFsCredentials.unconfigured

let catalog =
    Resolver.tryCreateCatalog [ gitFactory; lakeFsFactory ]
    |> Result.defaultWith invalidOp
```

The host persists `WorkspaceBinding` values. An explicit binding wins over probing. Without a binding, `ProviderResolver.resolve` reports one candidate, an ambiguity, or an unmanaged workspace. The host decides whether to call `Adopt` and when to save the returned binding.

## Public contract

Every opened session has `CoreVersionControl`. On a session the provider built, optional services are present only when the provider supports them, so feature discovery is an `Option.isSome` check. A consumer that fills the gaps gives that up, as the next paragraph describes. Operations return `Succeeded`, `PartiallySucceeded`, or `Failed` with structured state-change, recovery, affected-path, and revision evidence.

A consumer that wants one code path for every provider fills the gaps itself. `WorkspaceSession.withFallbackServices` returns a session with every service present, and `ProviderFactory.withFallbackServices` wraps a factory so that every session it opens is complete. A composition root wraps its factories before `ProviderResolver.tryCreateCatalog`. Every fallback operation succeeds as a no-op that carries a `service_unavailable` warning, except the synchronization service and the conflict mutations `Resolve`, `Finalize` and `Cancel`, which fail with that code. A no-op read returns an empty value: `ListObjects` an empty array, the text diff reads `UnsupportedContent`, `GetActiveSession` and `GetRepositoryWebUrl` `None`, and `GetSettings` no threshold with `MaterializeLargeObjects = true`. `Prune` and `Deduplicate` return the reason as their report. `WorkspaceSession.availability` reports which services the provider really supplies. Read it before filling, because a filled session reports every service as present. A wrapped factory never yields an unfilled session, so a host that needs the report keeps the unwrapped factory, or applies `WorkspaceSession.withFallbackServices` itself after reading the availability.

Everything outside an opened session (`Probe`, `VerifyLocation`, `Initialize`, `Clone`, `Adopt`, `Bind`, `CheckDependencies`, `InstallDependency`), repository locations and credentials stay provider-specific at the composition root. A consumer that prefers to see an absent service as absent does not apply the fallback, so two consumers may behave differently against the same provider by design.

See:

- [Consuming the library](docs/consuming.md)
- [Provider authoring](docs/provider-authoring.md)
- [Workspace bindings and resolution](docs/workspace-bindings.md)
- [Conformance profiles](docs/conformance-profiles.md)
- [Verification matrix](docs/verification/version-control-matrix.md)

## Build and test

```console
dotnet tool restore
dotnet restore VersionControlService.slnx
npm ci
dotnet build VersionControlService.slnx --no-restore -c Release
dotnet test tests/VersionControlService.Abstractions.Tests/VersionControlService.Abstractions.Tests.fsproj --no-restore
.\build.cmd test run
```

`build.cmd` and `build.sh` both forward their arguments to the build project, so on non-Windows systems the last command becomes:

```console
./build.sh test run
```

The ordinary suite skips live lakeFS tests. Run the pinned Docker matrix explicitly when Docker is available:

```console
.\build.cmd test lakefs
```

CI runs that row on Linux as `./build.sh test lakefs`.

Run `.\build.cmd` without a target to list every target, including the focused test run and the package graph checks.

## Pack a local feed

Contributors who want to try unreleased changes in a host can pack the five coordinated packages into a local feed. Run this from the repository root:

```console
dotnet restore VersionControlService.slnx
dotnet run --project build/Build.fsproj -- pack --version=0.0.1-local --output=<feed-dir>
dotnet nuget add source <feed-dir> --name vcs-local
dotnet add package VersionControlService --version 0.0.1-local
```

`pack` packs with `--no-restore`, so a fresh clone needs the restore first. It empties the output directory, so pick a directory outside the clone.

## NuGet release

Add the next version and its release notes to `CHANGELOG.md`, then run the release target:

```console
NUGET_KEY=<key> dotnet run --project build/Build.fsproj -- release nuget
```

The target packs into `nupkgs/` and verifies the package graph before it pushes to NuGet.
Pass `--dry-run` to pack and verify without publishing.
