# VersionControlService

VersionControlService defines portable version-control workspace contracts for Fable and .NET. The built-in Git and lakeFS providers target Fable/Node and implement the same `ProviderFactory` and `WorkspaceSession` contracts. Hosts compose the providers they need and keep application concerns such as settings, UI state, and credential storage outside the library.

## Install

The packages are not on nuget.org yet. Build the five coordinated packages into a local feed and restore from there:

```console
dotnet restore VersionControlService.slnx
dotnet run --project build/Build.fsproj -- pack --version=0.0.1-local --output=<feed-dir>
dotnet nuget add source <feed-dir> --name vcs-local
dotnet add package VersionControlService --version 0.0.1-local
```

Run the pack from the repository root. It packs with `--no-restore`, so the restore above is
what makes it work on a fresh clone, and it resolves the repository root from the current
directory.

The umbrella package is dependency-only and carries the public abstractions, the Node runtime, the Git provider, and the lakeFS provider at one coordinated version. An external provider that does not need the built-in implementations can reference `VersionControlService.Abstractions` alone. Once the packages are published, `dotnet add package VersionControlService --prerelease` replaces the three commands above.

The Git and lakeFS providers run on Fable and Node, and the Git provider needs the git binary. Git LFS is optional and only the large-object services use it. [Consuming the library](docs/consuming.md) lists what to install.

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
