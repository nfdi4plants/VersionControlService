# VersionControlService

VersionControlService defines portable version-control workspace contracts for Fable and .NET. The built-in Git and lakeFS providers target Fable/Node and implement the same `ProviderFactory` and `WorkspaceSession` contracts. Hosts compose the providers they need and keep application concerns such as settings, UI state, and credential storage outside the library.

## Install

Install the dependency-only umbrella package to get the public abstractions, Node runtime, Git provider, and lakeFS provider at one coordinated version:

```console
dotnet add package VersionControlService --prerelease
```

An external provider that does not need the built-in implementations can reference `VersionControlService.Abstractions` alone.

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
}

let lakeFsFactory =
    LakeFs.createFactory lakeFsOptions LakeFsCredentials.unconfigured

let catalog =
    Resolver.tryCreateCatalog [ gitFactory; lakeFsFactory ]
    |> Result.defaultWith invalidOp
```

The host persists `WorkspaceBinding` values. An explicit binding wins over probing. Without a binding, `ProviderResolver.resolve` reports one candidate, an ambiguity, or an unmanaged workspace; the host decides whether to call `Adopt` and when to save the returned binding.

## Public contract

Every opened session has `CoreVersionControl`. Optional services are present only when the provider supports them, so feature discovery is an `Option.isSome` check. Operations return `Succeeded`, `PartiallySucceeded`, or `Failed` with structured state-change, recovery, affected-path, and revision evidence.

A consumer that wants one code path for every provider fills the gaps itself. `WorkspaceSession.withFallbackServices` returns a session with every service present, and `ProviderFactory.withFallbackServices` wraps a factory so that every session it opens is complete. A composition root wraps its factories before `ProviderResolver.tryCreateCatalog`. Every fallback operation succeeds as a no-op that carries a `service_unavailable` warning, except the synchronization service and the conflict mutations `Resolve`, `Finalize` and `Cancel`, which fail with that code. `WorkspaceSession.availability` reports which services the provider really supplies. Read it before filling, because a filled session reports every service as present. A wrapped factory never yields an unfilled session, so a host that needs the report keeps the unwrapped factory, or applies `WorkspaceSession.withFallbackServices` itself after reading the availability.

Everything outside an opened session (`Probe`, `VerifyLocation`, `Initialize`, `Clone`, `Adopt`, `Bind`, `CheckDependencies`, `InstallDependency`), repository locations and credentials stay provider-specific at the composition root. A consumer that prefers to see an absent service as absent, as Swate does, does not apply the fallback, so two consumers may behave differently against the same provider by design.

See:

- [Provider authoring](docs/provider-authoring.md)
- [Workspace bindings and resolution](docs/workspace-bindings.md)
- [Conformance profiles](docs/conformance-profiles.md)
- [Verification matrix](docs/verification/version-control-matrix.md)

## Build and test

```console
dotnet restore VersionControlService.slnx
npm ci
dotnet build VersionControlService.slnx --no-restore -c Release
dotnet test tests/VersionControlService.Abstractions.Tests/VersionControlService.Abstractions.Tests.fsproj --no-restore
.\build.cmd test run
```

On non-Windows systems, replace the last command with:

```console
dotnet run --project build/Build.fsproj -- test run
```

The ordinary suite skips live lakeFS tests. Run the pinned Docker matrix explicitly when Docker is available:

```console
powershell -NoProfile -File build/RunLakeFsIntegration.ps1
```
