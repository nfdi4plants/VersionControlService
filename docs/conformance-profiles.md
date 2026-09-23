# Conformance profiles

The shared Vitest profiles exercise behavior through `ProviderFactory` and `WorkspaceSession`. Provider implementations supply a `ProviderTestHarness` that creates isolated repositories and workspaces, advances a target as another client, injects deterministic races, and cleans up its own state.

## Profiles

| Profile | What it checks |
|---|---|
| Core | selected-path revisions, exact restore, refs, workspace versions, path identity, and structured failures |
| Synchronization | refresh, preview, update, publish, synchronize composition, acceptance, pinned target, no-op behavior, retry, stale targets, and conflict creation |
| Conflict | rotating handles, candidate and supplied-content resolution, finalize/cancel, and destination races |
| Provisioning | access verification, initialize, clone, bind, destination rules, and dependency reporting |
| Operational | progress, cancellation, independent sessions, interrupted-mutation cleanup, and redaction |
| Extensions | advertised services, diff content, materialization, storage policy, maintenance, and browser URLs |
| Consumer workflow | selected save, pending publish, update/conflict flow, branch creation, discard, and optional controls |

Core is required for every opened session. A provider registers optional-profile expectations through `ExpectedServices`. The extension suite calls only services the harness says the provider supports and checks that discovery matches that declaration.

The consumer workflow profile is not an application-specific layer. It combines common operations in the order a typical host uses them and remains provider-neutral.

## Running the profiles

Restore .NET tools and JavaScript dependencies once:

```console
dotnet restore VersionControlService.slnx
npm ci
```

Run all local profiles and provider-specific tests:

```console
dotnet run --project build/Build.fsproj -- test run
```

Git profiles use isolated local repositories and bare remotes. They require Git 2.38 or newer and Git LFS 3.7 or newer but do not need Docker.

The ordinary run registers lakeFS tests as skipped when `LAKEFS_INTEGRATION` is not `1`. The release gate starts the pinned lakeFS container and makes every lakeFS profile mandatory:

```console
dotnet run --project build/Build.fsproj -- test lakefs
```

That command fails if a live profile reports a skip.

## External providers

Copy the harness shape rather than the fake provider implementation. The harness should use the provider's real factory and real storage boundary. Race hooks belong in the harness or factory construction, not in the public SPI. Use unique repository locations and workspace roots for each test, and clean them even when an assertion fails.
