# Version control verification matrix

## Prerequisites

| Prerequisite | Minimum | Verify |
|---|---|---|
| Git | 2.38 (`merge-tree --write-tree`) | `git --version` |
| Git LFS | 3.7 | `git lfs version` |
| .NET SDK | 10.0 | `dotnet --version` |
| Node.js (with npm) | 22 | `node --version` |
| Docker (Linux containers) | any current engine (pulls `treeverse/lakefs:1.83.0`) | `docker --version` |

Set up the repository before running a matrix row:

```console
dotnet restore VersionControlService.slnx
npm ci
```

Docker is required only for the live lakeFS row.

## Matrix

| Row | Command | Docker |
|---|---|---|
| Formatting and Release build | `dotnet format VersionControlService.slnx --verify-no-changes && dotnet build VersionControlService.slnx --no-restore -c Release` | No |
| Portable .NET tests | `dotnet test tests/VersionControlService.Abstractions.Tests/VersionControlService.Abstractions.Tests.fsproj --no-restore` | No |
| Fable and Vitest | `dotnet run --project build/Build.fsproj -- test run` | No |
| Portable Fable consumer | `dotnet run --project build/Build.fsproj -- test consumer` | No |
| Git and Git LFS scenarios | `dotnet run --project build/Build.fsproj -- test focused GitProviderContract.test.js ".*"` | No |
| Package graph and one-reference consumer | `dotnet run --project build/Build.fsproj -- package-consumer` | No |
| External provider sample | `dotnet build samples/ExternalProvider/ExternalProvider.fsproj -c Release` | No |
| Live lakeFS profiles and integration scenarios | `dotnet run --project build/Build.fsproj -- test lakefs` | Yes |

The package row packs the five coordinated packages at a unique local version, verifies
the graph and then restores and Fable-compiles the one-reference consumer against that
feed. The feed, the package cache and the compiler output are created under the temporary
directory. `--temp=<dir>` puts them somewhere else and `--version=<v>` pins the version.

The restore must resolve the umbrella and all four internal dependencies at that one
version. Only the umbrella package is referenced, so anything missing from the graph shows
up here.

The steps are also available on their own, which is useful when a feed should outlive the
check:

```console
dotnet run --project build/Build.fsproj -- pack --version=<v> --output=<dir>
dotnet run --project build/Build.fsproj -- verify packages --feed=<dir> --version=<v>
```

Packing empties the output directory first, so it refuses a directory inside the
repository, a volume root, or anything reached through a junction.
