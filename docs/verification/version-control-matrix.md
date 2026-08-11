# Version control verification matrix

## Prerequisites

| Prerequisite | Minimum | Verify |
|---|---|---|
| Git | 2.38 (`merge-tree --write-tree`) | `git --version` |
| Git LFS | 3.7 | `git lfs version` |
| .NET SDK | 10.0 | `dotnet --version` |
| Node.js (with npm) | 22 | `node --version` |
| Docker (Linux containers) | any current engine; pulls `treeverse/lakefs:1.83.0` | `docker --version` |

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
| Git and Git LFS scenarios | `powershell -NoProfile -File build/RunFocusedTest.ps1 -TestFile GitProviderContract.test.js -Filter ".*"` | No |
| Package graph and one-reference consumer | Run the package commands below | No |
| External provider sample | `dotnet build samples/ExternalProvider/ExternalProvider.fsproj -c Release` | No |
| Live lakeFS profiles and integration scenarios | `powershell -NoProfile -File build/RunLakeFsIntegration.ps1` | Yes |

For the package row, create a unique local version and use directories outside the repository:

```powershell
$version = "0.0.0-local.$(Get-Date -Format yyyyMMddHHmmss)"
$feed = Join-Path $env:TEMP "version-control-service-local-feed"
$cache = Join-Path $env:TEMP "version-control-service-local-cache"

powershell -NoProfile -File build/PackLocal.ps1 -Version $version -Output $feed
powershell -NoProfile -File build/VerifyPackageGraph.ps1 -Feed $feed -Version $version

dotnet restore tests/VersionControlService.PackageConsumer/VersionControlService.PackageConsumer.fsproj `
    --source $feed `
    --source https://api.nuget.org/v3/index.json `
    --packages $cache `
    --no-cache `
    -p:VersionControlServicePackageVersion=$version

$env:VersionControlServicePackageVersion = $version
dotnet fable tests/VersionControlService.PackageConsumer/VersionControlService.PackageConsumer.fsproj `
    --noRestore `
    --noCache `
    -o (Join-Path $env:TEMP "version-control-service-consumer-output") `
    -s
```

The environment property is deliberate: Fable 5.5 does not accept arbitrary `-p:` arguments. The restore must resolve the umbrella and all four internal dependencies at the unique local version.
