[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [ValidateNotNullOrEmpty()]
    [string]$Version,

    [Parameter(Mandatory = $true)]
    [ValidateNotNullOrEmpty()]
    [string]$Output
)

$ErrorActionPreference = 'Stop'

$repositoryRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$outputPath =
    if ([System.IO.Path]::IsPathRooted($Output)) {
        [System.IO.Path]::GetFullPath($Output)
    }
    else {
        [System.IO.Path]::GetFullPath((Join-Path (Get-Location) $Output))
    }

$volumeRoot = [System.IO.Path]::GetPathRoot($outputPath)
$comparison = [System.StringComparison]::OrdinalIgnoreCase
$separator = [System.IO.Path]::DirectorySeparatorChar
$repositoryPrefix = $repositoryRoot.TrimEnd($separator) + $separator
$outputPrefix = $outputPath.TrimEnd($separator) + $separator

if (
    $outputPath.Equals($volumeRoot, $comparison) -or
    $outputPath.Equals($repositoryRoot, $comparison) -or
    $outputPath.StartsWith($repositoryPrefix, $comparison) -or
    $repositoryRoot.StartsWith($outputPrefix, $comparison)
) {
    throw "Refusing to clear unsafe package output directory '$outputPath'."
}

if (Test-Path -LiteralPath $outputPath -PathType Leaf) {
    throw "Package output path is not a directory: '$outputPath'."
}

$pathSegment = [System.IO.DirectoryInfo]::new($outputPath)
while ($null -ne $pathSegment) {
    if (
        $pathSegment.Exists -and
        (($pathSegment.Attributes -band [System.IO.FileAttributes]::ReparsePoint) -ne 0)
    ) {
        throw "Refusing package output beneath reparse point '$($pathSegment.FullName)'."
    }

    $pathSegment = $pathSegment.Parent
}

if (Test-Path -LiteralPath $outputPath) {
    Get-ChildItem -LiteralPath $outputPath -Force | Remove-Item -Recurse -Force
}
else {
    New-Item -ItemType Directory -Path $outputPath | Out-Null
}

$projects = @(
    'src/VersionControlService.Abstractions/VersionControlService.Abstractions.fsproj',
    'src/VersionControlService.Runtime.Node/VersionControlService.Runtime.Node.fsproj',
    'src/VersionControlService.Git/VersionControlService.Git.fsproj',
    'src/VersionControlService.LakeFs/VersionControlService.LakeFs.fsproj',
    'src/VersionControlService/VersionControlService.fsproj'
)

Push-Location $repositoryRoot
try {
    foreach ($project in $projects) {
        & dotnet pack $project -c Release -o $outputPath -p:Version=$Version --no-restore
        if ($LASTEXITCODE -ne 0) {
            throw "Packing failed: $project"
        }
    }
}
finally {
    Pop-Location
}

Write-Host "Packed local VersionControlService graph into $outputPath"
