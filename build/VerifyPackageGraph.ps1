[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [ValidateNotNullOrEmpty()]
    [string]$Feed,

    [Parameter(Mandatory = $true)]
    [ValidateNotNullOrEmpty()]
    [string]$Version
)

$ErrorActionPreference = 'Stop'
Add-Type -AssemblyName System.IO.Compression.FileSystem

$feedPath = [System.IO.Path]::GetFullPath($Feed)
if (-not (Test-Path -LiteralPath $feedPath -PathType Container)) {
    throw "Package feed does not exist: $feedPath"
}

$expectedPackages = @(
    'VersionControlService.Abstractions',
    'VersionControlService.Runtime.Node',
    'VersionControlService.Git',
    'VersionControlService.LakeFs',
    'VersionControlService'
)

$expectedInternalDependencies = @{
    'VersionControlService.Abstractions' = @()
    'VersionControlService.Runtime.Node' = @('VersionControlService.Abstractions')
    'VersionControlService.Git' = @(
        'VersionControlService.Abstractions',
        'VersionControlService.Runtime.Node'
    )
    'VersionControlService.LakeFs' = @(
        'VersionControlService.Abstractions',
        'VersionControlService.Runtime.Node'
    )
    'VersionControlService' = @(
        'VersionControlService.Abstractions',
        'VersionControlService.Runtime.Node',
        'VersionControlService.Git',
        'VersionControlService.LakeFs'
    )
}

$implementationPackages = @(
    'VersionControlService.Abstractions',
    'VersionControlService.Runtime.Node',
    'VersionControlService.Git',
    'VersionControlService.LakeFs'
)

$errors = [System.Collections.Generic.List[string]]::new()
$packagesById = @{}

function Read-Package {
    param([Parameter(Mandatory = $true)][string]$Path)

    $archive = [System.IO.Compression.ZipFile]::OpenRead($Path)
    try {
        $nuspecEntry = $archive.Entries | Where-Object { $_.FullName -like '*.nuspec' } | Select-Object -First 1
        if (-not $nuspecEntry) {
            throw "No .nuspec was found in '$Path'."
        }

        $reader = [System.IO.StreamReader]::new($nuspecEntry.Open())
        try {
            [xml]$document = $reader.ReadToEnd()
        }
        finally {
            $reader.Dispose()
        }

        $namespace = [System.Xml.XmlNamespaceManager]::new($document.NameTable)
        $namespace.AddNamespace('n', $document.DocumentElement.NamespaceURI)
        $metadata = $document.SelectSingleNode('/n:package/n:metadata', $namespace)
        if (-not $metadata) {
            throw "No package metadata was found in '$Path'."
        }

        $dependencies = @(
            $metadata.SelectNodes('.//n:dependency', $namespace) | ForEach-Object {
                [pscustomobject]@{
                    Id = [string]$_.GetAttribute('id')
                    Version = [string]$_.GetAttribute('version')
                }
            }
        )

        return [pscustomobject]@{
            Path = $Path
            Id = [string]$metadata.SelectSingleNode('n:id', $namespace).InnerText
            Version = [string]$metadata.SelectSingleNode('n:version', $namespace).InnerText
            Authors = [string]$metadata.SelectSingleNode('n:authors', $namespace).InnerText
            Description = [string]$metadata.SelectSingleNode('n:description', $namespace).InnerText
            Tags = [string]$metadata.SelectSingleNode('n:tags', $namespace).InnerText
            License = $metadata.SelectSingleNode('n:license', $namespace)
            Repository = $metadata.SelectSingleNode('n:repository', $namespace)
            Dependencies = $dependencies
            Entries = @($archive.Entries | ForEach-Object { $_.FullName })
        }
    }
    finally {
        $archive.Dispose()
    }
}

function Test-PortablePdbSourceLink {
    param([Parameter(Mandatory = $true)]$Entry)

    $stream = $Entry.Open()
    try {
        $buffer = [System.IO.MemoryStream]::new()
        try {
            $stream.CopyTo($buffer)
            $portablePdbText = [System.Text.Encoding]::UTF8.GetString($buffer.ToArray())

            return (
                $portablePdbText -match '"documents"' -and
                $portablePdbText -match 'raw\.githubusercontent\.com/nfdi4plants/VersionControlService/'
            )
        }
        finally {
            $buffer.Dispose()
        }
    }
    catch {
        return $false
    }
    finally {
        $stream.Dispose()
    }
}

$packageFiles = @(Get-ChildItem -LiteralPath $feedPath -Filter '*.nupkg' -File)
if ($packageFiles.Count -ne 5) {
    $errors.Add("Expected exactly five .nupkg files, found $($packageFiles.Count).")
}

foreach ($packageFile in $packageFiles) {
    try {
        $package = Read-Package -Path $packageFile.FullName
        if ($packagesById.ContainsKey($package.Id)) {
            $errors.Add("Duplicate package ID '$($package.Id)'.")
        }
        else {
            $packagesById[$package.Id] = $package
        }
    }
    catch {
        $errors.Add($_.Exception.Message)
    }
}

foreach ($expectedId in $expectedPackages) {
    if (-not $packagesById.ContainsKey($expectedId)) {
        $errors.Add("Missing package '$expectedId'.")
    }
}

foreach ($unexpectedId in @($packagesById.Keys | Where-Object { $_ -notin $expectedPackages })) {
    $errors.Add("Unexpected package '$unexpectedId'.")
}

foreach ($packageId in $expectedPackages) {
    if (-not $packagesById.ContainsKey($packageId)) {
        continue
    }

    $package = $packagesById[$packageId]
    if ($package.Version -ne $Version) {
        $errors.Add("$packageId has version '$($package.Version)', expected '$Version'.")
    }

    if ($package.Authors -ne 'nfdi4plants') {
        $errors.Add("$packageId must declare author 'nfdi4plants'.")
    }

    if ([string]::IsNullOrWhiteSpace($package.Description)) {
        $errors.Add("$packageId is missing a description.")
    }

    if ([string]::IsNullOrWhiteSpace($package.Tags)) {
        $errors.Add("$packageId is missing package tags.")
    }

    if (-not $package.License -or $package.License.GetAttribute('type') -ne 'expression' -or $package.License.InnerText -ne 'MIT') {
        $errors.Add("$packageId must declare the MIT license expression.")
    }

    if (-not $package.Repository -or $package.Repository.GetAttribute('url') -ne 'https://github.com/nfdi4plants/VersionControlService') {
        $errors.Add("$packageId must declare the VersionControlService repository URL.")
    }

    $actualInternal = @($package.Dependencies | Where-Object { $_.Id -in $expectedPackages })
    $actualInternalIds = @($actualInternal | ForEach-Object Id | Sort-Object -Unique)
    $expectedInternalIds = @($expectedInternalDependencies[$packageId] | Sort-Object -Unique)

    if (($actualInternalIds -join '|') -ne ($expectedInternalIds -join '|')) {
        $errors.Add(
            "$packageId internal dependencies are [$($actualInternalIds -join ', ')], expected [$($expectedInternalIds -join ', ')]."
        )
    }

    foreach ($dependency in $actualInternal) {
        if ($dependency.Version -ne "[$Version]") {
            $errors.Add(
                "$packageId dependency '$($dependency.Id)' uses '$($dependency.Version)'; expected exact version '[$Version]'."
            )
        }
    }

    if ($packageId -eq 'VersionControlService') {
        $unexpectedDependencies = @($package.Dependencies | Where-Object { $_.Id -notin $expectedPackages })
        foreach ($dependency in $unexpectedDependencies) {
            $errors.Add("Umbrella package has forbidden direct dependency '$($dependency.Id)'.")
        }

        $implementationAssets = @(
            $package.Entries | Where-Object {
                $_ -match '^(lib|ref|runtimes|contentFiles|fable)/' -or
                $_ -match '\.(dll|pdb|fs|fsproj)$'
            }
        )

        if ($implementationAssets.Count -gt 0) {
            $errors.Add("Umbrella package contains implementation assets: $($implementationAssets -join ', ')")
        }
    }
}

foreach ($packageId in $implementationPackages) {
    $symbolPath = Join-Path $feedPath "$packageId.$Version.snupkg"
    if (-not (Test-Path -LiteralPath $symbolPath -PathType Leaf)) {
        $errors.Add("Missing symbol package '$packageId.$Version.snupkg'.")
        continue
    }

    $symbolArchive = [System.IO.Compression.ZipFile]::OpenRead($symbolPath)
    try {
        $pdbEntries = @($symbolArchive.Entries | Where-Object { $_.FullName -like '*.pdb' -and $_.Length -gt 0 })
        if ($pdbEntries.Count -eq 0) {
            $errors.Add("Symbol package '$packageId.$Version.snupkg' contains no portable PDB source information.")
        }
        elseif (-not ($pdbEntries | Where-Object { Test-PortablePdbSourceLink -Entry $_ })) {
            $errors.Add("Symbol package '$packageId.$Version.snupkg' contains no repository Source Link record.")
        }
    }
    finally {
        $symbolArchive.Dispose()
    }
}

if ($errors.Count -gt 0) {
    $message = "Package graph verification failed:`n - " + ($errors -join "`n - ")
    throw $message
}

Write-Host "Verified exact five-package graph at version $Version."
