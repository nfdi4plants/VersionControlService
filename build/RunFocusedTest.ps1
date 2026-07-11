param(
    [Parameter(Mandatory = $true)] [string] $TestFile,
    [Parameter(Mandatory = $true)] [string] $Filter
)

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
$testsRoot = Join-Path $repoRoot "tests/VersionControlService.Tests"

Push-Location $testsRoot
try {
    & dotnet fable -o output -s
    if ($LASTEXITCODE -ne 0) { throw "Fable compilation failed with exit code $LASTEXITCODE." }

    $outputFile = Join-Path "output" $TestFile
    if (-not (Test-Path -LiteralPath $outputFile)) {
        throw "Focused test output does not exist: $outputFile"
    }

    # npx.cmd, not npx: the npx.ps1 shim mis-parses "& npx" invocations and runs "npm exec -- px ..."
    & npx.cmd vitest run $outputFile -t $Filter --reporter=verbose
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}
finally {
    Pop-Location
}
