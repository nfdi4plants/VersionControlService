[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'

$repositoryRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$containerName = "vcs-lakefs-$([System.Guid]::NewGuid().ToString('N'))"
$containerStarted = $false

Push-Location $repositoryRoot
try {
    & docker --version
    if ($LASTEXITCODE -ne 0) {
        throw 'Docker is required for the live lakeFS matrix.'
    }

    & docker run --detach --name $containerName -p 8000:8000 `
        -e LAKEFS_DATABASE_TYPE=local `
        -e LAKEFS_BLOCKSTORE_TYPE=local `
        -e LAKEFS_AUTH_ENCRYPT_SECRET_KEY=local-test-encryption-key `
        -e LAKEFS_INSTALLATION_USER_NAME=integration `
        -e LAKEFS_INSTALLATION_ACCESS_KEY_ID=integration-access `
        -e LAKEFS_INSTALLATION_SECRET_ACCESS_KEY=integration-secret `
        treeverse/lakefs:1.83.0 run

    if ($LASTEXITCODE -ne 0) {
        throw 'Starting the lakeFS integration container failed.'
    }

    $containerStarted = $true
    $deadline = [System.DateTimeOffset]::UtcNow.AddMinutes(3)
    $healthy = $false

    while (-not $healthy -and [System.DateTimeOffset]::UtcNow -lt $deadline) {
        try {
            $response = Invoke-WebRequest `
                -Uri 'http://127.0.0.1:8000/api/v1/healthcheck' `
                -UseBasicParsing `
                -TimeoutSec 3

            $healthy = $response.StatusCode -ge 200 -and $response.StatusCode -lt 300
        }
        catch {
            Start-Sleep -Seconds 2
        }

        if (-not $healthy) {
            $running = (& docker inspect --format '{{.State.Running}}' $containerName 2>$null) -eq 'true'
            if (-not $running) {
                & docker logs $containerName
                throw 'The lakeFS integration container stopped before becoming healthy.'
            }
        }
    }

    if (-not $healthy) {
        & docker logs $containerName
        throw 'lakeFS did not become healthy within three minutes.'
    }

    $env:LAKEFS_INTEGRATION = '1'
    $env:LAKEFS_INTEGRATION_ENDPOINT = 'http://127.0.0.1:8000'
    $env:LAKEFS_INTEGRATION_ACCESS_KEY_ID = 'integration-access'
    $env:LAKEFS_INTEGRATION_SECRET_ACCESS_KEY = 'integration-secret'

    $previousErrorActionPreference = $ErrorActionPreference
    $ErrorActionPreference = 'Continue'

    try {
        & dotnet run --project build/Build.fsproj -- test run 2>&1 |
            Tee-Object -Variable testOutput
        $testExitCode = $LASTEXITCODE
    }
    finally {
        $ErrorActionPreference = $previousErrorActionPreference
    }

    if ($testExitCode -ne 0) {
        throw "The live lakeFS matrix failed with exit code $testExitCode."
    }

    $combinedOutput = @($testOutput) -join [System.Environment]::NewLine
    if ($combinedOutput -match 'lakeFS integration skipped') {
        throw 'The live lakeFS matrix reported a skipped profile.'
    }
}
finally {
    if ($containerStarted) {
        & docker rm --force $containerName | Out-Null
    }

    Pop-Location
}
