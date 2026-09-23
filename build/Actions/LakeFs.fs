[<RequireQualifiedAccessAttribute>]
module LakeFs

open System
open System.Net.Http
open System.Threading
open ProjectInfo

let private healthDeadline = TimeSpan.FromMinutes 3.

/// Polls the healthcheck until lakeFS answers, giving up early when the container has
/// already stopped.
let private waitForHealth (containerName: string) =
    use client = new HttpClient(Timeout = TimeSpan.FromSeconds 3.)
    let deadline = DateTimeOffset.UtcNow.Add healthDeadline
    let mutable healthy = false

    while not healthy && DateTimeOffset.UtcNow < deadline do
        healthy <-
            try
                let response =
                    client.GetAsync LakeFsIntegration.healthcheckUrl
                    |> Async.AwaitTask
                    |> Async.RunSynchronously

                response.IsSuccessStatusCode
            with _ ->
                Thread.Sleep 2000
                false

        if not healthy && not (Docker.isRunning containerName) then
            Docker.logs containerName
            failwith "The lakeFS integration container stopped before becoming healthy."

    healthy

/// Starts the pinned lakeFS container, runs the whole suite against it and fails on any
/// profile that reports a skip, because a silent skip here would look like a pass.
let Live () =
    Docker.requireCli "Docker is required for the live lakeFS matrix."

    let containerId = Guid.NewGuid().ToString "N"
    let containerName = $"vcs-lakefs-{containerId}"

    Docker.startDetached containerName [
        "-p"
        $"{LakeFsIntegration.port}:8000"
        "-e"
        "LAKEFS_DATABASE_TYPE=local"
        "-e"
        "LAKEFS_BLOCKSTORE_TYPE=local"
        "-e"
        $"LAKEFS_AUTH_ENCRYPT_SECRET_KEY={LakeFsIntegration.encryptionKey}"
        "-e"
        $"LAKEFS_INSTALLATION_USER_NAME={LakeFsIntegration.installationUser}"
        "-e"
        $"LAKEFS_INSTALLATION_ACCESS_KEY_ID={LakeFsIntegration.accessKeyId}"
        "-e"
        $"LAKEFS_INSTALLATION_SECRET_ACCESS_KEY={LakeFsIntegration.secretAccessKey}"
        LakeFsIntegration.image
        "run"
    ]

    try
        if not (waitForHealth containerName) then
            Docker.logs containerName
            failwith "lakeFS did not become healthy within three minutes."

        // The suite reads these in Node, which inherits them from this process.
        Environment.SetEnvironmentVariable("LAKEFS_INTEGRATION", "1")
        Environment.SetEnvironmentVariable("LAKEFS_INTEGRATION_ENDPOINT", LakeFsIntegration.endpoint)
        Environment.SetEnvironmentVariable("LAKEFS_INTEGRATION_ACCESS_KEY_ID", LakeFsIntegration.accessKeyId)

        Environment.SetEnvironmentVariable(
            "LAKEFS_INTEGRATION_SECRET_ACCESS_KEY",
            LakeFsIntegration.secretAccessKey
        )

        let exitCode, output = Test.Run.allCaptured ()

        if exitCode <> 0 then
            failwithf "The live lakeFS matrix failed with exit code %d." exitCode

        if output.Contains "lakeFS integration skipped" then
            failwith "The live lakeFS matrix reported a skipped profile."

        printGreenfn "The live lakeFS matrix passed with every profile enabled."
    finally
        Docker.remove containerName
