module VersionControlService.Tests.GitProviderContractTests

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.Tests.Contracts
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.NodePath
open Vitest

module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module NodeProcess = VersionControlService.Runtime.Node.Process

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

let private createTempDirectoryAsync () : JS.Promise<string> =
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-git-harness-" |]
    fsPromisesDynamic?mkdtemp (prefix) |> unbox<JS.Promise<string>>

let private removeDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (path, createObj [ "recursive" ==> true; "force" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private ensureDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?mkdir (path, createObj [ "recursive" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private writeUtf8FileAsync (path: string) (content: string) : JS.Promise<unit> = promise {
    let parent = dirname path
    do! ensureDirectoryAsync parent
    let! _ = fsPromisesDynamic?writeFile (path, content, "utf8") |> unbox<JS.Promise<obj>>
    return ()
}

let private tryReadUtf8FileAsync (path: string) : JS.Promise<string option> = promise {
    try
        let! content = fsPromisesDynamic?readFile (path, "utf8") |> unbox<JS.Promise<string>>
        return Some content
    with _ ->
        return None
}

/// Captures raw bytes as base64 so adoption tests can assert Git configuration
/// is untouched, rather than merely textually equivalent.
let private readFileBase64Async (path: string) : JS.Promise<string> = promise {
    let! bytes = fsPromisesDynamic?readFile (path) |> unbox<JS.Promise<obj>>
    return bytes?toString("base64") |> unbox<string>
}

let private removeFileAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (path, createObj [ "force" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private renameAsync (fromPath: string) (toPath: string) : JS.Promise<unit> = promise {
    let! _ = fsPromisesDynamic?rename (fromPath, toPath) |> unbox<JS.Promise<obj>>
    return ()
}

/// Direct git invocation for harness fixture work (never through the session).
let private runGitIn
    (cwd: string)
    (environment: (string * string)[])
    (arguments: string[])
    (stdinData: string option)
    : JS.Promise<string> =
    promise {
        let request = {
            NodeProcess.ProcessRequest.create "git" arguments with
                WorkingDirectory = Some cwd
                StdinData = stdinData
                Environment = environment
        }

        let! result = Async.StartAsPromise(NodeProcess.run request (OperationContext.detached "git-harness"))

        match result with
        | Succeeded outcome when outcome.Value.ExitCode = 0 -> return outcome.Value.StdOut
        | Succeeded outcome ->
            let command = String.concat " " arguments
            return failwith $"harness git {command} failed: {outcome.Value.StdErr}"
        | PartiallySucceeded _
        | Failed _ -> return failwith "harness git invocation failed"
    }

let private configureUser (repoPath: string) = promise {
    let! _ = runGitIn repoPath [||] [| "config"; "user.name"; "VCS Harness" |] None
    let! _ = runGitIn repoPath [||] [| "config"; "user.email"; "harness@example.org" |] None
    let! _ = runGitIn repoPath [||] [| "config"; "core.autocrlf"; "false" |] None
    return ()
}

let private gitProviderId =
    match ProviderId.tryCreate "git" with
    | Ok providerId -> providerId
    | Error message -> failwith message

type private WorkspaceBarriers = {
    BarePath: string
    mutable RaceMutations: TargetMutation[] option
    mutable SlowTransfer: bool
}

/// Builds the real Git harness over isolated temp repositories with a local bare
/// target per workspace.
let createGitHarness () : ProviderTestHarness =
    let tempRoots = ResizeArray<string>()
    let barriersByRoot = System.Collections.Generic.Dictionary<string, WorkspaceBarriers>()
    let mutable counter = 0

    let nextId () =
        counter <- counter + 1
        counter

    /// Applies mutations to a bare target through a scratch clone, like another client.
    let advanceBare (barePath: string) (mutations: TargetMutation[]) = promise {
        let! scratchRoot = createTempDirectoryAsync ()
        tempRoots.Add scratchRoot
        let clonePath = join [| scratchRoot; "advance" |]
        let! _ = runGitIn scratchRoot [||] [| "clone"; barePath; clonePath |] None
        do! configureUser clonePath

        for mutation in mutations do
            match mutation.Content with
            | Some content -> do! writeUtf8FileAsync (join [| clonePath; mutation.Path |]) content
            | None -> do! removeFileAsync (join [| clonePath; mutation.Path |])

        let! _ = runGitIn clonePath [||] [| "add"; "-A" |] None
        let! _ = runGitIn clonePath [||] [| "commit"; "-m"; "external: target advance" |] None
        let! _ = runGitIn clonePath [||] [| "push"; "origin"; "HEAD" |] None
        return ()
    }

    /// Moves the local current branch by one plumbing commit, like another process.
    let advanceLocalBranch (workspaceRoot: string) = promise {
        let! branchRef = runGitIn workspaceRoot [||] [| "symbolic-ref"; "-q"; "HEAD" |] None
        let! tree = runGitIn workspaceRoot [||] [| "rev-parse"; "HEAD^{tree}" |] None
        let! head = runGitIn workspaceRoot [||] [| "rev-parse"; "HEAD" |] None

        let! dummy =
            runGitIn
                workspaceRoot
                [||]
                [|
                    "commit-tree"
                    tree.Trim()
                    "-p"
                    head.Trim()
                    "-m"
                    "race: concurrent local advance"
                |]
                None

        let! _ =
            runGitIn workspaceRoot [||] [| "update-ref"; branchRef.Trim(); dummy.Trim() |] None

        return ()
    }

    let hooks: GitWorkspaceSession.GitSessionHooks = {
        RunProcess = None
        Barrier =
            Some(fun root point context ->
                async {
                    match barriersByRoot.TryGetValue root with
                    | true, barriers ->
                        if point = "publish-precheck-done" then
                            match barriers.RaceMutations with
                            | Some mutations ->
                                barriers.RaceMutations <- None
                                do! Async.AwaitPromise(advanceBare barriers.BarePath mutations)
                            | None -> ()
                        elif point = "finalize-precheck-done" then
                            // Git's finalize destination is the local branch, so the
                            // armed race moves the local ref, not the bare target.
                            match barriers.RaceMutations with
                            | Some _ ->
                                barriers.RaceMutations <- None
                                do! Async.AwaitPromise(advanceLocalBranch root)
                            | None -> ()
                        elif point = "transfer-start" && barriers.SlowTransfer then
                            barriers.SlowTransfer <- false
                            let mutable step = 0

                            while step < 200 && not (context.Cancellation.IsCancellationRequested()) do
                                do! Async.Sleep 2
                                step <- step + 1

                                context.ReportProgress {
                                    PhaseCode = "transfer"
                                    Item = None
                                    Completed = Some(float step)
                                    Total = Some 200.0
                                    DisplayMessage = Some "Transferring objects"
                                }
                    | false, _ -> ()
                })
    }

    let factory = GitWorkspaceSession.createFactory hooks

    let locationFor (barePath: string) (profile: string option) : RepositoryLocation = {
        ProviderId = gitProviderId
        DisplayName = None
        ProviderLocation = barePath
        ConnectionProfileId = profile
    }

    let bindingFor (workspaceRoot: string) (location: RepositoryLocation) : WorkspaceBinding = {
        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
        ProviderId = gitProviderId
        WorkspaceRoot = workspaceRoot
        ProviderStateRef = None
        Location = location
        ConnectionProfileId = location.ConnectionProfileId
    }

    let openHarnessWorkspace (binding: WorkspaceBinding) (barePath: string) = promise {
        let! openResult =
            Async.StartAsPromise(factory.Open binding (OperationContext.detached "git-harness-open"))

        let session =
            match openResult with
            | Succeeded outcome -> outcome.Value
            | PartiallySucceeded _
            | Failed _ -> failwith "The Git factory failed to open a session."

        barriersByRoot[binding.WorkspaceRoot] <- {
            BarePath = barePath
            RaceMutations = None
            SlowTransfer = false
        }

        return {
            Session = session
            Binding = binding
            WriteFile = fun path content -> writeUtf8FileAsync (join [| binding.WorkspaceRoot; path |]) content
            ReadFile = fun path -> tryReadUtf8FileAsync (join [| binding.WorkspaceRoot; path |])
            RemoveFile = fun path -> removeFileAsync (join [| binding.WorkspaceRoot; path |])
            // Windows NTFS: case-insensitive, normalization-sensitive, Windows name rules.
            LocalFileSystemAliasesCase = true
            LocalFileSystemAliasesNormalization = false
            LocalFileSystemWindowsRules = true
        }
    }

    let createWorkspace () = promise {
        let! root = createTempDirectoryAsync ()
        tempRoots.Add root
        let barePath = join [| root; "origin.git" |]
        let workPath = join [| root; "work" |]

        let! _ = runGitIn root [||] [| "init"; "--bare"; "-b"; "main"; barePath |] None
        let! _ = runGitIn root [||] [| "init"; "-b"; "main"; workPath |] None
        do! configureUser workPath
        do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "base content\n"
        let! _ = runGitIn workPath [||] [| "add"; "-A" |] None
        let! _ = runGitIn workPath [||] [| "commit"; "-m"; "init: base" |] None
        let! _ = runGitIn workPath [||] [| "remote"; "add"; "origin"; barePath |] None
        let! _ = runGitIn workPath [||] [| "push"; "-u"; "origin"; "main" |] None

        let binding = bindingFor workPath (locationFor barePath (Some "default-profile"))
        return! openHarnessWorkspace binding barePath
    }

    {
        Name = "Git"
        Factory = factory
        ExpectedServices = [
            "synchronization"
            "text-diff"
            "conflicts"
            "materialization"
            "storage-policy"
            "maintenance"
            "browser"
        ]
        CreateLocation =
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                tempRoots.Add root
                let barePath = join [| root; $"location-{nextId ()}.git" |]
                let! _ = runGitIn root [||] [| "init"; "--bare"; "-b"; "main"; barePath |] None
                return locationFor barePath None
            }
        CreateUnauthorizedLocation =
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                tempRoots.Add root
                return locationFor (join [| root; "missing-remote.git" |]) None
            }
        CreateLocalPath =
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                tempRoots.Add root
                return join [| root; "fresh" |]
            }
        CreateWorkspace = createWorkspace
        CreateLinkedWorkspace =
            fun anchor -> promise {
                let! root = createTempDirectoryAsync ()
                tempRoots.Add root
                let linkedPath = join [| root; "linked" |]
                let barePath = anchor.Binding.Location.ProviderLocation

                let! _ =
                    runGitIn
                        root
                        [||]
                        [|
                            "clone"
                            "-c"
                            "core.autocrlf=false"
                            barePath
                            linkedPath
                        |]
                        None
                do! configureUser linkedPath

                let location = {
                    anchor.Binding.Location with
                        ConnectionProfileId = Some $"linked-profile-{nextId ()}"
                }

                let binding = bindingFor linkedPath location
                return! openHarnessWorkspace binding barePath
            }
        OpenSecondSession =
            fun anchor -> promise {
                let! openResult =
                    Async.StartAsPromise(factory.Open anchor.Binding (OperationContext.detached "git-second-session"))

                match openResult with
                | Succeeded outcome -> return outcome.Value
                | PartiallySucceeded _
                | Failed _ -> return failwith "The Git factory failed to open a second session."
            }
        AdvanceTarget =
            fun workspace mutations -> advanceBare workspace.Binding.Location.ProviderLocation mutations
        SeedRevisionOnNewRef =
            fun workspace refName files -> promise {
                let workPath = workspace.Binding.WorkspaceRoot
                let! root = createTempDirectoryAsync ()
                tempRoots.Add root
                let indexPath = join [| root; "seed-index" |]
                let environment = [| "GIT_INDEX_FILE", indexPath |]

                let! _ = runGitIn workPath environment [| "read-tree"; "HEAD" |] None

                for path, content in files do
                    let! objectId = runGitIn workPath [||] [| "hash-object"; "-w"; "--stdin" |] (Some content)

                    let! _ =
                        runGitIn
                            workPath
                            environment
                            [|
                                "-c"
                                "core.protectNTFS=false"
                                "update-index"
                                "--add"
                                "--cacheinfo"
                                $"100644,{objectId.Trim()},{path}"
                            |]
                            None

                    ()

                let! treeId = runGitIn workPath environment [| "write-tree" |] None

                let! commitId =
                    runGitIn
                        workPath
                        [||]
                        [|
                            "commit-tree"
                            treeId.Trim()
                            "-p"
                            "HEAD"
                            "-m"
                            $"seed: {refName}"
                        |]
                        None

                let! _ = runGitIn workPath [||] [| "branch"; refName; commitId.Trim() |] None

                match ProviderRef.tryCreate $"git-local:{refName}" with
                | Ok reference -> return reference
                | Error message -> return failwith message
            }
        BreakPublish =
            fun workspace -> promise {
                let barePath = workspace.Binding.Location.ProviderLocation
                do! renameAsync barePath (barePath + ".offline")
                return ()
            }
        RestorePublish =
            fun workspace -> promise {
                let barePath = workspace.Binding.Location.ProviderLocation
                do! renameAsync (barePath + ".offline") barePath
                return ()
            }
        ArmDestinationRace =
            fun workspace mutations -> promise {
                barriersByRoot[workspace.Binding.WorkspaceRoot].RaceMutations <- Some mutations
                return ()
            }
        ArmSlowTransfer =
            fun workspace -> promise {
                barriersByRoot[workspace.Binding.WorkspaceRoot].SlowTransfer <- true
                return ()
            }
        Cleanup =
            fun () -> promise {
                for root in tempRoots do
                    do! removeDirectoryAsync root

                tempRoots.Clear()
                barriersByRoot.Clear()
                return ()
            }
    }

// Register the Git harness with all seven shared conformance profiles under the
// exact reserved describe names ("Git / core profile", ...). The initially failing
// tests are the recorded Task 8 profile baseline, burned down by cycles 8.1-8.6
// and Tasks 9-10.
let private gitHarness = createGitHarness ()

let private registrations = [|
    CoreProviderSuite.register gitHarness
    SynchronizationProviderSuite.register gitHarness
    ConflictProviderSuite.register gitHarness
    ProvisioningProviderSuite.register gitHarness
    OperationalProviderSuite.register gitHarness
    ExtensionProviderSuites.register gitHarness
    SwateSelectableSuite.register gitHarness
|]

let private expectProviderValue (operationName: string) (result: OperationResult<'T>) : 'T =
    match result with
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded(_, failure) ->
        failwith $"{operationName} unexpectedly returned partial success ({failure.Code})."
    | Failed failure ->
        failwith $"{operationName} failed ({failure.Category}/{failure.Code}): {failure.Message}"

let private expectProviderFailure (operationName: string) (result: OperationResult<'T>) : OperationFailure =
    match result with
    | Failed failure -> failure
    | Succeeded _
    | PartiallySucceeded _ -> failwith $"Expected {operationName} to fail."

Vitest.describe (
    "Git workspace adoption",
    fun () ->
        Vitest.test (
            "Git adopts an existing repository",
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let remotePath = join [| root; "origin.git" |]
                    let repoPath = join [| root; "existing" |]
                    let localOnlyPath = join [| root; "local-only" |]

                    let! _ = runGitIn root [||] [| "init"; "--bare"; "-b"; "main"; remotePath |] None
                    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; repoPath |] None
                    do! configureUser repoPath
                    do! writeUtf8FileAsync (join [| repoPath; "base.txt" |]) "base\n"
                    let! _ = runGitIn repoPath [||] [| "add"; "-A" |] None
                    let! _ = runGitIn repoPath [||] [| "commit"; "-m"; "test: base" |] None
                    let! _ = runGitIn repoPath [||] [| "remote"; "add"; "origin"; remotePath |] None
                    let! expectedRemoteLocation =
                        runGitIn repoPath [||] [| "config"; "--get"; "remote.origin.url" |] None

                    let configPath = join [| repoPath; ".git"; "config" |]
                    let! configBefore = readFileBase64Async configPath
                    let factory = GitWorkspaceSession.createFactory GitWorkspaceSession.GitSessionHooks.none

                    let! adoptionResult =
                        Async.StartAsPromise(
                            factory.Adopt
                                {
                                    WorkspaceRoot = repoPath
                                    ConnectionProfileId = Some "account-a"
                                }
                                (OperationContext.detached "adopt-remote")
                        )

                    let binding = expectProviderValue "adopt existing Git repository" adoptionResult
                    Vitest.expect(binding.WorkspaceRoot).toBe (repoPath.Replace("\\", "/"))
                    Vitest.expect(binding.Location.ProviderLocation).toBe (expectedRemoteLocation.Trim())
                    Vitest.expect(binding.ConnectionProfileId).toEqual (Some "account-a")
                    Vitest.expect(binding.Location.ConnectionProfileId).toEqual (Some "account-a")

                    let! configAfter = readFileBase64Async configPath
                    Vitest.expect(configAfter).toBe (configBefore)

                    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; localOnlyPath |] None
                    do! writeUtf8FileAsync (join [| localOnlyPath; "uncommitted.txt" |]) "local change\n"

                    let! localAdoptionResult =
                        Async.StartAsPromise(
                            factory.Adopt
                                {
                                    WorkspaceRoot = localOnlyPath
                                    ConnectionProfileId = None
                                }
                                (OperationContext.detached "adopt-local")
                        )

                    let localBinding = expectProviderValue "adopt remote-less Git repository" localAdoptionResult
                    Vitest.expect(localBinding.WorkspaceRoot).toBe (localOnlyPath.Replace("\\", "/"))
                    Vitest.expect(localBinding.Location.ProviderLocation).toBe (localOnlyPath.Replace("\\", "/"))

                    let! opened =
                        Async.StartAsPromise(factory.Open localBinding (OperationContext.detached "open-adopted-local"))

                    let session = expectProviderValue "open adopted local repository" opened
                    let! statusResult =
                        Async.StartAsPromise(session.Core.GetStatus(OperationContext.detached "adopted-local-status"))

                    let status = expectProviderValue "adopted local status" statusResult
                    Vitest.expect(status.Changes |> Array.map (fun change -> RepositoryPath.value change.Path)).toEqual ([| "uncommitted.txt" |])

                    match status.Synchronization with
                    | Some synchronization -> Vitest.expect(synchronization.Relationship).toEqual (NoTarget)
                    | None -> failwith "Expected synchronization information for the adopted local repository."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "Git initialization preserves an existing non-Git directory and reports an already initialized repository",
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let nonGitPath = join [| root; "non-git" |]
                    let existingGitPath = join [| root; "already-git" |]
                    let filePath = join [| nonGitPath; "uncommitted.txt" |]
                    do! writeUtf8FileAsync filePath "uncommitted bytes\n"
                    let! bytesBefore = readFileBase64Async filePath
                    let factory = GitWorkspaceSession.createFactory GitWorkspaceSession.GitSessionHooks.none

                    let! initializationResult =
                        Async.StartAsPromise(
                            factory.Initialize
                                {
                                    TargetPath = nonGitPath
                                    Location = None
                                }
                                (OperationContext.detached "initialize-non-git")
                        )

                    let binding = expectProviderValue "initialize non-Git directory" initializationResult
                    let! bytesAfter = readFileBase64Async filePath
                    Vitest.expect(bytesAfter).toBe (bytesBefore)

                    let! opened =
                        Async.StartAsPromise(factory.Open binding (OperationContext.detached "open-initialized-non-git"))

                    let session = expectProviderValue "open initialized non-Git directory" opened
                    let! statusResult =
                        Async.StartAsPromise(session.Core.GetStatus(OperationContext.detached "initialized-non-git-status"))

                    let status = expectProviderValue "initialized non-Git status" statusResult
                    Vitest.expect(status.Changes |> Array.map (fun change -> RepositoryPath.value change.Path)).toEqual ([| "uncommitted.txt" |])

                    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; existingGitPath |] None

                    let! existingResult =
                        Async.StartAsPromise(
                            factory.Initialize
                                {
                                    TargetPath = existingGitPath
                                    Location = None
                                }
                                (OperationContext.detached "initialize-existing-git")
                        )

                    let existingFailure = expectProviderFailure "initialize existing Git repository" existingResult
                    Vitest.expect(existingFailure.Category).toEqual (Validation)
                    Vitest.expect(existingFailure.Code).toBe ("already_initialized")

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)
