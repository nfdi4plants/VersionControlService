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

[<Emit("Buffer.from($0)")>]
let private bufferFromBytes (_bytes: int[]) : obj = jsNative

let private writeBinaryFileAsync (path: string) (bytes: int[]) : JS.Promise<unit> = promise {
    let parent = dirname path
    do! ensureDirectoryAsync parent
    let! _ = fsPromisesDynamic?writeFile (path, bufferFromBytes bytes) |> unbox<JS.Promise<obj>>
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

let private processOutput exitCode stdout stderr : NodeProcess.ProcessOutput = {
    ExitCode = exitCode
    StdOut = stdout
    StdErr = stderr
}

let private adoptionHooks (originResult: OperationResult<NodeProcess.ProcessOutput>) : GitWorkspaceSession.GitSessionHooks = {
    RunProcess =
        Some(fun request _ ->
            async {
                match request.Arguments with
                | [| "rev-parse"; "--show-toplevel" |] ->
                    return OperationResult.succeeded (processOutput 0 "C:/adoption-fixture\n" "")
                | [| "config"; "--get"; "remote.origin.url" |] -> return originResult
                | _ ->
                    return
                        OperationResult.failed(
                            OperationFailure.create ProviderError "unexpected_command" "Unexpected Git command."
                        )
            })
    Barrier = None
}

let private createBaseContentFixtureWithHooks (hooks: GitWorkspaceSession.GitSessionHooks) = promise {
    let! root = createTempDirectoryAsync ()
    let repoPath = join [| root; "work" |]
    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; repoPath |] None
    do! configureUser repoPath
    do! writeUtf8FileAsync (join [| repoPath; "base.txt" |]) "base content\n"
    do! writeUtf8FileAsync (join [| repoPath; "rename-source.txt" |]) "committed rename source\n"
    do! writeBinaryFileAsync (join [| repoPath; "binary.dat" |]) [| 0; 255; 1; 2; 3 |]
    let! _ = runGitIn repoPath [||] [| "add"; "-A" |] None
    let! _ = runGitIn repoPath [||] [| "commit"; "-m"; "test: base content" |] None

    do! writeUtf8FileAsync (join [| repoPath; "base.txt" |]) "modified content\n"
    let! _ = runGitIn repoPath [||] [| "mv"; "rename-source.txt"; "renamed.txt" |] None
    do! writeUtf8FileAsync (join [| repoPath; "added.txt" |]) "new content\n"
    do! writeBinaryFileAsync (join [| repoPath; "binary.dat" |]) [| 0; 254; 9; 8; 7 |]

    let binding: WorkspaceBinding = {
        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
        ProviderId = gitProviderId
        WorkspaceRoot = repoPath
        ProviderStateRef = None
        Location = {
            ProviderId = gitProviderId
            DisplayName = None
            ProviderLocation = repoPath
            ConnectionProfileId = None
        }
        ConnectionProfileId = None
    }

    return root, GitWorkspaceSession.createSession hooks binding
}

let private createBaseContentFixture () =
    createBaseContentFixtureWithHooks GitWorkspaceSession.GitSessionHooks.none

let private createWhitespaceBaseContentFixture () = promise {
    let! root = createTempDirectoryAsync ()
    let repoPath = join [| root; "work" |]
    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; repoPath |] None
    do! configureUser repoPath
    do! writeUtf8FileAsync (join [| repoPath; " leading.txt" |]) "leading base content\n"
    let! _ = runGitIn repoPath [||] [| "add"; "-A" |] None
    let! _ = runGitIn repoPath [||] [| "commit"; "-m"; "test: leading path" |] None

    let trailingPath = "trailing.txt "
    let renameSource = "rename-source.txt "
    let renamedPath = "renamed.txt "
    let! trailingBlob = runGitIn repoPath [||] [| "hash-object"; "-w"; "--stdin" |] (Some "trailing base content\n")
    let! renameBlob = runGitIn repoPath [||] [| "hash-object"; "-w"; "--stdin" |] (Some "rename base content\n")

    let! _ =
        runGitIn
            repoPath
            [||]
            [| "-c"; "core.protectNTFS=false"; "update-index"; "--add"; "--cacheinfo"; $"100644,{trailingBlob.Trim()},{trailingPath}" |]
            None

    let! _ =
        runGitIn
            repoPath
            [||]
            [| "-c"; "core.protectNTFS=false"; "update-index"; "--add"; "--cacheinfo"; $"100644,{renameBlob.Trim()},{renameSource}" |]
            None

    let! _ = runGitIn repoPath [||] [| "commit"; "-m"; "test: trailing paths" |] None
    do! writeUtf8FileAsync (join [| repoPath; " leading.txt" |]) "modified leading content\n"

    let zeroObjectId = "0000000000000000000000000000000000000000"
    let renameIndexPayload =
        $"0 {zeroObjectId}\t{renameSource}\000100644 {renameBlob.Trim()}\t{renamedPath}\000"

    let! _ =
        runGitIn
            repoPath
            [||]
            [| "-c"; "core.protectNTFS=false"; "update-index"; "-z"; "--index-info" |]
            (Some renameIndexPayload)

    let binding: WorkspaceBinding = {
        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
        ProviderId = gitProviderId
        WorkspaceRoot = repoPath
        ProviderStateRef = None
        Location = {
            ProviderId = gitProviderId
            DisplayName = None
            ProviderLocation = repoPath
            ConnectionProfileId = None
        }
        ConnectionProfileId = None
    }

    return root, GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding
}

let private textDiffService (session: WorkspaceSession) =
    match session.TextDiff with
    | Some service -> service
    | None -> failwith "Expected the Git text-diff service."

let private repositoryPath (value: string) =
    match RepositoryPath.tryCreate value with
    | Ok path -> path
    | Error message -> failwith message

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

        Vitest.test (
            "Git adoption preserves origin lookup failure semantics",
            fun () -> promise {
                let request = {
                    WorkspaceRoot = "C:/adoption-fixture"
                    ConnectionProfileId = None
                }

                let missingOriginFactory =
                    GitWorkspaceSession.createFactory(
                        adoptionHooks (OperationResult.succeeded (processOutput 1 "" ""))
                    )

                let! missingOriginResult =
                    Async.StartAsPromise(missingOriginFactory.Adopt request (OperationContext.detached "origin-missing"))

                let missingOrigin = expectProviderValue "adopt without origin" missingOriginResult
                Vitest.expect(missingOrigin.Location.ProviderLocation).toBe ("C:/adoption-fixture")

                let canceledFactory =
                    GitWorkspaceSession.createFactory(
                        adoptionHooks (OperationResult.canceled "The origin lookup was canceled.")
                    )

                let! canceledResult =
                    Async.StartAsPromise(canceledFactory.Adopt request (OperationContext.detached "origin-canceled"))

                let canceledFailure = expectProviderFailure "canceled origin lookup" canceledResult
                Vitest.expect(canceledFailure.Category).toEqual (Canceled)
                Vitest.expect(canceledFailure.Code).toBe ("operation_canceled")

                let processFailureFactory =
                    GitWorkspaceSession.createFactory(
                        adoptionHooks (
                            OperationResult.failed(
                                OperationFailure.create Network "origin_process_failed" "The origin lookup process failed."
                            )
                        )
                    )

                let! processFailureResult =
                    Async.StartAsPromise(processFailureFactory.Adopt request (OperationContext.detached "origin-process-failed"))

                let processFailure = expectProviderFailure "failed origin lookup process" processFailureResult
                Vitest.expect(processFailure.Category).toEqual (Network)
                Vitest.expect(processFailure.Code).toBe ("origin_process_failed")

                let unexpectedOutputFactory =
                    GitWorkspaceSession.createFactory(
                        adoptionHooks (OperationResult.succeeded (processOutput 2 "" "fatal: config is unreadable"))
                    )

                let! unexpectedOutputResult =
                    Async.StartAsPromise(unexpectedOutputFactory.Adopt request (OperationContext.detached "origin-unexpected-output"))

                let unexpectedOutputFailure =
                    expectProviderFailure "unexpected origin lookup output" unexpectedOutputResult

                Vitest.expect(unexpectedOutputFailure.Category).toEqual (ProviderError)
                Vitest.expect(unexpectedOutputFailure.Code).toBe ("origin_lookup_failed")
            }
        )
)

Vitest.describe (
    "Git base content",
    fun () ->
        Vitest.test (
            "returns the committed text for a modified path",
            fun () -> promise {
                let! root, session = createBaseContentFixture ()

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "base.txt")
                            (OperationContext.detached "base-modified")
                        |> Async.StartAsPromise

                    match expectProviderValue "modified base content" result with
                    | TextContent text -> Vitest.expect(text).toBe ("base content\n")
                    | UnsupportedContent _ -> failwith "Expected text base content."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "follows a rename to the committed old path",
            fun () -> promise {
                let! root, session = createBaseContentFixture ()

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "renamed.txt")
                            (OperationContext.detached "base-renamed")
                        |> Async.StartAsPromise

                    match expectProviderValue "renamed base content" result with
                    | TextContent text -> Vitest.expect(text).toBe ("committed rename source\n")
                    | UnsupportedContent _ -> failwith "Expected text base content."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "reports a path absent from the committed base as not found",
            fun () -> promise {
                let! root, session = createBaseContentFixture ()

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "added.txt")
                            (OperationContext.detached "base-absent")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "absent base content" result
                    Vitest.expect(failure.Category).toEqual (NotFound)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "returns unsupported content for committed binary bytes",
            fun () -> promise {
                let! root, session = createBaseContentFixture ()

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "binary.dat")
                            (OperationContext.detached "base-binary")
                        |> Async.StartAsPromise

                    match expectProviderValue "binary base content" result with
                    | UnsupportedContent reason ->
                        Vitest
                            .expect(reason |> Option.exists (fun value -> value.Contains "binary.dat"))
                            .toBe (true)
                    | TextContent text -> failwith $"Expected unsupported binary content, received text '{text}'."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

Vitest.describe (
    "Git base content hardening",
    fun () ->
        let expectTextBase expected operationName result =
            match expectProviderValue operationName result with
            | TextContent text -> Vitest.expect(text).toBe (expected)
            | UnsupportedContent _ -> failwith $"Expected text content for {operationName}."

        Vitest.test (
            "preserves a literal leading-space path",
            fun () -> promise {
                let! root, session = createWhitespaceBaseContentFixture ()

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath " leading.txt")
                            (OperationContext.detached "base-leading-space")
                        |> Async.StartAsPromise

                    expectTextBase "leading base content\n" "leading-space base content" result
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "preserves a literal trailing-space path",
            fun () -> promise {
                let! root, session = createWhitespaceBaseContentFixture ()

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "trailing.txt ")
                            (OperationContext.detached "base-trailing-space")
                        |> Async.StartAsPromise

                    expectTextBase "trailing base content\n" "trailing-space base content" result
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "follows an exact trailing-space rename without trimming status tokens",
            fun () -> promise {
                let! root, session = createWhitespaceBaseContentFixture ()

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "renamed.txt ")
                            (OperationContext.detached "base-whitespace-rename")
                        |> Async.StartAsPromise

                    expectTextBase "rename base content\n" "whitespace rename base content" result
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "preserves cancellation before the base-status subprocess",
            fun () -> promise {
                let! root, session = createBaseContentFixture ()
                let source = OperationCancellation.Source()
                source.Cancel()

                try
                    let context = OperationContext.create "base-canceled-status" source.Cancellation ignore

                    let! result =
                        (textDiffService session).GetBaseContent (repositoryPath "base.txt") context
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "canceled base status" result
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.Code).toBe ("operation_canceled")
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "preserves cancellation between base status and show",
            fun () -> promise {
                let source = OperationCancellation.Source()

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunProcess =
                        Some(fun request context ->
                            async {
                                let! result = NodeProcess.run request context

                                if request.Arguments |> Array.contains "--numstat" then
                                    source.Cancel()

                                return result
                            })
                    Barrier = None
                }

                let! root, session = createBaseContentFixtureWithHooks hooks

                try
                    let context = OperationContext.create "base-canceled-show" source.Cancellation ignore

                    let! result =
                        (textDiffService session).GetBaseContent (repositoryPath "base.txt") context
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "canceled base show" result
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.Code).toBe ("operation_canceled")
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)
