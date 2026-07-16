module VersionControlService.Tests.GitProviderContractTests

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.Contracts.Git
open VersionControlService.Tests.Contracts
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.NodePath
open Vitest

module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module GitLfsExtensions = VersionControlService.Git.GitLfsExtensions
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
        RunBytesProcess = None
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

                            context.ReportProgress {
                                PhaseCode = "transfer-bytes"
                                Item = Some "literal[object]*?.bin"
                                Completed = Some(3.0 * 1024.0 * 1024.0 * 1024.0)
                                Total = Some(4.0 * 1024.0 * 1024.0 * 1024.0)
                                DisplayMessage = Some "Transferring large object"
                            }

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
    "Git / Swate-selectable profile",
    fun () ->
        Vitest.test (
            "configured upstream target identity is exposed through the neutral synchronization state",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "configured-target-ref")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "configured target status" statusResult

                    let target =
                        status.Synchronization
                        |> Option.bind _.TargetRef
                        |> Option.defaultWith (fun () -> failwith "Expected the configured Git upstream target.")

                    Vitest.expect(target.Name).toBe "origin/main"
                    Vitest.expect(ProviderRef.value target.ProviderRef).toBe "git-remote:origin/main"
                    Vitest.expect(target.Kind).toEqual RemoteRef
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "refresh fetches the remote named by the configured upstream",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "remote"; "rename"; "origin"; "backup" |]
                            None

                    do!
                        harness.AdvanceTarget workspace [|
                            {
                                Path = "backup-target.txt"
                                Content = Some "advanced through backup\n"
                            }
                        |]

                    let synchronization =
                        workspace.Session.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected Git synchronization.")

                    let! refreshResult =
                        synchronization.Refresh(OperationContext.detached "refresh-configured-remote")
                        |> Async.StartAsPromise

                    let refreshed = expectProviderValue "refresh configured upstream remote" refreshResult
                    let target =
                        refreshed.TargetRef
                        |> Option.defaultWith (fun () -> failwith "Expected configured backup target.")

                    Vitest.expect(target.Name).toBe "backup/main"
                    Vitest.expect(refreshed.TargetRevision.IsSome).toBe true

                    let changedPaths =
                        refreshed.RemoteChangedPaths
                        |> Option.defaultWith (fun () -> failwith "Expected refreshed target changes.")
                        |> Array.map RepositoryPath.value

                    Vitest.expect(changedPaths |> Array.contains "backup-target.txt").toBe true
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "a branch without an upstream exposes no neutral synchronization target",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "branch"; "--unset-upstream" |] None

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "missing-target-ref")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "missing target status" statusResult
                    let synchronization =
                        status.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected Git synchronization state.")

                    Vitest.expect(synchronization.TargetRef).toEqual None
                    Vitest.expect(synchronization.TargetRevision).toEqual None
                    Vitest.expect(synchronization.LocalRevisionCount).toEqual None
                    Vitest.expect(synchronization.TargetRevisionCount).toEqual None
                    Vitest.expect(synchronization.Relationship).toEqual NoTarget
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "a detached workspace exposes no neutral synchronization target",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "checkout"; "--detach" |] None

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "detached-target-ref")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "detached target status" statusResult
                    let synchronization =
                        status.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected Git synchronization state.")

                    Vitest.expect(status.CurrentRef).toEqual None
                    Vitest.expect(synchronization.WorkspaceRevision.IsSome).toBe true
                    Vitest.expect(synchronization.TargetRef).toEqual None
                    Vitest.expect(synchronization.TargetRevision).toEqual None
                    Vitest.expect(synchronization.LocalRevisionCount).toEqual None
                    Vitest.expect(synchronization.TargetRevisionCount).toEqual None
                    Vitest.expect(synchronization.Relationship).toEqual NoTarget
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

let private processOutput exitCode stdout stderr : NodeProcess.ProcessOutput = {
    ExitCode = exitCode
    StdOut = stdout
    StdErr = stderr
}

Vitest.describe (
    "Git / extension suites",
    fun () ->
        Vitest.test (
            "storage policy treats repository paths with glob metacharacters literally",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()

                    let storagePolicy =
                        workspace.Session.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    let literalPaths = [|
                        "space name.bin", "spaceXname.bin", "nested/space name.bin"
                        "literal[meta].bin", "literalm.bin", "nested/literal[meta].bin"
                        "literal*.star", "literalX.star", "nested/literal*.star"
                        "literal?.question", "literalX.question", "nested/literal?.question"
                        "tab\tname.bin", "tab name.bin", "nested/tab\tname.bin"
                        "line\nname.bin", "lineXname.bin", "nested/line\nname.bin"
                    |]

                    let repositoryPath value =
                        RepositoryPath.tryCreate value
                        |> Result.defaultWith failwith

                    // RepositoryPath normalizes provider paths to forward slashes;
                    // a backslash therefore never reaches SetPathPolicy.
                    Vitest.expect(RepositoryPath.tryCreate "literal\\path.bin" |> Result.isError).toBe true

                    for literalPath, _, _ in literalPaths do
                        let! result =
                            storagePolicy.SetPathPolicy
                                (repositoryPath literalPath)
                                true
                                (OperationContext.detached $"track-{literalPath}")
                            |> Async.StartAsPromise

                        expectProviderValue $"track literal path {literalPath}" result |> ignore

                    for literalPath, decoyPath, nestedDecoyPath in literalPaths do
                        let! attributes =
                            runGitIn
                                workspace.Binding.WorkspaceRoot
                                [||]
                                [| "check-attr"; "-z"; "filter"; "--"; literalPath; decoyPath; nestedDecoyPath |]
                                None

                        Vitest.expect(attributes.Contains($"{literalPath}\000filter\000lfs\000")).toBe true
                        Vitest.expect(attributes.Contains($"{decoyPath}\000filter\000unspecified\000")).toBe true
                        Vitest.expect(attributes.Contains($"{nestedDecoyPath}\000filter\000unspecified\000")).toBe true

                    let! attributeFile =
                        tryReadUtf8FileAsync (join [| workspace.Binding.WorkspaceRoot; ".gitattributes" |])

                    let attributeFile = attributeFile |> Option.defaultValue ""
                    Vitest.expect(attributeFile.Contains("\"/space name.bin\" filter=lfs")).toBe true
                    Vitest.expect(attributeFile.Contains("\"/literal\\\\[meta\\\\].bin\" filter=lfs")).toBe true
                    Vitest.expect(attributeFile.Contains("\"/literal\\\\*.star\" filter=lfs")).toBe true
                    Vitest.expect(attributeFile.Contains("\"/literal\\\\?.question\" filter=lfs")).toBe true
                    Vitest.expect(attributeFile.Contains("\"/tab\\tname.bin\" filter=lfs")).toBe true
                    Vitest.expect(attributeFile.Contains("\"/line\\nname.bin\" filter=lfs")).toBe true

                    for literalPath, _, _ in literalPaths do
                        let! result =
                            storagePolicy.SetPathPolicy
                                (repositoryPath literalPath)
                                false
                                (OperationContext.detached $"untrack-{literalPath}")
                            |> Async.StartAsPromise

                        expectProviderValue $"untrack literal path {literalPath}" result |> ignore

                    for literalPath, _, nestedDecoyPath in literalPaths do
                        let! attributes =
                            runGitIn
                                workspace.Binding.WorkspaceRoot
                                [||]
                                [| "check-attr"; "-z"; "filter"; "--"; literalPath; nestedDecoyPath |]
                                None

                        Vitest.expect(attributes.Contains($"{literalPath}\000filter\000unspecified\000")).toBe true
                        Vitest.expect(attributes.Contains($"{nestedDecoyPath}\000filter\000unspecified\000")).toBe true

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "maintenance honors cancellation requested by its first progress report",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()

                    let maintenance =
                        workspace.Session.Maintenance
                        |> Option.defaultWith (fun () -> failwith "Expected Git maintenance.")

                    let cancellation = OperationCancellation.Source()
                    let mutable progressObserved = false

                    let context =
                        OperationContext.create
                            "cancel-running-maintenance"
                            cancellation.Cancellation
                            (fun progress ->
                                if not progressObserved then
                                    progressObserved <- true
                                    cancellation.Cancel())

                    let! result = maintenance.Deduplicate context |> Async.StartAsPromise

                    Vitest.expect(progressObserved).toBe true
                    let failure = expectProviderFailure "canceled running maintenance" result
                    Vitest.expect(failure.Category).toEqual Canceled
                    Vitest.expect(failure.Code).toBe "operation_canceled"
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "maintenance preserves provider progress totals above two GiB",
            fun () -> promise {
                let completedBytes = 3.0 * 1024.0 * 1024.0 * 1024.0
                let totalBytes = 4.0 * 1024.0 * 1024.0 * 1024.0

                let successfulOperation
                    (progress: (GitProgressDto -> unit) option)
                    (_: unit -> bool)
                    (onStarted: unit -> unit)
                    =
                    promise {
                        onStarted ()

                        progress
                        |> Option.iter (fun report ->
                            report {
                                Method = Some "deduplicate"
                                Stage = Some "objects"
                                Progress = Some 75.0
                                Processed = Some completedBytes
                                Total = Some totalBytes
                                Output = Some "Deduplicating large objects"
                            })

                        return Ok "deduplicated"
                    }

                let maintenance =
                    GitLfsExtensions.createMaintenanceWithOperations {
                        Prune = successfulOperation
                        Deduplicate = successfulOperation
                    }

                let reports = ResizeArray<OperationProgress>()
                let context =
                    OperationContext.create "large-maintenance-progress" OperationCancellation.none reports.Add

                let! result = maintenance.Deduplicate context |> Async.StartAsPromise
                expectProviderValue "large maintenance progress" result |> ignore

                let largeReport =
                    reports
                    |> Seq.find (fun report -> report.Completed.IsSome)

                Vitest.expect(largeReport.PhaseCode).toBe "maintenance-deduplicate"
                Vitest.expect(largeReport.Completed).toEqual (Some completedBytes)
                Vitest.expect(largeReport.Total).toEqual (Some totalBytes)
            }
        )

        Vitest.test (
            "cached LFS object availability is independent from literal-path materialization",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let literalPath = "literal[meta].bin"
                    let attributes = "/literal[[]meta].bin filter=lfs diff=lfs merge=lfs -text\n"
                    do! workspace.WriteFile ".gitattributes" attributes
                    do! workspace.WriteFile literalPath "cached object content\n"

                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "--literal-pathspecs"; "add"; "--"; ".gitattributes"; literalPath |]
                            None

                    let! _ =
                        runGitIn workspace.Binding.WorkspaceRoot [||] [| "commit"; "-m"; "test: cached lfs object" |] None

                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "push"; "origin"; "main" |] None

                    let! pointer =
                        runGitIn workspace.Binding.WorkspaceRoot [||] [| "show"; $"HEAD:{literalPath}" |] None

                    do! workspace.WriteFile literalPath pointer

                    let materialization =
                        workspace.Session.ObjectMaterialization
                        |> Option.defaultWith (fun () -> failwith "Expected Git object materialization.")

                    let! listResult =
                        materialization.ListObjects(OperationContext.detached "cached-literal-list")
                        |> Async.StartAsPromise

                    let objectState =
                        expectProviderValue "list cached literal LFS object" listResult
                        |> Array.find (fun item -> RepositoryPath.value item.Path = literalPath)

                    Vitest.expect(RepositoryPath.value objectState.Path).toBe literalPath
                    Vitest.expect(objectState.IsMaterialized).toBe false
                    Vitest.expect(objectState.IsLocallyAvailable).toBe true
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "preview classification failures are retryable indeterminate results",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    do! workspace.WriteFile "local.txt" "local revision\n"

                    let! beforeRevisionResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "preview-local-status")
                        |> Async.StartAsPromise

                    let beforeRevision = expectProviderValue "preview local status" beforeRevisionResult
                    let localPath =
                        RepositoryPath.tryCreate "local.txt"
                        |> Result.defaultWith failwith

                    let! localRevisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "local preview revision"
                                Paths = [| localPath |]
                                ExpectedWorkspaceVersion = beforeRevision.WorkspaceVersion
                            }
                            (OperationContext.detached "preview-local-revision")
                        |> Async.StartAsPromise

                    expectProviderValue "create local preview revision" localRevisionResult |> ignore

                    do!
                        harness.AdvanceTarget workspace [|
                            {
                                Path = "target.txt"
                                Content = Some "target revision\n"
                            }
                        |]

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess =
                            Some(fun request context ->
                                if request.Arguments |> Array.contains "merge-tree" then
                                    async {
                                        return
                                            OperationResult.failed(
                                                OperationFailure.create
                                                    ProviderError
                                                    "merge_tree_transport_failed"
                                                    "The merge-tree provider call failed."
                                            )
                                    }
                                else
                                    NodeProcess.run request context)
                        Barrier = None
                    }

                    let session = GitWorkspaceSession.createSession hooks workspace.Binding
                    let synchronization =
                        session.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected Git synchronization.")

                    let! previewResult =
                        synchronization.PreviewUpdate(OperationContext.detached "preview-indeterminate")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "indeterminate update preview" previewResult
                    Vitest.expect(failure.Category).toEqual ProviderError
                    Vitest.expect(failure.Code).toBe "preview_indeterminate"
                    Vitest.expect(failure.Retryable).toBe true
                    Vitest.expect(failure.StateChanged).toBe false

                    let changedPathHooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess =
                            Some(fun request context ->
                                if
                                    request.Arguments |> Array.contains "diff"
                                    && request.Arguments |> Array.contains "--name-only"
                                then
                                    async {
                                        return
                                            OperationResult.failed(
                                                OperationFailure.create
                                                    ProviderError
                                                    "changed_path_transport_failed"
                                                    "The changed-path provider call failed."
                                            )
                                    }
                                else
                                    NodeProcess.run request context)
                        Barrier = None
                    }

                    let changedPathSession =
                        GitWorkspaceSession.createSession changedPathHooks workspace.Binding

                    let changedPathSynchronization =
                        changedPathSession.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected Git synchronization.")

                    let! changedPathPreview =
                        changedPathSynchronization.PreviewUpdate(
                            OperationContext.detached "preview-indeterminate-changed-paths"
                        )
                        |> Async.StartAsPromise

                    let changedPathFailure =
                        expectProviderFailure "indeterminate changed-path preview" changedPathPreview

                    Vitest.expect(changedPathFailure.Category).toEqual ProviderError
                    Vitest.expect(changedPathFailure.Code).toBe "preview_indeterminate"
                    Vitest.expect(changedPathFailure.Retryable).toBe true
                    Vitest.expect(changedPathFailure.StateChanged).toBe false
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

let private adoptionHooks (originResult: OperationResult<NodeProcess.ProcessOutput>) : GitWorkspaceSession.GitSessionHooks = {
    RunBytesProcess = None
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
    do! writeUtf8FileAsync (join [| repoPath; "split-utf8.txt" |]) "prefix € suffix\n"
    do! writeUtf8FileAsync (join [| repoPath; "folder"; "content.txt" |]) "nested base content\n"
    do! writeUtf8FileAsync (join [| repoPath; "document.pdf" |]) "%PDF-1.4\nASCII fixture\n"
    do! writeBinaryFileAsync (join [| repoPath; "binary.dat" |]) [| 0; 255; 1; 2; 3 |]
    do! writeBinaryFileAsync (join [| repoPath; "unchanged-binary.dat" |]) [| 0; 253; 4; 5; 6 |]
    do! writeBinaryFileAsync (join [| repoPath; "invalid-utf8.dat" |]) [| 255; 254; 253 |]
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

        Vitest.test (
            "returns unsupported content for a known non-text extension with ASCII bytes",
            fun () -> promise {
                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess =
                        Some(fun _ _ ->
                            async {
                                return
                                    OperationResult.failed(
                                        OperationFailure.create
                                            ProviderError
                                            "unexpected_unsupported_blob_read"
                                            "Known unsupported extensions must not be buffered."
                                    )
                            })
                    RunProcess = None
                    Barrier = None
                }

                let! root, session = createBaseContentFixtureWithHooks hooks

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "document.pdf")
                            (OperationContext.detached "base-explicitly-unsupported")
                        |> Async.StartAsPromise

                    match expectProviderValue "explicitly unsupported base content" result with
                    | UnsupportedContent reason ->
                        Vitest
                            .expect(reason |> Option.exists (fun value -> value.Contains "document.pdf"))
                            .toBe (true)
                    | TextContent text -> failwith $"Expected unsupported PDF content, received '{text}'."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "returns the committed text when the worktree version became binary",
            fun () -> promise {
                let! root, session = createBaseContentFixture ()

                try
                    let repoPath = join [| root; "work" |]
                    do! writeBinaryFileAsync (join [| repoPath; "base.txt" |]) [| 0; 255; 7; 8 |]

                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "base.txt")
                            (OperationContext.detached "base-text-worktree-binary")
                        |> Async.StartAsPromise

                    match expectProviderValue "text base with binary worktree" result with
                    | TextContent text -> Vitest.expect(text).toBe ("base content\n")
                    | UnsupportedContent _ -> failwith "Expected the committed text blob."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "reports an added path in an unborn repository as absent from the base",
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let repoPath = join [| root; "unborn" |]

                try
                    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; repoPath |] None
                    do! writeUtf8FileAsync (join [| repoPath; "added.txt" |]) "new content\n"

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

                    let session =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding

                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "added.txt")
                            (OperationContext.detached "base-unborn-absent")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "unborn absent base content" result
                    Vitest.expect(failure.Category).toEqual (NotFound)
                    Vitest.expect(failure.Code).toBe ("base_content_not_found")
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
            "classifies an unchanged committed binary blob from its materialized bytes",
            fun () -> promise {
                let! root, session = createBaseContentFixture ()

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "unchanged-binary.dat")
                            (OperationContext.detached "base-unchanged-binary")
                        |> Async.StartAsPromise

                    match expectProviderValue "unchanged binary base content" result with
                    | UnsupportedContent reason ->
                        Vitest
                            .expect(reason |> Option.exists (fun value -> value.Contains "unchanged-binary.dat"))
                            .toBe (true)
                    | TextContent text -> failwith $"Expected unsupported binary content, received text '{text}'."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "rejects committed invalid UTF-8 without replacement decoding",
            fun () -> promise {
                let! root, session = createBaseContentFixture ()

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "invalid-utf8.dat")
                            (OperationContext.detached "base-invalid-utf8")
                        |> Async.StartAsPromise

                    match expectProviderValue "invalid UTF-8 base content" result with
                    | UnsupportedContent reason ->
                        Vitest
                            .expect(reason |> Option.exists (fun value -> value.Contains "invalid-utf8.dat"))
                            .toBe (true)
                    | TextContent text -> failwith $"Expected unsupported invalid UTF-8, received text '{text}'."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "follows renames even when repository status rename detection is disabled",
            fun () -> promise {
                let! root, session = createBaseContentFixture ()

                try
                    let repoPath = join [| root; "work" |]
                    let! _ = runGitIn repoPath [||] [| "config"; "status.renames"; "false" |] None

                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "renamed.txt")
                            (OperationContext.detached "base-rename-config-independent")
                        |> Async.StartAsPromise

                    expectTextBase
                        "committed rename source\n"
                        "config-independent renamed base content"
                        result

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "reports an absent known non-text extension as not found before classification",
            fun () -> promise {
                let! root, session = createBaseContentFixture ()

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "missing.pdf")
                            (OperationContext.detached "base-missing-unsupported-extension")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "missing unsupported-extension base content" result
                    Vitest.expect(failure.Category).toEqual (NotFound)
                    Vitest.expect(failure.Code).toBe ("base_content_not_found")
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "returns unsupported content for a present non-blob base object",
            fun () -> promise {
                let! root, session = createBaseContentFixture ()

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "folder")
                            (OperationContext.detached "base-present-tree")
                        |> Async.StartAsPromise

                    match expectProviderValue "present base tree" result with
                    | UnsupportedContent _ -> ()
                    | TextContent text -> failwith $"Expected unsupported tree content, received '{text}'."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "reads base bytes through a frozen object id",
            fun () -> promise {
                let mutable rawObjectName = ""

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess =
                        Some(fun request context ->
                            if
                                request.Arguments.Length = 3
                                && request.Arguments[0] = "cat-file"
                                && request.Arguments[1] = "blob"
                            then
                                rawObjectName <- request.Arguments[2]

                            NodeProcess.runBytes request context)
                    RunProcess = None
                    Barrier = None
                }

                let! root, session = createBaseContentFixtureWithHooks hooks

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "base.txt")
                            (OperationContext.detached "base-frozen-object")
                        |> Async.StartAsPromise

                    expectTextBase "base content\n" "frozen base object content" result
                    Vitest.expect(rawObjectName.Contains ":").toBe (false)
                    Vitest.expect(rawObjectName.Length).toBeGreaterThanOrEqual (40)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "freezes HEAD before rename discovery",
            fun () -> promise {
                let mutable repoPath = ""
                let mutable advancedHead = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = Some NodeProcess.runBytes
                    RunProcess =
                        Some(fun request context ->
                            async {
                                let! result = NodeProcess.run request context

                                if
                                    not advancedHead
                                    && (request.Arguments |> Array.contains "status")
                                then
                                    advancedHead <- true

                                    do!
                                        Async.AwaitPromise(
                                            writeUtf8FileAsync
                                                (join [| repoPath; "renamed.txt" |])
                                                "advanced committed rename content\n"
                                        )

                                    let! _ =
                                        Async.AwaitPromise(runGitIn repoPath [||] [| "add"; "-A" |] None)

                                    let! _ =
                                        Async.AwaitPromise(
                                            runGitIn
                                                repoPath
                                                [||]
                                                [| "commit"; "-m"; "test: advance head during base read" |]
                                                None
                                        )

                                    ()

                                return result
                            })
                    Barrier = None
                }

                let! root, session = createBaseContentFixtureWithHooks hooks
                repoPath <- join [| root; "work" |]

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "renamed.txt")
                            (OperationContext.detached "base-head-race")
                        |> Async.StartAsPromise

                    expectTextBase "advanced committed rename content\n" "base content across a HEAD race" result
                    Vitest.expect(advancedHead).toBe (true)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "decodes committed UTF-8 only after all split process chunks are materialized",
            fun () -> promise {
                let splitUtf8Script =
                    "const bytes=Buffer.from('prefix € suffix\\n','utf8');" +
                    "process.stdout.write(bytes.subarray(0,8));" +
                    "setTimeout(()=>process.stdout.write(bytes.subarray(8)),25);"

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess =
                        Some(fun request context ->
                            if
                                request.Arguments.Length = 3
                                && request.Arguments[0] = "cat-file"
                                && request.Arguments[1] = "blob"
                            then
                                NodeProcess.runBytes
                                    {
                                        request with
                                            Command = Fable.Core.JsInterop.emitJsExpr () "process.execPath"
                                            Arguments = [| "-e"; splitUtf8Script |]
                                            WorkingDirectory = None
                                            StdinData = None
                                    }
                                    context
                            else
                                NodeProcess.runBytes request context)
                    RunProcess =
                        Some(fun request context ->
                            NodeProcess.run request context)
                    Barrier = None
                }

                let! root, session = createBaseContentFixtureWithHooks hooks

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "split-utf8.txt")
                            (OperationContext.detached "base-split-utf8")
                        |> Async.StartAsPromise

                    expectTextBase "prefix € suffix\n" "split UTF-8 base content" result
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "reads committed bytes without a standalone node process or temporary materialization",
            fun () -> promise {
                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = Some NodeProcess.runBytes
                    RunProcess =
                        Some(fun request context ->
                            if
                                request.Command = "node"
                                || (request.Arguments |> Array.contains "--git-path")
                            then
                                async {
                                    return
                                        OperationResult.failed(
                                            OperationFailure.create
                                                ProviderError
                                                "unexpected_comparison_materialization"
                                                "Base content must not require a standalone Node process or temporary Git storage."
                                        )
                                }
                            else
                                NodeProcess.run request context)
                    Barrier = None
                }

                let! root, session = createBaseContentFixtureWithHooks hooks

                try
                    let! result =
                        (textDiffService session).GetBaseContent
                            (repositoryPath "base.txt")
                            (OperationContext.detached "base-no-materialization")
                        |> Async.StartAsPromise

                    expectTextBase "base content\n" "base content without materialization" result
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
                    RunBytesProcess =
                        Some(fun request context ->
                            async {
                                source.Cancel()
                                return! NodeProcess.runBytes request context
                            })
                    RunProcess =
                        Some(fun request context -> NodeProcess.run request context)
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
