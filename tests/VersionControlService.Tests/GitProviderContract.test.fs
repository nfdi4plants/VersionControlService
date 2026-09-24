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
module GitCredentialStrategy = VersionControlService.Git.GitCredentialStrategy
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeProcess = VersionControlService.Runtime.Node.Process

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

let private createTempDirectoryAsync () : JS.Promise<string> = promise {
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-git-harness-" |]
    let! created = fsPromisesDynamic?mkdtemp (prefix) |> unbox<JS.Promise<string>>
    // GitHub's Windows runners hand out TEMP as an 8.3 short path (RUNNER~1). Git reports the
    // long form, so resolve the directory once here and every path comparison agrees.
    return! fsPromisesDynamic?realpath (created) |> unbox<JS.Promise<string>>
}

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

[<Emit("Buffer.from($0, 'utf8').toString('base64')")>]
let private utf8Base64 (_value: string) : string = jsNative

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

let private createDirectoryJunctionAsync (targetPath: string) (linkPath: string) : JS.Promise<unit> = promise {
    let! _ = fsPromisesDynamic?symlink (targetPath, linkPath, "junction") |> unbox<JS.Promise<obj>>
    return ()
}

[<Emit("(() => { const fs = require('node:fs'); const moduleApi = require('node:module'); const original = fs.renameSync; fs.renameSync = () => { throw new Error('injected atomic rename failure'); }; moduleApi.syncBuiltinESMExports(); return () => { fs.renameSync = original; moduleApi.syncBuiltinESMExports(); }; })()")>]
let private injectRenameSyncFailure () : (unit -> unit) = jsNative

[<Emit("process.execPath")>]
let private nodeExecutablePath: string = jsNative

[<Emit("process.platform === 'win32'")>]
let private isWindowsProcess () : bool = jsNative

[<Emit("process.platform === 'win32' || process.platform === 'darwin'")>]
let private localFileSystemAliasesCase () : bool = jsNative

[<Emit("process.env.PATH || ''")>]
let private currentProcessPath () : string = jsNative

[<Emit("process.env.PATH = $0")>]
let private setCurrentProcessPath (_value: string) : unit = jsNative

[<Emit("process.cwd()")>]
let private currentProcessWorkingDirectory () : string = jsNative

[<Emit("process.chdir($0)")>]
let private setCurrentProcessWorkingDirectory (_value: string) : unit = jsNative

[<Emit("require('node:path').delimiter")>]
let private pathDelimiter: string = jsNative

[<Emit("Object.prototype.hasOwnProperty.call(process.env, $0) ? process.env[$0] : null")>]
let private tryGetProcessEnvironment (_name: string) : string = jsNative

[<Emit("process.env[$0] = $1")>]
let private setProcessEnvironment (_name: string) (_value: string) : unit = jsNative

[<Emit("delete process.env[$0]")>]
let private clearProcessEnvironment (_name: string) : unit = jsNative

[<Emit("""
(() => {
    const childProcess = require('node:child_process');
    const moduleApi = require('node:module');
    const originalSpawn = childProcess.spawn;
    childProcess.spawn = function(command, args, options) {
        if (String(command).toLowerCase() === 'taskkill') {
            setTimeout(() => originalSpawn.call(childProcess, command, args, options), 1500);
            return { on() { return this; }, unref() {} };
        }
        return originalSpawn.call(childProcess, command, args, options);
    };
    moduleApi.syncBuiltinESMExports();
    return () => {
        childProcess.spawn = originalSpawn;
        moduleApi.syncBuiltinESMExports();
    };
})()
""")>]
let private injectDelayedTaskkillSpawn () : (unit -> unit) = jsNative

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

let private runGitResultIn (cwd: string) (arguments: string[]) : JS.Promise<NodeProcess.ProcessOutput> = promise {
    let request = {
        NodeProcess.ProcessRequest.create "git" arguments with
            WorkingDirectory = Some cwd
    }

    let! result = Async.StartAsPromise(NodeProcess.run request (OperationContext.detached "git-harness"))

    match result with
    | Succeeded outcome -> return outcome.Value
    | PartiallySucceeded _
    | Failed _ -> return failwith "harness git invocation failed"
}

let private configureUser (repoPath: string) = promise {
    let! _ = runGitIn repoPath [||] [| "config"; "user.name"; "VCS Harness" |] None
    let! _ = runGitIn repoPath [||] [| "config"; "user.email"; "harness@example.org" |] None
    let! _ = runGitIn repoPath [||] [| "config"; "core.autocrlf"; "false" |] None
    return ()
}

let private mkRepositoryPath (value: string) =
    match RepositoryPath.tryCreate value with
    | Ok path -> path
    | Error message -> failwith message

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
            // Windows and macOS default filesystems alias case, and only Windows enforces its
            // reserved names. Linux runners do neither, so the flags follow the platform.
            LocalFileSystemAliasesCase = localFileSystemAliasesCase ()
            LocalFileSystemAliasesNormalization = false
            LocalFileSystemWindowsRules = isWindowsProcess ()
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
    ConsumerWorkflowSuite.register gitHarness
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
    "Git / consumer workflow profile",
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
            "a local upstream remains a local target and refresh does not require a remote",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let! _ =
                        harness.SeedRevisionOnNewRef
                            workspace
                            "integration"
                            [| "local-target.txt", "advanced through a local upstream\n" |]

                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "branch"; "--set-upstream-to=integration"; "main" |]
                            None

                    let! expectedTargetRevision =
                        runGitIn workspace.Binding.WorkspaceRoot [||] [| "rev-parse"; "refs/heads/integration" |] None

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "local-upstream-status")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "local upstream status" statusResult
                    let synchronization =
                        status.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected local-upstream synchronization state.")

                    let target =
                        synchronization.TargetRef
                        |> Option.defaultWith (fun () -> failwith "Expected configured local upstream target.")

                    Vitest.expect(target.Name).toBe "integration"
                    Vitest.expect(ProviderRef.value target.ProviderRef).toBe "git-local:integration"
                    Vitest.expect(target.Kind).toEqual LocalRef
                    Vitest.expect(synchronization.TargetRevision |> Option.map RevisionId.value).toEqual (Some(expectedTargetRevision.Trim()))
                    Vitest.expect(synchronization.Relationship).toEqual TargetAhead

                    let refresh =
                        workspace.Session.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected Git synchronization.")

                    let! refreshResult =
                        refresh.Refresh(OperationContext.detached "refresh-local-upstream")
                        |> Async.StartAsPromise

                    let refreshed = expectProviderValue "refresh local upstream" refreshResult
                    Vitest.expect(refreshed.TargetRef).toEqual (Some target)
                    Vitest.expect(refreshed.TargetRevision |> Option.map RevisionId.value).toEqual (Some(expectedTargetRevision.Trim()))
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

Vitest.describe (
    "Git / revision identity",
    fun () ->
        Vitest.test (
            "CreateRevision uses the injected revision identity",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()

                    let identity: GitCredentialStrategy.GitIdentityStrategy = {
                        ResolveIdentity = fun _request ->
                            async {
                                return Some { Name = "Test Author"; Email = "author@example.org" }
                            }
                    }

                    let factory =
                        GitWorkspaceSession.createFactoryWithCredentialsAndIdentity
                            GitWorkspaceSession.GitSessionHooks.none
                            GitCredentialStrategy.anonymous
                            identity

                    let! sessionResult =
                        factory.Open workspace.Binding (OperationContext.detached "revision-identity-open")
                        |> Async.StartAsPromise

                    let session = expectProviderValue "revision identity open" sessionResult

                    do! workspace.WriteFile "revision-identity.txt" "identity content\n"
                    let! status =
                        session.Core.GetStatus(OperationContext.detached "revision-identity-status")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "revision identity status" status

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: revision identity"
                                Paths = [| mkRepositoryPath "revision-identity.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "revision-identity-create")
                        |> Async.StartAsPromise

                    expectProviderValue "revision identity create" revisionResult |> ignore

                    let! commitIdentity =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "log"; "-1"; "--format=%an;%ae;%cn;%ce" |]
                            None

                    Vitest.expect(commitIdentity.Trim()).toBe "Test Author;author@example.org;Test Author;author@example.org"
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "revision identity requests carry the publish remote host and connection profile",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let requests = ResizeArray<GitCredentialStrategy.RevisionIdentityRequest>()

                    let identity: GitCredentialStrategy.GitIdentityStrategy = {
                        ResolveIdentity = fun request ->
                            async {
                                requests.Add request
                                return Some { Name = "Host Author"; Email = "host@example.org" }
                            }
                    }

                    let createRevision (binding: WorkspaceBinding) (name: string) = promise {
                        let session =
                            GitWorkspaceSession.createSessionWithCredentialsAndIdentity
                                GitWorkspaceSession.GitSessionHooks.none
                                GitCredentialStrategy.anonymous
                                identity
                                binding

                        do! workspace.WriteFile $"{name}.txt" $"{name} content\n"

                        let! status =
                            session.Core.GetStatus(OperationContext.detached $"{name}-status")
                            |> Async.StartAsPromise

                        let status = expectProviderValue $"{name} status" status

                        let! revisionResult =
                            session.Core.CreateRevision
                                {
                                    Message = $"test: {name}"
                                    Paths = [| mkRepositoryPath $"{name}.txt" |]
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                }
                                (OperationContext.detached $"{name}-create")
                            |> Async.StartAsPromise

                        expectProviderValue $"{name} create" revisionResult |> ignore
                    }

                    let workspaceRoot = workspace.Binding.WorkspaceRoot
                    let setOriginUrl (url: string) = runGitIn workspaceRoot [||] [| "remote"; "set-url"; "origin"; url |] None

                    // The harness clone publishes to a local bare repository, which has no host.
                    do! createRevision workspace.Binding "identity-local"

                    // The configured publish remote decides the host, the same way publish
                    // credentials do, even though the binding still points at the local path.
                    let! _ = setOriginUrl "https://Hub.Example.org/group/project.git"

                    let profileBinding = {
                        workspace.Binding with
                            ConnectionProfileId = Some "profile-a"
                    }

                    do! createRevision profileBinding "identity-remote"

                    // scp-style SSH remotes name their host too.
                    let! _ = setOriginUrl "git@scp.example.org:group/project.git"
                    do! createRevision workspace.Binding "identity-scp"

                    // A push URL wins over the fetch URL, because that is where git push goes.
                    let! _ = setOriginUrl "https://Hub.Example.org/group/project.git"

                    let! _ =
                        runGitIn
                            workspaceRoot
                            [||]
                            [| "remote"; "set-url"; "--push"; "origin"; "https://push.example.org/group/project.git" |]
                            None

                    do! createRevision workspace.Binding "identity-pushurl"

                    // Two distinct push URLs would publish to two hosts, which one identity
                    // cannot serve, so the target counts as invalid and there is no host.
                    let! _ =
                        runGitIn
                            workspaceRoot
                            [||]
                            [| "config"; "--add"; "remote.origin.pushurl"; "https://second.example.org/group/project.git" |]
                            None

                    do! createRevision workspace.Binding "identity-two-pushurls"
                    let! _ = runGitIn workspaceRoot [||] [| "config"; "--unset-all"; "remote.origin.pushurl" |] None

                    // insteadOf rewriting changes where git actually pushes, so the host
                    // comes from the rewritten URL.
                    let! _ = setOriginUrl "git@scp.example.org:group/project.git"

                    let! _ =
                        runGitIn
                            workspaceRoot
                            [||]
                            [| "config"; "url.https://rewritten.example.org/.insteadOf"; "git@scp.example.org:" |]
                            None

                    do! createRevision workspace.Binding "identity-insteadof"
                    let! _ = runGitIn workspaceRoot [||] [| "config"; "--remove-section"; "url.https://rewritten.example.org/" |] None

                    // An upstream that names no remote is an invalid target: no host at all,
                    // rather than a host the revision will never be published to.
                    let! _ = runGitIn workspaceRoot [||] [| "config"; "branch.main.remote"; "." |] None
                    do! createRevision workspace.Binding "identity-invalid-upstream"
                    let! _ = runGitIn workspaceRoot [||] [| "config"; "branch.main.remote"; "origin" |] None

                    // Without any publish remote the bound location is what remains.
                    let! _ = runGitIn workspaceRoot [||] [| "remote"; "remove"; "origin" |] None

                    let fallbackBinding = {
                        workspace.Binding with
                            Location = {
                                workspace.Binding.Location with
                                    ProviderLocation = "https://fallback.example.org/group/project.git"
                            }
                    }

                    do! createRevision fallbackBinding "identity-fallback"

                    Vitest.expect(requests.Count).toBe 8

                    for request in requests do
                        Vitest.expect(request.WorkspaceRoot).toBe workspaceRoot

                    Vitest.expect(requests.[0].TargetHost).toEqual None
                    Vitest.expect(requests.[0].ConnectionProfileId).toEqual workspace.Binding.ConnectionProfileId
                    Vitest.expect(requests.[1].TargetHost).toEqual (Some "hub.example.org")
                    Vitest.expect(requests.[1].ConnectionProfileId).toEqual (Some "profile-a")
                    Vitest.expect(requests.[2].TargetHost).toEqual (Some "scp.example.org")
                    Vitest.expect(requests.[3].TargetHost).toEqual (Some "push.example.org")
                    Vitest.expect(requests.[4].TargetHost).toEqual None
                    Vitest.expect(requests.[5].TargetHost).toEqual (Some "rewritten.example.org")
                    Vitest.expect(requests.[6].TargetHost).toEqual None
                    Vitest.expect(requests.[7].TargetHost).toEqual (Some "fallback.example.org")
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "revision identity None falls back to repository configuration",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()

                    let identity: GitCredentialStrategy.GitIdentityStrategy = {
                        ResolveIdentity = fun _request -> async { return None }
                    }

                    let session =
                        GitWorkspaceSession.createSessionWithCredentialsAndIdentity
                            GitWorkspaceSession.GitSessionHooks.none
                            GitCredentialStrategy.anonymous
                            identity
                            workspace.Binding

                    do! workspace.WriteFile "configured-identity.txt" "configured identity\n"
                    let! status =
                        session.Core.GetStatus(OperationContext.detached "configured-identity-status")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "configured identity status" status

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: configured identity"
                                Paths = [| mkRepositoryPath "configured-identity.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "configured-identity-create")
                        |> Async.StartAsPromise

                    expectProviderValue "configured identity create" revisionResult |> ignore

                    let! commitIdentity =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "log"; "-1"; "--format=%an;%ae;%cn;%ce" |]
                            None

                    Vitest.expect(commitIdentity.Trim()).toBe "VCS Harness;harness@example.org;VCS Harness;harness@example.org"
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "incomplete revision identity fails before git commit",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let mutable currentIdentity: GitCredentialStrategy.RevisionIdentity = {
                        Name = " "
                        Email = "author@example.org"
                    }

                    let identity: GitCredentialStrategy.GitIdentityStrategy = {
                        ResolveIdentity = fun _request -> async { return Some currentIdentity }
                    }

                    let session =
                        GitWorkspaceSession.createSessionWithCredentialsAndIdentity
                            GitWorkspaceSession.GitSessionHooks.none
                            GitCredentialStrategy.anonymous
                            identity
                            workspace.Binding

                    let reject path content contextName = promise {
                        do! workspace.WriteFile path content
                        let! statusResult =
                            session.Core.GetStatus(OperationContext.detached $"{contextName}-status")
                            |> Async.StartAsPromise

                        let status = expectProviderValue $"{contextName} status" statusResult
                        let! revisionResult =
                            session.Core.CreateRevision
                                {
                                    Message = "test: incomplete revision identity"
                                    Paths = [| mkRepositoryPath path |]
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                }
                                (OperationContext.detached contextName)
                            |> Async.StartAsPromise

                        return expectProviderFailure contextName revisionResult
                    }

                    let! blankNameFailure = reject "blank-name.txt" "blank name\n" "blank-name-revision-identity"
                    Vitest.expect(blankNameFailure.Category).toEqual Validation
                    Vitest.expect(blankNameFailure.Code).toBe "identity_missing"
                    Vitest.expect(blankNameFailure.StateChanged).toBe false

                    currentIdentity <- { Name = "Test Author"; Email = "\t" }
                    let! blankEmailFailure = reject "blank-email.txt" "blank email\n" "blank-email-revision-identity"
                    Vitest.expect(blankEmailFailure.Category).toEqual Validation
                    Vitest.expect(blankEmailFailure.Code).toBe "identity_missing"
                    Vitest.expect(blankEmailFailure.StateChanged).toBe false
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "revision identity resolves once per operation instead of at session open",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let mutable currentIdentity: GitCredentialStrategy.RevisionIdentity = {
                        Name = "First Author"
                        Email = "first@example.org"
                    }

                    let identity: GitCredentialStrategy.GitIdentityStrategy = {
                        ResolveIdentity = fun _request -> async { return Some currentIdentity }
                    }

                    let session =
                        GitWorkspaceSession.createSessionWithCredentialsAndIdentity
                            GitWorkspaceSession.GitSessionHooks.none
                            GitCredentialStrategy.anonymous
                            identity
                            workspace.Binding

                    let create path content message contextName = promise {
                        do! workspace.WriteFile path content
                        let! status =
                            session.Core.GetStatus(OperationContext.detached $"{contextName}-status")
                            |> Async.StartAsPromise

                        let status = expectProviderValue $"{contextName} status" status

                        let! result =
                            session.Core.CreateRevision
                                {
                                    Message = message
                                    Paths = [| mkRepositoryPath path |]
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                }
                                (OperationContext.detached contextName)
                            |> Async.StartAsPromise

                        expectProviderValue $"{contextName} create" result |> ignore
                    }

                    do! create "first-identity.txt" "first\n" "test: first identity" "first-revision-identity"
                    currentIdentity <- { Name = "Second Author"; Email = "second@example.org" }
                    do! create "second-identity.txt" "second\n" "test: second identity" "second-revision-identity"

                    let! commitIdentities =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "log"; "-2"; "--format=%an;%ae;%cn;%ce" |]
                            None

                    let lines =
                        commitIdentities.Replace("\r\n", "\n").Trim().Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)

                    Vitest.expect(lines).toEqual ([|
                        "Second Author;second@example.org;Second Author;second@example.org"
                        "First Author;first@example.org;First Author;first@example.org"
                    |])
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "missing revision identity fails before ref movement",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let emptyGlobal = join [| workspace.Binding.WorkspaceRoot; "empty-global.gitconfig" |]
                    let emptySystem = join [| workspace.Binding.WorkspaceRoot; "empty-system.gitconfig" |]
                    do! writeUtf8FileAsync emptyGlobal ""
                    do! writeUtf8FileAsync emptySystem ""

                    let environment = [| "GIT_CONFIG_GLOBAL", emptyGlobal; "GIT_CONFIG_SYSTEM", emptySystem |]
                    let hooks = {
                        GitWorkspaceSession.GitSessionHooks.none with
                            RunProcess =
                                Some(fun request processContext ->
                                    NodeProcess.run
                                        { request with
                                            Environment = Array.append environment request.Environment }
                                        processContext)
                    }

                    let! _ = runGitIn workspace.Binding.WorkspaceRoot environment [| "config"; "--local"; "--unset-all"; "user.name" |] None
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot environment [| "config"; "--local"; "--unset-all"; "user.email" |] None
                    let identity: GitCredentialStrategy.GitIdentityStrategy = {
                        ResolveIdentity = fun _request -> async { return None }
                    }

                    let session =
                        GitWorkspaceSession.createSessionWithCredentialsAndIdentity
                            hooks
                            GitCredentialStrategy.anonymous
                            identity
                            workspace.Binding

                    let! headBefore = runGitIn workspace.Binding.WorkspaceRoot environment [| "rev-parse"; "HEAD" |] None
                    do! workspace.WriteFile "missing-identity.txt" "missing identity\n"
                    let! status =
                        session.Core.GetStatus(OperationContext.detached "missing-identity-status")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "missing identity status" status

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: missing identity"
                                Paths = [| mkRepositoryPath "missing-identity.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "missing-identity-create")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "missing revision identity" revisionResult
                    Vitest.expect(failure.Category).toEqual Validation
                    Vitest.expect(failure.Code).toBe "identity_missing"
                    Vitest.expect(failure.RecoveryAction.IsSome).toBe true
                    Vitest.expect(failure.StateChanged).toBe false

                    let! headAfter = runGitIn workspace.Binding.WorkspaceRoot environment [| "rev-parse"; "HEAD" |] None
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
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
    "Git provider validation",
    fun () ->
        Vitest.test (
            "a checkout that git aborts for local changes is not reported as canceled",
            TestOptions(timeout = 120000),
            fun () -> promise {
                // git refuses the switch and ends its error text with "Aborting". The
                // failure classifier must not read that word as a user cancellation.
                let! root = createTempDirectoryAsync ()

                try
                    let repoPath = join [| root; "work" |]
                    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; repoPath |] None
                    do! configureUser repoPath
                    do! writeUtf8FileAsync (join [| repoPath; "base.txt" |]) "main content\n"
                    let! _ = runGitIn repoPath [||] [| "add"; "-A" |] None
                    let! _ = runGitIn repoPath [||] [| "commit"; "-m"; "test: main" |] None
                    let! _ = runGitIn repoPath [||] [| "checkout"; "-b"; "other" |] None
                    do! writeUtf8FileAsync (join [| repoPath; "base.txt" |]) "other content\n"
                    let! _ = runGitIn repoPath [||] [| "commit"; "-am"; "test: other" |] None
                    let! _ = runGitIn repoPath [||] [| "checkout"; "main" |] None
                    do! writeUtf8FileAsync (join [| repoPath; "base.txt" |]) "local edit\n"

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

                    let! statusResult =
                        session.Core.GetStatus(OperationContext.detached "aborting-status")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "aborting status" statusResult

                    let otherRef =
                        match ProviderRef.tryCreate "git-local:other" with
                        | Ok reference -> reference
                        | Error message -> failwith message

                    let! createResult =
                        session.Core.CreateRef
                            {
                                Name = "from-other"
                                BaseRef = Some otherRef
                                SwitchTo = true
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "aborting-create")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "aborted checkout" createResult
                    Vitest.expect(failure.Message.ToLowerInvariant().Contains "aborting").toBe true
                    Vitest.expect(failure.Category).toEqual ProviderError
                    Vitest.expect(failure.Code).toBe "git_failure"
                    Vitest.expect(failure.StateChanged).toBe false

                    let! branch = runGitIn repoPath [||] [| "rev-parse"; "--abbrev-ref"; "HEAD" |] None
                    Vitest.expect(branch.Trim()).toBe "main"
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "rejects invalid ref names through the provider contract",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()

                    for invalidName in [| ""; "-force"; ".foo"; "foo/.bar"; "foo//bar"; "name\000suffix" |] do
                        let! statusResult =
                            workspace.Session.Core.GetStatus(OperationContext.detached "invalid-ref-status")
                            |> Async.StartAsPromise

                        let status = expectProviderValue "invalid ref status" statusResult

                        let! createResult =
                            workspace.Session.Core.CreateRef
                                {
                                    Name = invalidName
                                    BaseRef = None
                                    SwitchTo = false
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                }
                                (OperationContext.detached "invalid-ref-create")
                            |> Async.StartAsPromise

                        let failure = expectProviderFailure $"create invalid ref '{invalidName}'" createResult
                        Vitest.expect(failure.Category).toEqual Validation
                        Vitest.expect(failure.Code).toBe "invalid_ref_name"
                        Vitest.expect(failure.StateChanged).toBe false

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "passes an option-like remote as one literal location and reports rejection structurally",
            fun () -> promise {
                let mutable observedArguments: string[] = [||]
                let providerLocation = "https://example.invalid/repository.git -c protocol.file.allow=always"

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request _ ->
                                async {
                                    observedArguments <- request.Arguments
                                    return OperationResult.succeeded (processOutput 128 "" "fatal: repository rejected")
                                })
                }

                let factory = GitWorkspaceSession.createFactory hooks

                let! result =
                    factory.VerifyLocation
                        {
                            Location = {
                                ProviderId = gitProviderId
                                DisplayName = None
                                ProviderLocation = providerLocation
                                ConnectionProfileId = None
                            }
                            Intents = [| ReadIntent |]
                        }
                        (OperationContext.detached "literal-remote-rejection")
                    |> Async.StartAsPromise

                let failure = expectProviderFailure "option-like remote rejection" result
                Vitest.expect(failure.Category).toEqual (Validation)
                Vitest.expect(failure.Code).toBe "location_not_allowed"
                Vitest.expect(failure.StateChanged).toBe false
                Vitest.expect(observedArguments.Length).toBe 0
            }
        )

        Vitest.test (
            "provider locations are validated before VerifyLocation, Clone, and Bind run Git",
            fun () -> promise {
                for providerLocation in [|
                    "--upload-pack=doesnotexist"
                    "ext::sh -c whatever"
                    "file:///some/path"
                |] do
                    let observed = ResizeArray<NodeProcess.ProcessRequest>()

                    let hooks = {
                        GitWorkspaceSession.GitSessionHooks.none with
                            RunProcess =
                                Some(fun request _ ->
                                    async {
                                        observed.Add request
                                        return OperationResult.succeeded (processOutput 0 "" "")
                                    })
                    }

                    let factory = GitWorkspaceSession.createFactory hooks
                    let location = {
                        ProviderId = gitProviderId
                        DisplayName = None
                        ProviderLocation = providerLocation
                        ConnectionProfileId = None
                    }

                    let! verifyResult =
                        Async.StartAsPromise(
                            factory.VerifyLocation
                                {
                                    Location = location
                                    Intents = [| ReadIntent |]
                                }
                                (OperationContext.detached "validate-provider-location")
                        )

                    let verifyFailure = expectProviderFailure "validated VerifyLocation" verifyResult

                    let! cloneResult =
                        Async.StartAsPromise(
                            factory.Clone
                                {
                                    Location = location
                                    TargetPath = "C:/provider-location-clone-target"
                                    TargetRef = None
                                    MaterializeAllObjects = false
                                }
                                (OperationContext.detached "validate-provider-location-clone")
                        )

                    let cloneFailure = expectProviderFailure "validated Clone" cloneResult

                    let! bindResult =
                        Async.StartAsPromise(
                            factory.Bind
                                {
                                    WorkspaceRoot = "C:/provider-location-bind-workspace"
                                    Location = location
                                }
                                (OperationContext.detached "validate-provider-location-bind")
                        )

                    let bindFailure = expectProviderFailure "validated Bind" bindResult

                    for failure in [| verifyFailure; cloneFailure; bindFailure |] do
                        Vitest.expect(failure.Category).toEqual (Validation)
                        Vitest.expect(failure.Code).toBe ("location_not_allowed")

                    Vitest.expect(observed.Count).toBe 0
            }
        )

        Vitest.test (
            "provider locations are validated with exact remote-name matching during Bind",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let originalPath = join [| root; "myorigin.git" |]
                    let boundPath = join [| root; "bound.git" |]
                    let workspacePath = join [| root; "workspace" |]

                    let! _ = runGitIn root [||] [| "init"; "--bare"; "-b"; "main"; originalPath |] None
                    let! _ = runGitIn root [||] [| "init"; "--bare"; "-b"; "main"; boundPath |] None
                    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; workspacePath |] None
                    let! _ = runGitIn workspacePath [||] [| "remote"; "add"; "myorigin"; originalPath |] None
                    let! originalLocation =
                        runGitIn workspacePath [||] [| "remote"; "get-url"; "myorigin" |] None

                    let location = {
                        ProviderId = gitProviderId
                        DisplayName = None
                        ProviderLocation = boundPath
                        ConnectionProfileId = None
                    }

                    let factory = GitWorkspaceSession.createFactory GitWorkspaceSession.GitSessionHooks.none

                    let! bindResult =
                        Async.StartAsPromise(
                            factory.Bind
                                {
                                    WorkspaceRoot = workspacePath
                                    Location = location
                                }
                                (OperationContext.detached "bind-exact-remote-name")
                        )

                    expectProviderValue "bind with myorigin" bindResult |> ignore

                    let! originLocation =
                        runGitIn workspacePath [||] [| "remote"; "get-url"; "origin" |] None

                    let! unchangedLocation =
                        runGitIn workspacePath [||] [| "remote"; "get-url"; "myorigin" |] None

                    Vitest.expect(originLocation.Trim()).toBe (boundPath.Trim())
                    Vitest.expect(unchangedLocation.Trim()).toBe (originalLocation.Trim())

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "provider locations are validated when Initialize configures and omits remotes",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let initializedPath = join [| root; "initialized" |]
                    let localOnlyPath = join [| root; "local-only" |]
                    let localLocationPath = join [| root; "local-location" |]
                    let remoteLocation = {
                        ProviderId = gitProviderId
                        DisplayName = None
                        ProviderLocation = "https://example.invalid/repository.git"
                        ConnectionProfileId = None
                    }

                    let factory = GitWorkspaceSession.createFactory GitWorkspaceSession.GitSessionHooks.none

                    let! initializedResult =
                        Async.StartAsPromise(
                            factory.Initialize
                                {
                                    TargetPath = initializedPath
                                    Location = Some remoteLocation
                                }
                                (OperationContext.detached "initialize-with-location")
                        )

                    let initializedBinding = expectProviderValue "initialize with remote location" initializedResult
                    let! configuredRemotes = runGitIn initializedPath [||] [| "remote" |] None
                    let! configuredOrigin =
                        runGitIn initializedPath [||] [| "config"; "--get"; "remote.origin.url" |] None

                    Vitest.expect(initializedBinding.Location).toEqual (remoteLocation)
                    Vitest.expect(configuredRemotes.Trim()).toBe ("origin")
                    Vitest.expect(configuredOrigin.Trim()).toBe (remoteLocation.ProviderLocation)

                    let! localOnlyResult =
                        Async.StartAsPromise(
                            factory.Initialize
                                {
                                    TargetPath = localOnlyPath
                                    Location = None
                                }
                                (OperationContext.detached "initialize-without-location")
                        )

                    let localOnlyBinding = expectProviderValue "initialize without remote location" localOnlyResult
                    let! configuredRemotes =
                        runGitIn localOnlyPath [||] [| "remote" |] None

                    Vitest.expect(configuredRemotes.Trim()).toBe ""

                    let! openedResult =
                        Async.StartAsPromise(
                            factory.Open localOnlyBinding (OperationContext.detached "open-local-only-initialize")
                        )

                    let session = expectProviderValue "open local-only initialized workspace" openedResult
                    let! statusResult =
                        Async.StartAsPromise(session.Core.GetStatus(OperationContext.detached "local-only-status"))

                    let status = expectProviderValue "local-only initialized status" statusResult
                    let synchronization =
                        status.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected local-only synchronization state.")

                    Vitest.expect(synchronization.Relationship).toEqual (NoTarget)

                    let localLocation = {
                        ProviderId = gitProviderId
                        DisplayName = None
                        ProviderLocation = localLocationPath
                        ConnectionProfileId = None
                    }

                    let! localLocationResult =
                        Async.StartAsPromise(
                            factory.Initialize
                                {
                                    TargetPath = localLocationPath
                                    Location = Some localLocation
                                }
                                (OperationContext.detached "initialize-with-local-location")
                        )

                    let localLocationBinding =
                        expectProviderValue "initialize with local self location" localLocationResult

                    let! localLocationRemotes = runGitIn localLocationPath [||] [| "remote" |] None
                    Vitest.expect(localLocationRemotes.Trim()).toBe ""

                    let! localLocationOpenResult =
                        Async.StartAsPromise(
                            factory.Open
                                localLocationBinding
                                (OperationContext.detached "open-local-location-initialize")
                        )

                    let localLocationSession =
                        expectProviderValue "open local self initialized workspace" localLocationOpenResult

                    let! localLocationStatusResult =
                        Async.StartAsPromise(
                            localLocationSession.Core.GetStatus(
                                OperationContext.detached "local-location-initialize-status"
                            )
                        )

                    let localLocationStatus =
                        expectProviderValue "local self initialized status" localLocationStatusResult

                    let localLocationSynchronization =
                        localLocationStatus.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected local-location synchronization state.")

                    Vitest.expect(localLocationSynchronization.Relationship).toEqual (NoTarget)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "provider locations are validated when Initialize reports repository creation after remote failure",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let targetPath = join [| root; "remote-add-failure" |]
                    let hooks = {
                        GitWorkspaceSession.GitSessionHooks.none with
                            RunProcess =
                                Some(fun request processContext ->
                                    if request.Arguments |> Array.contains "remote" then
                                        async {
                                            return
                                                OperationResult.succeeded(
                                                    processOutput 128 "" "fatal: injected remote add failure"
                                                )
                                        }
                                    else
                                        NodeProcess.run request processContext)
                    }

                    let factory = GitWorkspaceSession.createFactory hooks

                    let! result =
                        Async.StartAsPromise(
                            factory.Initialize
                                {
                                    TargetPath = targetPath
                                    Location =
                                        Some {
                                            ProviderId = gitProviderId
                                            DisplayName = None
                                            ProviderLocation = "https://example.invalid/repository.git"
                                            ConnectionProfileId = None
                                        }
                                }
                                (OperationContext.detached "initialize-remote-add-failure")
                        )

                    let failure = expectProviderFailure "initialize remote add failure" result
                    Vitest.expect(failure.StateChanged).toBe true
                    let! gitDirectory = runGitIn targetPath [||] [| "rev-parse"; "--git-dir" |] None
                    Vitest.expect(gitDirectory.Trim()).toBe ".git"

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "converts supported origin forms into credential-free repository browser URLs",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let browser =
                        workspace.Session.RepositoryBrowser
                        |> Option.defaultWith (fun () -> failwith "Expected Git repository browser service.")

                    let cases = [|
                        "https://github.com/example/version-control-service.git",
                        "https://github.com/example/version-control-service"
                        "https://gitlab.example/group/project",
                        "https://gitlab.example/group/project"
                        "ssh://git@gitlab.example/group/project.git",
                        "https://gitlab.example/group/project"
                        "https://oauth2:secret@gitlab.example/group/project.git",
                        "https://gitlab.example/group/project"
                        "https://localhost:3443/a/b.git", "https://localhost:3443/a/b"
                        "https://host:443/a/b", "https://host/a/b"
                        "ssh://git@host:2222/a/b.git", "https://host/a/b"
                    |]

                    for remoteUrl, expectedUrl in cases do
                        let! _ =
                            runGitIn
                                workspace.Binding.WorkspaceRoot
                                [||]
                                [| "remote"; "set-url"; "origin"; remoteUrl |]
                                None

                        let! result =
                            browser.GetRepositoryWebUrl(OperationContext.detached "repository-browser-url")
                            |> Async.StartAsPromise

                        Vitest.expect(expectProviderValue "repository browser URL" result).toEqual (Some expectedUrl)

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "rejects submodule-internal and gitlink selections without moving the ref",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let repoPath = workspace.Binding.WorkspaceRoot
                    let childPath = join [| dirname repoPath; "child-repository" |]

                    let! _ = runGitIn (dirname repoPath) [||] [| "init"; "-b"; "main"; childPath |] None
                    do! writeUtf8FileAsync (join [| childPath; "inner.txt" |]) "inner base\n"
                    do! configureUser childPath
                    let! _ = runGitIn childPath [||] [| "add"; "-A" |] None
                    let! _ = runGitIn childPath [||] [| "commit"; "-m"; "test: child base" |] None

                    let! _ =
                        runGitIn
                            repoPath
                            [||]
                            [|
                                "-c"
                                "protocol.file.allow=always"
                                "submodule"
                                "add"
                                childPath.Replace("\\", "/")
                                "sub"
                            |]
                            None

                    let! _ = runGitIn repoPath [||] [| "commit"; "-m"; "test: add submodule" |] None
                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "submodule-status")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "submodule status" statusResult

                    let! innerResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: submodule-internal selection"
                                Paths = [| RepositoryPath.tryCreate "sub/inner.txt" |> Result.defaultWith failwith |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "submodule-internal-selection")
                        |> Async.StartAsPromise

                    let innerFailure = expectProviderFailure "submodule-internal selection" innerResult
                    Vitest.expect(innerFailure.Category).toEqual Validation
                    Vitest.expect(innerFailure.Code).toBe "submodule_internal_path"

                    let checkedOutChild = join [| repoPath; "sub" |]
                    do! configureUser checkedOutChild
                    do! writeUtf8FileAsync (join [| checkedOutChild; "inner.txt" |]) "inner changed\n"
                    let! _ = runGitIn checkedOutChild [||] [| "add"; "-A" |] None
                    let! _ = runGitIn checkedOutChild [||] [| "commit"; "-m"; "test: advance submodule" |] None
                    let! headBefore = runGitIn repoPath [||] [| "rev-parse"; "HEAD" |] None
                    let! freshStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "gitlink-status")
                        |> Async.StartAsPromise

                    let freshStatus = expectProviderValue "gitlink status" freshStatusResult

                    let! gitlinkResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: gitlink selection"
                                Paths = [| RepositoryPath.tryCreate "sub" |> Result.defaultWith failwith |]
                                ExpectedWorkspaceVersion = freshStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "gitlink-selection")
                        |> Async.StartAsPromise

                    let gitlinkFailure = expectProviderFailure "gitlink selection" gitlinkResult
                    Vitest.expect(gitlinkFailure.Code).toBe "submodule_internal_path"
                    let! headAfter = runGitIn repoPath [||] [| "rev-parse"; "HEAD" |] None
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

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
                        "quote\"name.bin", "quoteXname.bin", "nested/quote\"name.bin"
                        "bell\u0007name.bin", "bellXname.bin", "nested/bell\u0007name.bin"
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
                    Vitest.expect(attributeFile.Contains("\"/quote\\\"name.bin\" filter=lfs")).toBe true
                    Vitest.expect(attributeFile.Contains("\"/bell\\aname.bin\" filter=lfs")).toBe true

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
            "storage policy rejects an attributes parent junction without mutating its outside target",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let attributesPath = join [| workspace.Binding.WorkspaceRoot; ".gitattributes" |]
                    let outsideContent = "outside.bin filter=lfs diff=lfs merge=lfs -text\n"
                    do! writeUtf8FileAsync attributesPath outsideContent
                    let! outsideBefore = readFileBase64Async attributesPath
                    let linkedRepo = workspace.Binding.WorkspaceRoot + "-junction"
                    do! createDirectoryJunctionAsync workspace.Binding.WorkspaceRoot linkedRepo
                    let factory = GitWorkspaceSession.createFactory GitWorkspaceSession.GitSessionHooks.none

                    let! openResult =
                        factory.Open
                            { workspace.Binding with WorkspaceRoot = linkedRepo }
                            (OperationContext.detached "open-junction-workspace")
                        |> Async.StartAsPromise

                    let linkedSession = expectProviderValue "open junction workspace" openResult
                    let linkedStoragePolicy =
                        linkedSession.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    let! result =
                        linkedStoragePolicy.SetPathPolicy
                            (RepositoryPath.tryCreate "selected.bin" |> Result.defaultWith failwith)
                            true
                            (OperationContext.detached "track-through-junction")
                        |> Async.StartAsPromise

                    expectProviderFailure "track through junction" result |> ignore
                    let! outsideAfter = readFileBase64Async attributesPath
                    Vitest.expect(outsideAfter).toBe outsideBefore
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "storage policy leaves previous attributes byte-identical when the atomic replacement fails",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let attributesPath = join [| workspace.Binding.WorkspaceRoot; ".gitattributes" |]
                    let original = "# preserved bytes\r\nlegacy.bin filter=lfs diff=lfs merge=lfs -text\r\n"
                    do! writeUtf8FileAsync attributesPath original
                    let before = readFileBase64Async attributesPath
                    let restoreRename = injectRenameSyncFailure ()

                    let storagePolicy =
                        workspace.Session.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    let selectedPath = RepositoryPath.tryCreate "selected.bin" |> Result.defaultWith failwith

                    let! result =
                        promise {
                            try
                                return!
                                    storagePolicy.SetPathPolicy
                                        selectedPath
                                        true
                                        (OperationContext.detached "atomic-attributes-failure")
                                    |> Async.StartAsPromise
                            finally
                                restoreRename ()
                        }

                    let failure = expectProviderFailure "atomic attributes replacement" result
                    Vitest.expect(failure.Category).toEqual ProviderError
                    Vitest.expect(failure.StateChanged).toBe false
                    let! before = before
                    let! after = readFileBase64Async attributesPath
                    Vitest.expect(after).toBe before
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "untracking a path preserves legacy unanchored and glob rules byte-for-byte while appending literal unsets",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let attributesPath = join [| workspace.Binding.WorkspaceRoot; ".gitattributes" |]
                    let legacyRules =
                        "file.bin filter=lfs diff=lfs merge=lfs -text\r\n*.dat filter=lfs diff=lfs merge=lfs -text\r\n"

                    do! writeUtf8FileAsync attributesPath legacyRules
                    let storagePolicy =
                        workspace.Session.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    for value in [| "nested/file.bin"; "nested/report.dat" |] do
                        let path = RepositoryPath.tryCreate value |> Result.defaultWith failwith
                        let! result =
                            storagePolicy.SetPathPolicy
                                path
                                false
                                (OperationContext.detached $"preserve-legacy-{value}")
                            |> Async.StartAsPromise

                        expectProviderValue $"untrack {value}" result |> ignore

                    let expectedAfter =
                        legacyRules
                        + "\"/nested/file.bin\" -filter -diff -merge\r\n"
                        + "\"/nested/report.dat\" -filter -diff -merge\r\n"

                    let! afterBase64 = readFileBase64Async attributesPath
                    Vitest.expect(afterBase64).toBe (utf8Base64 expectedAfter)
                    let! after = tryReadUtf8FileAsync attributesPath
                    let after = after |> Option.defaultValue ""
                    Vitest.expect(after.StartsWith legacyRules).toBe true
                    Vitest.expect(after.Contains("\"/nested/file.bin\" -filter -diff -merge")).toBe true
                    Vitest.expect(after.Contains("\"/nested/report.dat\" -filter -diff -merge")).toBe true

                    let! attributeOutput =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "check-attr"; "-z"; "filter"; "--"; "nested/file.bin"; "nested/report.dat" |]
                            None

                    Vitest.expect(attributeOutput.Contains("nested/file.bin\000filter\000lfs\000")).toBe false
                    Vitest.expect(attributeOutput.Contains("nested/report.dat\000filter\000lfs\000")).toBe false
                    Vitest.expect(attributeOutput.Contains("nested/file.bin\000filter\000unset\000")).toBe true
                    Vitest.expect(attributeOutput.Contains("nested/report.dat\000filter\000unset\000")).toBe true

                    for value in [| "nested/file.bin"; "nested/report.dat" |] do
                        let path = RepositoryPath.tryCreate value |> Result.defaultWith failwith
                        let! enableResult =
                            storagePolicy.SetPathPolicy
                                path
                                true
                                (OperationContext.detached $"cycle-enable-{value}")
                            |> Async.StartAsPromise

                        expectProviderValue $"cycle enable {value}" enableResult |> ignore
                        let! disableResult =
                            storagePolicy.SetPathPolicy
                                path
                                false
                                (OperationContext.detached $"cycle-disable-{value}")
                            |> Async.StartAsPromise

                        expectProviderValue $"cycle disable {value}" disableResult |> ignore

                    let! cycledAttributes = tryReadUtf8FileAsync attributesPath
                    let cycledAttributes = cycledAttributes |> Option.defaultValue ""

                    for value in [| "nested/file.bin"; "nested/report.dat" |] do
                        let escaped = $"\"/{value}\""
                        let literalLines =
                            cycledAttributes.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
                            |> Array.filter _.StartsWith(escaped)

                        Vitest.expect(literalLines).toEqual [| $"{escaped} -filter -diff -merge" |]

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
            "maintenance streams real process progress totals above two GiB",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()
                let! fakeRoot = createTempDirectoryAsync ()
                let completedBytes = 3.0 * 1024.0 * 1024.0 * 1024.0
                let totalBytes = 4.0 * 1024.0 * 1024.0 * 1024.0
                let completedBytesText = string (int64 completedBytes)
                let totalBytesText = string (int64 totalBytes)

                try
                    let! workspace = harness.CreateWorkspace()
                    let fakeGitPath = join [| fakeRoot; if isWindowsProcess () then "git.exe" else "git" |]

                    if isWindowsProcess () then
                        let! _ = fsPromisesDynamic?copyFile (nodeExecutablePath, fakeGitPath) |> unbox<JS.Promise<obj>>
                        do!
                            writeUtf8FileAsync
                                (join [| workspace.Binding.WorkspaceRoot; "rev-parse" |])
                                "console.log('true');\n"

                        do! writeUtf8FileAsync (join [| workspace.Binding.WorkspaceRoot; "status" |]) ""

                        do!
                            writeUtf8FileAsync
                                (join [| workspace.Binding.WorkspaceRoot; "lfs" |])
                                ("console.log('deduplicate: 75% ("
                                 + completedBytesText
                                 + "/"
                                 + totalBytesText
                                 + " bytes)');\n")
                    else
                        let dispatcher = join [| fakeRoot; "fake-git.js" |]

                        // Same contract as the Windows fake above: answer the repository probe with
                        // "true" and stay silent for status, so the maintenance path reaches dedup.
                        do!
                            writeUtf8FileAsync
                                dispatcher
                                ("if (process.argv.includes('rev-parse')) console.log('true');\nelse if (process.argv.includes('dedup')) console.log('deduplicate: 75% ("
                                 + completedBytesText
                                 + "/"
                                 + totalBytesText
                                 + " bytes)');\n")

                        do!
                            writeUtf8FileAsync
                                fakeGitPath
                                $"#!/bin/sh\nexec '{nodeExecutablePath}' '{dispatcher}' \"$@\"\n"

                        let! _ = fsPromisesDynamic?chmod (fakeGitPath, 493) |> unbox<JS.Promise<obj>>
                        ()

                    let reports = ResizeArray<OperationProgress>()
                    let context =
                        OperationContext.create "large-maintenance-progress" OperationCancellation.none reports.Add

                    let maintenance =
                        workspace.Session.Maintenance
                        |> Option.defaultWith (fun () -> failwith "Expected Git maintenance.")

                    let originalPath = currentProcessPath ()
                    let! result =
                        promise {
                            try
                                setCurrentProcessPath (fakeRoot + pathDelimiter + originalPath)
                                return! maintenance.Deduplicate context |> Async.StartAsPromise
                            finally
                                setCurrentProcessPath originalPath
                        }

                    expectProviderValue "large maintenance progress" result |> ignore

                    let largeReport =
                        reports
                        |> Seq.find (fun report -> report.Completed.IsSome)

                    Vitest.expect(largeReport.PhaseCode).toBe "maintenance-deduplicate"
                    Vitest.expect(largeReport.Completed).toEqual (Some completedBytes)
                    Vitest.expect(largeReport.Total).toEqual (Some totalBytes)
                    do! harness.Cleanup()
                    do! removeDirectoryAsync fakeRoot
                with error ->
                    do! harness.Cleanup()
                    do! removeDirectoryAsync fakeRoot
                    return raise error
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

Vitest.describe (
    "Git / materialization honors context",
    fun () ->
        Vitest.test (
            "materialization honors context for ListObjects",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    do! workspace.WriteFile ".gitattributes" "materialized.bin filter=lfs diff=lfs merge=lfs -text\n"
                    do! workspace.WriteFile "materialized.bin" "materialization context content\n"
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "add"; "-A" |] None
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "commit"; "-m"; "test: materialization context" |] None
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "push"; "origin"; "main" |] None

                    let materialization =
                        workspace.Session.ObjectMaterialization
                        |> Option.defaultWith (fun () -> failwith "Expected Git object materialization.")

                    let cancellation = OperationCancellation.Source()
                    let mutable transferStarted = false
                    let context =
                        OperationContext.create
                            "materialization-list-cancel"
                            cancellation.Cancellation
                            (fun progress ->
                                if progress.PhaseCode = "lfs-list-transfer" then
                                    transferStarted <- true
                                    cancellation.Cancel())

                    let! result = materialization.ListObjects context |> Async.StartAsPromise
                    Vitest.expect(transferStarted).toBe true
                    let failure = expectProviderFailure "canceled LFS listing" result
                    Vitest.expect(failure.Category).toEqual Canceled
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "materialization honors context after a spawned transfer and immediate retry rejects delayed taskkill lock races",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()
                let! fakeRoot = createTempDirectoryAsync ()

                try
                    let! workspace = harness.CreateWorkspace()
                    do! workspace.WriteFile ".gitattributes" "materialized.bin filter=lfs diff=lfs merge=lfs -text\n"
                    do! workspace.WriteFile "materialized.bin" "materialization context content\n"
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "add"; "-A" |] None
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "commit"; "-m"; "test: materialization context" |] None
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "push"; "origin"; "main" |] None

                    let materialization =
                        workspace.Session.ObjectMaterialization
                        |> Option.defaultWith (fun () -> failwith "Expected Git object materialization.")

                    let path = RepositoryPath.tryCreate "materialized.bin" |> Result.defaultWith failwith

                    let! initialDematerialize =
                        materialization.Dematerialize path (OperationContext.detached "materialization-prepare")
                        |> Async.StartAsPromise

                    expectProviderValue "prepare materialization context" initialDematerialize |> ignore

                    let materializeCancellation = OperationCancellation.Source()
                    let mutable materializeTransferStarted = false
                    let materializeContext =
                        OperationContext.create
                            "materialization-cancel"
                            materializeCancellation.Cancellation
                            (fun progress ->
                                if progress.PhaseCode = "lfs-materialize-transfer" then
                                    materializeTransferStarted <- true
                                    materializeCancellation.Cancel())

                    let! materializeResult =
                        materialization.Materialize path materializeContext |> Async.StartAsPromise

                    Vitest.expect(materializeTransferStarted).toBe true
                    let materializeFailure = expectProviderFailure "canceled materialization" materializeResult
                    Vitest.expect(materializeFailure.Category).toEqual Canceled

                    let! restoredMaterialization =
                        materialization.Materialize path (OperationContext.detached "materialization-restore")
                        |> Async.StartAsPromise

                    expectProviderValue "restore materialization" restoredMaterialization |> ignore

                    let! listingJson =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "lfs"; "ls-files"; "-j" |]
                            None

                    let! pointerText =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [|
                                "lfs"
                                "pointer"
                                "--file"
                                join [| workspace.Binding.WorkspaceRoot; "materialized.bin" |]
                            |]
                            None

                    let fakeGitPath = join [| fakeRoot; if isWindowsProcess () then "git.exe" else "git" |]
                    let listingPath = join [| fakeRoot; "listing.json" |]
                    let pointerPath = join [| fakeRoot; "pointer.txt" |]
                    let markerPath = join [| fakeRoot; "slow-transfer" |]

                    do! writeUtf8FileAsync listingPath listingJson
                    do! writeUtf8FileAsync pointerPath pointerText

                    if isWindowsProcess () then
                        let! _ = fsPromisesDynamic?copyFile (nodeExecutablePath, fakeGitPath) |> unbox<JS.Promise<obj>>
                        ()
                    else
                        do!
                            writeUtf8FileAsync
                                fakeGitPath
                                $"#!/bin/sh\nscript=\"$1\"\nshift\nexec '{nodeExecutablePath}' \"$VCS_TEST_LFS_DISPATCH_ROOT/$script\" \"$@\"\n"

                        let! _ = fsPromisesDynamic?chmod (fakeGitPath, 493) |> unbox<JS.Promise<obj>>
                        ()

                    do! writeUtf8FileAsync (join [| workspace.Binding.WorkspaceRoot; "status" |]) ""
                    do! writeUtf8FileAsync (join [| workspace.Binding.WorkspaceRoot; "checkout" |]) ""
                    do! writeUtf8FileAsync (join [| workspace.Binding.WorkspaceRoot; "rev-parse" |]) "console.log('true');\n"

                    do!
                        writeUtf8FileAsync
                            (join [| workspace.Binding.WorkspaceRoot; "config" |])
                            "process.stdout.write(process.env.VCS_TEST_LFS_REMOTE + '\\n');\n"

                    // The LFS session reads the effective remote URL with `git remote get-url`.
                    do!
                        writeUtf8FileAsync
                            (join [| workspace.Binding.WorkspaceRoot; "remote" |])
                            "process.stdout.write(process.env.VCS_TEST_LFS_REMOTE + '\\n');\n"

                    do!
                        writeUtf8FileAsync
                            (join [| workspace.Binding.WorkspaceRoot; "check-attr" |])
                            "process.stdout.write('materialized.bin\\0filter\\0lfs\\0');\n"

                    do!
                        writeUtf8FileAsync
                            (join [| workspace.Binding.WorkspaceRoot; "lfs" |])
                            """
const fs = require('node:fs');
const net = require('node:net');
const command = process.argv[2] || '';
const markerPath = process.env.VCS_TEST_LFS_MARKER;
const port = Number(process.env.VCS_TEST_LFS_LOCK_PORT);
if (command === 'ls-files') {
    process.stdout.write(fs.readFileSync(process.env.VCS_TEST_LFS_LISTING, 'utf8'));
} else if (command === 'pointer') {
    process.stdout.write(fs.readFileSync(process.env.VCS_TEST_LFS_POINTER, 'utf8'));
} else if (command === 'fetch' || command === 'smudge') {
    if (fs.existsSync(markerPath) && fs.readFileSync(markerPath, 'utf8').trim() === command) {
        fs.rmSync(markerPath, { force: true });
        const lock = net.createServer();
        lock.listen(port, '127.0.0.1', () => {
            process.stderr.write('controlled LFS transfer running\n');
            setTimeout(() => lock.close(() => process.exit(0)), 10000);
        });
    } else {
        const probe = net.createServer();
        probe.once('error', error => {
            if (error.code === 'EADDRINUSE') {
                process.stderr.write("fatal: Unable to create '.git/index.lock': File exists.\n");
                process.exit(2);
            } else {
                throw error;
            }
        });
        probe.listen(port, '127.0.0.1', () => probe.close(() => process.exit(0)));
    }
} else {
    process.exit(0);
}
"""

                    do! writeUtf8FileAsync markerPath "fetch"

                    let originalPath = currentProcessPath ()
                    let originalWorkingDirectory = currentProcessWorkingDirectory ()
                    let environmentNames = [|
                        "VCS_TEST_LFS_LISTING"
                        "VCS_TEST_LFS_POINTER"
                        "VCS_TEST_LFS_MARKER"
                        "VCS_TEST_LFS_LOCK_PORT"
                        "VCS_TEST_LFS_REMOTE"
                        "VCS_TEST_LFS_DISPATCH_ROOT"
                    |]
                    let originalEnvironment =
                        environmentNames
                        |> Array.map (fun name -> name, (tryGetProcessEnvironment name |> Option.ofObj))

                    let restoreTaskkill =
                        if isWindowsProcess () then Some(injectDelayedTaskkillSpawn ()) else None

                    let! dematerializeResult, retryResult, dematerializeTransferStarted =
                        promise {
                            try
                                setCurrentProcessPath (fakeRoot + pathDelimiter + originalPath)
                                setCurrentProcessWorkingDirectory workspace.Binding.WorkspaceRoot
                                setProcessEnvironment "VCS_TEST_LFS_LISTING" listingPath
                                setProcessEnvironment "VCS_TEST_LFS_POINTER" pointerPath
                                setProcessEnvironment "VCS_TEST_LFS_MARKER" markerPath
                                setProcessEnvironment "VCS_TEST_LFS_LOCK_PORT" "43871"
                                setProcessEnvironment "VCS_TEST_LFS_REMOTE" workspace.Binding.Location.ProviderLocation
                                setProcessEnvironment "VCS_TEST_LFS_DISPATCH_ROOT" workspace.Binding.WorkspaceRoot

                                let dematerializeCancellation = OperationCancellation.Source()
                                let mutable transferStarted = false
                                let dematerializeContext =
                                    OperationContext.create
                                        "dematerialization-cancel"
                                        dematerializeCancellation.Cancellation
                                        (fun progress ->
                                            if progress.PhaseCode = "lfs-dematerialize-transfer" && not transferStarted then
                                                transferStarted <- true

                                                Fable.Core.JS.setTimeout
                                                    (fun () -> dematerializeCancellation.Cancel())
                                                    50
                                                |> ignore)

                                let! canceledResult =
                                    materialization.Dematerialize path dematerializeContext |> Async.StartAsPromise

                                let! immediateRetry =
                                    materialization.Dematerialize
                                        path
                                        (OperationContext.detached "dematerialization-immediate-retry")
                                    |> Async.StartAsPromise

                                if isWindowsProcess () then
                                    do! Async.Sleep 750 |> Async.StartAsPromise

                                return canceledResult, immediateRetry, transferStarted
                            finally
                                setCurrentProcessWorkingDirectory originalWorkingDirectory
                                setCurrentProcessPath originalPath
                                restoreTaskkill |> Option.iter (fun restore -> restore ())

                                for name, originalValue in originalEnvironment do
                                    match originalValue with
                                    | Some value -> setProcessEnvironment name value
                                    | None -> clearProcessEnvironment name
                        }

                    Vitest.expect(dematerializeTransferStarted).toBe true
                    let dematerializeFailure = expectProviderFailure "canceled dematerialization" dematerializeResult
                    Vitest.expect(dematerializeFailure.Category).toEqual Canceled
                    expectProviderValue "dematerialization immediate retry" retryResult |> ignore
                    do! harness.Cleanup()
                    do! removeDirectoryAsync fakeRoot
                with error ->
                    do! harness.Cleanup()
                    do! removeDirectoryAsync fakeRoot
                    return raise error
            }
        )

        Vitest.test (
            "materialization honors context with byte progress",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let content = "materialization progress content\n"
                    let expectedSize = float content.Length
                    do! workspace.WriteFile ".gitattributes" "materialized.bin filter=lfs diff=lfs merge=lfs -text\n"
                    do! workspace.WriteFile "materialized.bin" content
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "add"; "-A" |] None
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "commit"; "-m"; "test: materialization progress" |] None
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "push"; "origin"; "main" |] None

                    let materialization =
                        workspace.Session.ObjectMaterialization
                        |> Option.defaultWith (fun () -> failwith "Expected Git object materialization.")

                    let path = RepositoryPath.tryCreate "materialized.bin" |> Result.defaultWith failwith
                    let listReports = ResizeArray<OperationProgress>()
                    let listContext =
                        OperationContext.create "listing-progress" OperationCancellation.none listReports.Add

                    let! listResult = materialization.ListObjects listContext |> Async.StartAsPromise
                    expectProviderValue "listing progress" listResult |> ignore

                    let! _ = materialization.Dematerialize path (OperationContext.detached "progress-prepare") |> Async.StartAsPromise

                    let materializeReports = ResizeArray<OperationProgress>()
                    let materializeContext =
                        OperationContext.create
                            "materialization-progress"
                            OperationCancellation.none
                            materializeReports.Add

                    let! materializeResult = materialization.Materialize path materializeContext |> Async.StartAsPromise
                    expectProviderValue "materialization progress" materializeResult |> ignore

                    let! _ = materialization.Dematerialize path (OperationContext.detached "progress-reset") |> Async.StartAsPromise
                    let dematerializeReports = ResizeArray<OperationProgress>()
                    let dematerializeContext =
                        OperationContext.create
                            "dematerialization-progress"
                            OperationCancellation.none
                            dematerializeReports.Add

                    let! dematerializeResult =
                        materialization.Dematerialize path dematerializeContext |> Async.StartAsPromise

                    expectProviderValue "dematerialization progress" dematerializeResult |> ignore

                    let hasByteProgress expectedPhase (reports: ResizeArray<OperationProgress>) =
                        reports
                        |> Seq.exists (fun report ->
                            report.PhaseCode = expectedPhase
                            && report.Completed.IsSome
                            && report.Total = Some expectedSize)

                    Vitest.expect(hasByteProgress "lfs-list" listReports).toBe true
                    Vitest.expect(hasByteProgress "lfs-materialize" materializeReports).toBe true
                    Vitest.expect(hasByteProgress "lfs-dematerialize" dematerializeReports).toBe true
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

Vitest.describe (
    "Git / automatic Git LFS policy",
    fun () ->
        Vitest.test (
            "tracks selected files at or above the default one MiB threshold",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let oneMiB = 1024 * 1024
                    do! workspace.WriteFile "below.bin" (String.replicate (oneMiB - 1) "b")
                    do! workspace.WriteFile "exact.bin" (String.replicate oneMiB "e")
                    do! workspace.WriteFile "above.bin" (String.replicate (oneMiB + 1) "a")

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "automatic-lfs-status")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "automatic LFS status" statusResult

                    let paths =
                        [| "below.bin"; "exact.bin"; "above.bin" |]
                        |> Array.map (RepositoryPath.tryCreate >> Result.defaultWith failwith)

                    let! revisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: automatic LFS threshold"
                                Paths = paths
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "automatic-lfs-revision")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic LFS revision" revisionResult |> ignore

                    let! below = runGitIn workspace.Binding.WorkspaceRoot [||] [| "show"; "HEAD:below.bin" |] None
                    let! exact = runGitIn workspace.Binding.WorkspaceRoot [||] [| "show"; "HEAD:exact.bin" |] None
                    let! above = runGitIn workspace.Binding.WorkspaceRoot [||] [| "show"; "HEAD:above.bin" |] None

                    Vitest.expect(below.Contains "git-lfs").toBe false
                    Vitest.expect(exact.Contains "git-lfs").toBe true
                    Vitest.expect(above.Contains "git-lfs").toBe true

                    let! attributes = workspace.ReadFile ".gitattributes"
                    let attributes = attributes |> Option.defaultValue ""
                    Vitest.expect(attributes.Contains "\"/exact.bin\" filter=lfs").toBe true
                    Vitest.expect(attributes.Contains "\"/above.bin\" filter=lfs").toBe true
                    Vitest.expect(attributes.Contains "\"/below.bin\" filter=lfs").toBe false

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "validates and persists only library-owned automatic LFS settings",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let storagePolicy =
                        workspace.Session.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    let! setResult =
                        storagePolicy.SetSettings
                            {
                                AutoPolicyThresholdMb = Some 4
                                MaterializeLargeObjects = true
                            }
                            (OperationContext.detached "set-library-lfs-settings")
                        |> Async.StartAsPromise

                    expectProviderValue "set library LFS settings" setResult |> ignore

                    let! threshold =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "config"; "--local"; "--get"; "versioncontrolservice.lfs.autotrackthresholdmb" |]
                            None

                    let! materialize =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "config"; "--local"; "--get"; "versioncontrolservice.lfs.materializelargeobjects" |]
                            None

                    let! localConfig =
                        runGitIn workspace.Binding.WorkspaceRoot [||] [| "config"; "--local"; "--list" |] None

                    Vitest.expect(threshold.Trim()).toBe "4"
                    Vitest.expect(materialize.Trim()).toBe "true"
                    let providerSettings =
                        localConfig.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
                        |> Array.filter (fun entry -> entry.StartsWith "versioncontrolservice.lfs.")

                    Vitest.expect(providerSettings.Length).toBe 2

                    let fourMiB = 4 * 1024 * 1024
                    do! workspace.WriteFile "configured-below.bin" (String.replicate (fourMiB - 1) "b")
                    do! workspace.WriteFile "configured exact [file].bin" (String.replicate fourMiB "e")

                    let! configuredStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "configured-lfs-status")
                        |> Async.StartAsPromise

                    let configuredStatus = expectProviderValue "configured LFS status" configuredStatusResult

                    let configuredPaths =
                        [| "configured-below.bin"; "configured exact [file].bin" |]
                        |> Array.map (RepositoryPath.tryCreate >> Result.defaultWith failwith)

                    let! configuredRevisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: configured automatic LFS threshold"
                                Paths = configuredPaths
                                ExpectedWorkspaceVersion = configuredStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "configured-lfs-revision")
                        |> Async.StartAsPromise

                    expectProviderValue "configured automatic LFS revision" configuredRevisionResult |> ignore

                    let! configuredBelow =
                        runGitIn workspace.Binding.WorkspaceRoot [||] [| "show"; "HEAD:configured-below.bin" |] None

                    let! configuredExact =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "show"; "HEAD:configured exact [file].bin" |]
                            None

                    Vitest.expect(configuredBelow.Contains "git-lfs").toBe false
                    Vitest.expect(configuredExact.Contains "git-lfs").toBe true

                    let! configuredAttributes = workspace.ReadFile ".gitattributes"
                    let configuredAttributes = configuredAttributes |> Option.defaultValue ""
                    Vitest.expect(configuredAttributes.Contains "\"/configured exact \\\\[file\\\\].bin\" filter=lfs").toBe true

                    let! literalRuleResult =
                        storagePolicy.SetPathPolicy
                            (RepositoryPath.tryCreate "literal[auto]* name.bin" |> Result.defaultWith failwith)
                            true
                            (OperationContext.detached "literal-rule-policy")
                        |> Async.StartAsPromise

                    expectProviderValue "literal rule policy" literalRuleResult |> ignore
                    let! literalAttributes = workspace.ReadFile ".gitattributes"

                    Vitest.expect(
                        literalAttributes
                        |> Option.exists _.Contains("\"/literal\\\\[auto\\\\]\\\\* name.bin\" filter=lfs diff=lfs merge=lfs -text")
                    ).toBe true

                    Vitest.expect(RepositoryPath.tryCreate "literal\\path.bin" |> Result.isError).toBe true

                    for invalidThreshold in [| 0; -1 |] do
                        let! invalidResult =
                            storagePolicy.SetSettings
                                {
                                    AutoPolicyThresholdMb = Some invalidThreshold
                                    MaterializeLargeObjects = false
                                }
                                (OperationContext.detached $"invalid-lfs-threshold-{invalidThreshold}")
                            |> Async.StartAsPromise

                        let failure = expectProviderFailure "invalid LFS threshold" invalidResult
                        Vitest.expect(failure.Category).toEqual Validation
                        Vitest.expect(failure.StateChanged).toBe false

                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [|
                                "config"
                                "--local"
                                "versioncontrolservice.lfs.autotrackthresholdmb"
                                "1.5"
                            |]
                            None

                    do! workspace.WriteFile "fractional.bin" (String.replicate (1024 * 1024) "f")

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "fractional-threshold-status")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "fractional threshold status" statusResult
                    let fractionalPath =
                        RepositoryPath.tryCreate "fractional.bin"
                        |> Result.defaultWith failwith

                    let! revisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: reject fractional threshold"
                                Paths = [| fractionalPath |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "fractional-threshold-revision")
                        |> Async.StartAsPromise

                    let fractionalFailure = expectProviderFailure "fractional LFS threshold" revisionResult
                    Vitest.expect(fractionalFailure.Category).toEqual Validation
                    Vitest.expect(fractionalFailure.StateChanged).toBe false
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "automatic Git LFS policy reuses committed wildcard attributes without appending rules",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let attributesContent = "# committed policy\n*.bin filter=lfs diff=lfs merge=lfs -text\n"
                    let attributesPath = join [| workspace.Binding.WorkspaceRoot; ".gitattributes" |]
                    do! workspace.WriteFile ".gitattributes" attributesContent
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "add"; ".gitattributes" |] None
                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "commit"; "-m"; "test: commit wildcard LFS policy" |]
                            None

                    let! attributesBefore = readFileBase64Async attributesPath
                    let oneMiB = 1024 * 1024

                    for index, marker in [| 1, "a"; 2, "b"; 3, "c" |] do
                        do! workspace.WriteFile "data/big.bin" (String.replicate oneMiB marker)

                        let! statusResult =
                            workspace.Session.Core.GetStatus(OperationContext.detached $"automatic-policy-covered-status-{index}")
                            |> Async.StartAsPromise

                        let status = expectProviderValue "automatic policy covered status" statusResult
                        let! revisionResult =
                            workspace.Session.Core.CreateRevision
                                {
                                    Message = $"test: automatic policy covered commit {index}"
                                    Paths = [| mkRepositoryPath "data/big.bin" |]
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                }
                                (OperationContext.detached $"automatic-policy-covered-revision-{index}")
                            |> Async.StartAsPromise

                        expectProviderValue "automatic policy covered revision" revisionResult |> ignore
                        let! committed = runGitIn workspace.Binding.WorkspaceRoot [||] [| "show"; "HEAD:data/big.bin" |] None
                        let! attributesAfter = readFileBase64Async attributesPath
                        Vitest.expect(committed.Contains "git-lfs").toBe true
                        Vitest.expect(attributesAfter).toBe attributesBefore

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "automatic Git LFS policy ignores dirty irrelevant attributes",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let committedAttributes = "*.bin filter=lfs diff=lfs merge=lfs -text\n"
                    let dirtyAttributes = committedAttributes + "# unrelated consumer edit\n"
                    do! workspace.WriteFile ".gitattributes" committedAttributes
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "add"; ".gitattributes" |] None
                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "commit"; "-m"; "test: commit wildcard policy" |]
                            None

                    do! workspace.WriteFile ".gitattributes" dirtyAttributes
                    do! workspace.WriteFile "data/big.bin" (String.replicate (1024 * 1024) "d")

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "automatic-policy-dirty-status")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "automatic policy dirty status" statusResult
                    let! revisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: automatic policy ignores dirty attributes"
                                Paths = [| mkRepositoryPath "data/big.bin" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "automatic-policy-dirty-revision")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy dirty revision" revisionResult |> ignore
                    let! committed = runGitIn workspace.Binding.WorkspaceRoot [||] [| "show"; "HEAD:data/big.bin" |] None
                    let! attributesAfter = tryReadUtf8FileAsync (join [| workspace.Binding.WorkspaceRoot; ".gitattributes" |])
                    Vitest.expect(committed.Contains "git-lfs").toBe true
                    Vitest.expect(attributesAfter).toEqual (Some dirtyAttributes)
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "automatic Git LFS policy disables a wildcard-covered path with a literal unset",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    do! workspace.WriteFile ".gitattributes" "*.bin filter=lfs diff=lfs merge=lfs -text\n"
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "add"; ".gitattributes" |] None
                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "commit"; "-m"; "test: commit wildcard disable policy" |]
                            None

                    let storagePolicy =
                        workspace.Session.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    let! result =
                        storagePolicy.SetPathPolicy
                            (mkRepositoryPath "data/big.bin")
                            false
                            (OperationContext.detached "automatic-policy-disable-wildcard")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy disable wildcard" result |> ignore
                    let! attributes =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "check-attr"; "-z"; "filter"; "--"; "data/big.bin" |]
                            None

                    Vitest.expect(attributes.Contains("data/big.bin\000filter\000lfs\000")).toBe false
                    Vitest.expect(attributes.Contains("data/big.bin\000filter\000unset\000")).toBe true
                    let! attributeFile = tryReadUtf8FileAsync (join [| workspace.Binding.WorkspaceRoot; ".gitattributes" |])
                    Vitest.expect(attributeFile |> Option.exists _.Contains("\"/data/big.bin\" -filter -diff -merge")).toBe true
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "automatic Git LFS policy preserves an explicit unset for an oversized path",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let attributesPath = join [| workspace.Binding.WorkspaceRoot; ".gitattributes" |]
                    do! workspace.WriteFile ".gitattributes" "*.bin filter=lfs diff=lfs merge=lfs -text\n"
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "add"; ".gitattributes" |] None
                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "commit"; "-m"; "test: commit wildcard opt-out policy" |]
                            None

                    let storagePolicy =
                        workspace.Session.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    let! unsetResult =
                        storagePolicy.SetPathPolicy
                            (mkRepositoryPath "data/big.bin")
                            false
                            (OperationContext.detached "automatic-policy-explicit-unset")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy explicit unset" unsetResult |> ignore
                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "add"; ".gitattributes" |] None
                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "commit"; "-m"; "test: commit explicit LFS opt-out" |]
                            None

                    let! attributesBefore = readFileBase64Async attributesPath
                    let content = String.replicate (1024 * 1024) "n"
                    do! workspace.WriteFile "data/big.bin" content
                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "automatic-policy-opt-out-status")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "automatic policy opt-out status" statusResult
                    let! revisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: preserve explicit automatic LFS opt-out"
                                Paths = [| mkRepositoryPath "data/big.bin" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "automatic-policy-opt-out-revision")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy opt-out revision" revisionResult |> ignore
                    let! committed = runGitIn workspace.Binding.WorkspaceRoot [||] [| "show"; "HEAD:data/big.bin" |] None
                    let! attributesAfter = readFileBase64Async attributesPath
                    let! filter =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "check-attr"; "-z"; "filter"; "--"; "data/big.bin" |]
                            None

                    Vitest.expect(committed.Contains "git-lfs").toBe false
                    Vitest.expect(committed.Length).toBe content.Length
                    Vitest.expect(attributesAfter).toBe attributesBefore
                    Vitest.expect(filter.Contains("data/big.bin\000filter\000unset\000")).toBe true
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "automatic Git LFS policy probes plans and disables a literal metacharacter path",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let relativePath =
                        if isWindowsProcess () then
                            "data/space [literal].bin"
                        else
                            "data/space [literal]*.bin"

                    do! workspace.WriteFile relativePath (String.replicate (1024 * 1024) "p")
                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "automatic-policy-literal-status")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "automatic policy literal status" statusResult
                    let! revisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: automatic LFS literal metacharacter path"
                                Paths = [| mkRepositoryPath relativePath |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "automatic-policy-literal-revision")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy literal revision" revisionResult |> ignore
                    let attributesPath = join [| workspace.Binding.WorkspaceRoot; ".gitattributes" |]
                    let! generatedAttributes = tryReadUtf8FileAsync attributesPath
                    let generatedAttributes = generatedAttributes |> Option.defaultValue ""
                    let escapedPattern =
                        if isWindowsProcess () then
                            "\"/data/space \\\\[literal\\\\].bin\""
                        else
                            "\"/data/space \\\\[literal\\\\]\\\\*.bin\""

                    if not (generatedAttributes.Contains($"{escapedPattern} filter=lfs")) then
                        failwith $"Expected generated literal tracking rule; attributes were: {generatedAttributes}"

                    do!
                        workspace.WriteFile
                            ".gitattributes"
                            (generatedAttributes + "*.bin filter=lfs diff=lfs merge=lfs -text\n")

                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "add"; ".gitattributes" |] None
                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "commit"; "-m"; "test: add wildcard for literal disable" |]
                            None

                    let storagePolicy =
                        workspace.Session.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    let! disableResult =
                        storagePolicy.SetPathPolicy
                            (mkRepositoryPath relativePath)
                            false
                            (OperationContext.detached "automatic-policy-literal-disable")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy literal disable" disableResult |> ignore
                    let! filter =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "check-attr"; "-z"; "filter"; "--"; relativePath |]
                            None

                    let! disabledAttributes = tryReadUtf8FileAsync attributesPath
                    let disabledAttributes = disabledAttributes |> Option.defaultValue ""
                    let literalLines =
                        disabledAttributes.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
                        |> Array.filter _.StartsWith(escapedPattern)

                    if not (filter.Contains($"{relativePath}\000filter\000unset\000")) then
                        failwith $"Expected literal path to resolve unset; check-attr returned: {filter}"

                    let expectedLiteralLines = [| $"{escapedPattern} -filter -diff -merge" |]

                    if literalLines <> expectedLiteralLines then
                        let literalSummary = String.concat " | " literalLines
                        failwith $"Expected one literal unset line; found: {literalSummary}"

                    let asteriskPath = "data/policy*.bin"
                    let! enableAsterisk =
                        storagePolicy.SetPathPolicy
                            (mkRepositoryPath asteriskPath)
                            true
                            (OperationContext.detached "automatic-policy-asterisk-enable")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy asterisk enable" enableAsterisk |> ignore
                    let! disableAsterisk =
                        storagePolicy.SetPathPolicy
                            (mkRepositoryPath asteriskPath)
                            false
                            (OperationContext.detached "automatic-policy-asterisk-disable")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy asterisk disable" disableAsterisk |> ignore
                    let! asteriskFilter =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "check-attr"; "-z"; "filter"; "--"; asteriskPath |]
                            None

                    if asteriskFilter.Contains($"{asteriskPath}\000filter\000lfs\000") then
                        let! finalAttributes = tryReadUtf8FileAsync attributesPath
                        failwith
                            $"Expected literal asterisk path to be disabled; check-attr returned {asteriskFilter}; attributes were {finalAttributes}."
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "automatic Git LFS policy reports local settings and commit threshold honestly",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let storagePolicy =
                        workspace.Session.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    let! setFourResult =
                        storagePolicy.SetSettings
                            {
                                AutoPolicyThresholdMb = Some 4
                                MaterializeLargeObjects = true
                            }
                            (OperationContext.detached "automatic-policy-set-four")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy set four" setFourResult |> ignore
                    let! settingsFourResult =
                        storagePolicy.GetSettings (OperationContext.detached "automatic-policy-get-four")
                        |> Async.StartAsPromise

                    let settingsFour = expectProviderValue "automatic policy get four" settingsFourResult
                    Vitest.expect(settingsFour.AutoPolicyThresholdMb).toEqual (Some 4)
                    Vitest.expect(settingsFour.MaterializeLargeObjects).toBe true
                    do! workspace.WriteFile "four-megabyte-threshold.bin" (String.replicate (1024 * 1024) "f")

                    let! firstStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "automatic-policy-four-status")
                        |> Async.StartAsPromise

                    let firstStatus = expectProviderValue "automatic policy four status" firstStatusResult
                    let! firstRevisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: automatic policy four MiB threshold"
                                Paths = [| mkRepositoryPath "four-megabyte-threshold.bin" |]
                                ExpectedWorkspaceVersion = firstStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "automatic-policy-four-revision")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy four revision" firstRevisionResult |> ignore
                    let! firstCommitted =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "show"; "HEAD:four-megabyte-threshold.bin" |]
                            None

                    Vitest.expect(firstCommitted.Contains "git-lfs").toBe false

                    let! setOneResult =
                        storagePolicy.SetSettings
                            {
                                AutoPolicyThresholdMb = Some 1
                                MaterializeLargeObjects = false
                            }
                            (OperationContext.detached "automatic-policy-set-one")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy set one" setOneResult |> ignore
                    let! settingsOneResult =
                        storagePolicy.GetSettings (OperationContext.detached "automatic-policy-get-one")
                        |> Async.StartAsPromise

                    let settingsOne = expectProviderValue "automatic policy get one" settingsOneResult
                    Vitest.expect(settingsOne.AutoPolicyThresholdMb).toEqual (Some 1)
                    Vitest.expect(settingsOne.MaterializeLargeObjects).toBe false
                    do! workspace.WriteFile "one-megabyte-threshold.bin" (String.replicate (1024 * 1024) "o")

                    let! secondStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "automatic-policy-one-status")
                        |> Async.StartAsPromise

                    let secondStatus = expectProviderValue "automatic policy one status" secondStatusResult
                    let! secondRevisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: automatic policy one MiB threshold"
                                Paths = [| mkRepositoryPath "one-megabyte-threshold.bin" |]
                                ExpectedWorkspaceVersion = secondStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "automatic-policy-one-revision")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy one revision" secondRevisionResult |> ignore
                    let! secondCommitted =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "show"; "HEAD:one-megabyte-threshold.bin" |]
                            None

                    Vitest.expect(secondCommitted.Contains "git-lfs").toBe true
                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "config"; "--local"; "versioncontrolservice.lfs.autotrackthresholdmb"; "garbage" |]
                            None

                    let! corruptSettingsResult =
                        storagePolicy.GetSettings (OperationContext.detached "automatic-policy-corrupt-settings")
                        |> Async.StartAsPromise

                    let corruptSettingsFailure = expectProviderFailure "automatic policy corrupt settings" corruptSettingsResult
                    Vitest.expect(corruptSettingsFailure.Code).toBe "invalid_lfs_threshold"

                    do! workspace.WriteFile "corrupt-threshold.bin" (String.replicate (1024 * 1024) "c")
                    let! corruptStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "automatic-policy-corrupt-status")
                        |> Async.StartAsPromise

                    let corruptStatus = expectProviderValue "automatic policy corrupt status" corruptStatusResult
                    let! corruptRevisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: reject corrupt automatic threshold"
                                Paths = [| mkRepositoryPath "corrupt-threshold.bin" |]
                                ExpectedWorkspaceVersion = corruptStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "automatic-policy-corrupt-revision")
                        |> Async.StartAsPromise

                    let corruptRevisionFailure = expectProviderFailure "automatic policy corrupt revision" corruptRevisionResult
                    Vitest.expect(corruptRevisionFailure.Code).toBe "invalid_lfs_threshold"
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "automatic Git LFS policy ignores global settings while local settings win",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()
                let originalGlobal = tryGetProcessEnvironment "GIT_CONFIG_GLOBAL" |> Option.ofObj

                try
                    let! workspace = harness.CreateWorkspace()
                    let globalConfig = join [| dirname workspace.Binding.WorkspaceRoot; "automatic-policy-global.config" |]
                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "config"; "--file"; globalConfig; "versioncontrolservice.lfs.autotrackthresholdmb"; "4" |]
                            None

                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "config"; "--file"; globalConfig; "versioncontrolservice.lfs.materializelargeobjects"; "true" |]
                            None

                    setProcessEnvironment "GIT_CONFIG_GLOBAL" globalConfig
                    let storagePolicy =
                        workspace.Session.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    let! defaultSettingsResult =
                        storagePolicy.GetSettings (OperationContext.detached "automatic-policy-global-default-settings")
                        |> Async.StartAsPromise

                    let defaultSettings = expectProviderValue "automatic policy global default settings" defaultSettingsResult
                    Vitest.expect(defaultSettings.AutoPolicyThresholdMb).toEqual (Some 1)
                    Vitest.expect(defaultSettings.MaterializeLargeObjects).toBe false
                    do! workspace.WriteFile "global-only.bin" (String.replicate (1024 * 1024) "g")
                    let! firstStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "automatic-policy-global-status")
                        |> Async.StartAsPromise

                    let firstStatus = expectProviderValue "automatic policy global status" firstStatusResult
                    let! firstRevisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: ignore global automatic LFS settings"
                                Paths = [| mkRepositoryPath "global-only.bin" |]
                                ExpectedWorkspaceVersion = firstStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "automatic-policy-global-revision")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy global revision" firstRevisionResult |> ignore
                    let! firstCommitted =
                        runGitIn workspace.Binding.WorkspaceRoot [||] [| "show"; "HEAD:global-only.bin" |] None

                    Vitest.expect(firstCommitted.Contains "git-lfs").toBe true
                    let! _ =
                        runGitIn
                            workspace.Binding.WorkspaceRoot
                            [||]
                            [| "config"; "--file"; globalConfig; "versioncontrolservice.lfs.autotrackthresholdmb"; "1" |]
                            None

                    let! localSettingsResult =
                        storagePolicy.SetSettings
                            {
                                AutoPolicyThresholdMb = Some 4
                                MaterializeLargeObjects = true
                            }
                            (OperationContext.detached "automatic-policy-local-wins-set")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy local wins set" localSettingsResult |> ignore
                    let! localReportedResult =
                        storagePolicy.GetSettings (OperationContext.detached "automatic-policy-local-wins-get")
                        |> Async.StartAsPromise

                    let localReported = expectProviderValue "automatic policy local wins get" localReportedResult
                    Vitest.expect(localReported.AutoPolicyThresholdMb).toEqual (Some 4)
                    Vitest.expect(localReported.MaterializeLargeObjects).toBe true
                    do! workspace.WriteFile "local-wins.bin" (String.replicate (1024 * 1024) "l")
                    let! secondStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "automatic-policy-local-wins-status")
                        |> Async.StartAsPromise

                    let secondStatus = expectProviderValue "automatic policy local wins status" secondStatusResult
                    let! secondRevisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: local automatic LFS settings win"
                                Paths = [| mkRepositoryPath "local-wins.bin" |]
                                ExpectedWorkspaceVersion = secondStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "automatic-policy-local-wins-revision")
                        |> Async.StartAsPromise

                    expectProviderValue "automatic policy local wins revision" secondRevisionResult |> ignore
                    let! secondCommitted =
                        runGitIn workspace.Binding.WorkspaceRoot [||] [| "show"; "HEAD:local-wins.bin" |] None

                    Vitest.expect(secondCommitted.Contains "git-lfs").toBe false
                    match originalGlobal with
                    | Some value -> setProcessEnvironment "GIT_CONFIG_GLOBAL" value
                    | None -> clearProcessEnvironment "GIT_CONFIG_GLOBAL"

                    do! harness.Cleanup()
                with error ->
                    match originalGlobal with
                    | Some value -> setProcessEnvironment "GIT_CONFIG_GLOBAL" value
                    | None -> clearProcessEnvironment "GIT_CONFIG_GLOBAL"

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

let private lfsPointerPrefix = "version https://git-lfs.github.com/spec/v1"
let private lfsCsvContent = "name,value\nalpha,1\nbeta,2\ngamma,3\n"

let private lfsOidFromPointer (pointer: string) =
    pointer.Replace("\r\n", "\n").Split('\n')
    |> Array.tryPick (fun line ->
        let prefix = "oid sha256:"

        if line.StartsWith(prefix, StringComparison.Ordinal) then
            Some(line.Substring(prefix.Length).Trim())
        else
            None)
    |> Option.defaultWith (fun () -> failwith "Expected an LFS pointer object id.")

let private lfsMediaDirectoryFromEnvironment (lfsEnvironment: string) =
    lfsEnvironment.Replace("\r\n", "\n").Split('\n')
    |> Array.tryPick (fun line ->
        let prefix = "LocalMediaDir="
        let line = line.TrimEnd('\r')

        if line.StartsWith(prefix, StringComparison.Ordinal) then
            Some(line.Substring(prefix.Length).Trim())
        else
            None)
    |> Option.defaultWith (fun () -> failwith "Expected Git LFS to report its local media directory.")

let private lfsObjectPath (mediaDirectory: string) (oid: string) =
    join [| mediaDirectory; oid.Substring(0, 2); oid.Substring(2, 2); oid |]

let private createLfsBaseContentFixture () = promise {
    let! root = createTempDirectoryAsync ()
    let repoPath = join [| root; "work" |]
    let csvPath = join [| repoPath; "data.csv" |]
    let binaryPath = join [| repoPath; "data.bin" |]
    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; repoPath |] None
    do! configureUser repoPath
    let! _ = runGitIn repoPath [||] [| "lfs"; "install"; "--local" |] None
    let! _ = runGitIn repoPath [||] [| "lfs"; "track"; "data.csv" |] None
    let! _ = runGitIn repoPath [||] [| "lfs"; "track"; "data.bin" |] None
    do! writeUtf8FileAsync csvPath lfsCsvContent
    do! writeBinaryFileAsync binaryPath [| 0; 255; 7; 8; 9 |]
    let! _ = runGitIn repoPath [||] [| "add"; "-A" |] None
    let! _ = runGitIn repoPath [||] [| "commit"; "-m"; "test: commit LFS base files" |] None
    let! csvPointer = runGitIn repoPath [||] [| "show"; "HEAD:data.csv" |] None
    do! writeUtf8FileAsync csvPath csvPointer
    let! _ = runGitIn repoPath [||] [| "lfs"; "checkout"; "--"; "data.csv" |] None

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

    return root, repoPath, GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding
}

let private createDownloadedLfsRestoreFixture () = promise {
    let! root = createTempDirectoryAsync ()
    let barePath = join [| root; "origin.git" |]
    let sourcePath = join [| root; "source" |]
    let repoPath = join [| root; "downloaded" |]

    let! _ = runGitIn root [||] [| "init"; "--bare"; "-b"; "main"; barePath |] None
    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; sourcePath |] None
    do! configureUser sourcePath
    let! _ = runGitIn sourcePath [||] [| "lfs"; "install"; "--local" |] None
    let! _ = runGitIn sourcePath [||] [| "lfs"; "track"; "*.bin" |] None
    do! writeUtf8FileAsync (join [| sourcePath; "large.bin" |]) "large binary payload\n"
    let! _ = runGitIn sourcePath [||] [| "add"; "-A" |] None
    let! _ = runGitIn sourcePath [||] [| "commit"; "-m"; "test: downloaded LFS object" |] None
    let! _ = runGitIn sourcePath [||] [| "remote"; "add"; "origin"; barePath |] None
    let! _ = runGitIn sourcePath [||] [| "push"; "-u"; "origin"; "main" |] None
    let! _ = runGitIn root [||] [| "clone"; barePath; repoPath |] None
    let! _ = runGitIn repoPath [||] [| "lfs"; "install"; "--local" |] None
    let! _ = runGitIn repoPath [||] [| "lfs"; "pull"; "origin" |] None

    let binding: WorkspaceBinding = {
        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
        ProviderId = gitProviderId
        WorkspaceRoot = repoPath
        ProviderStateRef = None
        Location = {
            ProviderId = gitProviderId
            DisplayName = None
            ProviderLocation = barePath
            ConnectionProfileId = None
        }
        ConnectionProfileId = None
    }

    return root, repoPath, GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding
}

let private createDownloadedLfsTextFixture (content: string) = promise {
    let! root = createTempDirectoryAsync ()
    let barePath = join [| root; "origin.git" |]
    let sourcePath = join [| root; "source" |]
    let repoPath = join [| root; "downloaded" |]

    let! _ = runGitIn root [||] [| "init"; "--bare"; "-b"; "main"; barePath |] None
    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; sourcePath |] None
    do! configureUser sourcePath
    let! _ = runGitIn sourcePath [||] [| "lfs"; "install"; "--local" |] None
    // A basename rule, as git lfs track writes it, also matches the absolute worktree path.
    let! _ = runGitIn sourcePath [||] [| "lfs"; "track"; "*.csv" |] None
    do! writeUtf8FileAsync (join [| sourcePath; "data.csv" |]) content
    let! _ = runGitIn sourcePath [||] [| "add"; "-A" |] None
    let! _ = runGitIn sourcePath [||] [| "commit"; "-m"; "test: downloaded LFS text" |] None
    let! _ = runGitIn sourcePath [||] [| "remote"; "add"; "origin"; barePath |] None
    let! _ = runGitIn sourcePath [||] [| "push"; "-u"; "origin"; "main" |] None
    let! _ = runGitIn root [||] [| "clone"; barePath; repoPath |] None
    let! _ = runGitIn repoPath [||] [| "lfs"; "install"; "--local" |] None
    let! _ = runGitIn repoPath [||] [| "lfs"; "pull"; "origin" |] None

    let binding: WorkspaceBinding = {
        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
        ProviderId = gitProviderId
        WorkspaceRoot = repoPath
        ProviderStateRef = None
        Location = {
            ProviderId = gitProviderId
            DisplayName = None
            ProviderLocation = barePath
            ConnectionProfileId = None
        }
        ConnectionProfileId = None
    }

    return root, repoPath, GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding
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
    "Git text diff",
    fun () ->
        Vitest.test (
            "text diff is literal and truthful about failures",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, session = createBaseContentFixture ()

                try
                    let canceled = OperationCancellation.Source()
                    canceled.Cancel()

                    let! canceledResult =
                        (textDiffService session).GetDiff
                            (repositoryPath "base.txt")
                            (OperationContext.create "diff-canceled" canceled.Cancellation ignore)
                        |> Async.StartAsPromise

                    let canceledFailure = expectProviderFailure "canceled text diff" canceledResult
                    Vitest.expect(canceledFailure.Category).toEqual (Canceled)

                    do! removeDirectoryAsync (join [| root; "work" |])

                    let! deletedResult =
                        (textDiffService session).GetDiff
                            (repositoryPath "base.txt")
                            (OperationContext.detached "diff-deleted-repository")
                        |> Async.StartAsPromise

                    let deletedFailure = expectProviderFailure "deleted-repository text diff" deletedResult
                    Vitest.expect(deletedFailure.Category).toEqual (ProviderError)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "text diff is literal and truthful for glob-metacharacter paths",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let files = [|
                        "report[1].csv", "report1.csv", "literal report original", "decoy report original"
                        "a*b.txt", "axb.txt", "literal star original", "decoy star original"
                    |]

                    for literalPath, decoyPath, literalContent, decoyContent in files do
                        let! literalObject =
                            runGitIn
                                workspace.Binding.WorkspaceRoot
                                [||]
                                [| "hash-object"; "-w"; "--stdin" |]
                                (Some literalContent)

                        let! _ =
                            runGitIn
                                workspace.Binding.WorkspaceRoot
                                [||]
                                [|
                                    "-c"
                                    "core.protectNTFS=false"
                                    "update-index"
                                    "--add"
                                    "--cacheinfo"
                                    $"100644,{literalObject.Trim()},{literalPath}"
                                |]
                                None

                        let! decoyObject =
                            runGitIn
                                workspace.Binding.WorkspaceRoot
                                [||]
                                [| "hash-object"; "-w"; "--stdin" |]
                                (Some decoyContent)

                        let! _ =
                            runGitIn
                                workspace.Binding.WorkspaceRoot
                                [||]
                                [|
                                    "-c"
                                    "core.protectNTFS=false"
                                    "update-index"
                                    "--add"
                                    "--cacheinfo"
                                    $"100644,{decoyObject.Trim()},{decoyPath}"
                                |]
                                None

                        ()

                    let! _ = runGitIn workspace.Binding.WorkspaceRoot [||] [| "commit"; "-m"; "test: literal diff paths" |] None

                    for literalPath, decoyPath, literalContent, decoyContent in files do
                        let! literalObject =
                            runGitIn
                                workspace.Binding.WorkspaceRoot
                                [||]
                                [| "hash-object"; "-w"; "--stdin" |]
                                (Some $"{literalContent} changed")

                        let! _ =
                            runGitIn
                                workspace.Binding.WorkspaceRoot
                                [||]
                                [|
                                    "-c"
                                    "core.protectNTFS=false"
                                    "update-index"
                                    "--add"
                                    "--cacheinfo"
                                    $"100644,{literalObject.Trim()},{literalPath}"
                                |]
                                None

                        let! decoyObject =
                            runGitIn
                                workspace.Binding.WorkspaceRoot
                                [||]
                                [| "hash-object"; "-w"; "--stdin" |]
                                (Some $"{decoyContent} changed")

                        let! _ =
                            runGitIn
                                workspace.Binding.WorkspaceRoot
                                [||]
                                [|
                                    "-c"
                                    "core.protectNTFS=false"
                                    "update-index"
                                    "--add"
                                    "--cacheinfo"
                                    $"100644,{decoyObject.Trim()},{decoyPath}"
                                |]
                                None

                        ()

                    let diff = textDiffService workspace.Session

                    for literalPathValue, decoyPathValue, literalContent, decoyContent in files do
                        let literalPath = repositoryPath literalPathValue

                        let! regularResult =
                            diff.GetDiff literalPath (OperationContext.detached $"diff-literal-{literalPathValue}")
                            |> Async.StartAsPromise

                        let regularText =
                            match expectProviderValue $"literal diff {literalPathValue}" regularResult with
                            | TextContent text -> text
                            | UnsupportedContent _ -> failwith "Expected a text diff."

                        Vitest.expect(regularText.Contains($"a/{literalPathValue}")).toBe (true)
                        Vitest.expect(regularText.Contains($"b/{literalPathValue}")).toBe (true)
                        Vitest.expect(regularText.Contains($"{literalContent} changed")).toBe (true)
                        Vitest.expect(regularText.Contains($"{decoyContent} changed")).toBe (false)
                        Vitest.expect(regularText.Contains(decoyPathValue)).toBe (false)

                        let! wordResult =
                            diff.GetWordDiff literalPath (OperationContext.detached $"word-diff-literal-{literalPathValue}")
                            |> Async.StartAsPromise

                        let wordText =
                            match expectProviderValue $"literal word diff {literalPathValue}" wordResult with
                            | TextContent text -> text
                            | UnsupportedContent _ -> failwith "Expected a text word diff."

                        Vitest.expect(wordText.Contains($"a/{literalPathValue}")).toBe (true)
                        Vitest.expect(wordText.Contains($"b/{literalPathValue}")).toBe (true)
                        Vitest.expect(wordText.Contains($"{literalContent} changed")).toBe (true)
                        Vitest.expect(wordText.Contains($"{decoyContent} changed")).toBe (false)
                        Vitest.expect(wordText.Contains(decoyPathValue)).toBe (false)

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

Vitest.describe (
    "Git clone target refs",
    fun () ->
        Vitest.test (
            "Clone rejects target refs before creating the target",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let targetPath = join [| root; "target-ref-clone" |]
                    let location = {
                        ProviderId = gitProviderId
                        DisplayName = None
                        ProviderLocation = join [| root; "origin.git" |]
                        ConnectionProfileId = None
                    }

                    let factory = GitWorkspaceSession.createFactory GitWorkspaceSession.GitSessionHooks.none
                    let! result =
                        factory.Clone
                            {
                                Location = location
                                TargetPath = targetPath
                                TargetRef = Some(ProviderRef.tryCreate "git-local:main" |> Result.defaultWith failwith)
                                MaterializeAllObjects = false
                            }
                            (OperationContext.detached "clone-unsupported-target-ref")
                        |> Async.StartAsPromise

                    match result with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Unsupported
                        Vitest.expect(failure.Code).toBe "target_ref_unsupported"
                    | Succeeded _
                    | PartiallySucceeded _ -> failwith "A clone with a target ref unexpectedly succeeded."

                    Vitest.expect(NodeFileSystem.existsSync targetPath).toBe(false)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

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
            "Git adoption strips origin userinfo from the binding and keeps the remote unchanged",
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let repoPath = join [| root; "credential-origin" |]
                    let credentialUrl = "https://user:secret@example.invalid/org/repo.git"

                    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; repoPath |] None
                    let! _ = runGitIn repoPath [||] [| "remote"; "add"; "origin"; credentialUrl |] None

                    let factory = GitWorkspaceSession.createFactory GitWorkspaceSession.GitSessionHooks.none

                    let! adoptionResult =
                        Async.StartAsPromise(
                            factory.Adopt
                                {
                                    WorkspaceRoot = repoPath
                                    ConnectionProfileId = None
                                }
                                (OperationContext.detached "adopt-credential-origin")
                        )

                    let binding = expectProviderValue "adopt credential-bearing Git origin" adoptionResult
                    Vitest.expect(binding.Location.ProviderLocation).toBe "https://example.invalid/org/repo.git"

                    let! configuredRemote =
                        runGitIn repoPath [||] [| "config"; "--get"; "remote.origin.url" |] None

                    Vitest.expect(configuredRemote.Trim()).toBe credentialUrl

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "Git Bind strips userinfo from the binding when fetch fails",
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let repoPath = join [| root; "bind-credential-origin" |]
                    let credentialUrl = "https://user:secret@example.invalid/org/repo.git"
                    let mutable fetchObserved = false

                    let! _ = runGitIn root [||] [| "init"; "-b"; "main"; repoPath |] None

                    let hooks = {
                        GitWorkspaceSession.GitSessionHooks.none with
                            RunProcess =
                                Some(fun request context ->
                                    if request.Arguments = [| "fetch"; "origin" |] then
                                        async {
                                            fetchObserved <- true
                                            return
                                                OperationResult.succeeded(
                                                    processOutput 128 "" "fatal: injected fetch failure"
                                                )
                                        }
                                    else
                                        NodeProcess.run request context)
                    }

                    let factory = GitWorkspaceSession.createFactory hooks
                    let location = {
                        ProviderId = gitProviderId
                        DisplayName = None
                        ProviderLocation = credentialUrl
                        ConnectionProfileId = None
                    }

                    let! bindResult =
                        Async.StartAsPromise(
                            factory.Bind
                                {
                                    WorkspaceRoot = repoPath
                                    Location = location
                                }
                                (OperationContext.detached "bind-credential-origin")
                        )

                    let binding = expectProviderValue "bind credential-bearing Git location" bindResult
                    Vitest.expect(fetchObserved).toBe true
                    Vitest.expect(binding.Location.ProviderLocation).toBe "https://example.invalid/org/repo.git"

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

        Vitest.test (
            "provider locations are validated before corrupt adoption guesses a binding",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let workspacePath = join [| root; "corrupt-repository" |]
                    let gitPath = join [| workspacePath; ".git" |]
                    do! ensureDirectoryAsync gitPath
                    do! writeBinaryFileAsync (join [| gitPath; "HEAD" |]) [| 0; 255; 1; 254 |]
                    do! writeBinaryFileAsync (join [| gitPath; "objects" |]) [| 9; 8; 7; 6 |]
                    let mutable revParseDetail = ""

                    let hooks = {
                        GitWorkspaceSession.GitSessionHooks.none with
                            RunProcess =
                                Some(fun request processContext ->
                                    async {
                                        let! result =
                                            NodeProcess.run
                                                {
                                                    request with
                                                        Environment =
                                                            Array.append
                                                                request.Environment
                                                                [| "GIT_CEILING_DIRECTORIES", root |]
                                                }
                                                processContext

                                        match request.Arguments, result with
                                        | [| "rev-parse"; "--show-toplevel" |], Succeeded outcome ->
                                            revParseDetail <- outcome.Value.StdErr + outcome.Value.StdOut
                                        | _ -> ()

                                        return result
                                    })
                    }

                    let factory = GitWorkspaceSession.createFactory hooks

                    let! adoptionResult =
                        Async.StartAsPromise(
                            factory.Adopt
                                {
                                    WorkspaceRoot = workspacePath
                                    ConnectionProfileId = None
                                }
                                (OperationContext.detached "adopt-corrupt-repository")
                        )

                    let failure = expectProviderFailure "adopt corrupt repository" adoptionResult
                    Vitest.expect(failure.Category).toEqual (Unsupported)
                    Vitest.expect(failure.Code).toBe ("adoption_unsupported")
                    Vitest.expect(failure.Message.Contains("without guessing")).toBe true
                    Vitest.expect(String.IsNullOrWhiteSpace revParseDetail).toBe false
                    Vitest.expect(failure.Message.Contains(revParseDetail.Trim())).toBe true

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "provider locations are validated without hiding dubious-ownership adoption remediation",
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let workspacePath = join [| root; "dubious-repository" |]
                    do! ensureDirectoryAsync (join [| workspacePath; ".git" |])
                    let detail =
                        "fatal: detected dubious ownership in repository at 'dubious-repository'\nTo add an exception, call git config --global --add safe.directory dubious-repository"

                    let hooks = {
                        GitWorkspaceSession.GitSessionHooks.none with
                            RunProcess =
                                Some(fun request _ ->
                                    async {
                                        match request.Arguments with
                                        | [| "rev-parse"; "--show-toplevel" |] ->
                                            return OperationResult.succeeded (processOutput 128 "" detail)
                                        | _ ->
                                            return
                                                OperationResult.failed(
                                                    OperationFailure.create
                                                        ProviderError
                                                        "unexpected_command"
                                                        "Unexpected Git command."
                                                )
                                    })
                    }

                    let factory = GitWorkspaceSession.createFactory hooks

                    let! result =
                        Async.StartAsPromise(
                            factory.Adopt
                                {
                                    WorkspaceRoot = workspacePath
                                    ConnectionProfileId = None
                                }
                                (OperationContext.detached "adopt-dubious-repository")
                        )

                    let failure = expectProviderFailure "adopt dubious repository" result
                    Vitest.expect(failure.Category).toEqual (ProviderError)
                    Vitest.expect(failure.Code).toBe ("git_failure")
                    Vitest.expect(failure.Message.Contains("dubious ownership")).toBe true
                    Vitest.expect(failure.Message.Contains("safe.directory")).toBe true

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
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
            "returns materialized committed LFS text when the object is local",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, repoPath, session = createLfsBaseContentFixture ()

                    try
                        do!
                            writeUtf8FileAsync
                                (join [| repoPath; "data.csv" |])
                                "name,value\nalpha,10\nbeta,20\ngamma,3\n"

                        let! result =
                            (textDiffService session).GetBaseContent
                                (repositoryPath "data.csv")
                                (OperationContext.detached "base-local-lfs-text")
                            |> Async.StartAsPromise

                        match expectProviderValue "local LFS text base content" result with
                        | TextContent text -> Vitest.expect(text).toBe lfsCsvContent
                        | UnsupportedContent _ -> failwith "Expected materialized local LFS text content."

                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "returns the LFS pointer when the base object is missing locally",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, repoPath, session = createLfsBaseContentFixture ()

                    try
                        let! pointer = runGitIn repoPath [||] [| "show"; "HEAD:data.csv" |] None
                        let! lfsEnvironment = runGitIn repoPath [||] [| "lfs"; "env" |] None
                        let mediaDirectory = lfsMediaDirectoryFromEnvironment lfsEnvironment
                        let oid = lfsOidFromPointer pointer
                        do! removeFileAsync (lfsObjectPath mediaDirectory oid)
                        do!
                            writeUtf8FileAsync
                                (join [| repoPath; "data.csv" |])
                                "name,value\nalpha,10\nbeta,20\ngamma,3\n"

                        let! result =
                            (textDiffService session).GetBaseContent
                                (repositoryPath "data.csv")
                                (OperationContext.detached "base-missing-lfs-object")
                            |> Async.StartAsPromise

                        match expectProviderValue "missing local LFS base content" result with
                        | TextContent text -> Vitest.expect(text.StartsWith(lfsPointerPrefix)).toBe true
                        | UnsupportedContent _ -> failwith "Expected the textual LFS pointer base content."

                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "word diff for a downloaded LFS file only removes the deleted row",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let rows = [| for lineNumber in 1..10 -> $"row-{lineNumber},value-{lineNumber}" |]
                    let baseContent = String.concat "\n" rows + "\n"

                    let worktreeContent =
                        rows
                        |> Array.filter (fun row -> row <> "row-3,value-3")
                        |> String.concat "\n"
                        |> fun content -> content + "\n"

                    let! root, repoPath, session = createDownloadedLfsTextFixture baseContent

                    try
                        do! writeUtf8FileAsync (join [| repoPath; "data.csv" |]) worktreeContent

                        let! wordResult =
                            (textDiffService session).GetWordDiff
                                (repositoryPath "data.csv")
                                (OperationContext.detached "word-diff-downloaded-lfs")
                            |> Async.StartAsPromise

                        let wordText =
                            match expectProviderValue "downloaded LFS word diff" wordResult with
                            | TextContent text -> text
                            | UnsupportedContent _ -> failwith "Expected a text word diff."

                        let diffLines = wordText.Replace("\r\n", "\n").Split('\n')

                        Vitest
                            .expect(
                                diffLines
                                |> Array.exists (fun line ->
                                    line.StartsWith("-row-3,value-3", StringComparison.Ordinal))
                            )
                            .toBe true

                        Vitest.expect(wordText.Contains("row-4,value-4")).toBe false

                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "RestorePaths materializes a downloaded LFS object from the local cache",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, repoPath, session = createDownloadedLfsRestoreFixture ()

                    try
                        do! writeUtf8FileAsync (join [| repoPath; "large.bin" |]) "local edit\n"
                        let! statusResult =
                            session.Core.GetStatus(OperationContext.detached "restore-downloaded-lfs-status")
                            |> Async.StartAsPromise

                        let status = expectProviderValue "status before restoring the LFS file" statusResult

                        let! restoreResult =
                            session.Core.RestorePaths
                                {
                                    Paths = [| repositoryPath "large.bin" |]
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                }
                                (OperationContext.detached "restore-downloaded-lfs")
                            |> Async.StartAsPromise

                        expectProviderValue "restore downloaded LFS file" restoreResult |> ignore
                        let! restored = tryReadUtf8FileAsync (join [| repoPath; "large.bin" |])
                        Vitest.expect(restored).toEqual (Some "large binary payload\n")

                        let! status = runGitIn repoPath [||] [| "status"; "--porcelain" |] None
                        Vitest.expect(status.Trim()).toBe ""
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "RestorePaths leaves a pointer when its LFS object is absent locally",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, repoPath, session = createDownloadedLfsRestoreFixture ()

                    try
                        let! pointer = runGitIn repoPath [||] [| "show"; "HEAD:large.bin" |] None
                        let! lfsEnvironment = runGitIn repoPath [||] [| "lfs"; "env" |] None
                        let mediaDirectory = lfsMediaDirectoryFromEnvironment lfsEnvironment
                        let objectId = lfsOidFromPointer pointer
                        do! removeFileAsync (lfsObjectPath mediaDirectory objectId)
                        do! writeUtf8FileAsync (join [| repoPath; "large.bin" |]) "local edit\n"

                        let! statusResult =
                            session.Core.GetStatus(OperationContext.detached "restore-lfs-without-local-object-status")
                            |> Async.StartAsPromise

                        let status = expectProviderValue "status before restoring the LFS file" statusResult

                        let! restoreResult =
                            session.Core.RestorePaths
                                {
                                    Paths = [| repositoryPath "large.bin" |]
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                }
                                (OperationContext.detached "restore-lfs-without-local-object")
                            |> Async.StartAsPromise

                        expectProviderValue "restore LFS path without a local object" restoreResult |> ignore
                        let! restored = tryReadUtf8FileAsync (join [| repoPath; "large.bin" |])
                        Vitest.expect(restored |> Option.exists (fun content -> content.StartsWith lfsPointerPrefix)).toBe true
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "RestorePaths restores an LFS directory",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, repoPath, session = createDownloadedLfsRestoreFixture ()

                    try
                        let firstPath = "data/first.bin"
                        let secondPath = "data/second.bin"
                        let firstContent = "first committed content\n"
                        let secondContent = "second committed content\n"

                        do! writeUtf8FileAsync (join [| repoPath; firstPath |]) firstContent
                        do! writeUtf8FileAsync (join [| repoPath; secondPath |]) secondContent
                        let! _ = runGitIn repoPath [||] [| "add"; "-A" |] None
                        let! _ = runGitIn repoPath [||] [| "commit"; "-m"; "test: restore an LFS directory" |] None

                        do! writeUtf8FileAsync (join [| repoPath; firstPath |]) "first local edit\n"
                        do! writeUtf8FileAsync (join [| repoPath; secondPath |]) "second local edit\n"

                        let! statusResult =
                            session.Core.GetStatus(OperationContext.detached "restore-LFS-directory-status")
                            |> Async.StartAsPromise

                        let status = expectProviderValue "status before restoring the LFS directory" statusResult

                        let! restoreResult =
                            session.Core.RestorePaths
                                {
                                    Paths = [| repositoryPath "data" |]
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                }
                                (OperationContext.detached "restore-LFS-directory")
                            |> Async.StartAsPromise

                        expectProviderValue "restore the LFS directory" restoreResult |> ignore
                        let! restoredFirst = tryReadUtf8FileAsync (join [| repoPath; firstPath |])
                        let! restoredSecond = tryReadUtf8FileAsync (join [| repoPath; secondPath |])

                        Vitest.expect(restoredFirst).toEqual (Some firstContent)
                        Vitest.expect(restoredSecond).toEqual (Some secondContent)
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "RestorePaths restores a non-ASCII tracked path",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, repoPath, session = createDownloadedLfsRestoreFixture ()

                    try
                        let relativePath = "data/Messung_ä.csv"
                        let! _ = runGitIn repoPath [||] [| "config"; "core.autocrlf"; "false" |] None
                        let committedContent = "Messung;Wert\nTemperatur;21.5\n"
                        let path = join [| repoPath; relativePath |]

                        do! writeUtf8FileAsync path committedContent
                        let! _ = runGitIn repoPath [||] [| "add"; "-A" |] None
                        let! _ = runGitIn repoPath [||] [| "commit"; "-m"; "test: restore a non-ASCII path" |] None
                        do! writeUtf8FileAsync path "edited;value\n"

                        let! statusResult =
                            session.Core.GetStatus(OperationContext.detached "restore-non-ASCII-status")
                            |> Async.StartAsPromise

                        let status = expectProviderValue "status before restoring the non-ASCII path" statusResult

                        let! restoreResult =
                            session.Core.RestorePaths
                                {
                                    Paths = [| repositoryPath relativePath |]
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                }
                                (OperationContext.detached "restore-non-ASCII-path")
                            |> Async.StartAsPromise

                        expectProviderValue "restore the non-ASCII path" restoreResult |> ignore
                        let! restored = tryReadUtf8FileAsync path
                        Vitest.expect(restored).toEqual (Some committedContent)
                        let! status = runGitIn repoPath [||] [| "status"; "--porcelain" |] None
                        Vitest.expect(status.Trim()).toBe ""
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "returns the LFS pointer when a same-sized local object has the wrong hash",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, repoPath, session = createLfsBaseContentFixture ()

                    try
                        let! pointer = runGitIn repoPath [||] [| "show"; "HEAD:data.csv" |] None
                        let! lfsEnvironment = runGitIn repoPath [||] [| "lfs"; "env" |] None
                        let mediaDirectory = lfsMediaDirectoryFromEnvironment lfsEnvironment
                        let objectPath = lfsObjectPath mediaDirectory (lfsOidFromPointer pointer)

                        do!
                            writeUtf8FileAsync
                                objectPath
                                (String.replicate lfsCsvContent.Length "x")

                        let! result =
                            (textDiffService session).GetBaseContent
                                (repositoryPath "data.csv")
                                (OperationContext.detached "base-mismatched-lfs-object")
                            |> Async.StartAsPromise

                        match expectProviderValue "mismatched local LFS base object" result with
                        | TextContent text -> Vitest.expect(text).toBe pointer
                        | UnsupportedContent _ -> failwith "Expected the LFS pointer text for the mismatched object."

                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "returns unsupported content for a local binary LFS object",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, repoPath, session = createLfsBaseContentFixture ()

                    try
                        let! result =
                            (textDiffService session).GetBaseContent
                                (repositoryPath "data.bin")
                                (OperationContext.detached "base-local-lfs-binary")
                            |> Async.StartAsPromise

                        match expectProviderValue "local LFS binary base content" result with
                        | UnsupportedContent _ -> ()
                        | TextContent text -> failwith $"Expected unsupported binary content, received '{text}'."

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

Vitest.describe (
    "Git index lock recovery",
    fun () ->
        Vitest.test (
            "Materialize reports a held index lock before running Git LFS",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let materialization =
                        workspace.Session.ObjectMaterialization
                        |> Option.defaultWith (fun () -> failwith "Expected Git object materialization.")

                    do!
                        writeUtf8FileAsync
                            (join [| workspace.Binding.WorkspaceRoot; ".git"; "index.lock" |])
                            "manual lock\n"

                    let! result =
                        materialization.Materialize
                            (mkRepositoryPath "base.txt")
                            (OperationContext.detached "materialize-index-lock")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "locked Materialize" result
                    Vitest.expect(failure.Category).toEqual Concurrency
                    Vitest.expect(failure.Code).toBe "index_locked"
                    Vitest.expect(failure.StateChanged).toBe false
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "remove_index_lock")
                    Vitest.expect(
                        failure.Details |> Array.exists (fun detail -> detail.StartsWith "Lock file age:")
                    ).toBe true
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "RestorePaths reports a held index lock with recovery details",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let harness = createGitHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    do! workspace.WriteFile "base.txt" "changed for restore\n"
                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "restore-index-lock-status")
                        |> Async.StartAsPromise

                    let status = expectProviderValue "status before locked restore" statusResult
                    do!
                        writeUtf8FileAsync
                            (join [| workspace.Binding.WorkspaceRoot; ".git"; "index.lock" |])
                            "manual lock\n"

                    let! restoreResult =
                        workspace.Session.Core.RestorePaths
                            {
                                Paths = [| mkRepositoryPath "base.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "restore-index-lock")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "locked RestorePaths" restoreResult
                    Vitest.expect(failure.Category).toEqual Concurrency
                    Vitest.expect(failure.Code).toBe "index_locked"
                    Vitest.expect(failure.StateChanged).toBe false
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "remove_index_lock")
                    Vitest.expect(
                        failure.Details |> Array.exists (fun detail -> detail.StartsWith "Lock file age:")
                    ).toBe true
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)
