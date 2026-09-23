module VersionControlService.Tests.LakeFsProviderContractTests

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.LakeFs.LakeFsTypes
open VersionControlService.Tests.Contracts
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.NodePath
open Vitest

module LakeFsApi = VersionControlService.LakeFs.LakeFsApi
module LakeFsCredentials = VersionControlService.LakeFs.LakeFsCredentials
module LakeFsConflictSession = VersionControlService.LakeFs.LakeFsConflictSession
module LakeFsMaterialization = VersionControlService.LakeFs.LakeFsMaterialization
module LakeFsProviderOptions = VersionControlService.LakeFs.LakeFsProviderOptions
module LakeFsPathSafety = VersionControlService.LakeFs.LakeFsPathSafety
module LakeFsStateStore = VersionControlService.LakeFs.LakeFsStateStore
module LakeFsSynchronization = VersionControlService.LakeFs.LakeFsSynchronization
module LakeFsWorkspaceIndex = VersionControlService.LakeFs.LakeFsWorkspaceIndex
module LakeFsWorkspaceSession = VersionControlService.LakeFs.LakeFsWorkspaceSession
module RuntimeNodePath = VersionControlService.Runtime.Node.Path
module RuntimeNodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module RuntimeNodeInterop = VersionControlService.Runtime.Node.Interop

[<Emit("process.env[$0] ?? null")>]
let private getEnvironmentVariable (_name: string) : string = jsNative

[<Emit("JSON.stringify($0, null, 2)")>]
let private jsonStringify (_value: obj) : string = jsNative

[<Emit("process.platform === 'win32'")>]
let private isWindowsProcess () : bool = jsNative

[<Emit("process.platform === 'win32' || process.platform === 'darwin'")>]
let private localFileSystemAliasesCase () : bool = jsNative

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

[<Emit("""(() => {
    const original = globalThis.fetch;
    let uploads = 0;
    globalThis.fetch = (url, options) => {
        const method = String(options?.method ?? 'GET').toUpperCase();
        if (method === 'POST' && String(url).includes('/objects?path=')) uploads++;
        return original(url, options);
    };
    return {
        count: () => uploads,
        restore: () => { globalThis.fetch = original; }
    };
})()""")>]
let private beginUploadRequestTracking () : obj = jsNative

[<Emit("(() => { const fs = require('node:fs'); const moduleApi = require('node:module'); const original = fs.renameSync; let pending = true; fs.renameSync = (...args) => { if (pending) { pending = false; const error = new Error('injected cross-volume rename'); error.code = 'EXDEV'; throw error; } return original(...args); }; moduleApi.syncBuiltinESMExports(); return () => { fs.renameSync = original; moduleApi.syncBuiltinESMExports(); }; })()")>]
let private injectNextCrossVolumeRename () : (unit -> unit) = jsNative

[<Emit("((targetPath) => { const fs = require('node:fs'); const moduleApi = require('node:module'); const originalRename = fs.renameSync; const originalLstat = fs.lstatSync; let installed = false; fs.renameSync = (...args) => { const result = originalRename(...args); if (args[1] === targetPath) installed = true; return result; }; fs.lstatSync = (path, ...args) => { const stats = originalLstat(path, ...args); if (installed && path === targetPath) return new Proxy(stats, { get(value, property) { if (property === 'isSymbolicLink') return () => true; return Reflect.get(value, property); } }); return stats; }; moduleApi.syncBuiltinESMExports(); return () => { fs.renameSync = originalRename; fs.lstatSync = originalLstat; moduleApi.syncBuiltinESMExports(); }; })($0)")>]
let private injectPostRenameLinkObservation (_targetPath: string) : (unit -> unit) = jsNative

[<Emit("(() => { const fs = require('node:fs/promises'); const moduleApi = require('node:module'); const original = fs.rm; let pending = true; fs.rm = (...args) => { if (pending && String(args[0]).includes('transactions')) { pending = false; const error = new Error('injected cleanup failure'); error.code = 'EACCES'; return Promise.reject(error); } return original(...args); }; moduleApi.syncBuiltinESMExports(); return () => { fs.rm = original; moduleApi.syncBuiltinESMExports(); }; })()")>]
let private injectNextTransactionCleanupFailure () : (unit -> unit) = jsNative

let lakeFsProviderOptions: LakeFsProviderOptions.LakeFsProviderOptions = {
    StateRoot = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-lakefs-provider-state" |]
    PathCaseSensitivity = CaseInsensitive
}

let stateDirectoryForBinding (binding: WorkspaceBinding) =
    match LakeFsStateStore.resolve lakeFsProviderOptions binding.WorkspaceRoot binding.ProviderStateRef with
    | Ok state -> state.StateDirectory
    | Error failure -> failwith $"Resolving test provider state failed ({failure.Code}): {failure.Message}"

let integrationEnabled () =
    getEnvironmentVariable "LAKEFS_INTEGRATION" = "1"

let connection () : LakeFsConnection = {
    Endpoint =
        getEnvironmentVariable "LAKEFS_INTEGRATION_ENDPOINT"
        |> Option.ofObj
        |> Option.defaultValue "http://127.0.0.1:8000"
    AccessKeyId =
        getEnvironmentVariable "LAKEFS_INTEGRATION_ACCESS_KEY_ID"
        |> Option.ofObj
        |> Option.defaultValue "integration-access"
    SecretAccessKey =
        getEnvironmentVariable "LAKEFS_INTEGRATION_SECRET_ACCESS_KEY"
        |> Option.ofObj
        |> Option.defaultValue "integration-secret"
}

let private createTempDirectoryAsync () : JS.Promise<string> = promise {
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-lakefs-harness-" |]
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
    do! ensureDirectoryAsync (dirname path)
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

let private removeFileAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (path, createObj [ "force" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private context name = OperationContext.detached name

let private repositoryPath value =
    RepositoryPath.tryCreate value |> Result.defaultWith failwith

type private MaterializationRecoveryScenario = {
    Root: string
    State: LakeFsStateStore.ResolvedState
    WorkspaceRoot: string
    CurrentIndex: LakeFsWorkspaceIndex.WorkspaceIndex
    Plan: LakeFsMaterialization.MaterializationPlan
    Paths: RepositoryPath[]
    Targets: string[]
    OldContents: string[]
    NewContents: string[]
}

let private createMaterializationRecoveryScenario () = promise {
    let! root = createTempDirectoryAsync ()
    let workspaceRoot = join [| root; "workspace" |]
    let stateRoot = join [| root; "provider-state" |]
    do! ensureDirectoryAsync workspaceRoot

    let options: LakeFsProviderOptions.LakeFsProviderOptions = {
        StateRoot = stateRoot
        PathCaseSensitivity = CaseInsensitive
    }

    let state =
        LakeFsStateStore.create options workspaceRoot
        |> Result.defaultWith (fun failure -> failwith failure.Message)

    let paths = [|
        repositoryPath "000-first.txt"
        repositoryPath "100-second.txt"
        repositoryPath "zzz-third.txt"
    |]

    let targets =
        paths
        |> Array.map (RepositoryPath.value >> fun path -> join [| workspaceRoot; path |])

    let oldContents = [| "old first bytes\n"; "old second bytes\n"; "old third bytes\n" |]
    let newContents = [| "new first bytes\n"; "new second bytes\n"; "new third bytes\n" |]

    for target, content in Array.zip targets oldContents do
        do! writeUtf8FileAsync target content

    let initialIndex: LakeFsWorkspaceIndex.WorkspaceIndex = {
        SchemaVersion = LakeFsWorkspaceIndex.CurrentSchemaVersion
        Repository = "recovery-repository"
        TargetRef = "main"
        Prefix = ""
        WorkspaceBranch = "vcs-workspace-recovery"
        OwnershipToken = "recovery-ownership-token"
        BaseRevision = Some "expected-revision"
        WorkspaceRevision = Some "expected-revision"
        Generation = 0
        Entries =
            Array.map3
                (fun path target content ->
                    let stats = RuntimeNodeFileSystem.lstatSync target

                    {
                        Path = RepositoryPath.value path
                        BaseChecksum = $"old-{RepositoryPath.value path}"
                        LocalHash = LakeFsWorkspaceIndex.hashMetadata content
                        LocalSize = stats.size
                        LocalMtimeMs = 0.0
                    })
                paths
                targets
                oldContents
    }

    let currentIndex =
        LakeFsWorkspaceIndex.save state.StateDirectory initialIndex
        |> Result.defaultWith failwith

    let objects: LakeFsMaterialization.MaterializationObject[] =
        Array.map3
            (fun path target _ -> {
                Path = path
                ObjectKey = RepositoryPath.value path
                TargetPath = target
                BaseChecksum = $"new-{RepositoryPath.value path}"
                Mtime = 1.0
            })
            paths
            targets
            newContents

    let! prepared =
        LakeFsMaterialization.prepare
            state.TransactionsDirectory
            currentIndex
            false
            objects
            [||]
            (fun prepared temporaryPath _ -> async {
                let index = paths |> Array.findIndex ((=) prepared.Path)
                let content = newContents[index]
                RuntimeNodeFileSystem.writeUtf8FileExclusiveAndFlushSync temporaryPath content

                return
                    Ok {
                        BytesCopied = float content.Length
                        Sha256 = LakeFsWorkspaceIndex.hashMetadata content
                    }
            })
            (fun _ _ -> async.Return())
            (context "recovery-scenario-prepare")
        |> Async.StartAsPromise

    return {
        Root = root
        State = state
        WorkspaceRoot = workspaceRoot
        CurrentIndex = currentIndex
        Plan = prepared |> Result.defaultWith (fun failure -> failwith failure.Message)
        Paths = paths
        Targets = targets
        OldContents = oldContents
        NewContents = newContents
    }
}

let private partiallyApplyRecoveryScenario scenario = promise {
    let mutable applied = 0

    return!
        LakeFsMaterialization.apply
            scenario.WorkspaceRoot
            scenario.State.StateDirectory
            scenario.State.RecoveryDirectory
            scenario.CurrentIndex.WorkspaceRevision
            "observed-revision"
            scenario.Plan
            (fun point _ -> async {
                if point = "materialization-apply-object" then
                    applied <- applied + 1

                    if applied = 1 then
                        failwith "injected apply failure"
            })
            (context "recovery-scenario-partial-apply")
        |> Async.StartAsPromise
}

let private expectApi operation = function
    | Ok value -> value
    | Error failure -> failwith $"{operation} failed ({failure.Category}/{failure.Code}): {failure.Message}"

let uploadTextObject connection repository reference path content context =
    async {
        let sourcePath =
            join [|
                osDynamic?tmpdir () |> unbox<string>
                $"vcs-lakefs-upload-{RuntimeNodeInterop.randomUuid()}.tmp"
            |]

        RuntimeNodeFileSystem.writeUtf8FileExclusiveAndFlushSync sourcePath content

        try
            let! uploaded =
                LakeFsApi.uploadObjectFromFile
                    connection
                    repository
                    reference
                    path
                    sourcePath
                    context

            return uploaded |> Result.map ignore
        finally
            if RuntimeNodeFileSystem.existsSync sourcePath then
                RuntimeNodeFileSystem.unlinkSync sourcePath
    }

let private createRepository (repository: string) = promise {
    let payload =
        JS.JSON.stringify (
            createObj [
                "name" ==> repository
                "storage_namespace" ==> $"local://{repository}"
                "default_branch" ==> "main"
            ]
        )

    let! result =
        LakeFsApi.requestChecked
            (connection ())
            "POST"
            "/repositories"
            (Some("application/json", payload))
            (context "harness-create-repository")
        |> Async.StartAsPromise

    result |> expectApi "create repository" |> ignore
}

let private deleteRepository (repository: string) = promise {
    let! _ =
        LakeFsApi.request
            (connection ())
            "DELETE"
            $"/repositories/{repository}"
            None
            (context "harness-delete-repository")
        |> Async.StartAsPromise

    return ()
}

let private parseLocation (location: RepositoryLocation) =
    LakeFsLocation.tryParse location.ProviderLocation
    |> Result.defaultWith failwith

type private WorkspaceControl = {
    Location: RepositoryLocation
    Binding: WorkspaceBinding
    mutable RaceMutations: TargetMutation[] option
    mutable SlowTransfer: bool
}

let private finalizePathMutations = Collections.Generic.Dictionary<string, unit -> JS.Promise<unit>>()
let private materializationApplyFailures = Collections.Generic.Dictionary<string, int>()

let private armFinalizePathMutation workspaceRoot mutation =
    finalizePathMutations[workspaceRoot] <- mutation

let private armMaterializationApplyFailure workspaceRoot =
    materializationApplyFailures[workspaceRoot] <- 1

let createLakeFsHarness () : ProviderTestHarness =
    let tempRoots = ResizeArray<string>()
    let repositories = ResizeArray<string>()
    let controls = Collections.Generic.Dictionary<string, WorkspaceControl>()
    let stateDirectories = ResizeArray<string>()
    let mutable counter = 0
    let mutable publishShouldBreak = false
    let mutable failNextConnection = false

    let nextId () =
        counter <- counter + 1
        $"task13-{RuntimeNodeInterop.randomUuid()}-{counter}"

    let credentials: LakeFsCredentials.LakeFsCredentialStrategy = {
        ResolveConnection =
            fun profileId -> async {
                if profileId = Some "unauthorized" then
                    return Ok { connection () with SecretAccessKey = "invalid-secret" }
                elif failNextConnection then
                    failNextConnection <- false
                    return Ok { connection () with SecretAccessKey = "invalid-secret" }
                else
                    return Ok(connection ())
            }
    }

    let locationFor (repository: string) (profile: string option) : RepositoryLocation = {
        ProviderId =
            ProviderId.tryCreate "lakefs"
            |> Result.defaultWith failwith
        DisplayName = Some repository
        ProviderLocation = $"lakefs://{repository}/main"
        ConnectionProfileId = profile
    }

    let bindingFor (root: string) (location: RepositoryLocation) : WorkspaceBinding =
        let state =
            match LakeFsStateStore.create lakeFsProviderOptions root with
            | Ok value -> value
            | Error failure -> failwith $"Creating test provider state failed ({failure.Code}): {failure.Message}"

        stateDirectories.Add state.StateDirectory

        {
            SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
            ProviderId = location.ProviderId
            WorkspaceRoot = root
            ProviderStateRef = Some state.StateId
            Location = location
            ConnectionProfileId = location.ConnectionProfileId
        }

    let advanceRef
        (location: RepositoryLocation)
        (reference: string)
        (mutations: TargetMutation[])
        = promise {
        let parsed = parseLocation location

        for mutation in mutations do
            let! result =
                match mutation.Content with
                | Some content ->
                    uploadTextObject
                        (connection ())
                        parsed.Repository
                        reference
                        mutation.Path
                        content
                        (context "harness-advance-upload")
                | None ->
                    LakeFsApi.deleteObject
                        (connection ())
                        parsed.Repository
                        reference
                        mutation.Path
                        (context "harness-advance-delete")
                |> Async.StartAsPromise

            result |> expectApi "advance target object" |> ignore

        let! committed =
            LakeFsApi.commit
                (connection ())
                parsed.Repository
                reference
                $"external: {reference} advance"
                (context "harness-advance-commit")
            |> Async.StartAsPromise

        committed |> expectApi "advance target commit" |> ignore
    }

    let advanceTarget (location: RepositoryLocation) (mutations: TargetMutation[]) =
        let parsed = parseLocation location
        advanceRef location parsed.TargetRef mutations

    let hooks: LakeFsWorkspaceSession.LakeFsSessionHooks = {
        Barrier =
            Some(fun root point operationContext -> async {
                match controls.TryGetValue root with
                | true, control ->
                    if point = "publish-connect" && publishShouldBreak then
                        publishShouldBreak <- false
                        failNextConnection <- true

                    if point = "materialization-apply-object" then
                        match materializationApplyFailures.TryGetValue root with
                        | true, remaining when remaining <= 1 ->
                            materializationApplyFailures.Remove root |> ignore
                            failwith "injected materialization recovery failure"
                        | true, remaining ->
                            materializationApplyFailures[root] <- remaining - 1
                        | false, _ -> ()

                    if point = "publish-precheck-done" then
                        match control.RaceMutations with
                        | Some mutations ->
                            control.RaceMutations <- None
                            do! Async.AwaitPromise(advanceTarget control.Location mutations)
                        | None -> ()

                    if point = "update-precheck-done" || point = "finalize-precheck-done" then
                        match control.RaceMutations with
                        | Some mutations ->
                            control.RaceMutations <- None

                            match LakeFsWorkspaceIndex.load (stateDirectoryForBinding control.Binding) with
                            | LakeFsWorkspaceIndex.Loaded index ->
                                do!
                                    Async.AwaitPromise(
                                        advanceRef control.Location index.WorkspaceBranch mutations
                                    )
                            | LakeFsWorkspaceIndex.Missing
                            | LakeFsWorkspaceIndex.Corrupt _ ->
                                failwith "Expected a persisted workspace index for the destination race."
                        | None -> ()

                    if point = "finalize-precheck-done" then
                        match finalizePathMutations.TryGetValue root with
                        | true, mutation ->
                            finalizePathMutations.Remove root |> ignore
                            do! Async.AwaitPromise(mutation ())
                        | false, _ -> ()

                    if point = "selected-revision-commit-done" then
                        match control.RaceMutations with
                        | Some mutations ->
                            control.RaceMutations <- None

                            match LakeFsWorkspaceIndex.load (stateDirectoryForBinding control.Binding) with
                            | LakeFsWorkspaceIndex.Loaded index ->
                                do!
                                    Async.AwaitPromise(
                                        advanceRef control.Location index.WorkspaceBranch mutations
                                    )
                            | LakeFsWorkspaceIndex.Missing
                            | LakeFsWorkspaceIndex.Corrupt _ ->
                                failwith "Expected a persisted workspace index for the selected-revision race."
                        | None -> ()

                    if
                        point = "selected-revision-upload-done"
                        && operationContext.Cancellation.IsCancellationRequested()
                    then
                        match control.RaceMutations with
                        | Some mutations ->
                            control.RaceMutations <- None

                            match LakeFsWorkspaceIndex.load (stateDirectoryForBinding control.Binding) with
                            | LakeFsWorkspaceIndex.Loaded index ->
                                do!
                                    Async.AwaitPromise(
                                        advanceRef control.Location index.WorkspaceBranch mutations
                                    )
                            | LakeFsWorkspaceIndex.Missing
                            | LakeFsWorkspaceIndex.Corrupt _ ->
                                failwith "Expected a persisted workspace index for interruption recovery."
                        | None -> ()

                    if point = "transfer-start" && control.SlowTransfer then
                        control.SlowTransfer <- false
                        let mutable step = 0

                        operationContext.ReportProgress {
                            PhaseCode = "transfer-bytes"
                            Item = Some "literal[object]*?.bin"
                            Completed = Some(3.0 * 1024.0 * 1024.0 * 1024.0)
                            Total = Some(4.0 * 1024.0 * 1024.0 * 1024.0)
                            DisplayMessage = Some "Transferring large object"
                        }

                        while step < 200 && not (operationContext.Cancellation.IsCancellationRequested()) do
                            do! Async.Sleep 2
                            step <- step + 1

                            operationContext.ReportProgress {
                                PhaseCode = "transfer"
                                Item = None
                                Completed = Some(float step)
                                Total = Some 200.0
                                DisplayMessage = Some "Transferring lakeFS objects"
                            }
                | false, _ -> ()
            })
    }

    let factory =
        LakeFsWorkspaceSession.createFactoryWithHooks lakeFsProviderOptions hooks credentials

    let createLocationWithProfile profile = promise {
        let repository = $"vcs-{nextId ()}"
        do! createRepository repository
        repositories.Add repository
        return locationFor repository profile
    }

    let openWorkspace (binding: WorkspaceBinding) = promise {
        let! opened = factory.Open binding (context "harness-open") |> Async.StartAsPromise

        let session =
            match opened with
            | Succeeded outcome -> outcome.Value
            | PartiallySucceeded(_, failure)
            | Failed failure ->
                failwith $"lakeFS session open failed ({failure.Category}/{failure.Code}): {failure.Message}"

        controls[binding.WorkspaceRoot] <- {
            Location = binding.Location
            Binding = binding
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

    let createRoot () = promise {
        let! root = createTempDirectoryAsync ()
        tempRoots.Add root
        return root
    }

    let seedBase location = promise {
        let parsed = parseLocation location
        let! uploaded =
            uploadTextObject
                (connection ())
                parsed.Repository
                parsed.TargetRef
                "base.txt"
                "base content\n"
                (context "harness-seed-base")
            |> Async.StartAsPromise

        uploaded |> expectApi "seed base upload" |> ignore

        let! committed =
            LakeFsApi.commit
                (connection ())
                parsed.Repository
                parsed.TargetRef
                "init: base"
                (context "harness-seed-base-commit")
            |> Async.StartAsPromise

        committed |> expectApi "seed base commit" |> ignore
    }

    let createWorkspace () = promise {
        let! location = createLocationWithProfile (Some "default")
        do! seedBase location
        let! root = createRoot ()
        return! openWorkspace (bindingFor root location)
    }

    {
        Name = "lakeFS"
        Factory = factory
        ExpectedServices = [ "conflicts"; "synchronization" ]
        CreateLocation = fun () -> createLocationWithProfile (Some "default")
        CreateUnauthorizedLocation = fun () -> createLocationWithProfile (Some "unauthorized")
        CreateLocalPath = createRoot
        CreateWorkspace = createWorkspace
        CreateLinkedWorkspace =
            fun anchor -> promise {
                let! root = createRoot ()
                let linkedLocation = { anchor.Binding.Location with ConnectionProfileId = Some "linked" }
                return! openWorkspace (bindingFor root linkedLocation)
            }
        OpenSecondSession =
            fun anchor -> promise {
                let! opened = factory.Open anchor.Binding (context "harness-open-second") |> Async.StartAsPromise

                return
                    match opened with
                    | Succeeded outcome -> outcome.Value
                    | PartiallySucceeded(_, failure)
                    | Failed failure -> failwith $"second lakeFS session failed: {failure.Message}"
            }
        AdvanceTarget = fun workspace mutations -> advanceTarget workspace.Binding.Location mutations
        SeedRevisionOnNewRef =
            fun workspace refName files -> promise {
                let parsed = parseLocation workspace.Binding.Location
                let! created =
                    LakeFsApi.createBranch
                        (connection ())
                        parsed.Repository
                        refName
                        parsed.TargetRef
                        (context "harness-seed-ref-create")
                    |> Async.StartAsPromise

                created |> expectApi "seed ref create" |> ignore

                for path, content in files do
                    let! uploaded =
                        uploadTextObject
                            (connection ())
                            parsed.Repository
                            refName
                            path
                            content
                            (context "harness-seed-ref-upload")
                        |> Async.StartAsPromise

                    uploaded |> expectApi "seed ref upload" |> ignore

                let! committed =
                    LakeFsApi.commit
                        (connection ())
                        parsed.Repository
                        refName
                        $"seed: {refName}"
                        (context "harness-seed-ref-commit")
                    |> Async.StartAsPromise

                committed |> expectApi "seed ref commit" |> ignore

                return ProviderRef.tryCreate $"lakefs:{refName}" |> Result.defaultWith failwith
            }
        BreakPublish =
            fun _ -> promise {
                publishShouldBreak <- true
                return ()
            }
        RestorePublish =
            fun _ -> promise {
                publishShouldBreak <- false
                failNextConnection <- false
                return ()
            }
        ArmDestinationRace =
            fun workspace mutations -> promise {
                controls[workspace.Binding.WorkspaceRoot].RaceMutations <- Some mutations
                return ()
            }
        ArmSlowTransfer =
            fun workspace -> promise {
                controls[workspace.Binding.WorkspaceRoot].SlowTransfer <- true
                return ()
            }
        Cleanup =
            fun () -> promise {
                for repository in repositories do
                    do! deleteRepository repository

                for root in tempRoots do
                    do! removeDirectoryAsync root

                for stateDirectory in stateDirectories do
                    do! removeDirectoryAsync stateDirectory

                repositories.Clear()
                tempRoots.Clear()
                controls.Clear()
                stateDirectories.Clear()
                finalizePathMutations.Clear()
                materializationApplyFailures.Clear()
                return ()
            }
    }

let private lakeFsHarness = createLakeFsHarness ()

let private registerProfiles () = [|
    CoreProviderSuite.register lakeFsHarness
    SynchronizationProviderSuite.register lakeFsHarness
    ConflictProviderSuite.register lakeFsHarness
    ProvisioningProviderSuite.register lakeFsHarness
    OperationalProviderSuite.register lakeFsHarness
    ExtensionProviderSuites.register lakeFsHarness
    ConsumerWorkflowSuite.register lakeFsHarness
|]

let private registrations =
    if integrationEnabled () then
        registerProfiles ()
    else
        printfn "lakeFS integration skipped: Docker not available"

        Vitest.Describe.Skip.skip (
            "lakeFS integration skipped: Docker not available",
            fun () -> registerProfiles () |> ignore
        )

        [||]

Vitest.describe (
    "lakeFS clone cancellation",
    fun () ->
        Vitest.test (
            "a pre-canceled clone leaves a missing target missing",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! anchor = harness.CreateWorkspace()
                    let! targetRoot = harness.CreateLocalPath()
                    let targetPath = RuntimeNodePath.join [| targetRoot; "pre-canceled-clone" |]
                    let cancellation = OperationCancellation.Source()
                    cancellation.Cancel()

                    let! result =
                        harness.Factory.Clone
                            {
                                Location = anchor.Binding.Location
                                TargetPath = targetPath
                                TargetRef = None
                                MaterializeAllObjects = true
                            }
                            (OperationContext.create "pre-canceled-clone" cancellation.Cancellation ignore)
                        |> Async.StartAsPromise

                    match result with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual FailureCategory.Canceled
                        Vitest.expect(failure.Code).toBe "operation_canceled"
                        Vitest.expect(failure.StateChanged).toBe false
                    | Succeeded _
                    | PartiallySucceeded _ ->
                        failwith "A pre-canceled lakeFS clone unexpectedly succeeded."

                    Vitest.expect(RuntimeNodeFileSystem.existsSync targetPath).toBe(false)
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

let private expectOperationValue operation = function
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded(_, failure)
    | Failed failure -> failwith $"{operation} failed ({failure.Category}/{failure.Code}): {failure.Message}"

let private createSingleFileConflict harness = promise {
    let! workspace = harness.CreateWorkspace()

    do!
        harness.AdvanceTarget workspace [|
            { Path = "base.txt"; Content = Some "target base\n" }
        |]

    do! workspace.WriteFile "base.txt" "workspace base\n"

    let! saveStatusResult =
        workspace.Session.Core.GetStatus(context "conflict-staleness-save-status")
        |> Async.StartAsPromise

    let saveStatus = expectOperationValue "conflict staleness save status" saveStatusResult
    let! saveResult =
        workspace.Session.Core.CreateRevision
            {
                Message = "create conflict staleness workspace revision"
                Paths = [| repositoryPath "base.txt" |]
                ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
            }
            (context "conflict-staleness-save")
        |> Async.StartAsPromise

    expectOperationValue "conflict staleness workspace revision" saveResult |> ignore

    let! updateStatusResult =
        workspace.Session.Core.GetStatus(context "conflict-staleness-update-status")
        |> Async.StartAsPromise

    let updateStatus = expectOperationValue "conflict staleness update status" updateStatusResult
    let synchronization =
        workspace.Session.Synchronization
        |> Option.defaultWith (fun () -> failwith "Expected lakeFS synchronization services.")

    let! updateResult =
        synchronization.Update
            { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
            (context "conflict-staleness-update")
        |> Async.StartAsPromise

    let updateFailure =
        match updateResult with
        | Succeeded _ -> failwith "The conflict staleness update unexpectedly succeeded."
        | PartiallySucceeded(_, failure)
        | Failed failure -> failure

    Vitest.expect(updateFailure.Category).toEqual FailureCategory.Conflict
    Vitest.expect(updateFailure.Code).toBe "conflicts_detected"

    let conflicts =
        workspace.Session.ConflictResolution
        |> Option.defaultWith (fun () -> failwith "Expected lakeFS conflict-resolution services.")

    let! sessionResult =
        conflicts.GetActiveSession(context "conflict-staleness-session")
        |> Async.StartAsPromise

    let summary =
        expectOperationValue "conflict staleness session" sessionResult
        |> Option.defaultWith (fun () -> failwith "Expected an active conflict session.")

    return workspace, conflicts, summary
}

Vitest.describe (
    "lakeFS materialization recovery conflict cleanup",
    fun () ->
        Vitest.test (
            "lakeFS materialization recovery removes conflict candidates on finalize and cancel",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! canceledWorkspace, canceledConflicts, canceledSummary =
                        createSingleFileConflict harness

                    let canceledTemporaryDirectory =
                        join [|
                            lakeFsProviderOptions.StateRoot
                            canceledWorkspace.Binding.ProviderStateRef.Value
                            "temporary"
                        |]

                    let candidatesBeforeCancel =
                        RuntimeNodeFileSystem.readdirSync canceledTemporaryDirectory
                        |> Array.filter (fun name -> name.StartsWith("conflict-candidate-", StringComparison.Ordinal))

                    Vitest.expect(candidatesBeforeCancel.Length > 0).toBe(true)

                    let! cancelStatusResult =
                        canceledWorkspace.Session.Core.GetStatus(context "recovery-cancel-status")
                        |> Async.StartAsPromise

                    let cancelStatus = expectOperationValue "recovery cancel status" cancelStatusResult
                    let! cancelResult =
                        canceledConflicts.Cancel
                            {
                                Handle = canceledSummary.Handle
                                ExpectedWorkspaceVersion = cancelStatus.WorkspaceVersion
                            }
                            (context "recovery-cancel")
                        |> Async.StartAsPromise

                    expectOperationValue "recovery cancel" cancelResult |> ignore

                    let candidatesAfterCancel =
                        RuntimeNodeFileSystem.readdirSync canceledTemporaryDirectory
                        |> Array.filter (fun name -> name.StartsWith("conflict-candidate-", StringComparison.Ordinal))

                    Vitest.expect(candidatesAfterCancel).toEqual [||]

                    let! finalizedWorkspace, finalizedConflicts, finalizedSummary =
                        createSingleFileConflict harness

                    let finalizedTemporaryDirectory =
                        join [|
                            lakeFsProviderOptions.StateRoot
                            finalizedWorkspace.Binding.ProviderStateRef.Value
                            "temporary"
                        |]

                    let! resolutionStatusResult =
                        finalizedWorkspace.Session.Core.GetStatus(context "recovery-finalize-resolution-status")
                        |> Async.StartAsPromise

                    let resolutionStatus =
                        expectOperationValue "recovery finalize resolution status" resolutionStatusResult

                    let! resolutionResult =
                        finalizedConflicts.Resolve
                            {
                                Handle = finalizedSummary.Handle
                                ExpectedWorkspaceVersion = resolutionStatus.WorkspaceVersion
                                Path = finalizedSummary.Items[0].Path
                                Resolution = PickCandidate "target"
                            }
                            (context "recovery-finalize-resolution")
                        |> Async.StartAsPromise

                    let resolution =
                        expectOperationValue "recovery finalize resolution" resolutionResult

                    let! finalizeStatusResult =
                        finalizedWorkspace.Session.Core.GetStatus(context "recovery-finalize-status")
                        |> Async.StartAsPromise

                    let finalizeStatus = expectOperationValue "recovery finalize status" finalizeStatusResult
                    let! finalizeResult =
                        finalizedConflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                Message = Some "cleanup candidates"
                            }
                            (context "recovery-finalize")
                        |> Async.StartAsPromise

                    expectOperationValue "recovery finalize" finalizeResult |> ignore

                    let candidatesAfterFinalize =
                        RuntimeNodeFileSystem.readdirSync finalizedTemporaryDirectory
                        |> Array.filter (fun name -> name.StartsWith("conflict-candidate-", StringComparison.Ordinal))

                    Vitest.expect(candidatesAfterFinalize).toEqual [||]
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

Vitest.describe (
    "lakeFS materialization recovery session gating",
    fun () ->
        Vitest.test (
            "lakeFS materialization recovery blocks mutations and replays the retained plan",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! workspace = harness.CreateWorkspace()

                    do!
                        harness.AdvanceTarget workspace [|
                            { Path = "base.txt"; Content = None }
                            { Path = "000-recovery-a.txt"; Content = Some "recovery a\n" }
                            { Path = "100-recovery-b.txt"; Content = Some "recovery b\n" }
                            { Path = "zzz-recovery-c.txt"; Content = Some "recovery c\n" }
                        |]

                    let! beforeUpdateResult =
                        workspace.Session.Core.GetStatus(context "recovery-gating-before-update")
                        |> Async.StartAsPromise

                    let beforeUpdate = expectOperationValue "recovery gating before update" beforeUpdateResult
                    armMaterializationApplyFailure workspace.Binding.WorkspaceRoot

                    let synchronization =
                        workspace.Session.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS synchronization services.")

                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion }
                            (context "recovery-gating-update")
                        |> Async.StartAsPromise

                    let partialFailure =
                        match updateResult with
                        | PartiallySucceeded(_, failure) -> failure
                        | Succeeded _ -> failwith "Injected materialization recovery failure unexpectedly succeeded."
                        | Failed failure -> failwith $"Injected materialization recovery was not partial: {failure.Code}"

                    Vitest.expect(partialFailure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_materialization")
                    Vitest.expect(partialFailure.AffectedPaths.Length).toBe 1

                    let! partialStatusResult =
                        workspace.Session.Core.GetStatus(context "recovery-gating-partial-status")
                        |> Async.StartAsPromise

                    let partialStatus = expectOperationValue "recovery gating partial status" partialStatusResult
                    let replacedPath = repositoryPath partialFailure.AffectedPaths[0]

                    Vitest.expect(
                        partialStatus.Changes
                        |> Array.exists (fun change -> RepositoryPath.value change.Path = partialFailure.AffectedPaths[0])
                    ).toBe(false)

                    let resolvedState =
                        LakeFsStateStore.resolve
                            lakeFsProviderOptions
                            workspace.Binding.WorkspaceRoot
                            workspace.Binding.ProviderStateRef
                        |> Result.defaultWith (fun failure -> failwith failure.Message)

                    let referencedTransaction =
                        RuntimeNodeFileSystem.readdirSync resolvedState.TransactionsDirectory
                        |> Array.exactlyOne
                        |> fun name -> join [| resolvedState.TransactionsDirectory; name |]

                    let! _ = harness.OpenSecondSession workspace
                    Vitest.expect(RuntimeNodeFileSystem.existsSync referencedTransaction).toBe(true)

                    let! emptyRestoreResult =
                        workspace.Session.Core.RestorePaths
                            {
                                Paths = [||]
                                ExpectedWorkspaceVersion = partialStatus.WorkspaceVersion
                            }
                            (context "recovery-gating-empty-restore")
                        |> Async.StartAsPromise

                    match emptyRestoreResult with
                    | Failed failure -> Vitest.expect(failure.Code).toBe "no_paths_selected"
                    | _ -> failwith "An empty restore bypassed request validation during recovery."

                    Vitest.expect(RuntimeNodeFileSystem.existsSync referencedTransaction).toBe(true)

                    let expectBlocked operation result =
                        match result with
                        | Failed failure ->
                            Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_materialization")
                            Vitest.expect(failure.Category).toEqual FailureCategory.ProviderError
                        | Succeeded _
                        | PartiallySucceeded _ -> failwith $"{operation} unexpectedly mutated during recovery."

                    let! createRevisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "blocked during materialization recovery"
                                Paths = [| replacedPath |]
                                ExpectedWorkspaceVersion = partialStatus.WorkspaceVersion
                            }
                            (context "recovery-gating-create-revision")
                        |> Async.StartAsPromise

                    expectBlocked "CreateRevision" createRevisionResult

                    let! blockedUpdateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = partialStatus.WorkspaceVersion }
                            (context "recovery-gating-blocked-update")
                        |> Async.StartAsPromise

                    expectBlocked "Update" blockedUpdateResult

                    do! workspace.WriteFile "caller-only.txt" "caller-selected edit\n"

                    let! callerStatusResult =
                        workspace.Session.Core.GetStatus(context "recovery-gating-caller-status")
                        |> Async.StartAsPromise

                    let callerStatus = expectOperationValue "recovery caller status" callerStatusResult

                    let! replayResult =
                        workspace.Session.Core.RestorePaths
                            {
                                Paths = [| repositoryPath "caller-only.txt" |]
                                ExpectedWorkspaceVersion = callerStatus.WorkspaceVersion
                            }
                            (context "recovery-gating-replay")
                        |> Async.StartAsPromise

                    match replayResult with
                    | Succeeded outcome ->
                        Vitest.expect(outcome.AffectedPaths).toEqual [| "caller-only.txt" |]
                    | PartiallySucceeded(_, failure)
                    | Failed failure ->
                        failwith $"Recovery followed by the caller restore failed: {failure.Code}"

                    let stateDirectory = stateDirectoryForBinding workspace.Binding
                    let recoveryDirectory = join [| stateDirectory; "recovery" |]
                    Vitest.expect(RuntimeNodeFileSystem.readdirSync recoveryDirectory).toEqual [||]

                    let! replayedA = workspace.ReadFile "000-recovery-a.txt"
                    let! replayedB = workspace.ReadFile "100-recovery-b.txt"
                    let! replayedC = workspace.ReadFile "zzz-recovery-c.txt"
                    Vitest.expect(replayedA).toEqual(Some "recovery a\n")
                    Vitest.expect(replayedB).toEqual(Some "recovery b\n")
                    Vitest.expect(replayedC).toEqual(Some "recovery c\n")
                    let! callerFile = workspace.ReadFile "caller-only.txt"
                    Vitest.expect(callerFile).toEqual None
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

Vitest.describe (
    "lakeFS provisioning profile external state",
    fun () ->
        Vitest.test (
            "provisioning keeps opaque state outside existing workspaces and rejects invalid reopen state",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let workspaceRoot = join [| root; "existing-workspace" |]
                let stateRoot = join [| root; "state-root" |]
                let existingPath = join [| workspaceRoot; "existing.bin" |]

                try
                    do! writeUtf8FileAsync existingPath "existing user bytes\u0000remain"

                    let credentials =
                        LakeFsCredentials.fixedConnection {
                            Endpoint = "http://127.0.0.1:1"
                            AccessKeyId = "unused"
                            SecretAccessKey = "unused"
                        }

                    let options: LakeFsProviderOptions.LakeFsProviderOptions = {
                        StateRoot = stateRoot
                        PathCaseSensitivity = CaseInsensitive
                    }
                    let factory = LakeFsWorkspaceSession.createFactory options credentials
                    let location: RepositoryLocation = {
                        ProviderId = ProviderId.tryCreate "lakefs" |> Result.defaultWith failwith
                        DisplayName = None
                        ProviderLocation = "lakefs://repository/main"
                        ConnectionProfileId = Some "profile"
                    }

                    let! initialized =
                        factory.Initialize
                            {
                                TargetPath = workspaceRoot
                                Location = Some location
                            }
                            (context "external-state-initialize")
                        |> Async.StartAsPromise

                    let binding = expectOperationValue "external-state initialize" initialized
                    Vitest.expect(binding.ProviderStateRef.IsSome).toBe true
                    Vitest.expect(RuntimeNodePath.isAbsolute binding.ProviderStateRef.Value).toBe false

                    let resolvedState =
                        match LakeFsStateStore.resolve options workspaceRoot binding.ProviderStateRef with
                        | Ok state -> state
                        | Error failure -> failwith $"State resolution failed: {failure.Code}"

                    let stateDirectory = resolvedState.StateDirectory

                    Vitest.expect(stateDirectory.StartsWith(stateRoot)).toBe true
                    Vitest.expect(resolvedState.ProvisioningMode).toBe LakeFsStateStore.InitializeProvisioning
                    let! workspaceFiles = fsPromisesDynamic?readdir (workspaceRoot) |> unbox<JS.Promise<string[]>>
                    Vitest.expect(workspaceFiles).toEqual [| "existing.bin" |]
                    let! existing = tryReadUtf8FileAsync existingPath
                    Vitest.expect(existing).toEqual(Some "existing user bytes\u0000remain")

                    match LakeFsStateStore.markReady resolvedState with
                    | Error failure -> failwith $"State readiness update failed: {failure.Code}"
                    | Ok _ -> ()

                    let! missingReadyIndex =
                        factory.Open binding (context "external-state-ready-index-missing")
                        |> Async.StartAsPromise

                    match missingReadyIndex with
                    | Failed failure -> Vitest.expect(failure.Code).toBe "provider_state_index_missing"
                    | _ -> failwith "Expected ready provider state with a missing index to fail without recreation."

                    let! cloneIntoNonempty =
                        factory.Clone
                            {
                                Location = location
                                TargetPath = workspaceRoot
                                TargetRef = None
                                MaterializeAllObjects = true
                            }
                            (context "external-state-clone-nonempty")
                        |> Async.StartAsPromise

                    match cloneIntoNonempty with
                    | Failed failure -> Vitest.expect(failure.Code).toBe "target_not_empty"
                    | _ -> failwith "Expected clone to retain its strict nonempty-target rule."

                    let cloneFileTarget = join [| root; "existing-file-target" |]
                    do! writeUtf8FileAsync cloneFileTarget "consumer-owned file"

                    let! cloneIntoFile =
                        factory.Clone
                            {
                                Location = location
                                TargetPath = cloneFileTarget
                                TargetRef = None
                                MaterializeAllObjects = true
                            }
                            (context "external-state-clone-file")
                        |> Async.StartAsPromise

                    match cloneIntoFile with
                    | Failed failure -> Vitest.expect(failure.Code).toBe "target_not_empty"
                    | _ -> failwith "Expected clone to reject an existing file target structurally."

                    let! missingReference =
                        factory.Open
                            { binding with ProviderStateRef = None }
                            (context "external-state-missing-ref")
                        |> Async.StartAsPromise

                    match missingReference with
                    | Failed failure -> Vitest.expect(failure.Code).toBe "provider_state_ref_missing"
                    | _ -> failwith "Expected a missing state reference to fail."

                    do! writeUtf8FileAsync (LakeFsWorkspaceIndex.indexPath stateDirectory) "corrupt index {"
                    let! corrupt = factory.Open binding (context "external-state-corrupt") |> Async.StartAsPromise

                    match corrupt with
                    | Failed failure -> Vitest.expect(failure.Code).toBe "index_corrupt"
                    | _ -> failwith "Expected corrupt state to fail without recreation."

                    let mismatchedIndex: LakeFsWorkspaceIndex.WorkspaceIndex = {
                        SchemaVersion = LakeFsWorkspaceIndex.CurrentSchemaVersion
                        Repository = "another-repository"
                        TargetRef = "main"
                        Prefix = ""
                        WorkspaceBranch = "vcs-workspace-mismatch-token"
                        OwnershipToken = "mismatch-token-1234567890"
                        BaseRevision = None
                        WorkspaceRevision = None
                        Generation = 0
                        Entries = [||]
                    }

                    match LakeFsWorkspaceIndex.save stateDirectory mismatchedIndex with
                    | Error message -> failwith message
                    | Ok _ -> ()

                    let! mismatched = factory.Open binding (context "external-state-mismatch") |> Async.StartAsPromise

                    match mismatched with
                    | Failed failure -> Vitest.expect(failure.Code).toBe "provider_state_mismatch"
                    | _ -> failwith "Expected mismatched state to fail without retargeting."

                    do! removeDirectoryAsync stateDirectory
                    let! missing = factory.Open binding (context "external-state-missing") |> Async.StartAsPromise

                    match missing with
                    | Failed failure -> Vitest.expect(failure.Code).toBe "provider_state_missing"
                    | _ -> failwith "Expected missing external state to fail without recreation."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization recovery treats cleanup failures as warnings and sweeps orphans",
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let workspaceRoot = join [| root; "workspace" |]
                let stateRoot = join [| root; "provider-state" |]
                let options: LakeFsProviderOptions.LakeFsProviderOptions = {
                    StateRoot = stateRoot
                    PathCaseSensitivity = CaseInsensitive
                }
                let restoreCleanupFailure = injectNextTransactionCleanupFailure ()

                try
                    do! ensureDirectoryAsync workspaceRoot

                    let state =
                        LakeFsStateStore.create options workspaceRoot
                        |> Result.defaultWith (fun failure -> failwith failure.Message)

                    let path = repositoryPath "cleanup-warning.txt"
                    let targetPath = join [| workspaceRoot; RepositoryPath.value path |]
                    let currentIndex: LakeFsWorkspaceIndex.WorkspaceIndex = {
                        SchemaVersion = LakeFsWorkspaceIndex.CurrentSchemaVersion
                        Repository = "cleanup-repository"
                        TargetRef = "main"
                        Prefix = ""
                        WorkspaceBranch = "vcs-workspace-cleanup"
                        OwnershipToken = "cleanup-ownership-token"
                        BaseRevision = Some "revision"
                        WorkspaceRevision = Some "revision"
                        Generation = 0
                        Entries = [||]
                    }

                    let! prepared =
                        LakeFsMaterialization.prepare
                            state.TransactionsDirectory
                            currentIndex
                            false
                            [| {
                                   Path = path
                                   ObjectKey = "cleanup-warning.txt"
                                   TargetPath = targetPath
                                   BaseChecksum = "checksum"
                                   Mtime = 1.0
                               } |]
                            [||]
                            (fun _ temporaryPath _ -> async {
                                let content = "cleanup warning bytes\n"
                                RuntimeNodeFileSystem.writeUtf8FileExclusiveAndFlushSync temporaryPath content

                                return
                                    Ok {
                                        BytesCopied = float content.Length
                                        Sha256 = LakeFsWorkspaceIndex.hashMetadata content
                                    }
                            })
                            (fun _ _ -> async.Return())
                            (context "cleanup-warning-prepare")
                        |> Async.StartAsPromise

                    let plan = prepared |> Result.defaultWith (fun failure -> failwith failure.Message)
                    let! applied =
                        LakeFsMaterialization.apply
                            workspaceRoot
                            state.StateDirectory
                            state.RecoveryDirectory
                            currentIndex.WorkspaceRevision
                            "revision"
                            plan
                            (fun _ _ -> async.Return())
                            (context "cleanup-warning-apply")
                        |> Async.StartAsPromise

                    restoreCleanupFailure ()

                    match applied with
                    | LakeFsMaterialization.Materialized(_, _, warnings) ->
                        Vitest.expect(warnings |> Array.map _.Code).toContain "materialization_cleanup_failed"
                    | _ -> failwith "Cleanup failure must remain a successful materialization."

                    Vitest.expect(RuntimeNodeFileSystem.readdirSync state.RecoveryDirectory).toEqual [||]
                    Vitest.expect(RuntimeNodeFileSystem.readdirSync state.TransactionsDirectory).toHaveLength 1

                    let orphan = join [| state.TemporaryDirectory; "conflict-candidate-orphan.tmp" |]
                    do! writeUtf8FileAsync orphan "orphan"

                    match LakeFsStateStore.resolve options workspaceRoot (Some state.StateId) with
                    | Error failure -> failwith $"Reopening provider state failed: {failure.Code}"
                    | Ok reopened ->
                        LakeFsStateStore.sweepTransientEntries reopened

                    Vitest.expect(RuntimeNodeFileSystem.existsSync orphan).toBe(false)
                    Vitest.expect(RuntimeNodeFileSystem.readdirSync state.TransactionsDirectory).toEqual [||]
                    do! removeDirectoryAsync root
                with error ->
                    restoreCleanupFailure ()
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

Vitest.describe (
    "lakeFS rejects unsafe paths and links",
    fun () ->
        Vitest.test (
            "remote keys cannot traverse escape alias or follow a workspace link",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let workspaceRoot = join [| root; "workspace" |]
                let outsideRoot = join [| root; "outside" |]
                do! ensureDirectoryAsync workspaceRoot
                do! ensureDirectoryAsync outsideRoot

                try
                    for key in [|
                        ""
                        "/absolute.txt"
                        "C:/drive-rooted.txt"
                        "back\\slash.txt"
                        "empty//segment.txt"
                        "dot/./segment.txt"
                        "dotdot/../escape.txt"
                        "nul\u0000byte.txt"
                    |] do
                        match LakeFsPathSafety.resolveRemotePath workspaceRoot "prefix" $"prefix/{key}" with
                        | Error failure -> Vitest.expect(failure.Code).toBe "unsafe_repository_path"
                        | Ok _ -> failwith $"Expected unsafe remote key to be rejected: {key}"

                    match LakeFsPathSafety.resolveRemotePath workspaceRoot "prefix" "another-prefix/file.txt" with
                    | Ok None -> ()
                    | _ -> failwith "A key outside the exact prefix must not be materialized."

                    let safePath = repositoryPath "nested/safe.txt"

                    match LakeFsPathSafety.writeUtf8File workspaceRoot safePath "safe content" with
                    | Ok () -> ()
                    | Error failure -> failwith $"Safe write failed: {failure.Code}"

                    match LakeFsPathSafety.readUtf8File workspaceRoot safePath with
                    | Ok(Some content) -> Vitest.expect(content).toBe "safe content"
                    | _ -> failwith "Expected the safely written file to be readable."

                    match LakeFsPathSafety.walkFiles workspaceRoot with
                    | Ok paths ->
                        paths
                        |> Array.map RepositoryPath.value
                        |> Vitest.expect
                        |> _.toContain("nested/safe.txt")
                    | Error failure -> failwith $"Safe walk failed: {failure.Code}"

                    match LakeFsPathSafety.removeFile workspaceRoot safePath with
                    | Ok () -> ()
                    | Error failure -> failwith $"Safe delete failed: {failure.Code}"

                    let linkPath = join [| workspaceRoot; "linked" |]
                    let! _ = fsPromisesDynamic?symlink (outsideRoot, linkPath, "junction") |> unbox<JS.Promise<obj>>
                    let linkedPath = repositoryPath "linked/secret.txt"

                    match LakeFsPathSafety.resolveWorkspacePath workspaceRoot linkedPath with
                    | Error failure ->
                        Vitest.expect(failure.Code).toBe "symlink_not_supported"
                        Vitest.expect(failure.AffectedPaths).toContain "linked/secret.txt"
                    | Ok _ -> failwith "Expected a link-containing parent chain to be rejected."

                    let expectLinkFailure
                        (operation: string)
                        (result: Result<'value, OperationFailure>)
                        =
                        match result with
                        | Error failure ->
                            Vitest.expect(failure.Code).toBe "symlink_not_supported"
                            Vitest.expect(failure.AffectedPaths).toContain "linked/secret.txt"
                        | Ok _ -> failwith $"Expected {operation} through a linked parent to fail."

                    LakeFsPathSafety.readUtf8File workspaceRoot linkedPath
                    |> expectLinkFailure "read"

                    LakeFsPathSafety.writeUtf8File workspaceRoot linkedPath "outside write"
                    |> expectLinkFailure "write"

                    LakeFsPathSafety.removeFile workspaceRoot linkedPath
                    |> expectLinkFailure "delete"

                    match LakeFsPathSafety.walkFiles workspaceRoot with
                    | Error failure -> Vitest.expect(failure.Code).toBe "symlink_not_supported"
                    | Ok _ -> failwith "Expected walking a workspace containing a link to fail."

                    let danglingTarget = join [| outsideRoot; "missing-target" |]
                    let danglingLink = join [| workspaceRoot; "dangling" |]
                    let! _ =
                        fsPromisesDynamic?symlink (danglingTarget, danglingLink, "junction")
                        |> unbox<JS.Promise<obj>>

                    match
                        LakeFsPathSafety.resolveWorkspacePath
                            workspaceRoot
                            (repositoryPath "dangling/secret.txt")
                    with
                    | Error failure ->
                        Vitest.expect(failure.Code).toBe "symlink_not_supported"
                        Vitest.expect(failure.AffectedPaths).toContain "dangling/secret.txt"
                    | Ok _ -> failwith "Expected a dangling link in the parent chain to be rejected."

                    let casePaths = [| repositoryPath "Data/File.txt"; repositoryPath "data/file.txt" |]

                    match LakeFsPathSafety.validateMaterializationPathsForPlatform "win32" casePaths with
                    | Error failure -> Vitest.expect(failure.Code).toBe "path_collision"
                    | Ok () -> failwith "Expected a Windows case collision."

                    let normalizedPaths = [|
                        repositoryPath "café.txt"
                        repositoryPath "café.txt"
                    |]

                    match LakeFsPathSafety.validateMaterializationPathsForPlatform "darwin" normalizedPaths with
                    | Error failure -> Vitest.expect(failure.Code).toBe "path_collision"
                    | Ok () -> failwith "Expected a macOS normalization collision."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

Vitest.describe (
    "lakeFS materialization",
    fun () ->
        Vitest.test (
            "lakeFS materialization reports post-replacement validation as changed",
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let workspaceRoot = join [| root; "workspace" |]
                let externalRoot = join [| root; "external-state" |]
                let preparedPath = join [| externalRoot; "prepared-object.tmp" |]
                let targetPath = join [| workspaceRoot; "post-rename.bin" |]
                let restoreFileSystem = injectPostRenameLinkObservation targetPath

                try
                    do! ensureDirectoryAsync workspaceRoot
                    do! ensureDirectoryAsync externalRoot
                    do! writeUtf8FileAsync targetPath "old target bytes\n"
                    do! writeUtf8FileAsync preparedPath "installed target bytes\n"

                    let failure =
                        match
                            LakeFsPathSafety.replaceFileFromTemporary
                                workspaceRoot
                                (repositoryPath "post-rename.bin")
                                preparedPath
                        with
                        | Ok() -> failwith "Post-rename validation unexpectedly succeeded."
                        | Error failure -> failure

                    Vitest.expect(failure.StateChanged).toBe true
                    Vitest.expect(failure.AffectedPaths).toEqual [| "post-rename.bin" |]
                    restoreFileSystem ()
                    let! installed = tryReadUtf8FileAsync targetPath
                    Vitest.expect(installed).toEqual(Some "installed target bytes\n")
                    do! removeDirectoryAsync root
                with error ->
                    restoreFileSystem ()
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization crosses state volumes atomically",
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let workspaceRoot = join [| root; "workspace" |]
                let externalRoot = join [| root; "external-state" |]
                let preparedPath = join [| externalRoot; "prepared-object.tmp" |]
                let targetPath = join [| workspaceRoot; "cross-volume.bin" |]
                let restoreRename = injectNextCrossVolumeRename ()

                try
                    do! ensureDirectoryAsync workspaceRoot
                    do! ensureDirectoryAsync externalRoot
                    do! writeUtf8FileAsync preparedPath "prepared cross-volume bytes\n"

                    match
                        LakeFsPathSafety.replaceFileFromTemporary
                            workspaceRoot
                            (repositoryPath "cross-volume.bin")
                            preparedPath
                    with
                    | Error failure ->
                        failwith $"Cross-volume materialization failed ({failure.Code}): {failure.Message}"
                    | Ok() -> ()

                    let! target = tryReadUtf8FileAsync targetPath
                    Vitest.expect(target).toEqual(Some "prepared cross-volume bytes\n")
                    Vitest.expect(
                        RuntimeNodeFileSystem.readdirSync workspaceRoot
                        |> Array.filter (fun path -> path.Contains ".vcs-")
                    ).toEqual [||]
                    restoreRename ()
                    do! removeDirectoryAsync root
                with error ->
                    restoreRename ()
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization recovery preserves partial indexes and re-applies retained plans",
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let workspaceRoot = join [| root; "workspace" |]
                let stateDirectory = join [| root; "state" |]
                let transactionsDirectory = join [| stateDirectory; "transactions" |]
                let recoveryDirectory = join [| stateDirectory; "recovery" |]
                let firstPath = repositoryPath "000-first.txt"
                let secondPath = repositoryPath "100-second.txt"
                let thirdPath = repositoryPath "zzz-third.txt"
                let firstTarget = join [| workspaceRoot; RepositoryPath.value firstPath |]
                let secondTarget = join [| workspaceRoot; RepositoryPath.value secondPath |]
                let thirdTarget = join [| workspaceRoot; RepositoryPath.value thirdPath |]

                try
                    do! ensureDirectoryAsync workspaceRoot
                    do! ensureDirectoryAsync stateDirectory
                    do! ensureDirectoryAsync transactionsDirectory
                    do! ensureDirectoryAsync recoveryDirectory
                    do! writeUtf8FileAsync firstTarget "old first bytes\n"
                    do! writeUtf8FileAsync secondTarget "old second bytes\n"
                    do! writeUtf8FileAsync thirdTarget "old third bytes\n"

                    let currentIndex: LakeFsWorkspaceIndex.WorkspaceIndex = {
                        SchemaVersion = LakeFsWorkspaceIndex.CurrentSchemaVersion
                        Repository = "transactional-repository"
                        TargetRef = "main"
                        Prefix = ""
                        WorkspaceBranch = "vcs-workspace-transactional"
                        OwnershipToken = "transactional-ownership-token"
                        BaseRevision = Some "expected-revision"
                        WorkspaceRevision = Some "expected-revision"
                        Generation = 0
                        Entries = [||]
                    }

                    let objects: LakeFsMaterialization.MaterializationObject[] = [|
                        {
                            Path = firstPath
                            ObjectKey = "000-first.txt"
                            TargetPath = firstTarget
                            BaseChecksum = "first-checksum"
                            Mtime = 1.0
                        }
                        {
                            Path = secondPath
                            ObjectKey = "100-second.txt"
                            TargetPath = secondTarget
                            BaseChecksum = "second-checksum"
                            Mtime = 2.0
                        }
                        {
                            Path = thirdPath
                            ObjectKey = "zzz-third.txt"
                            TargetPath = thirdTarget
                            BaseChecksum = "third-checksum"
                            Mtime = 3.0
                        }
                    |]

                    let mutable downloadCount = 0
                    let! failedPrepare =
                        LakeFsMaterialization.prepare
                            transactionsDirectory
                            currentIndex
                            false
                            objects
                            [||]
                            (fun prepared temporaryPath _ -> async {
                                downloadCount <- downloadCount + 1

                                if downloadCount = 2 then
                                    return
                                        Error(
                                            OperationFailure.create
                                                Network
                                                "injected_second_download_failure"
                                                "The second prepared object failed."
                                        )
                                else
                                    RuntimeNodeFileSystem.writeUtf8FileExclusiveAndFlushSync
                                        temporaryPath
                                        $"new {RepositoryPath.value prepared.Path} bytes\n"

                                    return
                                        Ok {
                                            BytesCopied = 20.0
                                            Sha256 = $"hash-{RepositoryPath.value prepared.Path}"
                                        }
                            })
                            (fun _ _ -> async.Return())
                            (context "transactional-prepare-failure")
                        |> Async.StartAsPromise

                    match failedPrepare with
                    | Ok _ -> failwith "The injected second download failure unexpectedly prepared a plan."
                    | Error failure ->
                        Vitest.expect(failure.Code).toBe "injected_second_download_failure"
                        Vitest.expect(failure.StateChanged).toBe false

                    Vitest.expect(downloadCount).toBe 2
                    let! firstBeforeApply = tryReadUtf8FileAsync firstTarget
                    let! secondBeforeApply = tryReadUtf8FileAsync secondTarget
                    Vitest.expect(firstBeforeApply).toEqual(Some "old first bytes\n")
                    Vitest.expect(secondBeforeApply).toEqual(Some "old second bytes\n")
                    Vitest.expect(RuntimeNodeFileSystem.readdirSync transactionsDirectory).toEqual [||]

                    let! prepared =
                        LakeFsMaterialization.prepare
                            transactionsDirectory
                            currentIndex
                            false
                            objects
                            [||]
                            (fun prepared temporaryPath _ -> async {
                                let content =
                                    match prepared.Path with
                                    | path when path = firstPath -> "new first bytes\n"
                                    | path when path = secondPath -> "new second bytes\n"
                                    | _ -> "new third bytes\n"

                                RuntimeNodeFileSystem.writeUtf8FileExclusiveAndFlushSync temporaryPath content

                                return
                                    Ok {
                                        BytesCopied = float content.Length
                                        Sha256 = LakeFsWorkspaceIndex.hashMetadata content
                                    }
                            })
                            (fun _ _ -> async.Return())
                            (context "transactional-prepare-success")
                        |> Async.StartAsPromise

                    let plan = prepared |> Result.defaultWith (fun failure -> failwith failure.Message)
                    let mutable applied = 0
                    let! appliedResult =
                        LakeFsMaterialization.apply
                            workspaceRoot
                            stateDirectory
                            recoveryDirectory
                            currentIndex.WorkspaceRevision
                            "observed-revision"
                            plan
                            (fun point _ -> async {
                                if point = "materialization-apply-object" then
                                    applied <- applied + 1

                                    if applied = 1 then
                                        failwith "injected apply failure"
                            })
                            (context "transactional-apply-failure")
                        |> Async.StartAsPromise

                    match appliedResult with
                    | LakeFsMaterialization.MaterializationPartiallyApplied(Some saved, failure) ->
                        Vitest.expect(failure.Code).toBe "materialization_apply_failed"
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.AffectedPaths).toEqual [| "000-first.txt" |]
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_materialization")
                        Vitest.expect(saved.Entries.Length).toBe 1
                        Vitest.expect(saved.Entries[0].Path).toBe "000-first.txt"
                        Vitest.expect(saved.Entries[0].LocalHash).toBe (LakeFsWorkspaceIndex.hashMetadata "new first bytes\n")
                    | LakeFsMaterialization.MaterializationPartiallyApplied(None, failure) ->
                        let details = String.concat ";" failure.Details
                        failwith $"The visible replacement was returned without publishing its index: {failure.Code} ({details})."
                    | LakeFsMaterialization.MaterializationFailed failure ->
                        failwith $"Visible apply mutation was concealed as Failed: {failure.Code}"
                    | LakeFsMaterialization.Materialized _ ->
                        failwith "The injected apply failure unexpectedly completed."

                    let! firstAfterApply = tryReadUtf8FileAsync firstTarget
                    let! secondAfterApply = tryReadUtf8FileAsync secondTarget
                    Vitest.expect(firstAfterApply).toEqual(Some "new first bytes\n")
                    Vitest.expect(secondAfterApply).toEqual(Some "old second bytes\n")
                    let! thirdAfterApply = tryReadUtf8FileAsync thirdTarget
                    Vitest.expect(thirdAfterApply).toEqual(Some "old third bytes\n")
                    Vitest.expect(RuntimeNodeFileSystem.readdirSync transactionsDirectory).toHaveLength 1
                    Vitest.expect(RuntimeNodeFileSystem.readdirSync recoveryDirectory).toHaveLength 1

                    let! successfulApply =
                        LakeFsMaterialization.apply
                            workspaceRoot
                            stateDirectory
                            recoveryDirectory
                            currentIndex.WorkspaceRevision
                            "observed-revision"
                            plan
                            (fun _ _ -> async.Return())
                            (context "transactional-apply-recovery")
                        |> Async.StartAsPromise

                    match successfulApply with
                    | LakeFsMaterialization.Materialized(saved, affectedPaths, _) ->
                        Vitest.expect(affectedPaths).toEqual [| "100-second.txt"; "zzz-third.txt" |]
                        Vitest.expect(saved.Entries.Length).toBe 3
                    | LakeFsMaterialization.MaterializationFailed failure
                    | LakeFsMaterialization.MaterializationPartiallyApplied(_, failure) ->
                        failwith $"Successful materialization failed ({failure.Code}): {failure.Message}"

                    match LakeFsWorkspaceIndex.load stateDirectory with
                    | LakeFsWorkspaceIndex.Loaded saved ->
                        Vitest.expect(saved.Entries.Length).toBe 3
                    | _ -> failwith "Successful materialization did not publish its index."

                    Vitest.expect(RuntimeNodeFileSystem.readdirSync transactionsDirectory).toEqual [||]
                    Vitest.expect(RuntimeNodeFileSystem.readdirSync recoveryDirectory).toEqual [||]
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization recovery retains prepared bytes after a zero-visible replay failure",
            fun () -> promise {
                let! scenario = createMaterializationRecoveryScenario ()

                try
                    let! partial = partiallyApplyRecoveryScenario scenario

                    match partial with
                    | LakeFsMaterialization.MaterializationPartiallyApplied _ -> ()
                    | _ -> failwith "Expected the injected apply failure to retain recovery state."

                    let missingReplacement = scenario.Plan.Replacements[1]
                    do! removeFileAsync missingReplacement.TemporaryPath

                    let! replay =
                        LakeFsMaterialization.reapplyPending
                            scenario.WorkspaceRoot
                            scenario.State.StateDirectory
                            scenario.State.RecoveryDirectory
                            (fun _ _ -> async.Return())
                            (context "recovery-zero-visible-replay")
                        |> Async.StartAsPromise

                    match replay with
                    | Ok(Some(LakeFsMaterialization.MaterializationFailed _))
                    | Ok(Some(LakeFsMaterialization.MaterializationPartiallyApplied _)) -> ()
                    | Ok(Some(LakeFsMaterialization.Materialized _)) ->
                        failwith "A replay with missing prepared bytes unexpectedly succeeded."
                    | Ok None -> failwith "The retained recovery record was not loaded."
                    | Error failure -> failwith $"Loading recovery failed unexpectedly: {failure.Code}"

                    Vitest.expect(RuntimeNodeFileSystem.existsSync scenario.Plan.TransactionDirectory).toBe(true)
                    Vitest.expect(RuntimeNodeFileSystem.readdirSync scenario.State.RecoveryDirectory).toHaveLength(1)

                    RuntimeNodeFileSystem.writeUtf8FileExclusiveAndFlushSync
                        missingReplacement.TemporaryPath
                        scenario.NewContents[1]

                    let! completed =
                        LakeFsMaterialization.reapplyPending
                            scenario.WorkspaceRoot
                            scenario.State.StateDirectory
                            scenario.State.RecoveryDirectory
                            (fun _ _ -> async.Return())
                            (context "recovery-zero-visible-complete")
                        |> Async.StartAsPromise

                    match completed with
                    | Ok(Some(LakeFsMaterialization.Materialized _)) -> ()
                    | Ok(Some(LakeFsMaterialization.MaterializationFailed failure))
                    | Ok(Some(LakeFsMaterialization.MaterializationPartiallyApplied(_, failure))) ->
                        failwith $"The retained recovery did not complete: {failure.Code}"
                    | Ok None -> failwith "The retained recovery disappeared before completion."
                    | Error failure -> failwith $"Loading retained recovery failed: {failure.Code}"

                    Vitest.expect(RuntimeNodeFileSystem.readdirSync scenario.State.RecoveryDirectory).toEqual [||]
                    do! removeDirectoryAsync scenario.Root
                with error ->
                    do! removeDirectoryAsync scenario.Root
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization recovery preserves a diverged user edit and retained prepared bytes",
            fun () -> promise {
                let! scenario = createMaterializationRecoveryScenario ()

                try
                    let! partial = partiallyApplyRecoveryScenario scenario

                    match partial with
                    | LakeFsMaterialization.MaterializationPartiallyApplied _ -> ()
                    | _ -> failwith "Expected the injected apply failure to retain recovery state."

                    let userEdit = "user edit after partial materialization\n"
                    do! writeUtf8FileAsync scenario.Targets[0] userEdit

                    let! replay =
                        LakeFsMaterialization.reapplyPending
                            scenario.WorkspaceRoot
                            scenario.State.StateDirectory
                            scenario.State.RecoveryDirectory
                            (fun _ _ -> async.Return())
                            (context "recovery-diverged-replay")
                        |> Async.StartAsPromise

                    let failure =
                        match replay with
                        | Ok(Some(LakeFsMaterialization.MaterializationFailed failure))
                        | Ok(Some(LakeFsMaterialization.MaterializationPartiallyApplied(_, failure))) -> failure
                        | Ok(Some(LakeFsMaterialization.Materialized _)) ->
                            failwith "A replay overwrote a diverged user edit."
                        | Ok None -> failwith "The retained recovery record was not loaded."
                        | Error failure -> failure

                    Vitest.expect(failure.Code).toBe "materialization_target_diverged"
                    Vitest.expect(failure.AffectedPaths).toEqual [| RepositoryPath.value scenario.Paths[0] |]
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual(Some "reconcile_materialization")
                    Vitest.expect(
                        failure.RecoveryAction
                        |> Option.bind _.Instructions
                        |> Option.exists (fun instructions -> instructions.Contains("RestorePaths"))
                    ).toBe(true)

                    let! preservedEdit = tryReadUtf8FileAsync scenario.Targets[0]
                    Vitest.expect(preservedEdit).toEqual(Some userEdit)
                    Vitest.expect(RuntimeNodeFileSystem.existsSync scenario.Plan.Replacements[0].TemporaryPath).toBe(true)
                    Vitest.expect(RuntimeNodeFileSystem.existsSync scenario.Plan.TransactionDirectory).toBe(true)
                    Vitest.expect(RuntimeNodeFileSystem.readdirSync scenario.State.RecoveryDirectory).toHaveLength(1)
                    do! removeDirectoryAsync scenario.Root
                with error ->
                    do! removeDirectoryAsync scenario.Root
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization recovery does not publish a replacement that reports StateChanged failure",
            fun () -> promise {
                let! scenario = createMaterializationRecoveryScenario ()
                let restoreFileSystem = injectPostRenameLinkObservation scenario.Targets[0]

                try
                    let! applied =
                        LakeFsMaterialization.apply
                            scenario.WorkspaceRoot
                            scenario.State.StateDirectory
                            scenario.State.RecoveryDirectory
                            scenario.CurrentIndex.WorkspaceRevision
                            "observed-revision"
                            scenario.Plan
                            (fun _ _ -> async.Return())
                            (context "recovery-statechanged-replacement")
                        |> Async.StartAsPromise

                    match applied with
                    | LakeFsMaterialization.MaterializationPartiallyApplied(Some saved, failure) ->
                        Vitest.expect(failure.StateChanged).toBe(true)
                        Vitest.expect(failure.AffectedPaths).toContain(RepositoryPath.value scenario.Paths[0])

                        let failedEntry =
                            saved.Entries
                            |> Array.find (fun entry -> entry.Path = RepositoryPath.value scenario.Paths[0])

                        Vitest.expect(failedEntry.LocalHash)
                            .toBe(LakeFsWorkspaceIndex.hashMetadata scenario.OldContents[0])
                    | LakeFsMaterialization.MaterializationPartiallyApplied(None, failure) ->
                        failwith $"The unchanged index was not retained: {failure.Code}"
                    | LakeFsMaterialization.MaterializationFailed failure ->
                        failwith $"The visible replacement was concealed as Failed: {failure.Code}"
                    | LakeFsMaterialization.Materialized _ ->
                        failwith "The injected post-replacement validation unexpectedly succeeded."

                    restoreFileSystem ()
                    let! target = tryReadUtf8FileAsync scenario.Targets[0]
                    Vitest.expect(target).toEqual(Some scenario.NewContents[0])
                    Vitest.expect(RuntimeNodeFileSystem.existsSync scenario.Plan.Replacements[0].TemporaryPath)
                        .toBe(true)
                    do! removeDirectoryAsync scenario.Root
                with error ->
                    restoreFileSystem ()
                    do! removeDirectoryAsync scenario.Root
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization recovery open sweep preserves referenced transactions",
            fun () -> promise {
                let! scenario = createMaterializationRecoveryScenario ()

                try
                    let! partial = partiallyApplyRecoveryScenario scenario

                    match partial with
                    | LakeFsMaterialization.MaterializationPartiallyApplied _ -> ()
                    | _ -> failwith "Expected the injected apply failure to retain recovery state."

                    let orphanTransaction = join [| scenario.State.TransactionsDirectory; "orphan-transaction" |]
                    let orphanTemporary = join [| scenario.State.TemporaryDirectory; "conflict-candidate-orphan.tmp" |]
                    do! ensureDirectoryAsync orphanTransaction
                    do! writeUtf8FileAsync orphanTemporary "orphan"

                    match LakeFsStateStore.loadRecoveryRecords scenario.State.RecoveryDirectory with
                    | Ok records ->
                        Vitest.expect(records).toHaveLength(1)
                        Vitest.expect(records[0].Record.TransactionId).toBe(scenario.Plan.TransactionId)
                    | Error failure -> failwith $"Loading typed recovery state failed: {failure.Code}"

                    LakeFsStateStore.sweepTransientEntries scenario.State

                    Vitest.expect(RuntimeNodeFileSystem.existsSync scenario.Plan.TransactionDirectory).toBe(true)
                    Vitest.expect(RuntimeNodeFileSystem.existsSync orphanTransaction).toBe(false)
                    Vitest.expect(RuntimeNodeFileSystem.existsSync orphanTemporary).toBe(false)
                    do! removeDirectoryAsync scenario.Root
                with error ->
                    do! removeDirectoryAsync scenario.Root
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization recovery grafts replay onto the newest persisted index",
            fun () -> promise {
                let! scenario = createMaterializationRecoveryScenario ()

                try
                    let! partial = partiallyApplyRecoveryScenario scenario

                    match partial with
                    | LakeFsMaterialization.MaterializationPartiallyApplied _ -> ()
                    | _ -> failwith "Expected the injected apply failure to retain recovery state."

                    let persisted =
                        match LakeFsWorkspaceIndex.load scenario.State.StateDirectory with
                        | LakeFsWorkspaceIndex.Loaded loaded -> loaded
                        | _ -> failwith "Expected the partial index to be persisted."

                    let unrelatedEntry: LakeFsWorkspaceIndex.IndexEntry = {
                        Path = "unrelated.txt"
                        BaseChecksum = "unrelated-checksum"
                        LocalHash = LakeFsWorkspaceIndex.hashMetadata "unrelated\n"
                        LocalSize = 10.0
                        LocalMtimeMs = 2.0
                    }

                    let concurrentIndex =
                        LakeFsWorkspaceIndex.save
                            scenario.State.StateDirectory
                            {
                                persisted with
                                    BaseRevision = Some "newer-base-revision"
                                    WorkspaceRevision = Some "newer-workspace-revision"
                                    Entries = Array.append persisted.Entries [| unrelatedEntry |]
                            }
                        |> Result.defaultWith failwith

                    let! replay =
                        LakeFsMaterialization.reapplyPending
                            scenario.WorkspaceRoot
                            scenario.State.StateDirectory
                            scenario.State.RecoveryDirectory
                            (fun _ _ -> async.Return())
                            (context "recovery-newest-index-replay")
                        |> Async.StartAsPromise

                    match replay with
                    | Ok(Some(LakeFsMaterialization.Materialized _)) -> ()
                    | Ok(Some(LakeFsMaterialization.MaterializationFailed failure))
                    | Ok(Some(LakeFsMaterialization.MaterializationPartiallyApplied(_, failure))) ->
                        failwith $"Replaying onto the newest index failed: {failure.Code}"
                    | Ok None -> failwith "The retained recovery record was not replayed."
                    | Error failure -> failwith $"Loading retained recovery failed: {failure.Code}"

                    match LakeFsWorkspaceIndex.load scenario.State.StateDirectory with
                    | LakeFsWorkspaceIndex.Loaded replayed ->
                        Vitest.expect(replayed.Generation > concurrentIndex.Generation).toBe(true)
                        Vitest.expect(replayed.BaseRevision).toEqual(Some "newer-base-revision")
                        Vitest.expect(replayed.WorkspaceRevision).toEqual(Some "newer-workspace-revision")
                        Vitest.expect(replayed.Entries |> Array.exists (fun entry -> entry.Path = "unrelated.txt"))
                            .toBe(true)
                    | _ -> failwith "The replayed index was not readable."

                    do! removeDirectoryAsync scenario.Root
                with error ->
                    do! removeDirectoryAsync scenario.Root
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization recovery handles missing corrupt and unknown-schema metadata structurally",
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let recoveryDirectory = join [| root; "missing-recovery" |]

                try
                    match LakeFsStateStore.loadRecoveryRecords recoveryDirectory with
                    | Ok records -> Vitest.expect(records).toEqual [||]
                    | Error failure -> failwith $"A missing recovery directory failed: {failure.Code}"

                    do! ensureDirectoryAsync recoveryDirectory
                    let recordPath = join [| recoveryDirectory; "materialization-invalid.json" |]

                    let expectAction
                        (result:
                            Result<
                                LakeFsStateStore.LoadedRecoveryRecord[],
                                OperationFailure
                             >)
                        =
                        match result with
                        | Error failure ->
                            Vitest.expect(failure.Code).toBe "materialization_recovery_corrupt"
                            Vitest.expect(failure.RecoveryAction |> Option.map _.Code)
                                .toEqual(Some "reconcile_materialization")
                            Vitest.expect(
                                failure.RecoveryAction
                                |> Option.bind _.Instructions
                                |> Option.exists (fun instructions ->
                                    instructions.Contains(recordPath)
                                    && instructions.Contains("RestorePaths"))
                            ).toBe(true)
                        | Ok _ -> failwith "Invalid recovery metadata was accepted."

                    do! writeUtf8FileAsync recordPath "{\"SchemaVersion\":99}"
                    LakeFsStateStore.loadRecoveryRecords recoveryDirectory |> expectAction
                    do! removeFileAsync recordPath
                    do! writeUtf8FileAsync recordPath "broken recovery json {"
                    LakeFsStateStore.loadRecoveryRecords recoveryDirectory |> expectAction
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization recovery drains every retained record in stable order",
            fun () -> promise {
                let! scenario = createMaterializationRecoveryScenario ()

                try
                    let transactionDirectories = ResizeArray<string>()

                    for suffix in [| "a"; "b" |] do
                        let transactionId = $"{suffix}-{RuntimeNodeInterop.randomUuid()}"
                        let transactionDirectory =
                            join [| scenario.State.TransactionsDirectory; transactionId |]

                        do! ensureDirectoryAsync transactionDirectory
                        transactionDirectories.Add transactionDirectory

                        let record: LakeFsStateStore.RecoveryRecord = {
                            SchemaVersion = LakeFsStateStore.RecoverySchemaVersion
                            TransactionId = transactionId
                            TransactionDirectory = transactionDirectory
                            ExpectedWorkspace = scenario.CurrentIndex.WorkspaceRevision
                            ObservedMaterialization = "observed-revision"
                            AffectedPaths = [||]
                            Replacements = [||]
                            Removals = [||]
                            CurrentIndex = box scenario.CurrentIndex
                            NextIndex = box scenario.CurrentIndex
                        }

                        RuntimeNodeFileSystem.writeUtf8FileExclusiveAndFlushSync
                            (join [|
                                scenario.State.RecoveryDirectory
                                $"materialization-{transactionId}.json"
                            |])
                            (jsonStringify record)

                    let! replay =
                        LakeFsMaterialization.reapplyPending
                            scenario.WorkspaceRoot
                            scenario.State.StateDirectory
                            scenario.State.RecoveryDirectory
                            (fun _ _ -> async.Return())
                            (context "recovery-drain-all")
                        |> Async.StartAsPromise

                    match replay with
                    | Ok(Some(LakeFsMaterialization.Materialized _)) -> ()
                    | Ok(Some(LakeFsMaterialization.MaterializationFailed failure))
                    | Ok(Some(LakeFsMaterialization.MaterializationPartiallyApplied(_, failure))) ->
                        failwith $"Draining retained recovery failed: {failure.Code}"
                    | Ok None -> failwith "Retained recovery records were not loaded."
                    | Error failure -> failwith $"Loading retained recovery failed: {failure.Code}"

                    Vitest.expect(RuntimeNodeFileSystem.readdirSync scenario.State.RecoveryDirectory).toEqual [||]

                    for directory in transactionDirectories do
                        Vitest.expect(RuntimeNodeFileSystem.existsSync directory).toBe(false)

                    do! removeDirectoryAsync scenario.Root
                with error ->
                    do! removeDirectoryAsync scenario.Root
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization recovery removes phantom index entries for already-absent targets",
            fun () -> promise {
                let! scenario = createMaterializationRecoveryScenario ()

                try
                    do! removeFileAsync scenario.Targets[0]
                    let transactionId = RuntimeNodeInterop.randomUuid()
                    let transactionDirectory =
                        join [| scenario.State.TransactionsDirectory; transactionId |]

                    do! ensureDirectoryAsync transactionDirectory

                    let removalPlan: LakeFsMaterialization.MaterializationPlan = {
                        TransactionId = transactionId
                        TransactionDirectory = transactionDirectory
                        Replacements = [||]
                        Removals = [| scenario.Paths[0], scenario.Targets[0] |]
                        CurrentIndex = scenario.CurrentIndex
                        NextIndex = {
                            scenario.CurrentIndex with
                                Entries =
                                    scenario.CurrentIndex.Entries
                                    |> Array.filter (fun entry ->
                                        entry.Path <> RepositoryPath.value scenario.Paths[0])
                        }
                    }

                    let! applied =
                        LakeFsMaterialization.apply
                            scenario.WorkspaceRoot
                            scenario.State.StateDirectory
                            scenario.State.RecoveryDirectory
                            scenario.CurrentIndex.WorkspaceRevision
                            "observed-revision"
                            removalPlan
                            (fun _ _ -> async.Return())
                            (context "recovery-absent-removal")
                        |> Async.StartAsPromise

                    match applied with
                    | LakeFsMaterialization.Materialized(saved, affectedPaths, _) ->
                        Vitest.expect(affectedPaths).toEqual [||]
                        Vitest.expect(
                            saved.Entries
                            |> Array.exists (fun entry ->
                                entry.Path = RepositoryPath.value scenario.Paths[0])
                        ).toBe(false)
                    | LakeFsMaterialization.MaterializationFailed failure
                    | LakeFsMaterialization.MaterializationPartiallyApplied(_, failure) ->
                        failwith $"Removing an absent indexed target failed: {failure.Code}"

                    do! removeDirectoryAsync scenario.Root
                with error ->
                    do! removeDirectoryAsync scenario.Root
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization recovery gates conflict resolve finalize and cancel",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! workspace, conflicts, summary = createSingleFileConflict harness
                    let! resolutionStatusResult =
                        workspace.Session.Core.GetStatus(context "recovery-conflict-resolution-status")
                        |> Async.StartAsPromise

                    let resolutionStatus =
                        expectOperationValue "recovery conflict resolution status" resolutionStatusResult

                    let! resolutionResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = resolutionStatus.WorkspaceVersion
                                Path = summary.Items[0].Path
                                Resolution = PickCandidate "target"
                            }
                            (context "recovery-conflict-preresolve")
                        |> Async.StartAsPromise

                    let resolution =
                        expectOperationValue "recovery conflict preresolve" resolutionResult

                    let! pendingStatusResult =
                        workspace.Session.Core.GetStatus(context "recovery-conflict-pending-status")
                        |> Async.StartAsPromise

                    let pendingStatus =
                        expectOperationValue "recovery conflict pending status" pendingStatusResult

                    let state =
                        LakeFsStateStore.resolve
                            lakeFsProviderOptions
                            workspace.Binding.WorkspaceRoot
                            workspace.Binding.ProviderStateRef
                        |> Result.defaultWith (fun failure -> failwith failure.Message)

                    let index =
                        match LakeFsWorkspaceIndex.load state.StateDirectory with
                        | LakeFsWorkspaceIndex.Loaded loaded -> loaded
                        | _ -> failwith "Expected a persisted index for conflict recovery gating."

                    let transactionId = RuntimeNodeInterop.randomUuid()
                    let transactionDirectory = join [| state.TransactionsDirectory; transactionId |]
                    do! ensureDirectoryAsync transactionDirectory

                    let record: LakeFsStateStore.RecoveryRecord = {
                        SchemaVersion = LakeFsStateStore.RecoverySchemaVersion
                        TransactionId = transactionId
                        TransactionDirectory = transactionDirectory
                        ExpectedWorkspace = index.WorkspaceRevision
                        ObservedMaterialization = index.WorkspaceRevision |> Option.defaultValue "unknown"
                        AffectedPaths = [| "base.txt" |]
                        Replacements = [||]
                        Removals = [||]
                        CurrentIndex = box index
                        NextIndex = box index
                    }

                    let recoveryPath =
                        join [| state.RecoveryDirectory; $"materialization-{transactionId}.json" |]

                    RuntimeNodeFileSystem.writeUtf8FileExclusiveAndFlushSync
                        recoveryPath
                        (jsonStringify record)

                    let expectRecoveryGate operation result =
                        match result with
                        | Failed failure ->
                            Vitest.expect(failure.Code).toBe "materialization_recovery_pending"
                            Vitest.expect(failure.RecoveryAction |> Option.map _.Code)
                                .toEqual(Some "reconcile_materialization")
                        | _ -> failwith $"Conflict {operation} bypassed pending recovery."

                        Vitest.expect(RuntimeNodeFileSystem.readdirSync state.RecoveryDirectory)
                            .toEqual [| RuntimeNodePath.basename recoveryPath |]

                    let! blockedResolve =
                        conflicts.Resolve
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = pendingStatus.WorkspaceVersion
                                Path = summary.Items[0].Path
                                Resolution = PickCandidate "workspace"
                            }
                            (context "recovery-conflict-blocked-resolve")
                        |> Async.StartAsPromise

                    expectRecoveryGate "resolve" blockedResolve

                    let! blockedFinalize =
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = pendingStatus.WorkspaceVersion
                                Message = Some "must remain blocked"
                            }
                            (context "recovery-conflict-blocked-finalize")
                        |> Async.StartAsPromise

                    expectRecoveryGate "finalize" blockedFinalize

                    let! blockedCancel =
                        conflicts.Cancel
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = pendingStatus.WorkspaceVersion
                            }
                            (context "recovery-conflict-blocked-cancel")
                        |> Async.StartAsPromise

                    expectRecoveryGate "cancel" blockedCancel
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
    )

Vitest.describe (
    "lakeFS binary conflicts",
    fun () ->
        Vitest.test (
            "uses unsupported previews and preserves original candidate selection",
            fun () -> promise {
                let binaryCandidate: LakeFsConflictSession.CandidateContent = {
                    SourcePath = "external/binary-candidate.dat"
                    Preview = UnsupportedPreview(Some "binary")
                }

                let item: LakeFsConflictSession.ItemState = {
                    ItemPath = "binary.dat"
                    BaseContent = Some binaryCandidate
                    WorkspaceContent = Some binaryCandidate
                    TargetContent = Some binaryCandidate
                    ResolvedContent = None
                }

                let conflict = LakeFsConflictSession.create "target-revision" "workspace-revision" [ item ]
                let summary =
                    LakeFsConflictSession.summary
                        (RevisionId.tryCreate "base-revision" |> Result.toOption)
                        (RevisionId.tryCreate "workspace-revision" |> Result.toOption)
                        (Some conflict)
                    |> Option.defaultWith (fun () -> failwith "Expected a binary conflict summary.")

                Vitest.expect(summary.Items[0].SupportsResolvedContent).toBe false
                Vitest.expect(
                    summary.Items[0].Candidates
                    |> Array.forall (fun candidate ->
                        match candidate.Preview with
                        | Some(UnsupportedPreview _) -> true
                        | _ -> false)
                ).toBe true

                match
                    LakeFsConflictSession.resolve
                        conflict
                        (repositoryPath "binary.dat")
                        (SupplyResolvedContent "text would corrupt bytes")
                with
                | Error failure -> Vitest.expect(failure.Code).toBe "manual_resolution_required"
                | Ok() -> failwith "Expected supplied text to be rejected for a binary conflict."

                match
                    LakeFsConflictSession.resolve
                        conflict
                        (repositoryPath "binary.dat")
                        (PickCandidate "bogus")
                with
                | Error failure -> Vitest.expect(failure.Code).toBe "unknown_candidate"
                | Ok() -> failwith "Expected an unknown candidate to be rejected for a binary conflict."

                match
                    LakeFsConflictSession.resolve
                        conflict
                        (repositoryPath "binary.dat")
                        (PickCandidate "target")
                with
                | Ok() -> ()
                | Error failure -> failwith $"Expected original binary candidate selection: {failure.Code}"
            }
        )
)

Vitest.describe (
    "lakeFS / extension suites",
    fun () ->
        Vitest.test (
            "unclassifiable previews use a retryable provider-neutral failure",
            fun () -> promise {
                let source =
                    OperationFailure.create Network "diff_transport_failed" "The object diff could not be read."

                let failure =
                    LakeFsSynchronization.previewIndeterminate
                        "locally committed objects"
                        source

                Vitest.expect(failure.Category).toEqual ProviderError
                Vitest.expect(failure.Code).toBe "preview_indeterminate"
                Vitest.expect(failure.Retryable).toBe true
                Vitest.expect(failure.StateChanged).toBe false
            }
        )

        Vitest.test (
            "target diff failures surface through PreviewUpdate as indeterminate",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let parsed = parseLocation workspace.Binding.Location
                    do! deleteRepository parsed.Repository

                    let synchronization =
                        workspace.Session.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS synchronization.")

                    let! previewResult =
                        synchronization.PreviewUpdate(context "preview-target-diff-failure")
                        |> Async.StartAsPromise

                    match previewResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual ProviderError
                        Vitest.expect(failure.Code).toBe "preview_indeterminate"
                        Vitest.expect(failure.Retryable).toBe true
                        Vitest.expect(failure.StateChanged).toBe false
                    | Succeeded _
                    | PartiallySucceeded _ -> failwith "Expected target diff classification to fail."

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "configured target identity is exposed through the neutral synchronization state",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let! statusResult =
                        workspace.Session.Core.GetStatus(context "configured-target-ref")
                        |> Async.StartAsPromise

                    let status = expectOperationValue "configured target status" statusResult

                    let target =
                        status.Synchronization
                        |> Option.bind _.TargetRef
                        |> Option.defaultWith (fun () -> failwith "Expected the configured lakeFS target branch.")

                    Vitest.expect(target.Name).toBe "main"
                    Vitest.expect(ProviderRef.value target.ProviderRef).toBe "lakefs:main"
                    Vitest.expect(target.Kind).toEqual LocalRef
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

Vitest.describe (
    "lakeFS selected revision cycles",
    fun () ->
        Vitest.test (
            "lakeFS selected revision preserves unrelated local changes and verifies commit head",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let! initialStatusResult =
                        workspace.Session.Core.GetStatus(context "selected-revision-status")
                        |> Async.StartAsPromise

                    let initialStatus = expectOperationValue "initial status" initialStatusResult
                    let initialIndex =
                        match VersionControlService.LakeFs.LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | VersionControlService.LakeFs.LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected a persisted workspace index."

                    let parsed = parseLocation workspace.Binding.Location
                    let! targetBeforeResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (context "selected-revision-target-before")
                        |> Async.StartAsPromise

                    let targetBefore = targetBeforeResult |> expectApi "read target before selected revision"

                    do! workspace.WriteFile "selected[1].txt" "selected content\n"
                    do! workspace.WriteFile "unselected.txt" "unselected content\n"

                    let! statusBeforeRevisionResult =
                        workspace.Session.Core.GetStatus(context "selected-revision-dirty-status")
                        |> Async.StartAsPromise

                    let statusBeforeRevision = expectOperationValue "dirty status" statusBeforeRevisionResult

                    let! revisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "save selected literal path"
                                Paths = [| repositoryPath "selected[1].txt" |]
                                ExpectedWorkspaceVersion = statusBeforeRevision.WorkspaceVersion
                            }
                            (context "selected-revision-create")
                        |> Async.StartAsPromise

                    let revision = expectOperationValue "selected revision" revisionResult
                    let revisionText = RevisionId.value revision

                    let! targetAfterResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (context "selected-revision-target-after")
                        |> Async.StartAsPromise

                    let targetAfter = targetAfterResult |> expectApi "read target after selected revision"
                    Vitest.expect(targetAfter.CommitId).toBe (targetBefore.CommitId)

                    let! workspaceBranchResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            initialIndex.WorkspaceBranch
                            (context "selected-revision-workspace-head")
                        |> Async.StartAsPromise

                    let workspaceBranch = workspaceBranchResult |> expectApi "read workspace branch"
                    Vitest.expect(workspaceBranch.CommitId).toBe (revisionText)

                    let! commitResult =
                        LakeFsApi.getCommit
                            (connection ())
                            parsed.Repository
                            revisionText
                            (context "selected-revision-commit")
                        |> Async.StartAsPromise

                    let commit = commitResult |> expectApi "read selected commit"
                    Vitest.expect(commit.Parents).toContain (initialIndex.WorkspaceRevision.Value)

                    let! statusAfterResult =
                        workspace.Session.Core.GetStatus(context "selected-revision-status-after")
                        |> Async.StartAsPromise

                    let statusAfter = expectOperationValue "status after selected revision" statusAfterResult
                    Vitest.expect(statusAfter.Changes |> Array.map (fun change -> RepositoryPath.value change.Path)).toEqual ([| "unselected.txt" |])

                    do! workspace.WriteFile "selected[1].txt" "selected content after race\n"
                    let! raceStatusResult =
                        workspace.Session.Core.GetStatus(context "selected-revision-race-status")
                        |> Async.StartAsPromise

                    let raceStatus = expectOperationValue "race status" raceStatusResult
                    let indexBeforeRace =
                        match VersionControlService.LakeFs.LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | VersionControlService.LakeFs.LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before the race."

                    do!
                        harness.ArmDestinationRace workspace [|
                            { Path = "racer.txt"; Content = Some "concurrent branch content\n" }
                        |]

                    let! racedRevision =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "race selected revision"
                                Paths = [| repositoryPath "selected[1].txt" |]
                                ExpectedWorkspaceVersion = raceStatus.WorkspaceVersion
                            }
                            (context "selected-revision-race")
                        |> Async.StartAsPromise

                    match racedRevision with
                    | Succeeded _ -> failwith "A moved workspace branch must not be reported as clean success."
                    | PartiallySucceeded(_, failure)
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual (FailureCategory.Concurrency)
                        Vitest.expect(failure.Code).toBe "precondition_failed"

                    let indexAfterRace =
                        match VersionControlService.LakeFs.LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | VersionControlService.LakeFs.LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index after the race."

                    Vitest.expect(indexAfterRace.WorkspaceRevision).toEqual (indexBeforeRace.WorkspaceRevision)
                    Vitest.expect(indexAfterRace.Generation).toBe (indexBeforeRace.Generation)
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

Vitest.describe (
    "lakeFS core read cycles",
    fun () ->
        Vitest.test (
            "lakeFS core reports status refs restore and paginated object diff",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let index =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded value -> value
                        | _ -> failwith "Expected a persisted workspace index."

                    let remoteMutations =
                        Array.init 105 (fun number -> {
                            Path = $"paged/object-{number:D3}.txt"
                            Content = Some $"remote object {number}\n"
                        })

                    do! harness.AdvanceTarget workspace remoteMutations

                    let parsed = parseLocation workspace.Binding.Location
                    let! targetResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (context "core-reads-target-head")
                        |> Async.StartAsPromise

                    let target = targetResult |> expectApi "read advanced target"
                    let! statusResult =
                        workspace.Session.Core.GetStatus(context "core-reads-status")
                        |> Async.StartAsPromise

                    let status = expectOperationValue "core read status" statusResult
                    let synchronization = status.Synchronization |> Option.defaultWith (fun () -> failwith "Expected synchronization state.")
                    Vitest.expect(synchronization.TargetRevision |> Option.map RevisionId.value).toEqual (Some target.CommitId)
                    Vitest.expect(synchronization.Relationship).toEqual (RevisionRelationship.TargetAhead)

                    let remotePaths =
                        synchronization.RemoteChangedPaths
                        |> Option.defaultWith (fun () -> failwith "Expected a paginated remote object diff.")
                        |> Array.map RepositoryPath.value

                    Vitest.expect(remotePaths.Length).toBe 105
                    Vitest.expect(remotePaths).toContain "paged/object-104.txt"

                    let! refsResult =
                        workspace.Session.Core.ListRefs(context "core-reads-refs")
                        |> Async.StartAsPromise

                    let refs = expectOperationValue "list refs" refsResult
                    Vitest.expect(refs |> Array.exists (fun reference -> reference.Name = parsed.TargetRef)).toBe true
                    Vitest.expect(refs |> Array.exists (fun reference -> reference.Name = index.WorkspaceBranch)).toBe false

                    do! workspace.WriteFile "base.txt" "local base edit\n"
                    do! workspace.WriteFile "keep-local.txt" "keep this local edit\n"

                    let! dirtyStatusResult =
                        workspace.Session.Core.GetStatus(context "core-reads-dirty-status")
                        |> Async.StartAsPromise

                    let dirtyStatus = expectOperationValue "dirty status" dirtyStatusResult
                    let! restoreResult =
                        workspace.Session.Core.RestorePaths
                            {
                                Paths = [| repositoryPath "base.txt" |]
                                ExpectedWorkspaceVersion = dirtyStatus.WorkspaceVersion
                            }
                            (context "core-reads-restore")
                        |> Async.StartAsPromise

                    expectOperationValue "restore selected path" restoreResult |> ignore

                    let! restoredBase = workspace.ReadFile "base.txt"
                    let! preservedLocal = workspace.ReadFile "keep-local.txt"
                    Vitest.expect(restoredBase).toEqual (Some "base content\n")
                    Vitest.expect(preservedLocal).toEqual (Some "keep this local edit\n")

                    let! diffResult =
                        workspace.Session.Core.GetDiffSummary(context "core-reads-diff")
                        |> Async.StartAsPromise

                    let diff = expectOperationValue "object diff" diffResult
                    let diffPaths = diff.Entries |> Array.map (fun entry -> RepositoryPath.value entry.Path)
                    Vitest.expect(diffPaths).toEqual ([| "keep-local.txt" |])

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

Vitest.describe (
    "lakeFS synchronization cycles",
    fun () ->
        Vitest.test (
            "lakeFS refresh and preview report paginated local target and overlap paths",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    let synchronization =
                        workspace.Session.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS synchronization services.")

                    let localPaths = Array.init 105 (fun number -> $"local/object-{number:D3}.txt")

                    for number, path in localPaths |> Array.indexed do
                        do! workspace.WriteFile path $"local object {number}\n"

                    let! localStatusResult =
                        workspace.Session.Core.GetStatus(context "refresh-preview-local-status")
                        |> Async.StartAsPromise

                    let localStatus = expectOperationValue "local status" localStatusResult
                    let! localRevisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "create paginated local revision"
                                Paths = localPaths |> Array.map repositoryPath
                                ExpectedWorkspaceVersion = localStatus.WorkspaceVersion
                            }
                            (context "refresh-preview-local-revision")
                        |> Async.StartAsPromise

                    expectOperationValue "paginated local revision" localRevisionResult |> ignore

                    let remoteMutations = [|
                        for number in 0..103 do
                            {
                                Path = $"remote/object-{number:D3}.txt"
                                Content = Some $"remote object {number}\n"
                            }

                        {
                            Path = "local/object-104.txt"
                            Content = Some "target overlap content\n"
                        }
                    |]

                    do! harness.AdvanceTarget workspace remoteMutations

                    let beforeRead =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before refresh/preview."

                    let! refreshResult =
                        synchronization.Refresh(context "refresh-preview-refresh")
                        |> Async.StartAsPromise

                    let refreshed = expectOperationValue "paginated refresh" refreshResult
                    Vitest.expect(refreshed.Relationship).toEqual (RevisionRelationship.Diverged)
                    Vitest.expect(refreshed.LocalRevisionCount).toEqual (None)
                    Vitest.expect(refreshed.TargetRevisionCount).toEqual (None)

                    let refreshedPaths =
                        refreshed.RemoteChangedPaths
                        |> Option.defaultWith (fun () -> failwith "Expected refreshed remote paths.")
                        |> Array.map RepositoryPath.value

                    Vitest.expect(refreshedPaths.Length).toBe 105
                    Vitest.expect(refreshedPaths).toContain "remote/object-103.txt"
                    Vitest.expect(refreshedPaths).toContain "local/object-104.txt"

                    let! previewResult =
                        synchronization.PreviewUpdate(context "refresh-preview-preview")
                        |> Async.StartAsPromise

                    let preview = expectOperationValue "paginated preview" previewResult
                    let changedPaths = preview.ChangedPaths |> Array.map RepositoryPath.value
                    let overlapPaths = preview.OverlappingPaths |> Array.map RepositoryPath.value
                    Vitest.expect(changedPaths.Length).toBe 105
                    Vitest.expect(changedPaths).toContain "remote/object-103.txt"
                    Vitest.expect(overlapPaths).toEqual ([| "local/object-104.txt" |])
                    Vitest.expect(preview.HasDataLossRisk).toBe false
                    Vitest.expect(preview.WouldCreateConflictSession).toBe true

                    let afterRead =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index after refresh/preview."

                    Vitest.expect(afterRead.Generation).toBe (beforeRead.Generation)
                    Vitest.expect(afterRead.BaseRevision).toEqual (beforeRead.BaseRevision)
                    Vitest.expect(afterRead.WorkspaceRevision).toEqual (beforeRead.WorkspaceRevision)
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

Vitest.describe (
    "lakeFS conflict-session cycles",
    fun () ->
        Vitest.test (
            "lakeFS conflict session exposes candidates supplied text resolution and stale-token rejection",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! workspace = harness.CreateWorkspace()

                    do!
                        harness.AdvanceTarget workspace [|
                            { Path = "base.txt"; Content = Some "target base\n" }
                            { Path = "second.txt"; Content = Some "target second\n" }
                        |]

                    let parsed = parseLocation workspace.Binding.Location
                    let! targetBeforeResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (context "conflict-cycle-target-before")
                        |> Async.StartAsPromise

                    let targetBefore = targetBeforeResult |> expectApi "conflict target before"

                    do! workspace.WriteFile "base.txt" "workspace base\n"
                    do! workspace.WriteFile "second.txt" "workspace second\n"

                    let! saveStatusResult =
                        workspace.Session.Core.GetStatus(context "conflict-cycle-save-status")
                        |> Async.StartAsPromise

                    let saveStatus = expectOperationValue "conflict save status" saveStatusResult
                    let! saveResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "create conflicting workspace revision"
                                Paths = [| repositoryPath "base.txt"; repositoryPath "second.txt" |]
                                ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                            }
                            (context "conflict-cycle-save")
                        |> Async.StartAsPromise

                    expectOperationValue "conflicting workspace revision" saveResult |> ignore

                    let! updateStatusResult =
                        workspace.Session.Core.GetStatus(context "conflict-cycle-update-status")
                        |> Async.StartAsPromise

                    let updateStatus = expectOperationValue "conflict update status" updateStatusResult
                    let synchronization =
                        workspace.Session.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS synchronization services.")

                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                            (context "conflict-cycle-update")
                        |> Async.StartAsPromise

                    let updateFailure =
                        match updateResult with
                        | Succeeded _ -> failwith "The conflicting update unexpectedly succeeded."
                        | PartiallySucceeded(_, failure)
                        | Failed failure -> failure

                    Vitest.expect(updateFailure.Category).toEqual FailureCategory.Conflict
                    Vitest.expect(updateFailure.Code).toBe "conflicts_detected"

                    let conflicts =
                        workspace.Session.ConflictResolution
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS conflict-resolution services.")

                    let! sessionResult =
                        conflicts.GetActiveSession(context "conflict-cycle-session")
                        |> Async.StartAsPromise

                    let summary =
                        expectOperationValue "active conflict session" sessionResult
                        |> Option.defaultWith (fun () -> failwith "Expected an active conflict session.")

                    Vitest.expect(summary.Handle.SessionId.Length).toBeGreaterThan 20
                    Vitest.expect(summary.Handle.Version.Length).toBeGreaterThan 0
                    Vitest.expect(summary.Items.Length).toBe 2

                    for item in summary.Items do
                        let candidateIds = item.Candidates |> Array.map _.CandidateId
                        Vitest.expect(candidateIds).toContain "workspace"
                        Vitest.expect(candidateIds).toContain "target"
                        Vitest.expect(item.SupportsResolvedContent).toBe true

                    let firstItem =
                        summary.Items
                        |> Array.find (fun item -> RepositoryPath.value item.Path = "base.txt")

                    let! resolutionStatusResult =
                        workspace.Session.Core.GetStatus(context "conflict-cycle-resolution-status")
                        |> Async.StartAsPromise

                    let resolutionStatus = expectOperationValue "resolution status" resolutionStatusResult
                    let! firstResolutionResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = resolutionStatus.WorkspaceVersion
                                Path = firstItem.Path
                                Resolution = PickCandidate "target"
                            }
                            (context "conflict-cycle-first-resolution")
                        |> Async.StartAsPromise

                    let firstResolution = expectOperationValue "first conflict resolution" firstResolutionResult
                    Vitest.expect(firstResolution.RefreshedHandle.SessionId).toBe summary.Handle.SessionId
                    Vitest.expect(firstResolution.RefreshedHandle.Version).not.toBe summary.Handle.Version
                    Vitest.expect(firstResolution.RemainingItems.Length).toBe 1

                    let! afterFirstStatusResult =
                        workspace.Session.Core.GetStatus(context "conflict-cycle-after-first-status")
                        |> Async.StartAsPromise

                    let afterFirstStatus = expectOperationValue "after first resolution status" afterFirstStatusResult
                    let secondItem = firstResolution.RemainingItems[0]
                    let! staleResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = afterFirstStatus.WorkspaceVersion
                                Path = secondItem.Path
                                Resolution = PickCandidate "workspace"
                            }
                            (context "conflict-cycle-stale-resolution")
                        |> Async.StartAsPromise

                    let staleFailure =
                        match staleResult with
                        | Succeeded _
                        | PartiallySucceeded _ -> failwith "A stale conflict handle unexpectedly mutated the session."
                        | Failed failure -> failure

                    Vitest.expect(staleFailure.Category).toEqual FailureCategory.Concurrency
                    Vitest.expect(staleFailure.Code).toBe "precondition_failed"
                    Vitest.expect(staleFailure.RecoveryAction |> Option.map _.Code).toEqual (Some ConflictRecovery.RefreshConflictSession)

                    let! afterStaleSessionResult =
                        conflicts.GetActiveSession(context "conflict-cycle-after-stale-session")
                        |> Async.StartAsPromise

                    let afterStaleSession =
                        expectOperationValue "session after stale resolution" afterStaleSessionResult
                        |> Option.defaultWith (fun () -> failwith "The stale request closed the live session.")

                    Vitest.expect(afterStaleSession.Handle).toEqual firstResolution.RefreshedHandle
                    Vitest.expect(afterStaleSession.Items.Length).toBe 1

                    let! secondResolutionResult =
                        conflicts.Resolve
                            {
                                Handle = firstResolution.RefreshedHandle
                                ExpectedWorkspaceVersion = afterFirstStatus.WorkspaceVersion
                                Path = secondItem.Path
                                Resolution = SupplyResolvedContent "supplied second\n"
                            }
                            (context "conflict-cycle-supplied-resolution")
                        |> Async.StartAsPromise

                    let secondResolution = expectOperationValue "supplied conflict resolution" secondResolutionResult
                    Vitest.expect(secondResolution.RemainingItems.Length).toBe 0

                    let! finalizeStatusResult =
                        workspace.Session.Core.GetStatus(context "conflict-cycle-finalize-status")
                        |> Async.StartAsPromise

                    let finalizeStatus = expectOperationValue "conflict finalize status" finalizeStatusResult
                    let! finalizeResult =
                        conflicts.Finalize
                            {
                                Handle = secondResolution.RefreshedHandle
                                ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                Message = Some "finalize versioned conflict session"
                            }
                            (context "conflict-cycle-finalize")
                        |> Async.StartAsPromise

                    expectOperationValue "finalize conflict session" finalizeResult |> ignore

                    let! closedResult =
                        conflicts.GetActiveSession(context "conflict-cycle-closed-session")
                        |> Async.StartAsPromise

                    Vitest.expect(expectOperationValue "closed conflict session" closedResult).toEqual None

                    let! closedStatusResult =
                        workspace.Session.Core.GetStatus(context "conflict-cycle-closed-status")
                        |> Async.StartAsPromise

                    let closedStatus = expectOperationValue "closed conflict status" closedStatusResult
                    let! closedReplayResult =
                        conflicts.Cancel
                            {
                                Handle = secondResolution.RefreshedHandle
                                ExpectedWorkspaceVersion = closedStatus.WorkspaceVersion
                            }
                            (context "conflict-cycle-closed-replay")
                        |> Async.StartAsPromise

                    let closedFailure =
                        match closedReplayResult with
                        | Succeeded _
                        | PartiallySucceeded _ -> failwith "A closed conflict handle unexpectedly succeeded."
                        | Failed failure -> failure

                    Vitest.expect(closedFailure.Category).toEqual FailureCategory.Concurrency
                    Vitest.expect(closedFailure.Code).toBe "precondition_failed"
                    Vitest.expect(closedFailure.RecoveryAction |> Option.map _.Code).toEqual (Some ConflictRecovery.RefreshConflictSession)

                    let! mergedBase = workspace.ReadFile "base.txt"
                    let! mergedSecond = workspace.ReadFile "second.txt"
                    Vitest.expect(mergedBase).toEqual (Some "target base\n")
                    Vitest.expect(mergedSecond).toEqual (Some "supplied second\n")

                    let! targetAfterResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (context "conflict-cycle-target-after")
                        |> Async.StartAsPromise

                    let targetAfter = targetAfterResult |> expectApi "conflict target after"
                    Vitest.expect(targetAfter.CommitId).toBe targetBefore.CommitId
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)

Vitest.describe (
    "lakeFS conflict staleness",
    fun () ->
        Vitest.test (
            "lakeFS conflict staleness rotates the workspace token for same-path content changes",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! (workspace, conflicts, summary) = createSingleFileConflict harness
                    let item = summary.Items[0]
                    do! workspace.WriteFile "base.txt" "workspace base changed first\n"

                    let! firstStatusResult =
                        workspace.Session.Core.GetStatus(context "conflict-staleness-first-status")
                        |> Async.StartAsPromise

                    let firstStatus = expectOperationValue "conflict staleness first status" firstStatusResult
                    let tracker = beginUploadRequestTracking ()

                    try
                        do! workspace.WriteFile "base.txt" "workspace base changed out of band\n"

                        let! secondStatusResult =
                            workspace.Session.Core.GetStatus(context "conflict-staleness-second-status")
                            |> Async.StartAsPromise

                        let secondStatus = expectOperationValue "conflict staleness second status" secondStatusResult
                        Vitest.expect(secondStatus.WorkspaceVersion).not.toBe firstStatus.WorkspaceVersion

                        let! resolutionResult =
                            conflicts.Resolve
                                {
                                    Handle = summary.Handle
                                    ExpectedWorkspaceVersion = firstStatus.WorkspaceVersion
                                    Path = item.Path
                                    Resolution = PickCandidate "workspace"
                                }
                                (context "conflict-staleness-stale-resolution")
                            |> Async.StartAsPromise

                        let resolutionFailure =
                            match resolutionResult with
                            | Succeeded _
                            | PartiallySucceeded _ ->
                                failwith "A stale content-sensitive conflict resolution unexpectedly succeeded."
                            | Failed failure -> failure

                        Vitest.expect(resolutionFailure.Category).toEqual FailureCategory.Concurrency
                        Vitest.expect(resolutionFailure.Code).toBe "precondition_failed"
                        Vitest.expect(resolutionFailure.RecoveryAction |> Option.map _.Code).toEqual (Some ConflictRecovery.RefreshConflictSession)
                        Vitest.expect(tracker?count () |> unbox<int>).toBe 0
                    finally
                        tracker?restore () |> ignore

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS conflict staleness rejects an outside symlink during finalize before upload",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()
                let mutable candidatePath = ""
                let mutable outsideDirectory = ""

                try
                    let! (workspace, conflicts, summary) = createSingleFileConflict harness
                    let item = summary.Items[0]

                    let! resolutionStatusResult =
                        workspace.Session.Core.GetStatus(context "conflict-staleness-resolution-status")
                        |> Async.StartAsPromise

                    let resolutionStatus = expectOperationValue "conflict staleness resolution status" resolutionStatusResult
                    let! resolutionResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = resolutionStatus.WorkspaceVersion
                                Path = item.Path
                                Resolution = PickCandidate "workspace"
                            }
                            (context "conflict-staleness-workspace-resolution")
                        |> Async.StartAsPromise

                    let resolution = expectOperationValue "conflict staleness workspace resolution" resolutionResult

                    let! finalizeStatusResult =
                        workspace.Session.Core.GetStatus(context "conflict-staleness-finalize-status")
                        |> Async.StartAsPromise

                    let finalizeStatus = expectOperationValue "conflict staleness finalize status" finalizeStatusResult
                    candidatePath <- join [| workspace.Binding.WorkspaceRoot; "base.txt" |]
                    outsideDirectory <- workspace.Binding.WorkspaceRoot + "-outside"
                    let outsideFile = join [| outsideDirectory; "base.txt" |]
                    let tracker = beginUploadRequestTracking ()

                    try
                        armFinalizePathMutation
                            workspace.Binding.WorkspaceRoot
                            (fun () -> promise {
                                RuntimeNodeFileSystem.mkdirSync outsideDirectory (RuntimeNodeFileSystem.MkdirOptions(recursive = true))
                                RuntimeNodeFileSystem.writeFileSync
                                    outsideFile
                                    "outside conflict content\n"
                                    RuntimeNodeFileSystem.TextEncoding.Utf8
                                let! _ =
                                    fsPromisesDynamic?rm
                                        (candidatePath, createObj [ "force" ==> true ])
                                    |> unbox<JS.Promise<obj>>
                                let! _ =
                                    fsPromisesDynamic?symlink
                                        (outsideDirectory, candidatePath, "junction")
                                    |> unbox<JS.Promise<obj>>
                                return ()
                            })

                        let! finalizeResult =
                            conflicts.Finalize
                                {
                                    Handle = resolution.RefreshedHandle
                                    ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                    Message = Some "finalize symlink conflict"
                                }
                                (context "conflict-staleness-symlink-finalize")
                            |> Async.StartAsPromise

                        let finalizeFailure =
                            match finalizeResult with
                            | Succeeded _
                            | PartiallySucceeded _ -> failwith "Symlinked conflict finalization unexpectedly succeeded."
                            | Failed failure -> failure

                        Vitest.expect(finalizeFailure.Code).toBe "symlink_not_supported"
                        Vitest.expect(finalizeFailure.AffectedPaths).toContain "base.txt"
                        Vitest.expect(tracker?count () |> unbox<int>).toBe 0
                    finally
                        tracker?restore () |> ignore

                    if RuntimeNodeFileSystem.existsSync candidatePath then
                        RuntimeNodeFileSystem.unlinkSync candidatePath

                    do! harness.Cleanup()
                    do! removeDirectoryAsync outsideDirectory
                with error ->
                    if candidatePath <> "" && RuntimeNodeFileSystem.existsSync candidatePath then
                        RuntimeNodeFileSystem.unlinkSync candidatePath

                    do! harness.Cleanup()
                    if outsideDirectory <> "" then
                        do! removeDirectoryAsync outsideDirectory
                    return raise error
            }
        )
)
