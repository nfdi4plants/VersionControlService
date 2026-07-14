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
module LakeFsWorkspaceIndex = VersionControlService.LakeFs.LakeFsWorkspaceIndex
module LakeFsWorkspaceSession = VersionControlService.LakeFs.LakeFsWorkspaceSession

[<Emit("process.env[$0] ?? null")>]
let private getEnvironmentVariable (_name: string) : string = jsNative

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

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
        |> Option.defaultValue "task13-access"
    SecretAccessKey =
        getEnvironmentVariable "LAKEFS_INTEGRATION_SECRET_ACCESS_KEY"
        |> Option.ofObj
        |> Option.defaultValue "task13-secret"
}

let private createTempDirectoryAsync () : JS.Promise<string> =
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-lakefs-harness-" |]
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

let private expectApi operation = function
    | Ok value -> value
    | Error failure -> failwith $"{operation} failed ({failure.Category}/{failure.Code}): {failure.Message}"

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
    mutable RaceMutations: TargetMutation[] option
    mutable SlowTransfer: bool
}

let createLakeFsHarness () : ProviderTestHarness =
    let tempRoots = ResizeArray<string>()
    let repositories = ResizeArray<string>()
    let controls = Collections.Generic.Dictionary<string, WorkspaceControl>()
    let mutable counter = 0
    let mutable publishBroken = false

    let nextId () =
        counter <- counter + 1
        $"task13-{DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()}-{counter}"

    let credentials: LakeFsCredentials.LakeFsCredentialStrategy = {
        ResolveConnection =
            fun profileId -> async {
                if publishBroken || profileId = Some "unauthorized" then
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

    let bindingFor (root: string) (location: RepositoryLocation) : WorkspaceBinding = {
        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
        ProviderId = location.ProviderId
        WorkspaceRoot = root
        ProviderStateRef = None
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
                    LakeFsApi.uploadObject
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
                    if point = "publish-precheck-done" || point = "update-precheck-done" || point = "finalize-precheck-done" then
                        match control.RaceMutations with
                        | Some mutations ->
                            control.RaceMutations <- None
                            do! Async.AwaitPromise(advanceTarget control.Location mutations)
                        | None -> ()

                    if point = "selected-revision-commit-done" then
                        match control.RaceMutations with
                        | Some mutations ->
                            control.RaceMutations <- None

                            match LakeFsWorkspaceIndex.load root with
                            | LakeFsWorkspaceIndex.Loaded index ->
                                do!
                                    Async.AwaitPromise(
                                        advanceRef control.Location index.WorkspaceBranch mutations
                                    )
                            | LakeFsWorkspaceIndex.Missing
                            | LakeFsWorkspaceIndex.Corrupt _ ->
                                failwith "Expected a persisted workspace index for the selected-revision race."
                        | None -> ()

                    if point = "transfer-start" && control.SlowTransfer then
                        control.SlowTransfer <- false
                        let mutable step = 0

                        while step < 200 && not (operationContext.Cancellation.IsCancellationRequested()) do
                            do! Async.Sleep 2
                            step <- step + 1

                            operationContext.ReportProgress {
                                PhaseCode = "transfer"
                                Item = None
                                Completed = Some step
                                Total = Some 200
                                DisplayMessage = Some "Transferring lakeFS objects"
                            }
                | false, _ -> ()
            })
    }

    let factory = LakeFsWorkspaceSession.createFactory hooks credentials

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
            RaceMutations = None
            SlowTransfer = false
        }

        return {
            Session = session
            Binding = binding
            WriteFile = fun path content -> writeUtf8FileAsync (join [| binding.WorkspaceRoot; path |]) content
            ReadFile = fun path -> tryReadUtf8FileAsync (join [| binding.WorkspaceRoot; path |])
            RemoveFile = fun path -> removeFileAsync (join [| binding.WorkspaceRoot; path |])
            LocalFileSystemAliasesCase = true
            LocalFileSystemAliasesNormalization = false
            LocalFileSystemWindowsRules = true
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
            LakeFsApi.uploadObject
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
        ExpectedServices = [ "synchronization" ]
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
                        LakeFsApi.uploadObject
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
                publishBroken <- true
                return ()
            }
        RestorePublish =
            fun _ -> promise {
                publishBroken <- false
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

                repositories.Clear()
                tempRoots.Clear()
                controls.Clear()
                return ()
            }
    }

let private lakeFsHarness = createLakeFsHarness ()

let private registrations = [|
    CoreProviderSuite.register lakeFsHarness
    SynchronizationProviderSuite.register lakeFsHarness
    ConflictProviderSuite.register lakeFsHarness
    ProvisioningProviderSuite.register lakeFsHarness
    OperationalProviderSuite.register lakeFsHarness
    ExtensionProviderSuites.register lakeFsHarness
    SwateSelectableSuite.register lakeFsHarness
|]

let private expectOperationValue operation = function
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded(_, failure)
    | Failed failure -> failwith $"{operation} failed ({failure.Category}/{failure.Code}): {failure.Message}"

let private repositoryPath value =
    RepositoryPath.tryCreate value |> Result.defaultWith failwith

Vitest.describe (
    "lakeFS selected revision cycles",
    fun () ->
        Vitest.test (
            "lakeFS selected revision preserves unrelated local changes and verifies commit head",
            TestOptions(timeout = 120000),
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
                        match VersionControlService.LakeFs.LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
                        match VersionControlService.LakeFs.LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
                        match VersionControlService.LakeFs.LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
