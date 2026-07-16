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
module LakeFsSynchronization = VersionControlService.LakeFs.LakeFsSynchronization
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
    let mutable publishShouldBreak = false
    let mutable failNextConnection = false

    let nextId () =
        counter <- counter + 1
        $"task13-{DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()}-{counter}"

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
                    if point = "publish-connect" && publishShouldBreak then
                        publishShouldBreak <- false
                        failNextConnection <- true

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

                            match LakeFsWorkspaceIndex.load root with
                            | LakeFsWorkspaceIndex.Loaded index ->
                                do!
                                    Async.AwaitPromise(
                                        advanceRef control.Location index.WorkspaceBranch mutations
                                    )
                            | LakeFsWorkspaceIndex.Missing
                            | LakeFsWorkspaceIndex.Corrupt _ ->
                                failwith "Expected a persisted workspace index for the destination race."
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

                    if
                        point = "selected-revision-upload-done"
                        && operationContext.Cancellation.IsCancellationRequested()
                    then
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

                repositories.Clear()
                tempRoots.Clear()
                controls.Clear()
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
    SwateSelectableSuite.register lakeFsHarness
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

let private expectOperationValue operation = function
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded(_, failure)
    | Failed failure -> failwith $"{operation} failed ({failure.Category}/{failure.Code}): {failure.Message}"

let private repositoryPath value =
    RepositoryPath.tryCreate value |> Result.defaultWith failwith

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
                        match LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
                        match LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
                        match LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
