module VersionControlService.Tests.LakeFsIntegrationTests

open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.LakeFs
open VersionControlService.Tests.LakeFsProviderContractTests
open Vitest

module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path

let private fsPromisesDynamic: obj = importAll "fs/promises"

let private expectValue operation = function
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded(_, failure)
    | Failed failure -> failwith $"{operation} failed ({failure.Category}/{failure.Code}): {failure.Message}"

let private expectFailure operation = function
    | Succeeded _ -> failwith $"{operation} unexpectedly succeeded."
    | PartiallySucceeded(_, failure)
    | Failed failure -> failure

let private repositoryPath value =
    RepositoryPath.tryCreate value |> Result.defaultWith failwith

Vitest.describe (
    "lakeFS integration",
    fun () ->
        Vitest.test (
            "lakeFS initialize preserves existing user bytes and reports ordinary changes",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! anchor = harness.CreateWorkspace()
                    let! workspaceRoot = harness.CreateLocalPath()
                    let collidingPath = NodePath.join [| workspaceRoot; "base.txt" |]
                    let unrelatedPath = NodePath.join [| workspaceRoot; "consumer.bin" |]
                    let collidingBytes = "consumer base\u0000bytes"
                    let unrelatedBytes = "unrelated\u0000bytes"

                    NodeFileSystem.writeFileSync collidingPath collidingBytes NodeFileSystem.TextEncoding.Utf8
                    NodeFileSystem.writeFileSync unrelatedPath unrelatedBytes NodeFileSystem.TextEncoding.Utf8

                    let! initialized =
                        harness.Factory.Initialize
                            {
                                TargetPath = workspaceRoot
                                Location = Some anchor.Binding.Location
                            }
                            (OperationContext.detached "integration-initialize-existing")
                        |> Async.StartAsPromise

                    let binding = expectValue "initialize existing workspace" initialized
                    let! opened =
                        harness.Factory.Open
                            binding
                            (OperationContext.detached "integration-open-existing")
                        |> Async.StartAsPromise

                    let session = expectValue "open initialized workspace" opened

                    Vitest.expect(
                        NodeFileSystem.readFileSync collidingPath NodeFileSystem.TextEncoding.Utf8
                    ).toBe collidingBytes

                    Vitest.expect(
                        NodeFileSystem.readFileSync unrelatedPath NodeFileSystem.TextEncoding.Utf8
                    ).toBe unrelatedBytes

                    let! statusResult =
                        session.Core.GetStatus(OperationContext.detached "integration-initialize-existing-status")
                        |> Async.StartAsPromise

                    let status = expectValue "initialized workspace status" statusResult

                    let changes =
                        status.Changes
                        |> Array.map (fun change -> RepositoryPath.value change.Path, change.Kind)
                        |> Map.ofArray

                    Vitest.expect(changes.Count).toBe 2
                    Vitest.expect(changes["base.txt"]).toEqual FileChangeKind.ModifiedChange
                    Vitest.expect(changes["consumer.bin"]).toEqual FileChangeKind.AddedChange

                    Vitest.expect(
                        NodeFileSystem.readdirSync workspaceRoot |> Array.sort
                    ).toEqual [| "base.txt"; "consumer.bin" |]

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS restore rejects colliding selected paths before workspace mutation",
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
                        | _ -> failwith "Expected a persisted index before the restore collision."

                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    for path, content in [|
                        "Case.txt", "upper restore content\n"
                        "case.txt", "lower restore content\n"
                    |] do
                        let! uploaded =
                            uploadTextObject
                                (connection ())
                                parsed.Repository
                                index.WorkspaceBranch
                                path
                                content
                                (OperationContext.detached "integration-restore-collision-upload")
                            |> Async.StartAsPromise

                        uploaded |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore

                    let! committed =
                        LakeFsApi.commit
                            (connection ())
                            parsed.Repository
                            index.WorkspaceBranch
                            "test: colliding restore objects"
                            (OperationContext.detached "integration-restore-collision-commit")
                        |> Async.StartAsPromise

                    let commit = committed |> Result.defaultWith (fun failure -> failwith failure.Message)

                    match
                        LakeFsWorkspaceIndex.save
                            (stateDirectoryForBinding workspace.Binding)
                            { index with WorkspaceRevision = Some commit.Id }
                    with
                    | Error message -> failwith message
                    | Ok _ -> ()

                    let! reopened =
                        harness.Factory.Open
                            workspace.Binding
                            (OperationContext.detached "integration-restore-collision-reopen")
                        |> Async.StartAsPromise

                    let session = expectValue "reopen for restore collision" reopened
                    let! statusResult =
                        session.Core.GetStatus
                            (OperationContext.detached "integration-restore-collision-status")
                        |> Async.StartAsPromise

                    let status = expectValue "restore collision status" statusResult
                    let! restoreResult =
                        session.Core.RestorePaths
                            {
                                Paths = [| repositoryPath "Case.txt"; repositoryPath "case.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "integration-restore-collision")
                        |> Async.StartAsPromise

                    let failure = expectFailure "restore collision" restoreResult
                    Vitest.expect(failure.Code).toBe "path_collision"
                    Vitest.expect(failure.StateChanged).toBe false
                    let! upper = workspace.ReadFile "Case.txt"
                    let! lower = workspace.ReadFile "case.txt"
                    Vitest.expect(upper).toEqual None
                    Vitest.expect(lower).toEqual None
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS selected revision rejects a parent junction swapped in after source classification",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()
                let mutable barrierRan = false
                let mutable linkedDirectory = ""
                let mutable backupDirectory = ""
                let mutable outsideDirectory = ""
                let mutable raceCleaned = false

                let cleanupRace () = promise {
                    if barrierRan && not raceCleaned then
                        let! _ =
                            fsPromisesDynamic?rm
                                (linkedDirectory, createObj [ "recursive" ==> true; "force" ==> true ])
                            |> unbox<JS.Promise<obj>>

                        NodeFileSystem.renameSync backupDirectory linkedDirectory

                    if outsideDirectory <> "" && not raceCleaned then
                        let! _ =
                            fsPromisesDynamic?rm
                                (outsideDirectory, createObj [ "recursive" ==> true; "force" ==> true ])
                            |> unbox<JS.Promise<obj>>

                        ()

                    raceCleaned <- true
                }

                try
                    let! workspace = harness.CreateWorkspace()
                    linkedDirectory <- NodePath.join [| workspace.Binding.WorkspaceRoot; "nested" |]
                    backupDirectory <- NodePath.join [| workspace.Binding.WorkspaceRoot; "nested-safe" |]
                    outsideDirectory <- workspace.Binding.WorkspaceRoot + "-outside"
                    NodeFileSystem.mkdirSync outsideDirectory (NodeFileSystem.MkdirOptions(recursive = true))
                    do! workspace.WriteFile "nested/selected.txt" "safe selected bytes\n"
                    NodeFileSystem.writeFileSync
                        (NodePath.join [| outsideDirectory; "selected.txt" |])
                        "outside secret must not upload\n"
                        NodeFileSystem.TextEncoding.Utf8

                    let hooks: LakeFsWorkspaceSession.LakeFsSessionHooks = {
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "selected-revision-sources-classified" then
                                    barrierRan <- true
                                    NodeFileSystem.renameSync linkedDirectory backupDirectory

                                    do!
                                        fsPromisesDynamic?symlink
                                            (outsideDirectory, linkedDirectory, "junction")
                                        |> unbox<JS.Promise<obj>>
                                        |> Async.AwaitPromise
                                        |> Async.Ignore
                            })
                    }

                    let factory =
                        LakeFsWorkspaceSession.createFactoryWithHooks
                            lakeFsProviderOptions
                            hooks
                            (LakeFsCredentials.fixedConnection (connection ()))

                    let! opened =
                        factory.Open
                            workspace.Binding
                            (OperationContext.detached "integration-source-swap-open")
                        |> Async.StartAsPromise

                    let session = expectValue "open source-swap session" opened
                    let beforeIndex =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded value -> value
                        | _ -> failwith "Expected an index before the selected-source race."

                    let! statusResult =
                        session.Core.GetStatus(OperationContext.detached "integration-source-swap-status")
                        |> Async.StartAsPromise

                    let status = expectValue "source-swap status" statusResult
                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: reject source swap"
                                Paths = [| repositoryPath "nested/selected.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "integration-source-swap-revision")
                        |> Async.StartAsPromise

                    do! cleanupRace ()
                    Vitest.expect(barrierRan).toBe true
                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let! branchResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            beforeIndex.WorkspaceBranch
                            (OperationContext.detached "integration-source-swap-branch")
                        |> Async.StartAsPromise

                    let branch = branchResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(Some branch.CommitId).toEqual beforeIndex.WorkspaceRevision

                    match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                    | LakeFsWorkspaceIndex.Loaded afterIndex ->
                        Vitest.expect(afterIndex.WorkspaceRevision).toEqual beforeIndex.WorkspaceRevision
                    | _ -> failwith "Expected the selected-source race to preserve the index."

                    let failure = expectFailure "source-swap revision" revisionResult
                    Vitest.expect(failure.Code).toBe "symlink_not_supported"
                    Vitest.expect(failure.StateChanged).toBe false
                    Vitest.expect(failure.AffectedPaths).toContain "nested/selected.txt"
                    Vitest.expect(failure.Message.Contains "outside secret").toBe false
                    do! harness.Cleanup()
                with error ->
                    do! cleanupRace ()
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS selected revision rejects a selected file replaced after source classification",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()
                let mutable barrierRan = false
                let mutable selectedPath = ""
                let mutable backupPath = ""
                let mutable raceCleaned = false

                let cleanupRace () =
                    if barrierRan && not raceCleaned then
                        if NodeFileSystem.existsSync selectedPath then
                            NodeFileSystem.unlinkSync selectedPath

                        NodeFileSystem.renameSync backupPath selectedPath

                    raceCleaned <- true

                try
                    let! workspace = harness.CreateWorkspace()
                    selectedPath <- NodePath.join [| workspace.Binding.WorkspaceRoot; "selected.txt" |]
                    backupPath <- NodePath.join [| workspace.Binding.WorkspaceRoot; "selected-original.txt" |]
                    do! workspace.WriteFile "selected.txt" "classified selected bytes\n"

                    let hooks: LakeFsWorkspaceSession.LakeFsSessionHooks = {
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "selected-revision-sources-classified" then
                                    barrierRan <- true
                                    NodeFileSystem.renameSync selectedPath backupPath

                                    NodeFileSystem.writeFileSync
                                        selectedPath
                                        "replacement selected bytes\n"
                                        NodeFileSystem.TextEncoding.Utf8
                            })
                    }

                    let factory =
                        LakeFsWorkspaceSession.createFactoryWithHooks
                            lakeFsProviderOptions
                            hooks
                            (LakeFsCredentials.fixedConnection (connection ()))

                    let! opened =
                        factory.Open
                            workspace.Binding
                            (OperationContext.detached "integration-source-replace-open")
                        |> Async.StartAsPromise

                    let session = expectValue "open source-replace session" opened
                    let beforeIndex =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded value -> value
                        | _ -> failwith "Expected an index before the selected-source replacement race."

                    let! statusResult =
                        session.Core.GetStatus(OperationContext.detached "integration-source-replace-status")
                        |> Async.StartAsPromise

                    let status = expectValue "source-replace status" statusResult
                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: reject source replacement"
                                Paths = [| repositoryPath "selected.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "integration-source-replace-revision")
                        |> Async.StartAsPromise

                    cleanupRace ()
                    Vitest.expect(barrierRan).toBe true
                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let! branchResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            beforeIndex.WorkspaceBranch
                            (OperationContext.detached "integration-source-replace-branch")
                        |> Async.StartAsPromise

                    let branch = branchResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(Some branch.CommitId).toEqual beforeIndex.WorkspaceRevision

                    match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                    | LakeFsWorkspaceIndex.Loaded afterIndex ->
                        Vitest.expect(afterIndex.WorkspaceRevision).toEqual beforeIndex.WorkspaceRevision
                    | _ -> failwith "Expected the selected-source replacement race to preserve the index."

                    let failure = expectFailure "source-replace revision" revisionResult
                    Vitest.expect(failure.Code).toBe "workspace_path_changed"
                    Vitest.expect(failure.StateChanged).toBe false
                    Vitest.expect(failure.AffectedPaths).toContain "selected.txt"
                    do! harness.Cleanup()
                with error ->
                    cleanupRace ()
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS creates a unique owned server-visible workspace branch",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! first = harness.CreateWorkspace()
                    let! second = harness.CreateLinkedWorkspace first

                    let firstIndex = LakeFsWorkspaceIndex.load (stateDirectoryForBinding first.Binding)
                    let secondIndex = LakeFsWorkspaceIndex.load (stateDirectoryForBinding second.Binding)

                    let firstWorkspaceIndex =
                        match firstIndex with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected the first workspace index to be persisted."

                    let secondWorkspaceIndex =
                        match secondIndex with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected the second workspace index to be persisted."

                    Vitest.expect(firstWorkspaceIndex.WorkspaceBranch).not.toBe (secondWorkspaceIndex.WorkspaceBranch)
                    Vitest.expect(firstWorkspaceIndex.OwnershipToken.Length).toBeGreaterThan (20)
                    Vitest.expect(secondWorkspaceIndex.OwnershipToken.Length).toBeGreaterThan (20)

                    let firstLeaf =
                        first.Binding.WorkspaceRoot.Replace('\\', '/').TrimEnd('/').Split('/')
                        |> Array.last
                        |> _.ToLowerInvariant()

                    Vitest.expect(firstWorkspaceIndex.WorkspaceBranch).toContain (firstLeaf)

                    let! refsResult =
                        first.Session.Core.ListRefs(OperationContext.detached "integration-list-refs")
                        |> Async.StartAsPromise

                    let logicalRefs = expectValue "list refs" refsResult

                    Vitest.expect(
                        logicalRefs
                        |> Array.exists (fun reference ->
                            reference.Name = firstWorkspaceIndex.WorkspaceBranch
                            || reference.Name = secondWorkspaceIndex.WorkspaceBranch)
                    ).toBe (false)

                    let parsedLocation =
                        LakeFsTypes.LakeFsLocation.tryParse first.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let! branches =
                        LakeFsApi.listBranches
                            (connection ())
                            parsedLocation.Repository
                            (OperationContext.detached "integration-list-server-branches")
                        |> Async.StartAsPromise

                    let serverBranches = branches |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(serverBranches |> Array.map _.Id).toContain (firstWorkspaceIndex.WorkspaceBranch)
                    Vitest.expect(serverBranches |> Array.map _.Id).toContain (secondWorkspaceIndex.WorkspaceBranch)
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS selected revision reports and recovers interrupted remote state",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! workspace = harness.CreateWorkspace()
                    do! workspace.WriteFile "interrupted.txt" "interrupted content\n"

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "interruption-status")
                        |> Async.StartAsPromise

                    let status = expectValue "interruption status" statusResult
                    let before =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before interruption."

                    let cancelAfterUpload = OperationCancellation.Source()
                    let interruptContext =
                        OperationContext.create
                            "interrupt-after-upload"
                            cancelAfterUpload.Cancellation
                            (fun progress ->
                                if progress.PhaseCode = "selected-upload-complete" then
                                    cancelAfterUpload.Cancel())

                    let! interruptedResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "interrupted selected revision"
                                Paths = [| repositoryPath "interrupted.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            interruptContext
                        |> Async.StartAsPromise

                    let interrupted = expectFailure "interrupted selected revision" interruptedResult
                    Vitest.expect(interrupted.Category).toEqual (FailureCategory.Canceled)
                    Vitest.expect(interrupted.StateChanged).toBe false

                    let afterCleanup =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index after interruption cleanup."

                    Vitest.expect(afterCleanup.WorkspaceRevision).toEqual (before.WorkspaceRevision)
                    Vitest.expect(afterCleanup.Generation).toBe (before.Generation)

                    let parsed = LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation |> Result.defaultWith failwith
                    let! cleanedBranchResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            before.WorkspaceBranch
                            (OperationContext.detached "interruption-cleaned-head")
                        |> Async.StartAsPromise

                    let cleanedBranch = cleanedBranchResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(Some cleanedBranch.CommitId).toEqual (before.WorkspaceRevision)

                    let! retryStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "interruption-retry-status")
                        |> Async.StartAsPromise

                    let retryStatus = expectValue "retry status" retryStatusResult
                    let! retryResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "retry interrupted selected revision"
                                Paths = [| repositoryPath "interrupted.txt" |]
                                ExpectedWorkspaceVersion = retryStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "interruption-retry")
                        |> Async.StartAsPromise

                    expectValue "interruption retry" retryResult |> ignore

                    do! workspace.WriteFile "moved-during-interruption.txt" "selected content in moved branch\n"
                    let! movedStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "moved-interruption-status")
                        |> Async.StartAsPromise

                    let movedStatus = expectValue "moved interruption status" movedStatusResult
                    let beforeMoved =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before the moved interruption."

                    do!
                        harness.ArmDestinationRace workspace [|
                            { Path = "concurrent-owner.txt"; Content = Some "concurrent content\n" }
                        |]

                    let cancelMoved = OperationCancellation.Source()
                    let movedContext =
                        OperationContext.create
                            "interrupt-after-upload-with-move"
                            cancelMoved.Cancellation
                            (fun progress ->
                                if progress.PhaseCode = "selected-upload-complete" then
                                    cancelMoved.Cancel())

                    let! movedResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "interrupted while workspace branch moves"
                                Paths = [| repositoryPath "moved-during-interruption.txt" |]
                                ExpectedWorkspaceVersion = movedStatus.WorkspaceVersion
                            }
                            movedContext
                        |> Async.StartAsPromise

                    let movedFailure = expectFailure "moved interrupted revision" movedResult
                    Vitest.expect(movedFailure.Category).toEqual (FailureCategory.Canceled)
                    Vitest.expect(movedFailure.StateChanged).toBe true
                    Vitest.expect(movedFailure.RecoveryAction |> Option.map _.Code).toEqual (Some "review_workspace_branch")

                    let! movedBranchResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            beforeMoved.WorkspaceBranch
                            (OperationContext.detached "interruption-moved-head")
                        |> Async.StartAsPromise

                    let movedBranch = movedBranchResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(Some movedBranch.CommitId).not.toEqual (beforeMoved.WorkspaceRevision)

                    let afterMoved =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index after the moved interruption."

                    Vitest.expect(afterMoved.WorkspaceRevision).toEqual (beforeMoved.WorkspaceRevision)
                    Vitest.expect(afterMoved.Generation).toBe (beforeMoved.Generation)
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS update verifies destination head and merge parentage after a race",
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

                    do!
                        harness.AdvanceTarget workspace [|
                            { Path = "target-before-update.txt"; Content = Some "target content\n" }
                        |]

                    let before =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before the update race."

                    let parsed = LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation |> Result.defaultWith failwith
                    let! targetResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (OperationContext.detached "update-race-target")
                        |> Async.StartAsPromise

                    let target = targetResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "update-race-status")
                        |> Async.StartAsPromise

                    let status = expectValue "update race status" statusResult

                    do!
                        harness.ArmDestinationRace workspace [|
                            { Path = "workspace-racer.txt"; Content = Some "workspace racer content\n" }
                        |]

                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (OperationContext.detached "update-race")
                        |> Async.StartAsPromise

                    let outcome, failure =
                        match updateResult with
                        | Succeeded _ -> failwith "A raced update must not report clean success."
                        | Failed failure -> None, failure
                        | PartiallySucceeded(outcome, failure) -> Some outcome, failure

                    Vitest.expect(failure.Category).toEqual (FailureCategory.Concurrency)
                    let evidence = failure.RevisionEvidence |> Map.ofArray
                    Vitest.expect(evidence.ContainsKey "expected_workspace").toBe true
                    Vitest.expect(evidence.ContainsKey "observed_workspace").toBe true
                    Vitest.expect(evidence["expected_workspace"] |> RevisionId.value).toBe (before.WorkspaceRevision.Value)

                    let! workspaceHeadResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            before.WorkspaceBranch
                            (OperationContext.detached "update-race-workspace-head")
                        |> Async.StartAsPromise

                    let workspaceHead = workspaceHeadResult |> Result.defaultWith (fun error -> failwith error.Message)
                    Vitest.expect(workspaceHead.CommitId).not.toBe (before.WorkspaceRevision.Value)

                    let! mergeCommitResult =
                        LakeFsApi.getCommit
                            (connection ())
                            parsed.Repository
                            workspaceHead.CommitId
                            (OperationContext.detached "update-race-merge-commit")
                        |> Async.StartAsPromise

                    let mergeCommit = mergeCommitResult |> Result.defaultWith (fun error -> failwith error.Message)
                    Vitest.expect(mergeCommit.Parents).toContain (target.CommitId)

                    let observedWorkspace = evidence["observed_workspace"] |> RevisionId.value
                    Vitest.expect(mergeCommit.Parents).toContain observedWorkspace

                    match outcome with
                    | Some partial -> Vitest.expect(partial.ResultingRevision |> Option.map RevisionId.value).toEqual (Some workspaceHead.CommitId)
                    | None -> failwith "The merge changed the workspace branch and must report partial success."

                    let after =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index after the update race."

                    Vitest.expect(after.Generation).toBe (before.Generation)
                    Vitest.expect(after.WorkspaceRevision).toEqual (before.WorkspaceRevision)

                    let! targetAfterResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (OperationContext.detached "update-race-target-after")
                        |> Async.StartAsPromise

                    let targetAfter = targetAfterResult |> Result.defaultWith (fun error -> failwith error.Message)
                    Vitest.expect(targetAfter.CommitId).toBe (target.CommitId)
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS publish verifies target head and merge parentage after a race",
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

                    do! workspace.WriteFile "local-before-publish.txt" "local content\n"

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "publish-race-status")
                        |> Async.StartAsPromise

                    let status = expectValue "publish race status" statusResult
                    let! revisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "local revision before publish race"
                                Paths = [| repositoryPath "local-before-publish.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "publish-race-local-revision")
                        |> Async.StartAsPromise

                    expectValue "publish race local revision" revisionResult |> ignore

                    let before =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before the publish race."

                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let! targetResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (OperationContext.detached "publish-race-target")
                        |> Async.StartAsPromise

                    let target = targetResult |> Result.defaultWith (fun failure -> failwith failure.Message)

                    let! publishStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "publish-race-publish-status")
                        |> Async.StartAsPromise

                    let publishStatus = expectValue "publish race publish status" publishStatusResult

                    do!
                        harness.ArmDestinationRace workspace [|
                            { Path = "target-racer.txt"; Content = Some "target racer content\n" }
                        |]

                    let! publishResult =
                        synchronization.Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = Some(RevisionId.tryCreate target.CommitId |> Result.defaultWith failwith)
                            }
                            (OperationContext.detached "publish-race")
                        |> Async.StartAsPromise

                    let outcome, failure =
                        match publishResult with
                        | Succeeded _ -> failwith "A raced publish must not report clean success."
                        | Failed failure -> None, failure
                        | PartiallySucceeded(outcome, failure) -> Some outcome, failure

                    Vitest.expect(failure.Category).toEqual (FailureCategory.Concurrency)
                    let evidence = failure.RevisionEvidence |> Map.ofArray
                    Vitest.expect(evidence.ContainsKey "expected_target").toBe true
                    Vitest.expect(evidence.ContainsKey "observed_target").toBe true
                    Vitest.expect(evidence["expected_target"] |> RevisionId.value).toBe target.CommitId

                    let! targetAfterResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (OperationContext.detached "publish-race-target-after")
                        |> Async.StartAsPromise

                    let targetAfter = targetAfterResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    let! mergeCommitResult =
                        LakeFsApi.getCommit
                            (connection ())
                            parsed.Repository
                            targetAfter.CommitId
                            (OperationContext.detached "publish-race-merge-commit")
                        |> Async.StartAsPromise

                    let mergeCommit = mergeCommitResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    let observedTarget = evidence["observed_target"] |> RevisionId.value
                    let expectedWorkspace = before.WorkspaceRevision |> Option.defaultWith (fun () -> failwith "Expected a workspace revision.")
                    Vitest.expect(mergeCommit.Parents).toContain observedTarget
                    Vitest.expect(mergeCommit.Parents).toContain expectedWorkspace

                    match outcome with
                    | Some partial ->
                        Vitest.expect(partial.ResultingRevision |> Option.map RevisionId.value).toEqual (Some targetAfter.CommitId)
                    | None -> failwith "The merge changed the target and must report partial success."

                    let after =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index after the publish race."

                    Vitest.expect(after.Generation).toBe before.Generation
                    Vitest.expect(after.BaseRevision).toEqual before.BaseRevision
                    Vitest.expect(after.WorkspaceRevision).toEqual before.WorkspaceRevision

                    let! workspaceHeadResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            before.WorkspaceBranch
                            (OperationContext.detached "publish-race-workspace-after")
                        |> Async.StartAsPromise

                    let workspaceHead = workspaceHeadResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(workspaceHead.CommitId).toBe expectedWorkspace
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS conflict finalize verifies destination head and parentage after a race",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! workspace = harness.CreateWorkspace()

                    do!
                        harness.AdvanceTarget workspace [|
                            { Path = "base.txt"; Content = Some "target conflict content\n" }
                        |]

                    do! workspace.WriteFile "base.txt" "workspace conflict content\n"
                    let! saveStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-race-save-status")
                        |> Async.StartAsPromise

                    let saveStatus = expectValue "finalize race save status" saveStatusResult
                    let! saveResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "workspace conflict before finalize race"
                                Paths = [| repositoryPath "base.txt" |]
                                ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "finalize-race-save")
                        |> Async.StartAsPromise

                    expectValue "finalize race save" saveResult |> ignore

                    let! updateStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-race-update-status")
                        |> Async.StartAsPromise

                    let updateStatus = expectValue "finalize race update status" updateStatusResult
                    let synchronization =
                        workspace.Session.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS synchronization services.")

                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                            (OperationContext.detached "finalize-race-update")
                        |> Async.StartAsPromise

                    let updateFailure = expectFailure "finalize race conflicting update" updateResult
                    Vitest.expect(updateFailure.Category).toEqual FailureCategory.Conflict

                    let conflicts =
                        workspace.Session.ConflictResolution
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS conflict-resolution services.")

                    let! sessionResult =
                        conflicts.GetActiveSession(OperationContext.detached "finalize-race-session")
                        |> Async.StartAsPromise

                    let summary =
                        expectValue "finalize race session" sessionResult
                        |> Option.defaultWith (fun () -> failwith "Expected an active conflict session.")

                    let! resolutionStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-race-resolution-status")
                        |> Async.StartAsPromise

                    let resolutionStatus = expectValue "finalize race resolution status" resolutionStatusResult
                    let! resolutionResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = resolutionStatus.WorkspaceVersion
                                Path = repositoryPath "base.txt"
                                Resolution = PickCandidate "target"
                            }
                            (OperationContext.detached "finalize-race-resolution")
                        |> Async.StartAsPromise

                    let resolution = expectValue "finalize race resolution" resolutionResult
                    let before =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before the finalize race."

                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let! targetResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (OperationContext.detached "finalize-race-target")
                        |> Async.StartAsPromise

                    let target = targetResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    let! finalizeStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-race-finalize-status")
                        |> Async.StartAsPromise

                    let finalizeStatus = expectValue "finalize race finalize status" finalizeStatusResult

                    do!
                        harness.ArmDestinationRace workspace [|
                            { Path = "finalize-racer.txt"; Content = Some "finalize racer content\n" }
                        |]

                    let! finalizeResult =
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                Message = Some "finalize during destination race"
                            }
                            (OperationContext.detached "finalize-race")
                        |> Async.StartAsPromise

                    let partialRevision, failure =
                        match finalizeResult with
                        | Succeeded _ -> failwith "A raced conflict finalize must not report clean success."
                        | Failed failure -> None, failure
                        | PartiallySucceeded(outcome, failure) -> outcome.Value, failure

                    Vitest.expect(failure.Category).toEqual FailureCategory.Concurrency
                    let evidence = failure.RevisionEvidence |> Map.ofArray
                    Vitest.expect(evidence.ContainsKey "expected_destination").toBe true
                    Vitest.expect(evidence.ContainsKey "observed_destination").toBe true
                    Vitest.expect(evidence.ContainsKey "observed_result").toBe true

                    let expectedWorkspace =
                        before.WorkspaceRevision |> Option.defaultWith (fun () -> failwith "Expected a workspace revision.")

                    Vitest.expect(evidence["expected_destination"] |> RevisionId.value).toBe expectedWorkspace
                    let observedDestination = evidence["observed_destination"] |> RevisionId.value
                    let observedResult = evidence["observed_result"] |> RevisionId.value
                    Vitest.expect(partialRevision |> Option.map RevisionId.value).toEqual (Some observedResult)

                    let! workspaceHeadResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            before.WorkspaceBranch
                            (OperationContext.detached "finalize-race-workspace-head")
                        |> Async.StartAsPromise

                    let workspaceHead = workspaceHeadResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(workspaceHead.CommitId).toBe observedResult

                    let! resolutionCommitResult =
                        LakeFsApi.getCommit
                            (connection ())
                            parsed.Repository
                            observedResult
                            (OperationContext.detached "finalize-race-resolution-commit")
                        |> Async.StartAsPromise

                    let resolutionCommit = resolutionCommitResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(resolutionCommit.Parents.Length).toBe 1
                    let mergeRevision = resolutionCommit.Parents[0]

                    let! mergeCommitResult =
                        LakeFsApi.getCommit
                            (connection ())
                            parsed.Repository
                            mergeRevision
                            (OperationContext.detached "finalize-race-merge-commit")
                        |> Async.StartAsPromise

                    let mergeCommit = mergeCommitResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(mergeCommit.Parents).toContain target.CommitId
                    Vitest.expect(mergeCommit.Parents).toContain observedDestination

                    let afterPartial =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index after the raced finalize."

                    Vitest.expect(afterPartial.Generation).toBe before.Generation
                    Vitest.expect(afterPartial.WorkspaceRevision).toEqual before.WorkspaceRevision

                    let! liveSessionResult =
                        conflicts.GetActiveSession(OperationContext.detached "finalize-race-live-session")
                        |> Async.StartAsPromise

                    let liveSession =
                        expectValue "live session after raced finalize" liveSessionResult
                        |> Option.defaultWith (fun () -> failwith "The raced finalize left no recoverable session.")

                    Vitest.expect(liveSession.Handle.Version).not.toBe resolution.RefreshedHandle.Version
                    let! retryStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-race-retry-status")
                        |> Async.StartAsPromise

                    let retryStatus = expectValue "finalize race retry status" retryStatusResult
                    let! retryResult =
                        conflicts.Finalize
                            {
                                Handle = liveSession.Handle
                                ExpectedWorkspaceVersion = retryStatus.WorkspaceVersion
                                Message = Some "confirm raced conflict finalize"
                            }
                            (OperationContext.detached "finalize-race-retry")
                        |> Async.StartAsPromise

                    let retryRevision = expectValue "finalize race retry" retryResult
                    Vitest.expect(retryRevision |> Option.map RevisionId.value).toEqual (Some observedResult)

                    let! closedSessionResult =
                        conflicts.GetActiveSession(OperationContext.detached "finalize-race-closed-session")
                        |> Async.StartAsPromise

                    Vitest.expect(expectValue "closed raced session" closedSessionResult).toEqual None

                    let! targetAfterResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (OperationContext.detached "finalize-race-target-after")
                        |> Async.StartAsPromise

                    let targetAfter = targetAfterResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(targetAfter.CommitId).toBe target.CommitId
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS cleanup requires ownership and unchanged expected head",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()
                let credentials = LakeFsCredentials.fixedConnection (connection ())

                try
                    let! workspace = harness.CreateWorkspace()
                    let index =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before cleanup."

                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    // Closing a reusable session releases in-process resources but
                    // does not discard the persisted workspace branch.
                    do! workspace.Session.Close() |> Async.StartAsPromise
                    let! afterCloseResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            index.WorkspaceBranch
                            (OperationContext.detached "cleanup-after-close")
                        |> Async.StartAsPromise

                    afterCloseResult |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore

                    let! foreignResult =
                        LakeFsWorkspaceSession.cleanupOwnedWorkspaceBranch
                            lakeFsProviderOptions
                            LakeFsWorkspaceSession.LakeFsSessionHooks.none
                            credentials
                            workspace.Binding
                            "foreign-ownership-token"
                            index.WorkspaceRevision
                            (OperationContext.detached "cleanup-foreign-owner")
                        |> Async.StartAsPromise

                    let foreignFailure = expectFailure "foreign cleanup" foreignResult
                    Vitest.expect(foreignFailure.Category).toEqual FailureCategory.Concurrency
                    Vitest.expect(foreignFailure.Code).toBe "precondition_failed"

                    let! stillOwnedResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            index.WorkspaceBranch
                            (OperationContext.detached "cleanup-still-owned")
                        |> Async.StartAsPromise

                    stillOwnedResult |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore

                    let raceHooks: LakeFsWorkspaceSession.LakeFsSessionHooks = {
                        Barrier =
                            Some(fun _ point context -> async {
                                if point = "cleanup-precheck-done" then
                                    let! uploaded =
                                        uploadTextObject
                                            (connection ())
                                            parsed.Repository
                                            index.WorkspaceBranch
                                            "cleanup-racer.txt"
                                            "cleanup racer content\n"
                                            context

                                    uploaded |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore

                                    let! committed =
                                        LakeFsApi.commit
                                            (connection ())
                                            parsed.Repository
                                            index.WorkspaceBranch
                                            "advance cleanup destination"
                                            context

                                    committed |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore
                            })
                    }

                    let! racedResult =
                        LakeFsWorkspaceSession.cleanupOwnedWorkspaceBranch
                            lakeFsProviderOptions
                            raceHooks
                            credentials
                            workspace.Binding
                            index.OwnershipToken
                            index.WorkspaceRevision
                            (OperationContext.detached "cleanup-raced-head")
                        |> Async.StartAsPromise

                    let racedFailure = expectFailure "raced cleanup" racedResult
                    Vitest.expect(racedFailure.Category).toEqual FailureCategory.Concurrency
                    let raceEvidence = racedFailure.RevisionEvidence |> Map.ofArray
                    Vitest.expect(raceEvidence.ContainsKey "expected_head").toBe true
                    Vitest.expect(raceEvidence.ContainsKey "observed_head").toBe true

                    let! advancedResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            index.WorkspaceBranch
                            (OperationContext.detached "cleanup-advanced-head")
                        |> Async.StartAsPromise

                    let advanced = advancedResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(Some advanced.CommitId).not.toEqual index.WorkspaceRevision

                    let! cleanedResult =
                        LakeFsWorkspaceSession.cleanupOwnedWorkspaceBranch
                            lakeFsProviderOptions
                            LakeFsWorkspaceSession.LakeFsSessionHooks.none
                            credentials
                            workspace.Binding
                            index.OwnershipToken
                            (Some advanced.CommitId)
                            (OperationContext.detached "cleanup-owned-head")
                        |> Async.StartAsPromise

                    expectValue "owned cleanup" cleanedResult |> ignore

                    let! missingResult =
                        LakeFsWorkspaceSession.cleanupOwnedWorkspaceBranch
                            lakeFsProviderOptions
                            LakeFsWorkspaceSession.LakeFsSessionHooks.none
                            credentials
                            workspace.Binding
                            index.OwnershipToken
                            (Some advanced.CommitId)
                            (OperationContext.detached "cleanup-abandoned-branch")
                        |> Async.StartAsPromise

                    match missingResult with
                    | Succeeded outcome ->
                        match outcome.Effect with
                        | NoOp _ -> ()
                        | Performed -> failwith "Cleaning an already-absent branch must be a NoOp."
                    | PartiallySucceeded(_, failure)
                    | Failed failure -> failwith $"Absent-branch cleanup failed: {failure.Message}"

                    let! permissionWorkspace = harness.CreateWorkspace()
                    let permissionIndex =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding permissionWorkspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded value -> value
                        | _ -> failwith "Expected an index for permission cleanup."

                    let permissionLocation =
                        LakeFsTypes.LakeFsLocation.tryParse permissionWorkspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let unauthorized =
                        LakeFsCredentials.fixedConnection {
                            connection () with
                                SecretAccessKey = "invalid-cleanup-secret"
                        }

                    let! permissionResult =
                        LakeFsWorkspaceSession.cleanupOwnedWorkspaceBranch
                            lakeFsProviderOptions
                            LakeFsWorkspaceSession.LakeFsSessionHooks.none
                            unauthorized
                            permissionWorkspace.Binding
                            permissionIndex.OwnershipToken
                            permissionIndex.WorkspaceRevision
                            (OperationContext.detached "cleanup-permission-denied")
                        |> Async.StartAsPromise

                    let permissionFailure = expectFailure "permission cleanup" permissionResult
                    Vitest.expect(permissionFailure.Category = FailureCategory.Authentication
                                  || permissionFailure.Category = FailureCategory.Authorization).toBe true

                    let! permissionBranchResult =
                        LakeFsApi.getBranch
                            (connection ())
                            permissionLocation.Repository
                            permissionIndex.WorkspaceBranch
                            (OperationContext.detached "cleanup-permission-branch")
                        |> Async.StartAsPromise

                    permissionBranchResult |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)
