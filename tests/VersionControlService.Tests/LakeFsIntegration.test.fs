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

                    if workspace.LocalFileSystemAliasesCase then
                        let failure = expectFailure "restore collision" restoreResult
                        Vitest.expect(failure.Code).toBe "path_collision"
                        Vitest.expect(failure.StateChanged).toBe false
                        let! upper = workspace.ReadFile "Case.txt"
                        let! lower = workspace.ReadFile "case.txt"
                        Vitest.expect(upper).toEqual None
                        Vitest.expect(lower).toEqual None
                    else
                        // A case-sensitive filesystem keeps both names side by side, so there is
                        // nothing to reject and both files have to arrive with their own content.
                        expectValue "restore distinct-case paths" restoreResult |> ignore
                        let! upper = workspace.ReadFile "Case.txt"
                        let! lower = workspace.ReadFile "case.txt"
                        Vitest.expect(upper).toEqual (Some "upper restore content\n")
                        Vitest.expect(lower).toEqual (Some "lower restore content\n")
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS restore materialization reports exact partial recovery",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()
                let mutable appliedObjects = 0

                try
                    let! workspace = harness.CreateWorkspace()
                    do! workspace.WriteFile "000-restore-first.txt" "tracked first bytes\n"
                    do! workspace.WriteFile "zzz-restore-second.txt" "tracked second bytes\n"

                    let! revisionStatusResult =
                        workspace.Session.Core.GetStatus
                            (OperationContext.detached "restore-materialization-revision-status")
                        |> Async.StartAsPromise

                    let revisionStatus = expectValue "restore materialization revision status" revisionStatusResult
                    let! revisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: tracked restore objects"
                                Paths = [|
                                    repositoryPath "000-restore-first.txt"
                                    repositoryPath "zzz-restore-second.txt"
                                |]
                                ExpectedWorkspaceVersion = revisionStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "restore-materialization-revision")
                        |> Async.StartAsPromise

                    expectValue "restore materialization revision" revisionResult |> ignore
                    do! workspace.WriteFile "000-restore-first.txt" "dirty first bytes\n"
                    do! workspace.WriteFile "zzz-restore-second.txt" "dirty second bytes\n"

                    let hooks: LakeFsWorkspaceSession.LakeFsSessionHooks = {
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "materialization-apply-object" then
                                    appliedObjects <- appliedObjects + 1

                                    if appliedObjects = 1 then
                                        failwith "injected restore materialization failure"
                            })
                    }

                    let factory =
                        LakeFsWorkspaceSession.createFactoryWithHooks
                            lakeFsProviderOptions
                            hooks
                            (LakeFsCredentials.fixedConnection (connection ()))

                    let! reopened =
                        factory.Open
                            workspace.Binding
                            (OperationContext.detached "restore-materialization-open")
                        |> Async.StartAsPromise

                    let session = expectValue "restore materialization open" reopened
                    let stateDirectory = stateDirectoryForBinding workspace.Binding
                    let before =
                        match LakeFsWorkspaceIndex.load stateDirectory with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before interrupted restore."

                    let! listedResult =
                        LakeFsApi.listObjects
                            (connection ())
                            before.Repository
                            before.WorkspaceBranch
                            before.Prefix
                            (OperationContext.detached "restore-materialization-list")
                        |> Async.StartAsPromise

                    let expectedFirstStat =
                        listedResult
                        |> Result.defaultWith (fun failure -> failwith failure.Message)
                        |> Array.find (fun stat -> stat.Path = "000-restore-first.txt")

                    let! statusResult =
                        session.Core.GetStatus
                            (OperationContext.detached "restore-materialization-status")
                        |> Async.StartAsPromise

                    let status = expectValue "restore materialization status" statusResult
                    let! restored =
                        session.Core.RestorePaths
                            {
                                Paths = [|
                                    repositoryPath "000-restore-first.txt"
                                    repositoryPath "zzz-restore-second.txt"
                                |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "restore-materialization-restore")
                        |> Async.StartAsPromise

                    let outcome, failure =
                        match restored with
                        | PartiallySucceeded(outcome, failure) -> outcome, failure
                        | Failed failure ->
                            failwith $"Visible restore mutation was concealed as Failed: {failure.Code}"
                        | Succeeded _ -> failwith "Injected restore materialization failure unexpectedly succeeded."

                    Vitest.expect(appliedObjects).toBe 1
                    Vitest.expect(failure.StateChanged).toBe true
                    Vitest.expect(failure.AffectedPaths).toEqual [| "000-restore-first.txt" |]
                    Vitest.expect(outcome.AffectedPaths).toEqual [| "000-restore-first.txt" |]
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_materialization")
                    let! first = workspace.ReadFile "000-restore-first.txt"
                    let! second = workspace.ReadFile "zzz-restore-second.txt"
                    Vitest.expect(first).toEqual(Some "tracked first bytes\n")
                    Vitest.expect(second).toEqual(Some "dirty second bytes\n")

                    match LakeFsWorkspaceIndex.load stateDirectory with
                    | LakeFsWorkspaceIndex.Loaded after ->
                        let expectedFirst: LakeFsWorkspaceIndex.IndexEntry = {
                            Path = "000-restore-first.txt"
                            BaseChecksum = expectedFirstStat.Checksum
                            LocalHash =
                                LakeFsWorkspaceIndex.hashMetadata "tracked first bytes\n"
                            LocalSize = 20.0
                            LocalMtimeMs = expectedFirstStat.Mtime
                        }

                        let expectedEntries =
                            before.Entries
                            |> Array.map (fun entry ->
                                if entry.Path = expectedFirst.Path then expectedFirst else entry)

                        Vitest.expect(after.WorkspaceRevision).toEqual before.WorkspaceRevision
                        Vitest.expect(after.Entries).toEqual expectedEntries
                    | _ -> failwith "Expected interrupted restore to preserve the published index."

                    let transactionDirectories =
                        NodeFileSystem.readdirSync (NodePath.join [| stateDirectory; "transactions" |])

                    let recoveryDirectory = NodePath.join [| stateDirectory; "recovery" |]
                    let recoveryFiles = NodeFileSystem.readdirSync recoveryDirectory
                    Vitest.expect(transactionDirectories.Length).toBe 1
                    Vitest.expect(recoveryFiles.Length).toBe 1
                    let recoveryText =
                        NodeFileSystem.readFileSync
                            (NodePath.join [| recoveryDirectory; recoveryFiles[0] |])
                            NodeFileSystem.TextEncoding.Utf8

                    Vitest.expect(recoveryText.Contains "000-restore-first.txt").toBe true
                    Vitest.expect(recoveryText.Contains "zzz-restore-second.txt").toBe true
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
            "lakeFS materialization prepare cancellation leaves the workspace unchanged",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()
                let cancellation = OperationCancellation.Source()
                let mutable preparedObjects = 0

                try
                    let! workspace = harness.CreateWorkspace()
                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let branchName = "materialization-prepare-cancel"
                    let! created =
                        LakeFsApi.createBranch
                            (connection ())
                            parsed.Repository
                            branchName
                            parsed.TargetRef
                            (OperationContext.detached "materialization-prepare-create-branch")
                        |> Async.StartAsPromise

                    created |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore

                    for path, content in [|
                        "prepared-first.txt", "prepared first bytes\n"
                        "prepared-second.txt", "prepared second bytes\n"
                    |] do
                        let! uploaded =
                            uploadTextObject
                                (connection ())
                                parsed.Repository
                                branchName
                                path
                                content
                                (OperationContext.detached "materialization-prepare-upload")
                            |> Async.StartAsPromise

                        uploaded |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore

                    let! committed =
                        LakeFsApi.commit
                            (connection ())
                            parsed.Repository
                            branchName
                            "test: prepare cancellation target"
                            (OperationContext.detached "materialization-prepare-commit")
                        |> Async.StartAsPromise

                    committed |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore

                    let hooks: LakeFsWorkspaceSession.LakeFsSessionHooks = {
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "materialization-prepare-object" then
                                    preparedObjects <- preparedObjects + 1

                                    if preparedObjects = 1 then
                                        cancellation.Cancel()
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
                            (OperationContext.detached "materialization-prepare-open")
                        |> Async.StartAsPromise

                    let session = expectValue "materialization prepare open" opened
                    let before =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before prepare cancellation."

                    let! statusResult =
                        session.Core.GetStatus(OperationContext.detached "materialization-prepare-status")
                        |> Async.StartAsPromise

                    let status = expectValue "materialization prepare status" statusResult
                    let switchContext =
                        OperationContext.create
                            "materialization-prepare-switch"
                            cancellation.Cancellation
                            ignore

                    let! switched =
                        session.Core.SwitchRef
                            {
                                TargetRef = ProviderRef.tryCreate $"lakefs:{branchName}" |> Result.defaultWith failwith
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            switchContext
                        |> Async.StartAsPromise

                    let failure =
                        match switched with
                        | Failed failure -> failure
                        | PartiallySucceeded _ ->
                            failwith "Prepare cancellation must not report visible partial mutation."
                        | Succeeded _ -> failwith "Prepare cancellation unexpectedly switched the workspace."

                    Vitest.expect(preparedObjects).toBe 1
                    Vitest.expect(failure.Category).toEqual FailureCategory.Canceled
                    Vitest.expect(failure.StateChanged).toBe false
                    let! first = workspace.ReadFile "prepared-first.txt"
                    let! second = workspace.ReadFile "prepared-second.txt"
                    Vitest.expect(first).toEqual None
                    Vitest.expect(second).toEqual None

                    match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                    | LakeFsWorkspaceIndex.Loaded after ->
                        Vitest.expect(after.WorkspaceRevision).toEqual before.WorkspaceRevision
                        Vitest.expect(after.Entries).toEqual before.Entries
                    | _ -> failwith "Expected prepare cancellation to preserve the index."

                    let transactionsDirectory =
                        NodePath.join [| stateDirectoryForBinding workspace.Binding; "transactions" |]

                    Vitest.expect(NodeFileSystem.readdirSync transactionsDirectory).toEqual [||]
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization apply failure reports exact partial recovery",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()
                let mutable appliedObjects = 0

                try
                    let! workspace = harness.CreateWorkspace()
                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let branchName = "materialization-apply-failure"
                    let! created =
                        LakeFsApi.createBranch
                            (connection ())
                            parsed.Repository
                            branchName
                            parsed.TargetRef
                            (OperationContext.detached "materialization-apply-create-branch")
                        |> Async.StartAsPromise

                    created |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore

                    for path, content in [|
                        "000-apply-first.txt", "new first bytes\n"
                        "zzz-apply-second.txt", "new second bytes\n"
                    |] do
                        let! uploaded =
                            uploadTextObject
                                (connection ())
                                parsed.Repository
                                branchName
                                path
                                content
                                (OperationContext.detached "materialization-apply-upload")
                            |> Async.StartAsPromise

                        uploaded |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore

                    let! committedResult =
                        LakeFsApi.commit
                            (connection ())
                            parsed.Repository
                            branchName
                            "test: apply failure target"
                            (OperationContext.detached "materialization-apply-commit")
                        |> Async.StartAsPromise

                    let committed =
                        committedResult |> Result.defaultWith (fun failure -> failwith failure.Message)

                    let! listedResult =
                        LakeFsApi.listObjects
                            (connection ())
                            parsed.Repository
                            committed.Id
                            ""
                            (OperationContext.detached "materialization-apply-list")
                        |> Async.StartAsPromise

                    let expectedFirstStat =
                        listedResult
                        |> Result.defaultWith (fun failure -> failwith failure.Message)
                        |> Array.find (fun stat -> stat.Path = "000-apply-first.txt")

                    do! workspace.WriteFile "000-apply-first.txt" "old first bytes\n"
                    do! workspace.WriteFile "zzz-apply-second.txt" "old second bytes\n"

                    let hooks: LakeFsWorkspaceSession.LakeFsSessionHooks = {
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "materialization-apply-object" then
                                    appliedObjects <- appliedObjects + 1

                                    if appliedObjects = 1 then
                                        failwith "injected materialization apply failure"
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
                            (OperationContext.detached "materialization-apply-open")
                        |> Async.StartAsPromise

                    let session = expectValue "materialization apply open" opened
                    let stateDirectory = stateDirectoryForBinding workspace.Binding
                    let before =
                        match LakeFsWorkspaceIndex.load stateDirectory with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before apply failure."

                    let! statusResult =
                        session.Core.GetStatus(OperationContext.detached "materialization-apply-status")
                        |> Async.StartAsPromise

                    let status = expectValue "materialization apply status" statusResult
                    let! switched =
                        session.Core.SwitchRef
                            {
                                TargetRef = ProviderRef.tryCreate $"lakefs:{branchName}" |> Result.defaultWith failwith
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (OperationContext.detached "materialization-apply-switch")
                        |> Async.StartAsPromise

                    let outcome, failure =
                        match switched with
                        | PartiallySucceeded(outcome, failure) -> outcome, failure
                        | Failed failure ->
                            failwith $"Visible apply mutation was concealed as Failed: {failure.Code}"
                        | Succeeded _ -> failwith "Injected apply failure unexpectedly switched the workspace."

                    Vitest.expect(appliedObjects).toBe 1
                    Vitest.expect(failure.StateChanged).toBe true
                    Vitest.expect(failure.Code).toBe "materialization_apply_failed"
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_materialization")
                    Vitest.expect(outcome.AffectedPaths).toEqual [| "000-apply-first.txt" |]
                    Vitest.expect(failure.AffectedPaths).toEqual [| "000-apply-first.txt" |]
                    let evidence = failure.RevisionEvidence |> Map.ofArray
                    Vitest.expect(evidence["expected_workspace"] |> RevisionId.value).toEqual before.WorkspaceRevision.Value
                    Vitest.expect(evidence["observed_materialization"] |> RevisionId.value).toBe committed.Id

                    let! first = workspace.ReadFile "000-apply-first.txt"
                    let! second = workspace.ReadFile "zzz-apply-second.txt"
                    Vitest.expect(first).toEqual(Some "new first bytes\n")
                    Vitest.expect(second).toEqual(Some "old second bytes\n")

                    match LakeFsWorkspaceIndex.load stateDirectory with
                    | LakeFsWorkspaceIndex.Loaded after ->
                        let expectedFirst: LakeFsWorkspaceIndex.IndexEntry = {
                            Path = "000-apply-first.txt"
                            BaseChecksum = expectedFirstStat.Checksum
                            LocalHash = LakeFsWorkspaceIndex.hashMetadata "new first bytes\n"
                            LocalSize = 16.0
                            LocalMtimeMs = expectedFirstStat.Mtime
                        }

                        Vitest.expect(after.WorkspaceRevision).toEqual before.WorkspaceRevision
                        Vitest.expect(after.Entries).toEqual (Array.append before.Entries [| expectedFirst |])
                        Vitest.expect(
                            after.Entries
                            |> Array.exists (fun entry -> entry.Path = "zzz-apply-second.txt")
                        ).toBe false
                    | _ -> failwith "Expected apply failure to preserve the published index."

                    let transactionsDirectory = NodePath.join [| stateDirectory; "transactions" |]
                    let recoveryDirectory = NodePath.join [| stateDirectory; "recovery" |]
                    Vitest.expect(NodeFileSystem.readdirSync transactionsDirectory).toHaveLength 1
                    let recoveryFiles = NodeFileSystem.readdirSync recoveryDirectory
                    Vitest.expect(recoveryFiles.Length).toBe 1
                    let recoveryText =
                        NodeFileSystem.readFileSync
                            (NodePath.join [| recoveryDirectory; recoveryFiles[0] |])
                            NodeFileSystem.TextEncoding.Utf8

                    Vitest.expect(recoveryText.Contains "000-apply-first.txt").toBe true
                    Vitest.expect(recoveryText.Contains "zzz-apply-second.txt").toBe true
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS materialization apply cancellation reports exact partial recovery",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()
                let cancellation = OperationCancellation.Source()
                let mutable appliedObjects = 0

                try
                    let! workspace = harness.CreateWorkspace()
                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let branchName = "materialization-apply-cancellation"
                    let! created =
                        LakeFsApi.createBranch
                            (connection ())
                            parsed.Repository
                            branchName
                            parsed.TargetRef
                            (OperationContext.detached "materialization-apply-cancel-create-branch")
                        |> Async.StartAsPromise

                    created |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore

                    for path, content in [|
                        "000-cancel-first.txt", "new first bytes\n"
                        "zzz-cancel-second.txt", "new second bytes\n"
                    |] do
                        let! uploaded =
                            uploadTextObject
                                (connection ())
                                parsed.Repository
                                branchName
                                path
                                content
                                (OperationContext.detached "materialization-apply-cancel-upload")
                            |> Async.StartAsPromise

                        uploaded |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore

                    let! committedResult =
                        LakeFsApi.commit
                            (connection ())
                            parsed.Repository
                            branchName
                            "test: apply cancellation target"
                            (OperationContext.detached "materialization-apply-cancel-commit")
                        |> Async.StartAsPromise

                    let committed =
                        committedResult |> Result.defaultWith (fun failure -> failwith failure.Message)

                    let! listedResult =
                        LakeFsApi.listObjects
                            (connection ())
                            parsed.Repository
                            committed.Id
                            ""
                            (OperationContext.detached "materialization-apply-cancel-list")
                        |> Async.StartAsPromise

                    let expectedFirstStat =
                        listedResult
                        |> Result.defaultWith (fun failure -> failwith failure.Message)
                        |> Array.find (fun stat -> stat.Path = "000-cancel-first.txt")

                    do! workspace.WriteFile "000-cancel-first.txt" "old first bytes\n"
                    do! workspace.WriteFile "zzz-cancel-second.txt" "old second bytes\n"

                    let hooks: LakeFsWorkspaceSession.LakeFsSessionHooks = {
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "materialization-apply-object" then
                                    appliedObjects <- appliedObjects + 1

                                    if appliedObjects = 1 then
                                        cancellation.Cancel()
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
                            (OperationContext.detached "materialization-apply-cancel-open")
                        |> Async.StartAsPromise

                    let session = expectValue "materialization apply cancellation open" opened
                    let stateDirectory = stateDirectoryForBinding workspace.Binding
                    let before =
                        match LakeFsWorkspaceIndex.load stateDirectory with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before apply cancellation."

                    let! statusResult =
                        session.Core.GetStatus(OperationContext.detached "materialization-apply-cancel-status")
                        |> Async.StartAsPromise

                    let status = expectValue "materialization apply cancellation status" statusResult
                    let switchContext =
                        OperationContext.create
                            "materialization-apply-cancel-switch"
                            cancellation.Cancellation
                            ignore

                    let! switched =
                        session.Core.SwitchRef
                            {
                                TargetRef = ProviderRef.tryCreate $"lakefs:{branchName}" |> Result.defaultWith failwith
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            switchContext
                        |> Async.StartAsPromise

                    let outcome, failure =
                        match switched with
                        | PartiallySucceeded(outcome, failure) -> outcome, failure
                        | Failed failure ->
                            failwith $"Visible apply cancellation was concealed as Failed: {failure.Code}"
                        | Succeeded _ -> failwith "Canceled apply unexpectedly switched the workspace."

                    Vitest.expect(appliedObjects).toBe 1
                    Vitest.expect(failure.Category).toEqual FailureCategory.Canceled
                    Vitest.expect(failure.StateChanged).toBe true
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_materialization")
                    Vitest.expect(outcome.AffectedPaths).toEqual [| "000-cancel-first.txt" |]
                    Vitest.expect(failure.AffectedPaths).toEqual [| "000-cancel-first.txt" |]
                    let evidence = failure.RevisionEvidence |> Map.ofArray
                    Vitest.expect(evidence["expected_workspace"] |> RevisionId.value).toEqual before.WorkspaceRevision.Value
                    Vitest.expect(evidence["observed_materialization"] |> RevisionId.value).toBe committed.Id

                    let! first = workspace.ReadFile "000-cancel-first.txt"
                    let! second = workspace.ReadFile "zzz-cancel-second.txt"
                    Vitest.expect(first).toEqual(Some "new first bytes\n")
                    Vitest.expect(second).toEqual(Some "old second bytes\n")

                    match LakeFsWorkspaceIndex.load stateDirectory with
                    | LakeFsWorkspaceIndex.Loaded after ->
                        let expectedFirst: LakeFsWorkspaceIndex.IndexEntry = {
                            Path = "000-cancel-first.txt"
                            BaseChecksum = expectedFirstStat.Checksum
                            LocalHash = LakeFsWorkspaceIndex.hashMetadata "new first bytes\n"
                            LocalSize = 16.0
                            LocalMtimeMs = expectedFirstStat.Mtime
                        }

                        Vitest.expect(after.WorkspaceRevision).toEqual before.WorkspaceRevision
                        Vitest.expect(after.Entries).toEqual (Array.append before.Entries [| expectedFirst |])
                        Vitest.expect(
                            after.Entries
                            |> Array.exists (fun entry -> entry.Path = "zzz-cancel-second.txt")
                        ).toBe false
                    | _ -> failwith "Expected apply cancellation to preserve the published index."

                    let transactionsDirectory = NodePath.join [| stateDirectory; "transactions" |]
                    let recoveryDirectory = NodePath.join [| stateDirectory; "recovery" |]
                    Vitest.expect(NodeFileSystem.readdirSync transactionsDirectory).toHaveLength 1
                    let recoveryFiles = NodeFileSystem.readdirSync recoveryDirectory
                    Vitest.expect(recoveryFiles.Length).toBe 1
                    do! harness.Cleanup()
                with error ->
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
            "lakeFS update preserves an unrelated dirty tracked file",
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
                            { Path = "target-update.txt"; Content = Some "target update content\n" }
                        |]

                    do! workspace.WriteFile "base.txt" "local base content\n"

                    let! previewResult =
                        synchronization.PreviewUpdate(OperationContext.detached "update-preserve-preview")
                        |> Async.StartAsPromise

                    let preview = expectValue "update preserve preview" previewResult
                    Vitest.expect(preview.HasDataLossRisk).toBe false
                    Vitest.expect(preview.WouldCreateConflictSession).toBe false

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "update-preserve-status")
                        |> Async.StartAsPromise

                    let status = expectValue "update preserve status" statusResult
                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (OperationContext.detached "update-preserve")
                        |> Async.StartAsPromise

                    expectValue "update preserve" updateResult |> ignore

                    let! targetContent = workspace.ReadFile "target-update.txt"
                    let! localContent = workspace.ReadFile "base.txt"
                    Vitest.expect(targetContent).toEqual(Some "target update content\n")
                    Vitest.expect(localContent).toEqual(Some "local base content\n")

                    let! afterStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "update-preserve-after-status")
                        |> Async.StartAsPromise

                    let afterStatus = expectValue "update preserve after status" afterStatusResult
                    Vitest.expect(
                        afterStatus.Changes
                        |> Array.exists (fun change -> RepositoryPath.value change.Path = "base.txt")
                    ).toBe true

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS update preserves a dirty file after a local commit",
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

                    do! workspace.WriteFile "committed-local.txt" "committed local content\n"

                    let! beforeRevisionStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "update-preserve-local-commit-status")
                        |> Async.StartAsPromise

                    let beforeRevisionStatus =
                        expectValue "update preserve local commit status" beforeRevisionStatusResult

                    let! revisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: commit local file before update"
                                Paths = [| repositoryPath "committed-local.txt" |]
                                ExpectedWorkspaceVersion = beforeRevisionStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "update-preserve-local-commit-revision")
                        |> Async.StartAsPromise

                    expectValue "update preserve local commit revision" revisionResult |> ignore

                    do! workspace.WriteFile "committed-local.txt" "uncommitted local content\n"

                    do!
                        harness.AdvanceTarget workspace [|
                            { Path = "target-after-local-commit.txt"; Content = Some "target after local commit\n" }
                        |]

                    let! previewResult =
                        synchronization.PreviewUpdate(OperationContext.detached "update-preserve-local-commit-preview")
                        |> Async.StartAsPromise

                    let preview = expectValue "update preserve local commit preview" previewResult
                    Vitest.expect(preview.HasDataLossRisk).toBe false
                    Vitest.expect(preview.WouldCreateConflictSession).toBe false

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "update-preserve-local-commit-update-status")
                        |> Async.StartAsPromise

                    let status = expectValue "update preserve local commit update status" statusResult
                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (OperationContext.detached "update-preserve-local-commit-update")
                        |> Async.StartAsPromise

                    expectValue "update preserve local commit" updateResult |> ignore

                    let! localContent = workspace.ReadFile "committed-local.txt"
                    Vitest.expect(localContent).toEqual(Some "uncommitted local content\n")

                    let! afterStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "update-preserve-local-commit-after-status")
                        |> Async.StartAsPromise

                    let afterStatus = expectValue "update preserve local commit after status" afterStatusResult
                    Vitest.expect(
                        afterStatus.Changes
                        |> Array.exists (fun change -> RepositoryPath.value change.Path = "committed-local.txt")
                    ).toBe true

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS update preserves a local deletion outside the target diff",
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

                    do! workspace.RemoveFile "base.txt"

                    do!
                        harness.AdvanceTarget workspace [|
                            { Path = "target-after-deletion.txt"; Content = Some "target after deletion\n" }
                        |]

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "update-local-deletion-status")
                        |> Async.StartAsPromise

                    let status = expectValue "local deletion status" statusResult
                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (OperationContext.detached "update-local-deletion")
                        |> Async.StartAsPromise

                    expectValue "local deletion update" updateResult |> ignore

                    let! deletedContent = workspace.ReadFile "base.txt"
                    let! targetContent = workspace.ReadFile "target-after-deletion.txt"
                    Vitest.expect(deletedContent).toEqual None
                    Vitest.expect(targetContent).toEqual(Some "target after deletion\n")

                    let! afterStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "update-local-deletion-after-status")
                        |> Async.StartAsPromise

                    let afterStatus = expectValue "local deletion after status" afterStatusResult
                    let deletion =
                        afterStatus.Changes
                        |> Array.tryFind (fun change -> RepositoryPath.value change.Path = "base.txt")

                    Vitest.expect(deletion |> Option.map _.Kind).toEqual (Some FileChangeKind.DeletedChange)

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS update reports the merged head after post-merge materialization failure",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()
                let mutable preparedObjects = 0

                try
                    let! workspace = harness.CreateWorkspace()
                    do!
                        harness.AdvanceTarget workspace [|
                            { Path = "post-merge-update.txt"; Content = Some "post-merge update content\n" }
                        |]

                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let! targetResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (OperationContext.detached "post-merge-materialization-target")
                        |> Async.StartAsPromise

                    let target = targetResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    let hooks: LakeFsWorkspaceSession.LakeFsSessionHooks = {
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "materialization-prepare-object" then
                                    preparedObjects <- preparedObjects + 1

                                    if preparedObjects = 1 then
                                        failwith "injected post-merge materialization failure"
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
                            (OperationContext.detached "post-merge-materialization-open")
                        |> Async.StartAsPromise

                    let session = expectValue "post-merge materialization open" opened
                    let synchronization =
                        session.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS synchronization services.")

                    let! previewResult =
                        synchronization.PreviewUpdate(OperationContext.detached "post-merge-materialization-preview")
                        |> Async.StartAsPromise

                    let preview = expectValue "post-merge materialization preview" previewResult
                    Vitest.expect(preview.HasDataLossRisk).toBe false
                    Vitest.expect(preview.WouldCreateConflictSession).toBe false

                    let! statusResult =
                        session.Core.GetStatus(OperationContext.detached "post-merge-materialization-status")
                        |> Async.StartAsPromise

                    let status = expectValue "post-merge materialization status" statusResult
                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (OperationContext.detached "post-merge-materialization-update")
                        |> Async.StartAsPromise

                    let outcome, failure =
                        match updateResult with
                        | PartiallySucceeded(outcome, failure) -> outcome, failure
                        | Failed failure ->
                            failwith $"Post-merge materialization failure was reported as Failed: {failure.Code}"
                        | Succeeded _ -> failwith "Post-merge materialization failure unexpectedly succeeded."

                    Vitest.expect(preparedObjects).toBe 1
                    Vitest.expect(failure.StateChanged).toBe true
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_materialization")
                    Vitest.expect(outcome.Value.WorkspaceRevision |> Option.map RevisionId.value).toEqual (Some target.CommitId)
                    Vitest.expect(outcome.Value.Relationship).toEqual RevisionRelationship.UpToDate

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS conflict finalize deletes a locally recreated file the target deleted",
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

                    do! workspace.RemoveFile "base.txt"
                    do! workspace.WriteFile "base.txt" "recreated locally after target deletion\n"
                    do!
                        harness.AdvanceTarget workspace [|
                            { Path = "base.txt"; Content = None }
                        |]

                    let! previewResult =
                        synchronization.PreviewUpdate(OperationContext.detached "finalize-deletion-preview")
                        |> Async.StartAsPromise

                    let preview = expectValue "finalize deletion preview" previewResult
                    Vitest.expect(preview.WouldCreateConflictSession).toBe true

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-deletion-status")
                        |> Async.StartAsPromise

                    let status = expectValue "finalize deletion status" statusResult
                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (OperationContext.detached "finalize-deletion-update")
                        |> Async.StartAsPromise

                    expectFailure "finalize deletion update" updateResult |> ignore

                    let conflicts =
                        workspace.Session.ConflictResolution
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS conflict-resolution services.")

                    let! sessionResult =
                        conflicts.GetActiveSession(OperationContext.detached "finalize-deletion-session")
                        |> Async.StartAsPromise

                    let summary =
                        expectValue "finalize deletion session" sessionResult
                        |> Option.defaultWith (fun () -> failwith "Expected an active conflict session.")

                    let! resolutionStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-deletion-resolution-status")
                        |> Async.StartAsPromise

                    let resolutionStatus = expectValue "finalize deletion resolution status" resolutionStatusResult
                    let! resolutionResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = resolutionStatus.WorkspaceVersion
                                Path = repositoryPath "base.txt"
                                Resolution = PickCandidate "target"
                            }
                            (OperationContext.detached "finalize-deletion-resolution")
                        |> Async.StartAsPromise

                    let resolution = expectValue "finalize deletion resolution" resolutionResult
                    let! finalizeStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-deletion-finalize-status")
                        |> Async.StartAsPromise

                    let finalizeStatus = expectValue "finalize deletion finalize status" finalizeStatusResult
                    let! finalizeResult =
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                Message = Some "finalize target deletion"
                            }
                            (OperationContext.detached "finalize-deletion-finalize")
                        |> Async.StartAsPromise

                    expectValue "finalize target deletion" finalizeResult |> ignore
                    let! content = workspace.ReadFile "base.txt"
                    Vitest.expect(content).toEqual None

                    let afterIndex =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index after the conflict finalize."

                    Vitest.expect(
                        afterIndex.Entries |> Array.exists (fun entry -> entry.Path = "base.txt")
                    ).toBe false

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS conflict finalize commits only the resolution paths",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! workspace = harness.CreateWorkspace()

                    // The foreign object is staged from the barrier after the merge, because lakeFS
                    // refuses to merge into a branch that already has uncommitted changes.
                    let hooks: LakeFsWorkspaceSession.LakeFsSessionHooks = {
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "finalize-merge-verified" then
                                    let parsedForStage =
                                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                                        |> Result.defaultWith failwith

                                    let indexForStage =
                                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                                        | LakeFsWorkspaceIndex.Loaded value -> value
                                        | _ -> failwith "Expected an index before staging the unrelated object."

                                    let! staged =
                                        LakeFsApi.uploadObjectFromFile
                                            (connection ())
                                            parsedForStage.Repository
                                            indexForStage.WorkspaceBranch
                                            "staged-unrelated.txt"
                                            (NodePath.join [| workspace.Binding.WorkspaceRoot; "conflict.txt" |])
                                            (OperationContext.detached "finalize-paths-stage-unrelated")

                                    staged |> Result.defaultWith (fun failure -> failwith failure.Message) |> ignore
                            })
                    }

                    let factory =
                        LakeFsWorkspaceSession.createFactoryWithHooks
                            lakeFsProviderOptions
                            hooks
                            (LakeFsCredentials.fixedConnection (connection ()))

                    let! hookedOpen =
                        factory.Open workspace.Binding (OperationContext.detached "finalize-paths-open")
                        |> Async.StartAsPromise

                    let session = expectValue "finalize paths open" hookedOpen

                    let synchronization =
                        session.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS synchronization services.")

                    do! workspace.WriteFile "conflict.txt" "workspace conflict content\n"
                    let! beforeRevisionStatusResult =
                        session.Core.GetStatus(OperationContext.detached "finalize-paths-save-status")
                        |> Async.StartAsPromise

                    let beforeRevisionStatus = expectValue "finalize paths save status" beforeRevisionStatusResult
                    let! beforeRevisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: workspace conflict content"
                                Paths = [| repositoryPath "conflict.txt" |]
                                ExpectedWorkspaceVersion = beforeRevisionStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "finalize-paths-save")
                        |> Async.StartAsPromise

                    expectValue "finalize paths save" beforeRevisionResult |> ignore
                    do!
                        harness.AdvanceTarget workspace [|
                            { Path = "conflict.txt"; Content = Some "target conflict content\n" }
                        |]

                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let! targetResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (OperationContext.detached "finalize-paths-target")
                        |> Async.StartAsPromise

                    let target = targetResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    let beforeIndex =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before the conflict update."

                    let! previewResult =
                        synchronization.PreviewUpdate(OperationContext.detached "finalize-paths-preview")
                        |> Async.StartAsPromise

                    let preview = expectValue "finalize paths preview" previewResult
                    Vitest.expect(preview.WouldCreateConflictSession).toBe true

                    let! statusResult =
                        session.Core.GetStatus(OperationContext.detached "finalize-paths-update-status")
                        |> Async.StartAsPromise

                    let status = expectValue "finalize paths update status" statusResult
                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (OperationContext.detached "finalize-paths-update")
                        |> Async.StartAsPromise

                    expectFailure "finalize paths update" updateResult |> ignore
                    let conflicts =
                        session.ConflictResolution
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS conflict-resolution services.")

                    let! sessionResult =
                        conflicts.GetActiveSession(OperationContext.detached "finalize-paths-session")
                        |> Async.StartAsPromise

                    let summary =
                        expectValue "finalize paths session" sessionResult
                        |> Option.defaultWith (fun () -> failwith "Expected an active conflict session.")

                    let! resolutionStatusResult =
                        session.Core.GetStatus(OperationContext.detached "finalize-paths-resolution-status")
                        |> Async.StartAsPromise

                    let resolutionStatus = expectValue "finalize paths resolution status" resolutionStatusResult
                    let! resolutionResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = resolutionStatus.WorkspaceVersion
                                Path = repositoryPath "conflict.txt"
                                Resolution = PickCandidate "workspace"
                            }
                            (OperationContext.detached "finalize-paths-resolution")
                        |> Async.StartAsPromise

                    let resolution = expectValue "finalize paths resolution" resolutionResult
                    let! finalizeStatusResult =
                        session.Core.GetStatus(OperationContext.detached "finalize-paths-finalize-status")
                        |> Async.StartAsPromise

                    let finalizeStatus = expectValue "finalize paths finalize status" finalizeStatusResult
                    let! finalizeResult =
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                Message = Some "finalize only reviewed paths"
                            }
                            (OperationContext.detached "finalize-paths-finalize")
                        |> Async.StartAsPromise

                    let finalizedRevision = expectValue "finalize only reviewed paths" finalizeResult
                    let finalizedRevision =
                        finalizedRevision |> Option.defaultWith (fun () -> failwith "Expected a finalized revision.")
                    let! finalizedBranchResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            beforeIndex.WorkspaceBranch
                            (OperationContext.detached "finalize-paths-workspace-head")
                        |> Async.StartAsPromise

                    let finalizedBranch = finalizedBranchResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(finalizedBranch.CommitId).toBe (RevisionId.value finalizedRevision)
                    let! finalizedCommitResult =
                        LakeFsApi.getCommit
                            (connection ())
                            parsed.Repository
                            finalizedBranch.CommitId
                            (OperationContext.detached "finalize-paths-finalized-commit")
                        |> Async.StartAsPromise

                    let finalizedCommit = finalizedCommitResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(finalizedCommit.Parents).toContain target.CommitId
                    Vitest.expect(finalizedCommit.Parents).toContain beforeIndex.WorkspaceRevision.Value
                    let! pendingResult =
                        LakeFsApi.diffBranch
                            (connection ())
                            parsed.Repository
                            beforeIndex.WorkspaceBranch
                            (OperationContext.detached "finalize-paths-pending")
                        |> Async.StartAsPromise

                    let pending = pendingResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(pending |> Array.exists (fun entry -> entry.Path = "staged-unrelated.txt")).toBe true

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS resolve refuses an unadvertised base candidate",
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
                            { Path = "new-conflict.txt"; Content = Some "target new content\n" }
                        |]
                    do! workspace.WriteFile "new-conflict.txt" "workspace new content\n"

                    let! previewResult =
                        synchronization.PreviewUpdate(OperationContext.detached "unadvertised-base-preview")
                        |> Async.StartAsPromise

                    let preview = expectValue "unadvertised base preview" previewResult
                    Vitest.expect(preview.WouldCreateConflictSession).toBe true
                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "unadvertised-base-status")
                        |> Async.StartAsPromise

                    let status = expectValue "unadvertised base status" statusResult
                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (OperationContext.detached "unadvertised-base-update")
                        |> Async.StartAsPromise

                    expectFailure "unadvertised base update" updateResult |> ignore
                    let conflicts =
                        workspace.Session.ConflictResolution
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS conflict-resolution services.")

                    let! sessionResult =
                        conflicts.GetActiveSession(OperationContext.detached "unadvertised-base-session")
                        |> Async.StartAsPromise

                    let summary =
                        expectValue "unadvertised base session" sessionResult
                        |> Option.defaultWith (fun () -> failwith "Expected an active conflict session.")
                    let! resolutionStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "unadvertised-base-resolution-status")
                        |> Async.StartAsPromise

                    let resolutionStatus = expectValue "unadvertised base resolution status" resolutionStatusResult
                    let! resolutionResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = resolutionStatus.WorkspaceVersion
                                Path = repositoryPath "new-conflict.txt"
                                Resolution = PickCandidate "base"
                            }
                            (OperationContext.detached "unadvertised-base-resolution")
                        |> Async.StartAsPromise

                    let failure = expectFailure "unadvertised base resolution" resolutionResult
                    Vitest.expect(failure.Category).toEqual FailureCategory.Validation
                    Vitest.expect(failure.Code).toBe "unknown_candidate"
                    Vitest.expect(failure.Message).toContain "base"
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS conflict finalize materializes the reviewed target write set",
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
                            { Path = "conflict.txt"; Content = Some "target conflict content\n" }
                            { Path = "target-unrelated.txt"; Content = Some "target unrelated content\n" }
                        |]

                    do! workspace.WriteFile "conflict.txt" "local conflict content\n"
                    do! workspace.WriteFile "dirty-unrelated.txt" "dirty local content\n"

                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let! targetResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (OperationContext.detached "finalize-write-set-target")
                        |> Async.StartAsPromise

                    let target = targetResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    let! previewResult =
                        synchronization.PreviewUpdate(OperationContext.detached "finalize-write-set-preview")
                        |> Async.StartAsPromise

                    let preview = expectValue "finalize write set preview" previewResult
                    Vitest.expect(preview.WouldCreateConflictSession).toBe true

                    let! updateStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-write-set-update-status")
                        |> Async.StartAsPromise

                    let updateStatus = expectValue "finalize write set update status" updateStatusResult
                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                            (OperationContext.detached "finalize-write-set-update")
                        |> Async.StartAsPromise

                    let updateFailure = expectFailure "finalize write set update" updateResult
                    Vitest.expect(updateFailure.Category).toEqual FailureCategory.Conflict

                    let conflicts =
                        workspace.Session.ConflictResolution
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS conflict-resolution services.")

                    let! sessionResult =
                        conflicts.GetActiveSession(OperationContext.detached "finalize-write-set-session")
                        |> Async.StartAsPromise

                    let summary =
                        expectValue "finalize write set session" sessionResult
                        |> Option.defaultWith (fun () -> failwith "Expected an active conflict session.")

                    let! resolutionStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-write-set-resolution-status")
                        |> Async.StartAsPromise

                    let resolutionStatus = expectValue "finalize write set resolution status" resolutionStatusResult
                    let! resolutionResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = resolutionStatus.WorkspaceVersion
                                Path = repositoryPath "conflict.txt"
                                Resolution = PickCandidate "target"
                            }
                            (OperationContext.detached "finalize-write-set-resolution")
                        |> Async.StartAsPromise

                    let resolution = expectValue "finalize write set resolution" resolutionResult
                    let! finalizeStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-write-set-finalize-status")
                        |> Async.StartAsPromise

                    let finalizeStatus = expectValue "finalize write set finalize status" finalizeStatusResult
                    let! finalizeResult =
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                Message = Some "finalize reviewed write set"
                            }
                            (OperationContext.detached "finalize-write-set-finalize")
                        |> Async.StartAsPromise

                    expectValue "finalize write set finalize" finalizeResult |> ignore

                    let! unrelatedTarget = workspace.ReadFile "target-unrelated.txt"
                    let! dirtyUnrelated = workspace.ReadFile "dirty-unrelated.txt"
                    Vitest.expect(unrelatedTarget).toEqual(Some "target unrelated content\n")
                    Vitest.expect(dirtyUnrelated).toEqual(Some "dirty local content\n")

                    let afterIndex =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index after the conflict finalize."

                    Vitest.expect(afterIndex.BaseRevision).toEqual(Some target.CommitId)
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS conflict finalize canceled after the merge resumes through the pending revision",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()
                let cancellation = OperationCancellation.Source()
                let mutable cancelFirstFinalize = true

                try
                    let! workspace = harness.CreateWorkspace()
                    let hooks: LakeFsWorkspaceSession.LakeFsSessionHooks = {
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "finalize-before-completion" && cancelFirstFinalize then
                                    cancelFirstFinalize <- false
                                    cancellation.Cancel()
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
                            (OperationContext.detached "finalize-cancel-after-merge-open")
                        |> Async.StartAsPromise

                    let session = expectValue "finalize cancel after merge open" opened
                    let synchronization =
                        session.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS synchronization services.")

                    do!
                        harness.AdvanceTarget workspace [|
                            { Path = "conflict.txt"; Content = Some "target conflict content\n" }
                            { Path = "target-unrelated.txt"; Content = Some "target unrelated content\n" }
                        |]

                    do! workspace.WriteFile "conflict.txt" "local conflict content\n"
                    do! workspace.WriteFile "dirty-unrelated.txt" "dirty local content\n"

                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let! targetResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (OperationContext.detached "finalize-cancel-after-merge-target")
                        |> Async.StartAsPromise

                    let target = targetResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    let! previewResult =
                        synchronization.PreviewUpdate(OperationContext.detached "finalize-cancel-after-merge-preview")
                        |> Async.StartAsPromise

                    let preview = expectValue "finalize cancel after merge preview" previewResult
                    Vitest.expect(preview.WouldCreateConflictSession).toBe true

                    let! updateStatusResult =
                        session.Core.GetStatus(OperationContext.detached "finalize-cancel-after-merge-update-status")
                        |> Async.StartAsPromise

                    let updateStatus = expectValue "finalize cancel after merge update status" updateStatusResult
                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                            (OperationContext.detached "finalize-cancel-after-merge-update")
                        |> Async.StartAsPromise

                    expectFailure "finalize cancel after merge update" updateResult |> ignore
                    let conflicts =
                        session.ConflictResolution
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS conflict-resolution services.")

                    let! sessionResult =
                        conflicts.GetActiveSession(OperationContext.detached "finalize-cancel-after-merge-session")
                        |> Async.StartAsPromise

                    let summary =
                        expectValue "finalize cancel after merge session" sessionResult
                        |> Option.defaultWith (fun () -> failwith "Expected an active conflict session.")

                    let! resolutionStatusResult =
                        session.Core.GetStatus(OperationContext.detached "finalize-cancel-after-merge-resolution-status")
                        |> Async.StartAsPromise

                    let resolutionStatus = expectValue "finalize cancel after merge resolution status" resolutionStatusResult
                    let! resolutionResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = resolutionStatus.WorkspaceVersion
                                Path = repositoryPath "conflict.txt"
                                Resolution = PickCandidate "target"
                            }
                            (OperationContext.detached "finalize-cancel-after-merge-resolution")
                        |> Async.StartAsPromise

                    let resolution = expectValue "finalize cancel after merge resolution" resolutionResult
                    let! finalizeStatusResult =
                        session.Core.GetStatus(OperationContext.detached "finalize-cancel-after-merge-finalize-status")
                        |> Async.StartAsPromise

                    let finalizeStatus = expectValue "finalize cancel after merge finalize status" finalizeStatusResult
                    let beforeIndex =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index before conflict finalize."

                    let! branchBeforeResult =
                        LakeFsApi.getBranch
                            (connection ())
                            beforeIndex.Repository
                            beforeIndex.WorkspaceBranch
                            (OperationContext.detached "finalize-cancel-after-merge-head-before")
                        |> Async.StartAsPromise

                    let branchBefore = branchBeforeResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    let firstFinalizeContext =
                        OperationContext.create
                            "finalize-cancel-after-merge-first"
                            cancellation.Cancellation
                            ignore

                    let! firstFinalizeResult =
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                Message = Some "finalize reviewed write set"
                            }
                            firstFinalizeContext
                        |> Async.StartAsPromise

                    let firstFailure =
                        match firstFinalizeResult with
                        | PartiallySucceeded(_, failure) -> failure
                        | Failed failure ->
                            failwith $"Canceled finalize did not partially succeed: {failure.Code}"
                        | Succeeded _ -> failwith "Canceled finalize unexpectedly succeeded."

                    Vitest.expect(firstFailure.Category).toEqual FailureCategory.Canceled
                    Vitest.expect(firstFailure.StateChanged).toBe true
                    Vitest.expect(firstFailure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_materialization")

                    let! branchAfterResult =
                        LakeFsApi.getBranch
                            (connection ())
                            beforeIndex.Repository
                            beforeIndex.WorkspaceBranch
                            (OperationContext.detached "finalize-cancel-after-merge-head-after")
                        |> Async.StartAsPromise

                    let branchAfter = branchAfterResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(branchAfter.CommitId).not.toEqual branchBefore.CommitId

                    let! activeAfterFailureResult =
                        conflicts.GetActiveSession(OperationContext.detached "finalize-cancel-after-merge-session-after")
                        |> Async.StartAsPromise

                    let activeAfterFailure =
                        expectValue "finalize cancel after merge session after" activeAfterFailureResult
                        |> Option.defaultWith (fun () -> failwith "The conflict session closed after canceled finalize.")

                    let! retryStatusResult =
                        session.Core.GetStatus(OperationContext.detached "finalize-cancel-after-merge-retry-status")
                        |> Async.StartAsPromise

                    let retryStatus = expectValue "finalize cancel after merge retry status" retryStatusResult
                    let! retryResult =
                        conflicts.Finalize
                            {
                                Handle = activeAfterFailure.Handle
                                ExpectedWorkspaceVersion = retryStatus.WorkspaceVersion
                                Message = Some "retry finalize reviewed write set"
                            }
                            (OperationContext.detached "finalize-cancel-after-merge-retry")
                        |> Async.StartAsPromise

                    expectValue "finalize cancel after merge retry" retryResult |> ignore
                    let! targetUnrelated = workspace.ReadFile "target-unrelated.txt"
                    Vitest.expect(targetUnrelated).toEqual(Some "target unrelated content\n")

                    let afterIndex =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index after conflict finalize."

                    Vitest.expect(afterIndex.BaseRevision).toEqual(Some target.CommitId)

                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS conflict finalize without a changed resolution takes the merge commit",
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
                            { Path = "conflict.txt"; Content = Some "target conflict content\n" }
                            { Path = "target-unrelated.txt"; Content = Some "target unrelated content\n" }
                        |]

                    do! workspace.WriteFile "conflict.txt" "local conflict content\n"
                    do! workspace.WriteFile "dirty-unrelated.txt" "dirty local content\n"

                    // The local edit is committed, so the destination-wins merge already holds it and the
                    // workspace pick changes nothing on the branch.
                    let! revisionStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-merge-commit-revision-status")
                        |> Async.StartAsPromise

                    let revisionStatus = expectValue "finalize merge commit revision status" revisionStatusResult

                    let! revisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: local conflict content"
                                Paths = [| repositoryPath "conflict.txt" |]
                                ExpectedWorkspaceVersion = revisionStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "finalize-merge-commit-revision")
                        |> Async.StartAsPromise

                    expectValue "finalize merge commit revision" revisionResult |> ignore

                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let! targetResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            parsed.TargetRef
                            (OperationContext.detached "finalize-merge-commit-target")
                        |> Async.StartAsPromise

                    let target = targetResult |> Result.defaultWith (fun failure -> failwith failure.Message)
                    let! previewResult =
                        synchronization.PreviewUpdate(OperationContext.detached "finalize-merge-commit-preview")
                        |> Async.StartAsPromise

                    let preview = expectValue "finalize merge commit preview" previewResult
                    Vitest.expect(preview.WouldCreateConflictSession).toBe true

                    let! updateStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-merge-commit-update-status")
                        |> Async.StartAsPromise

                    let updateStatus = expectValue "finalize merge commit update status" updateStatusResult
                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                            (OperationContext.detached "finalize-merge-commit-update")
                        |> Async.StartAsPromise

                    let updateFailure = expectFailure "finalize merge commit update" updateResult
                    Vitest.expect(updateFailure.Category).toEqual FailureCategory.Conflict

                    let conflicts =
                        workspace.Session.ConflictResolution
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS conflict-resolution services.")

                    let! sessionResult =
                        conflicts.GetActiveSession(OperationContext.detached "finalize-merge-commit-session")
                        |> Async.StartAsPromise

                    let summary =
                        expectValue "finalize merge commit session" sessionResult
                        |> Option.defaultWith (fun () -> failwith "Expected an active conflict session.")

                    let! resolutionStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-merge-commit-resolution-status")
                        |> Async.StartAsPromise

                    let resolutionStatus = expectValue "finalize merge commit resolution status" resolutionStatusResult
                    let! resolutionResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = resolutionStatus.WorkspaceVersion
                                Path = repositoryPath "conflict.txt"
                                Resolution = PickCandidate "workspace"
                            }
                            (OperationContext.detached "finalize-merge-commit-resolution")
                        |> Async.StartAsPromise

                    let resolution = expectValue "finalize merge commit resolution" resolutionResult
                    let! finalizeStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "finalize-merge-commit-finalize-status")
                        |> Async.StartAsPromise

                    let finalizeStatus = expectValue "finalize merge commit finalize status" finalizeStatusResult
                    let! finalizeResult =
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                Message = Some "finalize unchanged resolution"
                            }
                            (OperationContext.detached "finalize-merge-commit-finalize")
                        |> Async.StartAsPromise

                    let finalizedRevision =
                        match finalizeResult with
                        | Succeeded outcome ->
                            outcome.Value
                            |> Option.defaultWith (fun () -> failwith "Expected finalize to return a revision.")
                        | PartiallySucceeded(_, failure)
                        | Failed failure ->
                            failwith $"Finalize did not succeed ({failure.Category}/{failure.Code}): {failure.Message}"

                    let! conflictContent = workspace.ReadFile "conflict.txt"
                    let! unrelatedTarget = workspace.ReadFile "target-unrelated.txt"
                    Vitest.expect(conflictContent).toEqual(Some "local conflict content\n")
                    Vitest.expect(unrelatedTarget).toEqual(Some "target unrelated content\n")

                    let afterIndex =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected an index after the conflict finalize."

                    let! workspaceBranchResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            afterIndex.WorkspaceBranch
                            (OperationContext.detached "finalize-merge-commit-workspace-head")
                        |> Async.StartAsPromise

                    let workspaceBranch =
                        workspaceBranchResult |> Result.defaultWith (fun failure -> failwith failure.Message)

                    Vitest.expect(workspaceBranch.CommitId).toBe (RevisionId.value finalizedRevision)

                    let! finalizedCommitResult =
                        LakeFsApi.getCommit
                            (connection ())
                            parsed.Repository
                            (RevisionId.value finalizedRevision)
                            (OperationContext.detached "finalize-merge-commit-commit")
                        |> Async.StartAsPromise

                    let finalizedCommit =
                        finalizedCommitResult |> Result.defaultWith (fun failure -> failwith failure.Message)

                    Vitest.expect(finalizedCommit.Parents).toContain target.CommitId
                    Vitest.expect(afterIndex.BaseRevision).toEqual(Some target.CommitId)
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS synchronization reports missing base state without inventing remote paths",
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

                    let index =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded value -> value
                        | _ -> failwith "Expected an index before removing the synchronized base."

                    match
                        LakeFsWorkspaceIndex.save
                            (stateDirectoryForBinding workspace.Binding)
                            { index with BaseRevision = None }
                    with
                    | Error message -> failwith message
                    | Ok _ -> ()

                    let! statusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "missing-base-status")
                        |> Async.StartAsPromise

                    let status = expectValue "missing base status" statusResult
                    let! previewResult =
                        synchronization.PreviewUpdate(OperationContext.detached "missing-base-preview")
                        |> Async.StartAsPromise

                    let previewFailure = expectFailure "missing base preview" previewResult
                    Vitest.expect(previewFailure.Code).toBe ("preview_indeterminate")
                    Vitest.expect(previewFailure.Message).toContain ("no synchronized base revision")

                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (OperationContext.detached "missing-base-update")
                        |> Async.StartAsPromise

                    let updateFailure = expectFailure "missing base update" updateResult
                    Vitest.expect(updateFailure.Code).toBe ("preview_indeterminate")
                    Vitest.expect(updateFailure.Message).toContain ("no synchronized base revision")

                    let! refreshResult =
                        synchronization.Refresh(OperationContext.detached "missing-base-refresh")
                        |> Async.StartAsPromise

                    let refreshed = expectValue "missing base refresh" refreshResult
                    Vitest.expect(refreshed.RemoteChangedPaths).toEqual None
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )

        Vitest.test (
            "lakeFS update reports the merged branch head after post-merge materialization failure",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()
                let mutable preparedObjects = 0

                try
                    let! workspace = harness.CreateWorkspace()
                    do! workspace.WriteFile "local-before-merge.txt" "local commit content\n"

                    let! localStatusResult =
                        workspace.Session.Core.GetStatus(OperationContext.detached "post-merge-local-status")
                        |> Async.StartAsPromise

                    let localStatus = expectValue "post-merge local status" localStatusResult
                    let! localRevisionResult =
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "test: local revision before update merge"
                                Paths = [| repositoryPath "local-before-merge.txt" |]
                                ExpectedWorkspaceVersion = localStatus.WorkspaceVersion
                            }
                            (OperationContext.detached "post-merge-local-revision")
                        |> Async.StartAsPromise

                    expectValue "post-merge local revision" localRevisionResult |> ignore

                    do!
                        harness.AdvanceTarget workspace [|
                            { Path = "post-merge-local-update.txt"; Content = Some "post-merge local update\n" }
                        |]

                    let parsed =
                        LakeFsTypes.LakeFsLocation.tryParse workspace.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let hooks: LakeFsWorkspaceSession.LakeFsSessionHooks = {
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "materialization-prepare-object" then
                                    preparedObjects <- preparedObjects + 1

                                    if preparedObjects = 1 then
                                        failwith "injected post-merge materialization failure after local commit"
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
                            (OperationContext.detached "post-merge-local-materialization-open")
                        |> Async.StartAsPromise

                    let session = expectValue "post-merge local materialization open" opened
                    let synchronization =
                        session.Synchronization
                        |> Option.defaultWith (fun () -> failwith "Expected lakeFS synchronization services.")

                    let! previewResult =
                        synchronization.PreviewUpdate(OperationContext.detached "post-merge-local-materialization-preview")
                        |> Async.StartAsPromise

                    let preview = expectValue "post-merge local materialization preview" previewResult
                    Vitest.expect(preview.HasDataLossRisk).toBe false
                    Vitest.expect(preview.WouldCreateConflictSession).toBe false

                    let! statusResult =
                        session.Core.GetStatus(OperationContext.detached "post-merge-local-materialization-status")
                        |> Async.StartAsPromise

                    let status = expectValue "post-merge local materialization status" statusResult
                    let! updateResult =
                        synchronization.Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (OperationContext.detached "post-merge-local-materialization-update")
                        |> Async.StartAsPromise

                    let outcome, failure =
                        match updateResult with
                        | PartiallySucceeded(outcome, failure) -> outcome, failure
                        | Failed failure ->
                            failwith $"Post-merge local materialization failure was reported as Failed: {failure.Code}"
                        | Succeeded _ -> failwith "Post-merge local materialization failure unexpectedly succeeded."

                    let index =
                        match LakeFsWorkspaceIndex.load (stateDirectoryForBinding workspace.Binding) with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected a persisted index after the merged update."

                    let! mergedBranchResult =
                        LakeFsApi.getBranch
                            (connection ())
                            parsed.Repository
                            index.WorkspaceBranch
                            (OperationContext.detached "post-merge-local-materialization-merged-head")
                        |> Async.StartAsPromise

                    let mergedBranch = mergedBranchResult |> Result.defaultWith (fun error -> failwith error.Message)
                    Vitest.expect(preparedObjects).toBe 1
                    Vitest.expect(failure.StateChanged).toBe true
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_materialization")
                    Vitest.expect(outcome.Value.WorkspaceRevision |> Option.map RevisionId.value).toEqual (Some mergedBranch.CommitId)
                    Vitest.expect(outcome.Value.Relationship).toEqual RevisionRelationship.LocalAhead

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
