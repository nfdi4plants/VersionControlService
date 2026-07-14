module VersionControlService.Tests.LakeFsIntegrationTests

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.LakeFs
open VersionControlService.Tests.LakeFsProviderContractTests
open Vitest

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
            "lakeFS creates a unique owned server-visible workspace branch",
            TestOptions(timeout = 120000, skip = not (integrationEnabled ())),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! first = harness.CreateWorkspace()
                    let! second = harness.CreateLinkedWorkspace first

                    let firstIndex = LakeFsWorkspaceIndex.load first.Binding.WorkspaceRoot
                    let secondIndex = LakeFsWorkspaceIndex.load second.Binding.WorkspaceRoot

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
                        match LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
                        match LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
                        match LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
                        match LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
                        match LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
                        match LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
                        match LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
                        match LakeFsWorkspaceIndex.load workspace.Binding.WorkspaceRoot with
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
)
