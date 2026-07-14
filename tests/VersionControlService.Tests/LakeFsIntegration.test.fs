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
            TestOptions(timeout = 120000),
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
            TestOptions(timeout = 120000),
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
)
