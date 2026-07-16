module VersionControlService.Tests.Contracts.OperationalProviderSuite

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.Contracts.ProviderHarness.SuiteHelpers
open Vitest

/// Operational profile: progress redaction, cancellation, concurrent sessions,
/// in-process mutation serialization, and interruption recovery.
let register (harness: ProviderTestHarness) : string * (unit -> int) =
    let profileName = $"{harness.Name} / operational profile"
    let mutable testCount = 0

    Vitest.describe (
        profileName,
        fun () ->
            let profileTest (name: string) (body: unit -> JS.Promise<unit>) =
                testCount <- testCount + 1
                Vitest.test (name, suiteTestOptions, body)

            profileTest "transfer progress carries stable phases and redacted messages"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! harness.ArmSlowTransfer workspace

                do!
                    harness.AdvanceTarget workspace [|
                        {
                            Path = "progress.txt"
                            Content = Some "progress content\n"
                        }
                    |]

                let! status = getStatus workspace
                let progressReports = ResizeArray<OperationProgress>()

                let context =
                    OperationContext.create "operational-progress" OperationCancellation.none progressReports.Add

                let! updateResult =
                    run ((syncService workspace.Session).Update { ExpectedWorkspaceVersion = status.WorkspaceVersion } context)

                expectOutcome "update with progress" updateResult |> ignore
                Vitest.expect(progressReports.Count > 0).toBe (true)

                for report in progressReports do
                    Vitest.expect(report.PhaseCode.Length > 0).toBe (true)

                    match report.DisplayMessage with
                    | Some message -> assertRedacted message
                    | None -> ()
            }

            profileTest "progress preserves byte totals above two GiB"
            <| fun () -> promise {
                let reports = ResizeArray<OperationProgress>()
                let context =
                    OperationContext.create "large-byte-progress" OperationCancellation.none reports.Add

                let completedBytes = 3.0 * 1024.0 * 1024.0 * 1024.0
                let totalBytes = 4.0 * 1024.0 * 1024.0 * 1024.0

                context.ReportProgress {
                    PhaseCode = "transfer-bytes"
                    Item = Some "literal[object]*?.bin"
                    Completed = Some completedBytes
                    Total = Some totalBytes
                    DisplayMessage = None
                }

                Vitest.expect(reports[0].Completed).toEqual (Some completedBytes)
                Vitest.expect(reports[0].Total).toEqual (Some totalBytes)
            }

            profileTest "transfer operations can be canceled with structured results"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! harness.ArmSlowTransfer workspace

                do!
                    harness.AdvanceTarget workspace [|
                        {
                            Path = "canceled.txt"
                            Content = Some "canceled content\n"
                        }
                    |]

                let! status = getStatus workspace
                let source = OperationCancellation.Source()

                let context =
                    OperationContext.create "operational-cancel" source.Cancellation (fun progress ->
                        match progress.Completed with
                        | Some completed when completed = 3 -> source.Cancel()
                        | _ -> ())

                let! updateResult =
                    run ((syncService workspace.Session).Update { ExpectedWorkspaceVersion = status.WorkspaceVersion } context)

                let failure = expectFailure "canceled update" updateResult
                Vitest.expect(failure.Category).toEqual (Canceled)
                Vitest.expect(failure.StateChanged).toBe (false)

                // The workspace stayed unchanged.
                let! afterStatus = getStatus workspace
                Vitest.expect(afterStatus.WorkspaceVersion).toBe (status.WorkspaceVersion)

                let! canceledFile = workspace.ReadFile "canceled.txt"
                Vitest.expect(canceledFile).toEqual (None)
            }

            profileTest "two sessions on different workspaces stay independent"
            <| fun () -> promise {
                let! firstWorkspace = harness.CreateWorkspace()
                let! secondWorkspace = harness.CreateWorkspace()

                do! firstWorkspace.WriteFile "first-only.txt" "first content\n"

                let! firstStatus = getStatus firstWorkspace
                let! secondStatus = getStatus secondWorkspace

                Vitest.expect(changePaths firstStatus).toEqual ([| "first-only.txt" |])
                Vitest.expect(secondStatus.Changes.Length).toBe (0)

                let! revisionResult =
                    run (
                        firstWorkspace.Session.Core.CreateRevision
                            {
                                Message = "first workspace revision"
                                Paths = [| mkPath "first-only.txt" |]
                                ExpectedWorkspaceVersion = firstStatus.WorkspaceVersion
                            }
                            (ctx "independent-revision")
                    )

                expectPerformed "first workspace revision" revisionResult |> ignore

                let! secondAfter = getStatus secondWorkspace
                Vitest.expect(secondAfter.WorkspaceVersion).toBe (secondStatus.WorkspaceVersion)
            }

            profileTest "two sessions on the same workspace serialize mutations through version tokens"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let! secondSession = harness.OpenSecondSession workspace

                do! workspace.WriteFile "first.txt" "first content\n"
                do! workspace.WriteFile "second.txt" "second content\n"

                let! firstStatus = getStatus workspace
                let! secondStatus = sessionStatus secondSession
                Vitest.expect(secondStatus.WorkspaceVersion).toBe (firstStatus.WorkspaceVersion)

                let! firstMutation =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "first session revision"
                                Paths = [| mkPath "first.txt" |]
                                ExpectedWorkspaceVersion = firstStatus.WorkspaceVersion
                            }
                            (ctx "first-session-revision")
                    )

                expectPerformed "first session mutation" firstMutation |> ignore

                // The second session's token is now stale; the mutation is rejected.
                let! staleMutation =
                    run (
                        secondSession.Core.CreateRevision
                            {
                                Message = "second session stale revision"
                                Paths = [| mkPath "second.txt" |]
                                ExpectedWorkspaceVersion = secondStatus.WorkspaceVersion
                            }
                            (ctx "stale-session-revision")
                    )

                let failure = expectFailure "stale second-session mutation" staleMutation
                Vitest.expect(failure.Category).toEqual (Concurrency)
                Vitest.expect(failure.Code).toBe (ConformanceCodes.PreconditionFailed)

                // Refreshing the token lets the second session proceed.
                let! refreshedStatus = sessionStatus secondSession

                let! retriedMutation =
                    run (
                        secondSession.Core.CreateRevision
                            {
                                Message = "second session retried revision"
                                Paths = [| mkPath "second.txt" |]
                                ExpectedWorkspaceVersion = refreshedStatus.WorkspaceVersion
                            }
                            (ctx "retried-session-revision")
                    )

                expectPerformed "retried second-session mutation" retriedMutation |> ignore
            }

            profileTest "an interrupted mutation cleans transient state and can be retried"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! workspace.WriteFile "interrupted.txt" "interrupted content\n"

                do! harness.ArmSlowTransfer workspace
                let! status = getStatus workspace
                let source = OperationCancellation.Source()

                let context =
                    OperationContext.create "operational-interrupt" source.Cancellation (fun progress ->
                        match progress.Completed with
                        | Some completed when completed = 2 -> source.Cancel()
                        | _ -> ())

                let! interruptedResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "interrupted revision"
                                Paths = [| mkPath "interrupted.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            context
                    )

                let failure = expectFailure "interrupted revision" interruptedResult
                Vitest.expect(failure.Category).toEqual (Canceled)
                Vitest.expect(failure.StateChanged).toBe (false)

                let! afterStatus = getStatus workspace
                Vitest.expect(afterStatus.WorkspaceVersion).toBe (status.WorkspaceVersion)
                Vitest.expect(changePaths afterStatus).toEqual ([| "interrupted.txt" |])

                // The retry succeeds cleanly.
                let! retriedResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "retried revision"
                                Paths = [| mkPath "interrupted.txt" |]
                                ExpectedWorkspaceVersion = afterStatus.WorkspaceVersion
                            }
                            (ctx "retried-after-interrupt")
                    )

                expectPerformed "retried revision" retriedResult |> ignore
            }

            profileTest "failures and serialized bindings never leak credential material"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                do! harness.BreakPublish workspace
                do! workspace.WriteFile "leak-probe.txt" "content\n"
                let! status = getStatus workspace

                let! revisionResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "revision before broken publish"
                                Paths = [| mkPath "leak-probe.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "leak-revision")
                    )

                expectPerformed "revision" revisionResult |> ignore
                let! prePublish = getStatus workspace

                let! publishResult =
                    run (
                        (syncService workspace.Session).Publish
                            {
                                ExpectedWorkspaceVersion = prePublish.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "leak-publish")
                    )

                let failure = expectFailure "broken publish" publishResult
                assertRedacted failure.Message

                for detail in failure.Details do
                    assertRedacted detail

                // The binding itself is serializable without secrets.
                assertRedacted workspace.Binding.Location.ProviderLocation
                assertRedacted (workspace.Binding.ConnectionProfileId |> Option.defaultValue "")

                do! harness.RestorePublish workspace
            }
    )

    // Vitest defers describe callbacks to the collection phase, so expose the
    // registered-test count as a getter evaluated at test time.
    profileName, (fun () -> testCount)
