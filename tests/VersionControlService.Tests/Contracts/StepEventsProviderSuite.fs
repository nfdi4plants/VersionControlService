module VersionControlService.Tests.Contracts.StepEventsProviderSuite

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.Contracts.ProviderHarness.SuiteHelpers
open Vitest

/// Step events profile: the steps that every provider with synchronization announces, with the phase codes of
/// `ProgressPhaseCodes`. The profile registers for the real providers only, because the in-memory fake reports no
/// progress. It checks the three steps a provider always runs. The optional steps depend on the engine and have
/// provider tests of their own. It arms no synthetic transfer progress, so every event comes from real work.
let register (harness: ProviderTestHarness) : string * (unit -> int) =
    let profileName = $"{harness.Name} / step events profile"
    let mutable testCount = 0

    Vitest.describe (
        profileName,
        fun () ->
            let profileTest (name: string) (body: unit -> JS.Promise<unit>) =
                testCount <- testCount + 1
                Vitest.test (name, suiteTestOptions, body)

            let commonPhases = [|
                ProgressPhaseCodes.Refresh
                ProgressPhaseCodes.Update
                ProgressPhaseCodes.Clone
            |]

            // A step start has no numbers and names the step. A meter carries numbers and a reset carries no message.
            let stepStartsOf (reports: ResizeArray<OperationProgress>) =
                reports
                |> Seq.filter (fun report ->
                    Array.contains report.PhaseCode commonPhases
                    && report.Item.IsNone
                    && report.Completed.IsNone
                    && report.Total.IsNone
                    && report.DisplayMessage.IsSome
                )
                |> Seq.toArray

            // The phases are compared without removing duplicates, so a step that starts twice fails the check.
            let expectStepStarts (expected: string[]) (reports: ResizeArray<OperationProgress>) =
                let starts = stepStartsOf reports
                Vitest.expect(starts |> Array.map _.PhaseCode).toEqual expected

                for start in starts do
                    Vitest.expect(start.Item).toEqual None
                    Vitest.expect(start.Completed).toEqual None
                    Vitest.expect(start.Total).toEqual None
                    Vitest.expect(start.DisplayMessage.Value.Trim().Length > 0).toBe true
                    assertRedacted start.DisplayMessage.Value

            let capture (name: string) =
                let reports = ResizeArray<OperationProgress>()
                reports, OperationContext.create name OperationCancellation.none reports.Add

            profileTest "a refresh reports one fetch step start"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let reports, context = capture "step-events-refresh"

                let! result = run ((syncService workspace.Session).Refresh context)

                expectOutcome "refresh with step events" result |> ignore
                expectStepStarts [| ProgressPhaseCodes.Refresh |] reports
            }

            profileTest "a synchronize of a workspace behind its target without publishing reports fetch then merge"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                do!
                    harness.AdvanceTarget workspace [|
                        {
                            Path = "ahead.txt"
                            Content = Some "ahead content\n"
                        }
                    |]

                let! status = getStatus workspace
                let reports, context = capture "step-events-synchronize"

                let! result =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = false
                            }
                            context
                    )

                expectPerformed "synchronize with step events" result |> ignore
                expectStepStarts [| ProgressPhaseCodes.Refresh; ProgressPhaseCodes.Update |] reports
            }

            profileTest "a synchronize of an up-to-date workspace reports the fetch only"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let! status = getStatus workspace
                let reports, context = capture "step-events-up-to-date"

                let! result =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = false
                            }
                            context
                    )

                expectNoOp "synchronize of an up-to-date workspace" result |> ignore
                expectStepStarts [| ProgressPhaseCodes.Refresh |] reports
            }

            profileTest "a clone reports one clone step start"
            <| fun () -> promise {
                let! anchor = harness.CreateWorkspace()
                let! clonePath = harness.CreateLocalPath()
                let reports, context = capture "step-events-clone"

                // The clone does not materialize large objects, so a provider with a large-object engine adds no step
                // of its own to the common ones.
                let! result =
                    run (
                        harness.Factory.Clone
                            {
                                Location = anchor.Binding.Location
                                TargetPath = clonePath
                                TargetRef = None
                                MaterializeAllObjects = false
                            }
                            context
                    )

                expectValue "clone with step events" result |> ignore
                expectStepStarts [| ProgressPhaseCodes.Clone |] reports
            }
    )

    // Vitest defers describe callbacks to the collection phase, so expose the
    // registered-test count as a getter evaluated at test time.
    profileName, (fun () -> testCount)
