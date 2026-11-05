module VersionControlService.Tests.Contracts.OutputProviderSuite

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.Contracts.ProviderHarness.SuiteHelpers
open Vitest

/// Output profile: the raw output channel of the external calls an operation makes. The profile registers for the
/// real providers only, because the in-memory fake makes no external call and reports no output. `secrets` are the
/// credential values the harness configured, and no entry may contain one. A harness without credentials passes an
/// empty array, and the structural redaction check still runs on every entry.
let register (harness: ProviderTestHarness) (secrets: string[]) : string * (unit -> int) =
    let profileName = $"{harness.Name} / output profile"
    let mutable testCount = 0

    Vitest.describe (
        profileName,
        fun () ->
            let profileTest (name: string) (body: unit -> JS.Promise<unit>) =
                testCount <- testCount + 1
                Vitest.test (name, suiteTestOptions, body)

            let capture (name: string) =
                let batches = ResizeArray<OperationOutput[]>()

                let context =
                    OperationContext.createWithOutput name OperationCancellation.none ignore batches.Add

                batches, context

            let entriesOf (batches: ResizeArray<OperationOutput[]>) =
                batches |> Seq.collect id |> Seq.toArray

            let isCommand (entry: OperationOutput) =
                entry.Kind = OperationOutputKind.Command

            // Every call has a Command header with its own id, and the follow-up entries of the call carry that id
            // after the header. A notice about the whole operation has an empty id and belongs to no call.
            let expectWellFormedOutput (entries: OperationOutput[]) =
                Vitest.expect(entries |> Array.exists isCommand).toBe true

                let headerIds = entries |> Array.filter isCommand |> Array.map _.CommandId

                for id in headerIds do
                    Vitest.expect(id.Length > 0).toBe true

                Vitest.expect(headerIds |> Array.distinct).toEqual headerIds

                entries
                |> Array.iteri (fun index entry ->
                    if not (isCommand entry) && entry.CommandId.Length > 0 then
                        let headerIndex =
                            entries
                            |> Array.tryFindIndex (fun other -> isCommand other && other.CommandId = entry.CommandId)

                        Vitest.expect(headerIndex.IsSome).toBe true
                        Vitest.expect(headerIndex.Value < index).toBe true
                )

                for entry in entries do
                    assertRedacted entry.Text

                    for secret in secrets do
                        Vitest.expect(entry.Text.Contains secret).toBe false

            profileTest "a refresh with an output callback reports its external calls"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let batches, context = capture "output-refresh"

                let! result = run ((syncService workspace.Session).Refresh context)

                expectOutcome "refresh with output" result |> ignore
                expectWellFormedOutput (entriesOf batches)
            }

            profileTest "a revision creation with an output callback reports its external calls"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! workspace.WriteFile "output.txt" "output content\n"
                let! status = getStatus workspace
                let batches, context = capture "output-revision"

                let! result =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "revision with output"
                                Paths = [| mkPath "output.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            context
                    )

                expectPerformed "revision with output" result |> ignore
                expectWellFormedOutput (entriesOf batches)
            }
    )

    // Vitest defers describe callbacks to the collection phase, so expose the
    // registered-test count as a getter evaluated at test time.
    profileName, (fun () -> testCount)
