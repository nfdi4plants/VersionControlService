module VersionControlService.FableConsumer.Tests.PortableConsumer

open Fable.Core
open VersionControlService.Abstractions
open Vitest

let private fakeProviderId =
    match ProviderId.tryCreate "fake.portable" with
    | Ok providerId -> providerId
    | Error message -> failwith message

/// The same fake provider shape as the .NET consumer: a 50-step long operation that
/// reports progress each step and observes cancellation between steps.
let private createFakeFactory () : ProviderFactory =
    let createSession descriptor = {
        Descriptor = descriptor
        GetWorkspaceVersion =
            fun context -> async {
                let mutable canceled = false
                let mutable step = 0

                while not canceled && step < 50 do
                    do! Async.Sleep 1
                    step <- step + 1

                    context.ReportProgress {
                        PhaseCode = "fake-step"
                        Item = None
                        Completed = Some step
                        Total = Some 50
                        DisplayMessage = None
                    }

                    if context.Cancellation.IsCancellationRequested() then
                        canceled <- true

                if canceled then
                    return OperationResult.canceled $"The fake operation stopped after {step} steps."
                else
                    return OperationResult.succeeded "fake-version-1"
            }
    }

    {
        Id = fakeProviderId
        Open = fun descriptor _context -> async { return OperationResult.succeeded (createSession descriptor) }
    }

let private fakeDescriptor = {
    ProviderId = fakeProviderId
    WorkspaceRoot = "/fake/workspace"
    Location = None
}

let private expectSucceeded (operationName: string) (result: OperationResult<'T>) : 'T =
    match result with
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded _ -> failwith $"{operationName} unexpectedly returned partial success."
    | Failed failure -> failwith $"{operationName} failed ({failure.Category}/{failure.Code}): {failure.Message}"

Vitest.describe (
    "Portable abstractions consumer (Fable)",
    fun () ->
        Vitest.test (
            "a Fable consumer awaits one core operation through the Async carrier",
            fun () -> promise {
                let workflow = async {
                    let factory = createFakeFactory ()
                    let context = OperationContext.detached "portable-open"

                    let! openResult = factory.Open fakeDescriptor context
                    let session = expectSucceeded "open session" openResult

                    let! versionResult = session.GetWorkspaceVersion context
                    return expectSucceeded "get workspace version" versionResult
                }

                let! version = Async.StartAsPromise workflow
                Vitest.expect(version).toBe ("fake-version-1")
            }
        )

        Vitest.test (
            "cancellation reaches and stops a Promise-backed fake long operation",
            fun () -> promise {
                let factory = createFakeFactory ()
                let source = OperationCancellation.Source()
                let mutable lastCompleted = 0

                // Deterministic cancellation: cancel from the progress callback at step 3.
                let context =
                    OperationContext.create "portable-cancel" source.Cancellation (fun progress ->
                        match progress.Completed with
                        | Some completed ->
                            lastCompleted <- completed

                            if completed = 3 then
                                source.Cancel()
                        | None -> ())

                let workflow = async {
                    let! openResult = factory.Open fakeDescriptor context
                    let session = expectSucceeded "open session" openResult
                    return! session.GetWorkspaceVersion context
                }

                let! versionResult = Async.StartAsPromise workflow

                match versionResult with
                | Failed failure ->
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.Code).toBe ("operation_canceled")
                    Vitest.expect(failure.StateChanged).toBe (false)
                | Succeeded _
                | PartiallySucceeded _ -> failwith "Expected the canceled operation to fail."

                Vitest.expect(lastCompleted < 50).toBe (true)
            }
        )

        Vitest.test (
            "progress callbacks flow through the operation context in Fable",
            fun () -> promise {
                let factory = createFakeFactory ()
                let progressPhases = ResizeArray<string>()

                let context =
                    OperationContext.create "portable-progress" OperationCancellation.none (fun progress ->
                        progressPhases.Add progress.PhaseCode)

                let workflow = async {
                    let! openResult = factory.Open fakeDescriptor context
                    let session = expectSucceeded "open session" openResult
                    return! session.GetWorkspaceVersion context
                }

                let! versionResult = Async.StartAsPromise workflow
                expectSucceeded "get workspace version" versionResult |> ignore

                Vitest.expect(progressPhases.Count).toBe (50)
                Vitest.expect(progressPhases |> Seq.forall (fun phase -> phase = "fake-step")).toBe (true)
            }
        )
)
