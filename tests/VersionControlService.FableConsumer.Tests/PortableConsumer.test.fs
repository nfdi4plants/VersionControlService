module VersionControlService.FableConsumer.Tests.PortableConsumer

open Fable.Core
open VersionControlService.Abstractions
open Vitest

let private fakeProviderId =
    match ProviderId.tryCreate "fake.portable" with
    | Ok providerId -> providerId
    | Error message -> failwith message

let private fakeLocation: RepositoryLocation = {
    ProviderId = fakeProviderId
    DisplayName = None
    ProviderLocation = "fake://repository"
    ConnectionProfileId = None
}

/// Core whose diff summary is a 50-step long operation reporting progress each step
/// and observing cancellation between steps.
let private createFakeCore () : CoreVersionControl =
    let status = {
        CurrentRef = None
        WorkspaceVersion = "fake-v1"
        Changes = [||]
        ActiveConflictSession = None
        Synchronization = None
    }

    let notSupported (name: string) =
        OperationResult.failed (OperationFailure.create Unsupported "fake_not_exercised" $"{name} is not exercised.")

    {
        GetStatus = fun _ -> async { return OperationResult.succeeded status }
        ListRefs = fun _ -> async { return OperationResult.succeeded [||] }
        CreateRef = fun _ _ -> async { return notSupported "CreateRef" }
        PreflightSwitchRef = fun _ _ -> async { return notSupported "PreflightSwitchRef" }
        SwitchRef = fun _ _ -> async { return notSupported "SwitchRef" }
        CreateRevision = fun _ _ -> async { return notSupported "CreateRevision" }
        RestorePaths = fun _ _ -> async { return notSupported "RestorePaths" }
        GetDiffSummary =
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
                    return OperationResult.succeeded { Entries = [||] }
            }
    }

let private createFakeFactory () : ProviderFactory =
    let binding workspaceRoot location = {
        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
        ProviderId = fakeProviderId
        WorkspaceRoot = workspaceRoot
        ProviderStateRef = None
        Location = location
        ConnectionProfileId = None
    }

    {
        Id = fakeProviderId
        Probe = fun _ -> async { return NotDetected }
        VerifyLocation =
            fun request _ -> async {
                return
                    OperationResult.succeeded {
                        Location = request.Location
                        GrantedIntents = request.Intents
                        DeniedIntents = [||]
                    }
            }
        Initialize =
            fun request _ -> async {
                let location = request.Location |> Option.defaultValue fakeLocation
                return OperationResult.succeeded (binding request.TargetPath location)
            }
        Clone = fun request _ -> async { return OperationResult.succeeded (binding request.TargetPath request.Location) }
        Bind =
            fun request _ -> async { return OperationResult.succeeded (binding request.WorkspaceRoot request.Location) }
        Open =
            fun workspaceBinding _ -> async {
                let descriptor = {
                    ProviderId = workspaceBinding.ProviderId
                    WorkspaceRoot = workspaceBinding.WorkspaceRoot
                    Location = Some workspaceBinding.Location
                }

                return OperationResult.succeeded (WorkspaceSession.createCoreOnly descriptor (createFakeCore ()))
            }
        CheckDependencies = fun _ -> async { return OperationResult.succeeded [||] }
    }

let private expectSucceeded (operationName: string) (result: OperationResult<'T>) : 'T =
    match result with
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded _ -> failwith $"{operationName} unexpectedly returned partial success."
    | Failed failure -> failwith $"{operationName} failed ({failure.Category}/{failure.Code}): {failure.Message}"

let private openFakeSession (factory: ProviderFactory) (context: OperationContext) =
    async {
        let! bindResult =
            factory.Bind
                {
                    WorkspaceRoot = "/fake/workspace"
                    Location = fakeLocation
                }
                context

        let workspaceBinding = expectSucceeded "bind" bindResult
        let! openResult = factory.Open workspaceBinding context
        return expectSucceeded "open session" openResult
    }

Vitest.describe (
    "Portable abstractions consumer (Fable)",
    fun () ->
        Vitest.test (
            "a Fable consumer awaits one core operation through the Async carrier",
            fun () -> promise {
                let workflow = async {
                    let factory = createFakeFactory ()
                    let context = OperationContext.detached "portable-open"

                    let! session = openFakeSession factory context
                    let! statusResult = session.Core.GetStatus context
                    return expectSucceeded "get status" statusResult
                }

                let! status = Async.StartAsPromise workflow
                Vitest.expect(status.WorkspaceVersion).toBe ("fake-v1")
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
                    let! session = openFakeSession factory context
                    return! session.Core.GetDiffSummary context
                }

                let! diffResult = Async.StartAsPromise workflow

                match diffResult with
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
                    let! session = openFakeSession factory context
                    return! session.Core.GetDiffSummary context
                }

                let! diffResult = Async.StartAsPromise workflow
                expectSucceeded "get diff summary" diffResult |> ignore

                Vitest.expect(progressPhases.Count).toBe (50)
                Vitest.expect(progressPhases |> Seq.forall (fun phase -> phase = "fake-step")).toBe (true)
            }
        )
)
