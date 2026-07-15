module VersionControlService.Abstractions.Tests.PortableConsumerTests

open System.IO
open Expecto
open VersionControlService.Abstractions
open VersionControlService.Abstractions.Tests.ContractShapeTests

let private fakeProviderId = FakeProvider.providerId "fake.portable"

/// A core whose diff summary is a 50-step long operation that reports progress each
/// step and observes cancellation between steps.
let private createLongOperationCore () : CoreVersionControl =
    let baseCore = FakeProvider.createCore ()

    {
        baseCore with
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
                            Completed = Some(float step)
                            Total = Some 50.0
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
    FakeProvider.createFactory fakeProviderId (fun descriptor ->
        WorkspaceSession.createCoreOnly descriptor (createLongOperationCore ()))

let private expectSucceeded (operationName: string) (result: OperationResult<'T>) : 'T =
    match result with
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded _ -> failtest $"{operationName} unexpectedly returned partial success."
    | Failed failure -> failtest $"{operationName} failed ({failure.Category}/{failure.Code}): {failure.Message}"

let private openFakeSession (factory: ProviderFactory) (context: OperationContext) =
    async {
        let! bindResult =
            factory.Bind
                {
                    WorkspaceRoot = "/fake/workspace"
                    Location = FakeProvider.fakeLocation factory.Id
                }
                context

        let binding = expectSucceeded "bind" bindResult
        let! openResult = factory.Open binding context
        return expectSucceeded "open session" openResult
    }

let private findRepositoryRoot () =
    let rec walkUp (directory: DirectoryInfo) =
        if isNull directory then
            failtest "Could not locate the repository root from the test base directory."
        elif File.Exists(Path.Combine(directory.FullName, "VersionControlService.slnx")) then
            directory.FullName
        else
            walkUp directory.Parent

    walkUp (DirectoryInfo(System.AppContext.BaseDirectory))

[<Tests>]
let portableConsumerTests =
    testList "PortableConsumer" [
        testCaseAsync "a .NET consumer awaits one core operation through the Async carrier"
        <| async {
            let factory = createFakeFactory ()
            let context = OperationContext.detached "portable-open"

            let! session = openFakeSession factory context
            let! statusResult = session.Core.GetStatus context
            let status = expectSucceeded "get status" statusResult

            Expect.equal status.WorkspaceVersion "fake-v1" "The fake session returns its workspace version."
        }

        testCaseAsync "cancellation reaches and stops a fake long operation in .NET"
        <| async {
            let factory = createFakeFactory ()
            let source = OperationCancellation.Source()
            let mutable lastCompleted = 0.0

            // Deterministic cancellation: cancel from the progress callback at step 3.
            let context =
                OperationContext.create "portable-cancel" source.Cancellation (fun progress ->
                    match progress.Completed with
                    | Some completed ->
                        lastCompleted <- completed

                        if completed = 3.0 then
                            source.Cancel()
                    | None -> ())

            let! session = openFakeSession factory context
            let! diffResult = session.Core.GetDiffSummary context

            match diffResult with
            | Failed failure ->
                Expect.equal failure.Category Canceled "Cancellation produces a structured canceled failure."
                Expect.equal failure.Code "operation_canceled" "The canceled failure carries its stable code."
                Expect.isFalse failure.StateChanged "The fake operation changed no state."
            | Succeeded _
            | PartiallySucceeded _ -> failtest "Expected the canceled operation to fail."

            Expect.isLessThan lastCompleted 50 "The long operation stopped early."
        }

        testCaseAsync "progress callbacks flow through the operation context in .NET"
        <| async {
            let factory = createFakeFactory ()
            let progressPhases = ResizeArray<string>()

            let context =
                OperationContext.create "portable-progress" OperationCancellation.none (fun progress ->
                    progressPhases.Add progress.PhaseCode)

            let! session = openFakeSession factory context
            let! diffResult = session.Core.GetDiffSummary context
            expectSucceeded "get diff summary" diffResult |> ignore

            Expect.equal progressPhases.Count 50 "Every step reported progress."
            Expect.allEqual progressPhases "fake-step" "All progress reports carry the stable phase code."
        }

        testCase "the abstractions project references no Fable, Node, SimpleGit, Git, lakeFS, or Swate dependency"
        <| fun () ->
            let repositoryRoot = findRepositoryRoot ()

            let abstractionsProject =
                Path.Combine(
                    repositoryRoot,
                    "src",
                    "VersionControlService.Abstractions",
                    "VersionControlService.Abstractions.fsproj"
                )

            let projectXml = File.ReadAllText abstractionsProject

            let forbiddenReferences = [
                "Fable.Core"
                "Fable.Promise"
                "SimpleGit"
                "simple-git"
                "Node"
                "Swate"
                "LakeFs"
                "ARCtrl"
                "YAMLicious"
                "ProjectReference"
            ]

            for forbidden in forbiddenReferences do
                Expect.isFalse
                    (projectXml.Contains forbidden)
                    $"The abstractions project must not reference '{forbidden}'."
    ]
