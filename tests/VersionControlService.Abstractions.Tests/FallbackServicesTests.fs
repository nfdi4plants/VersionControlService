module VersionControlService.Abstractions.Tests.FallbackServicesTests

open Expecto
open VersionControlService.Abstractions
open VersionControlService.Abstractions.Tests.ContractShapeTests

let private providerId =
    match ProviderId.tryCreate "test.no-op" with
    | Ok value -> value
    | Error message -> failwith message

let private descriptor: WorkspaceDescriptor = {
    ProviderId = providerId
    WorkspaceRoot = "/test/workspace"
    Location = None
}

let private context = OperationContext.detached "no-op-services"

let private binding =
    FakeProvider.createBinding providerId "/test/workspace" (FakeProvider.fakeLocation providerId)

let private failed<'T> () : OperationResult<'T> =
    OperationResult.failed (OperationFailure.create Unsupported "placeholder" "placeholder")

let private minimalFactory
    (openSession: WorkspaceBinding -> OperationContext -> Async<OperationResult<WorkspaceSession>>)
    : ProviderFactory = {
    Id = providerId
    Probe = fun _ -> async { return NotDetected }
    VerifyLocation = fun _ _ -> async { return failed () }
    Initialize = fun _ _ -> async { return failed () }
    Clone = fun _ _ -> async { return failed () }
    Adopt = fun _ _ -> async { return failed () }
    Bind = fun _ _ -> async { return failed () }
    Open = openSession
    CheckDependencies = fun _ -> async { return failed () }
    InstallDependency = fun _ _ -> async { return failed () }
}

let private path =
    match RepositoryPath.tryCreate "file.txt" with
    | Ok value -> value
    | Error message -> failwith message

let private expectNoOp reason (result: OperationResult<'T>) : 'T =
    match result with
    | Succeeded outcome ->
        Expect.equal outcome.Effect (NoOp(Some reason)) "The result carries its no-op reason."
        Expect.equal
            outcome.Warnings
            [| { Code = FallbackServiceCodes.ServiceUnavailable; Message = reason } |]
            "The result carries the service-unavailable warning."
        outcome.Value
    | PartiallySucceeded _ -> failtest "The no-op service returned partial success."
    | Failed failure -> failtest $"The no-op service failed ({failure.Category}/{failure.Code}): {failure.Message}"

let private expectUnsupported reason (result: OperationResult<'T>) =
    match result with
    | Failed failure ->
        Expect.equal failure.Category Unsupported "The unavailable operation fails as Unsupported."
        Expect.equal failure.Code FallbackServiceCodes.ServiceUnavailable "The failure uses the stable service-unavailable code."
        Expect.equal failure.Message reason "The failure explains which service is absent."
        Expect.isFalse failure.StateChanged "The failure reports no state change."
        Expect.isNone failure.RecoveryAction "The failure has no recovery action."
    | Succeeded _
    | PartiallySucceeded _ -> failtest "The unavailable operation did not fail."

let private require name (service: 'T option) : 'T =
    service |> Option.defaultWith (fun () -> failtest $"The {name} service is absent.")

[<Tests>]
let fallbackServicesTests =
    testList "FallbackServices" [
        testCase "a core-only session gets all optional services and keeps its core fields"
        <| fun () ->
            let core = FakeProvider.createCore ()
            let session = WorkspaceSession.createCoreOnly descriptor core
            let filled = WorkspaceSession.withFallbackServices session

            Expect.isSome filled.Synchronization "Synchronization is present."
            Expect.isSome filled.TextDiff "Text diff is present."
            Expect.isSome filled.ConflictResolution "Conflict resolution is present."
            Expect.isSome filled.ObjectMaterialization "Object materialization is present."
            Expect.isSome filled.StoragePolicy "Storage policy is present."
            Expect.isSome filled.Maintenance "Storage maintenance is present."
            Expect.isSome filled.RepositoryBrowser "Repository browser is present."
            Expect.equal filled.Descriptor session.Descriptor "The descriptor is unchanged."
            Expect.isTrue (System.Object.ReferenceEquals(box filled.Core, box session.Core)) "The core instance is unchanged."
            Expect.isTrue (System.Object.ReferenceEquals(box filled.Close, box session.Close)) "The close function is unchanged."

        testCase "availability reports provider services before and after filling"
        <| fun () ->
            let coreOnly = WorkspaceSession.createCoreOnly descriptor (FakeProvider.createCore ())
            let withTwoServices = {
                coreOnly with
                    TextDiff = Some(FakeProvider.createFinalTextDiff ())
                    RepositoryBrowser = Some(FakeProvider.createBrowser ())
            }
            let noneAvailable: ServiceAvailability = {
                Synchronization = false
                TextDiff = false
                ConflictResolution = false
                ObjectMaterialization = false
                StoragePolicy = false
                Maintenance = false
                RepositoryBrowser = false
            }
            let twoAvailable = { noneAvailable with TextDiff = true; RepositoryBrowser = true }
            let allAvailable: ServiceAvailability = {
                Synchronization = true
                TextDiff = true
                ConflictResolution = true
                ObjectMaterialization = true
                StoragePolicy = true
                Maintenance = true
                RepositoryBrowser = true
            }

            Expect.equal (WorkspaceSession.availability coreOnly) noneAvailable "A core-only session has no optional services."
            Expect.equal (WorkspaceSession.availability withTwoServices) twoAvailable "Only provider services are reported."

            withTwoServices
            |> WorkspaceSession.withFallbackServices
            |> WorkspaceSession.availability
            |> fun availability -> Expect.equal availability allAvailable "A filled session reports every service."

        testCase "a present service keeps its instance"
        <| fun () ->
            let original: TextDiffService = {
                GetDiff = fun _ _ -> async { return OperationResult.succeeded (TextContent "original") }
                GetWordDiff = fun _ _ -> async { return OperationResult.succeeded (TextContent "original word diff") }
                GetBaseContent = fun _ _ -> async { return OperationResult.succeeded (TextContent "original base") }
            }

            let session = {
                WorkspaceSession.createCoreOnly descriptor (FakeProvider.createCore ()) with
                    TextDiff = Some original
            }

            let filled = WorkspaceSession.withFallbackServices session
            let kept = require "text diff" filled.TextDiff

            Expect.isTrue (System.Object.ReferenceEquals(box original, box kept)) "The provider service instance is retained."

        testCaseAsync "fallback services return their values, reasons, and warning codes"
        <| async {
            let session = WorkspaceSession.withFallbackServices (WorkspaceSession.createCoreOnly descriptor (FakeProvider.createCore ()))
            let textDiff = require "text diff" session.TextDiff
            let objectMaterialization = require "object materialization" session.ObjectMaterialization
            let storagePolicy = require "storage policy" session.StoragePolicy
            let maintenance = require "maintenance" session.Maintenance
            let repositoryBrowser = require "repository browser" session.RepositoryBrowser
            let conflicts = require "conflict resolution" session.ConflictResolution

            let textDiffReason = "The provider has no text diff service."
            let! diffResult = textDiff.GetDiff path context
            let! wordDiffResult = textDiff.GetWordDiff path context
            let! baseContentResult = textDiff.GetBaseContent path context
            Expect.equal (expectNoOp textDiffReason diffResult) (UnsupportedContent(Some textDiffReason)) "GetDiff returns unsupported content."
            Expect.equal (expectNoOp textDiffReason wordDiffResult) (UnsupportedContent(Some textDiffReason)) "GetWordDiff returns unsupported content."
            Expect.equal (expectNoOp textDiffReason baseContentResult) (UnsupportedContent(Some textDiffReason)) "GetBaseContent returns unsupported content."

            let objectReason = "The provider has no object materialization service."
            let! objectsResult = objectMaterialization.ListObjects context
            Expect.equal (expectNoOp objectReason objectsResult) [||] "ListObjects returns an empty array."

            let policyReason = "The provider has no storage policy service."
            let! settingsResult = storagePolicy.GetSettings context
            Expect.equal
                (expectNoOp policyReason settingsResult)
                { AutoPolicyThresholdMb = None; MaterializeLargeObjects = true }
                "GetSettings reports no threshold and default materialization."

            let maintenanceReason = "The provider has no storage maintenance service."
            let! pruneResult = maintenance.Prune context
            let! deduplicateResult = maintenance.Deduplicate context
            Expect.equal (expectNoOp maintenanceReason pruneResult) maintenanceReason "Prune returns its reason as the report."
            Expect.equal (expectNoOp maintenanceReason deduplicateResult) maintenanceReason "Deduplicate returns its reason as the report."

            let browserReason = "The provider has no repository browser service."
            let! webUrlResult = repositoryBrowser.GetRepositoryWebUrl context
            Expect.equal (expectNoOp browserReason webUrlResult) None "The browser returns no URL."

            let conflictReason = "The provider has no conflict resolution service."
            let! activeSessionResult = conflicts.GetActiveSession context
            Expect.equal (expectNoOp conflictReason activeSessionResult) None "GetActiveSession returns no session."
        }

        testCaseAsync "operations with no work return fallback values and warning codes"
        <| async {
            let session = WorkspaceSession.withFallbackServices (WorkspaceSession.createCoreOnly descriptor (FakeProvider.createCore ()))
            let objectMaterialization = require "object materialization" session.ObjectMaterialization
            let storagePolicy = require "storage policy" session.StoragePolicy

            let objectReason = "The provider has no object materialization service."
            let! materializeResult = objectMaterialization.Materialize path context
            let! dematerializeResult = objectMaterialization.Dematerialize path context
            expectNoOp objectReason materializeResult |> ignore
            expectNoOp objectReason dematerializeResult |> ignore

            let policyReason = "The provider has no storage policy service."
            let settings: StoragePolicySettings = { AutoPolicyThresholdMb = None; MaterializeLargeObjects = false }
            let! setPathPolicyResult = storagePolicy.SetPathPolicy path true context
            let! setSettingsResult = storagePolicy.SetSettings settings context
            expectNoOp policyReason setPathPolicyResult |> ignore
            expectNoOp policyReason setSettingsResult |> ignore
        }

        testCaseAsync "synchronization operations and conflict mutations fail as unsupported"
        <| async {
            let session = WorkspaceSession.withFallbackServices (WorkspaceSession.createCoreOnly descriptor (FakeProvider.createCore ()))
            let synchronization = require "synchronization" session.Synchronization
            let conflicts = require "conflict resolution" session.ConflictResolution

            let synchronizationReason = "The provider has no synchronization service."
            let! refreshResult = synchronization.Refresh context
            let! previewResult = synchronization.PreviewUpdate context
            let updateRequest: UpdateRequest = { ExpectedWorkspaceVersion = "version" }
            let publishRequest: PublishRequest = { ExpectedWorkspaceVersion = "version"; ExpectedTargetRevision = None }
            let synchronizeRequest: SynchronizeRequest = {
                ExpectedWorkspaceVersion = "version"
                ExpectedTargetRevision = None
                AcceptUpdateRisks = false
                PublishLocalRevisions = false
            }
            let! updateResult = synchronization.Update updateRequest context
            let! publishResult = synchronization.Publish publishRequest context
            let! synchronizeResult = synchronization.Synchronize synchronizeRequest context
            expectUnsupported synchronizationReason refreshResult
            expectUnsupported synchronizationReason previewResult
            expectUnsupported synchronizationReason updateResult
            expectUnsupported synchronizationReason publishResult
            expectUnsupported synchronizationReason synchronizeResult

            let conflictReason = "The provider has no conflict resolution service."
            let handle: ConflictSessionHandle = { SessionId = "session"; Version = "version" }
            let resolveRequest: ResolveConflictRequest = {
                Handle = handle
                ExpectedWorkspaceVersion = "version"
                Path = path
                Resolution = PickCandidate "candidate"
            }
            let finalizeRequest: FinalizeConflictRequest = {
                Handle = handle
                ExpectedWorkspaceVersion = "version"
                Message = None
            }
            let cancelRequest: CancelConflictRequest = { Handle = handle; ExpectedWorkspaceVersion = "version" }
            let! resolveResult = conflicts.Resolve resolveRequest context
            let! finalizeResult = conflicts.Finalize finalizeRequest context
            let! cancelResult = conflicts.Cancel cancelRequest context
            expectUnsupported conflictReason resolveResult
            expectUnsupported conflictReason finalizeResult
            expectUnsupported conflictReason cancelResult
        }

        testCaseAsync "factory fallback fills successful opens and preserves outcome fields"
        <| async {
            let originalTextDiff: TextDiffService = {
                GetDiff = fun _ _ -> async { return OperationResult.succeeded (TextContent "original") }
                GetWordDiff = fun _ _ -> async { return OperationResult.succeeded (TextContent "original word diff") }
                GetBaseContent = fun _ _ -> async { return OperationResult.succeeded (TextContent "original base") }
            }
            let coreOnly = {
                WorkspaceSession.createCoreOnly descriptor (FakeProvider.createCore ()) with
                    TextDiff = Some originalTextDiff
            }
            let originalOutcome = {
                OperationOutcome.noOp (Some "open returned without changes") coreOnly with
                    Warnings = [| { Code = "provider_notice"; Message = "Open completed with a notice." } |]
                    ResultingWorkspaceVersion = Some "workspace-version-7"
            }
            let originalFactory =
                minimalFactory (fun _ _ -> async { return Succeeded originalOutcome })
            let factory = ProviderFactory.withFallbackServices originalFactory

            let! originalProbe = originalFactory.Probe "/test/workspace"
            let! wrappedProbe = factory.Probe "/test/workspace"
            Expect.equal factory.Id originalFactory.Id "The wrapped factory keeps its id."
            Expect.equal wrappedProbe originalProbe "The wrapped probe returns the original answer."
            Expect.equal wrappedProbe NotDetected "The original probe answer is NotDetected."

            let! result = factory.Open binding context

            match result with
            | Succeeded outcome ->
                Expect.equal
                    (WorkspaceSession.availability outcome.Value)
                    {
                        Synchronization = true
                        TextDiff = true
                        ConflictResolution = true
                        ObjectMaterialization = true
                        StoragePolicy = true
                        Maintenance = true
                        RepositoryBrowser = true
                    }
                    "The factory fills each optional service."
                Expect.equal outcome.Value.Descriptor originalOutcome.Value.Descriptor "The session descriptor is unchanged."
                Expect.equal outcome.Effect originalOutcome.Effect "The outcome effect is unchanged."
                Expect.equal outcome.Warnings originalOutcome.Warnings "The outcome warnings are unchanged."
                let keptTextDiff = require "text diff" outcome.Value.TextDiff
                Expect.isTrue
                    (System.Object.ReferenceEquals(box originalTextDiff, box keptTextDiff))
                    "The factory keeps the provider text-diff instance."
                Expect.equal
                    outcome.ResultingWorkspaceVersion
                    originalOutcome.ResultingWorkspaceVersion
                    "The resulting workspace version is unchanged."
            | PartiallySucceeded _
            | Failed _ -> failtest "Expected the successful open outcome to pass through."
        }

        testCaseAsync "factory fallback fills a partial open and keeps its failure"
        <| async {
            let coreOnly = WorkspaceSession.createCoreOnly descriptor (FakeProvider.createCore ())
            let originalOutcome = {
                OperationOutcome.noOp (Some "open returned with partial results") coreOnly with
                    Warnings = [| { Code = "provider_notice"; Message = "Open completed with a notice." } |]
            }
            let originalFailure = OperationFailure.create Network "open_partial" "The provider could not finish opening."
            let factory =
                minimalFactory (fun _ _ -> async { return PartiallySucceeded(originalOutcome, originalFailure) })
                |> ProviderFactory.withFallbackServices

            let! result = factory.Open binding context

            match result with
            | PartiallySucceeded(outcome, failure) ->
                Expect.equal
                    (WorkspaceSession.availability outcome.Value)
                    {
                        Synchronization = true
                        TextDiff = true
                        ConflictResolution = true
                        ObjectMaterialization = true
                        StoragePolicy = true
                        Maintenance = true
                        RepositoryBrowser = true
                    }
                    "The factory fills each optional service on a partial open."
                Expect.equal outcome.Effect originalOutcome.Effect "The partial outcome effect is unchanged."
                Expect.equal outcome.Warnings originalOutcome.Warnings "The partial outcome warnings are unchanged."
                Expect.equal failure originalFailure "The original open failure is unchanged."
            | Succeeded _
            | Failed _ -> failtest "Expected the partial open result to pass through."
        }

        testCaseAsync "factory fallback passes failed opens through unchanged"
        <| async {
            let failure = OperationFailure.create Network "open_failed" "The provider could not open the workspace."
            let factory =
                minimalFactory (fun _ _ -> async { return Failed failure })
                |> ProviderFactory.withFallbackServices

            let! result = factory.Open binding context

            match result with
            | Failed actual -> Expect.equal actual failure "The open failure is unchanged."
            | Succeeded _
            | PartiallySucceeded _ -> failtest "The failed open changed result case."
        }
    ]
