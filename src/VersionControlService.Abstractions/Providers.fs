namespace VersionControlService.Abstractions

/// One opened per-workspace session. Service presence controls feature discovery.
/// A provider leaves a service absent when it has nothing to offer there, and a
/// consumer checks presence once and enables the matching UI.
///
/// After prerelease stabilization this record and its service records are frozen: new
/// capabilities are added as new optional service records, never as new members
/// on already-published records.
///
/// A consumer that wants one code path for every provider fills the absent services
/// with WorkspaceSession.withFallbackServices and reads WorkspaceSession.availability
/// beforehand to learn what the provider really supplies.
type WorkspaceSession = {
    Descriptor: WorkspaceDescriptor
    Core: CoreVersionControl
    Synchronization: SynchronizationService option
    TextDiff: TextDiffService option
    ConflictResolution: ConflictResolutionService option
    ObjectMaterialization: ObjectMaterializationService option
    StoragePolicy: StoragePolicyService option
    Maintenance: StorageMaintenanceService option
    RepositoryBrowser: RepositoryBrowserService option
    /// Releases session-owned resources (locks, clients, caches).
    Close: unit -> Async<unit>
}

/// Reports which optional services the provider supplies on an opened session.
type ServiceAvailability = {
    Synchronization: bool
    TextDiff: bool
    ConflictResolution: bool
    ObjectMaterialization: bool
    StoragePolicy: bool
    Maintenance: bool
    RepositoryBrowser: bool
}

/// The code the fallback services report. It means the provider supplies no such
/// service at all. A provider that has the service reports operation_not_supported
/// for an operation it implements but cannot perform for the given input.
module FallbackServiceCodes =

    [<Literal>]
    let ServiceUnavailable = "service_unavailable"

module WorkspaceSession =

    let private unsupported reason =
        OperationResult.failed (
            OperationFailure.create Unsupported FallbackServiceCodes.ServiceUnavailable reason
        )

    /// A fallback answer that changes nothing: a no-op with its reason, plus the same
    /// code as a warning so a consumer recognizes a fallback answer without inspecting
    /// the effect.
    let private noOp (reason: string) (value: 'T) : OperationResult<'T> =
        Succeeded {
            OperationOutcome.noOp (Some reason) value with
                Warnings = [| { Code = FallbackServiceCodes.ServiceUnavailable; Message = reason } |]
        }

    /// Which optional services the provider supplies. Read it before applying the
    /// fallback, because a filled session reports every service as present.
    let availability (session: WorkspaceSession) : ServiceAvailability = {
        Synchronization = session.Synchronization.IsSome
        TextDiff = session.TextDiff.IsSome
        ConflictResolution = session.ConflictResolution.IsSome
        ObjectMaterialization = session.ObjectMaterialization.IsSome
        StoragePolicy = session.StoragePolicy.IsSome
        Maintenance = session.Maintenance.IsSome
        RepositoryBrowser = session.RepositoryBrowser.IsSome
    }

    let private textDiffReason = "The provider has no text diff service."

    let private noOpTextDiff: TextDiffService = {
        GetDiff = fun _ _ -> async { return noOp textDiffReason (UnsupportedContent(Some textDiffReason)) }
        GetWordDiff = fun _ _ -> async { return noOp textDiffReason (UnsupportedContent(Some textDiffReason)) }
        GetBaseContent = fun _ _ -> async { return noOp textDiffReason (UnsupportedContent(Some textDiffReason)) }
    }

    let private objectMaterializationReason = "The provider has no object materialization service."

    let private noOpObjectMaterialization: ObjectMaterializationService = {
        ListObjects = fun _ -> async { return noOp objectMaterializationReason [||] }
        Materialize = fun _ _ -> async { return noOp objectMaterializationReason () }
        Dematerialize = fun _ _ -> async { return noOp objectMaterializationReason () }
    }

    let private storagePolicyReason = "The provider has no storage policy service."

    let private noOpStoragePolicy: StoragePolicyService = {
        SetPathPolicy = fun _ _ _ -> async { return noOp storagePolicyReason () }
        GetSettings = fun _ -> async { return noOp storagePolicyReason { AutoPolicyThresholdMb = None; MaterializeLargeObjects = true } }
        SetSettings = fun _ _ -> async { return noOp storagePolicyReason () }
    }

    let private storageMaintenanceReason = "The provider has no storage maintenance service."

    let private noOpStorageMaintenance: StorageMaintenanceService = {
        Prune = fun _ -> async { return noOp storageMaintenanceReason storageMaintenanceReason }
        Deduplicate = fun _ -> async { return noOp storageMaintenanceReason storageMaintenanceReason }
    }

    let private repositoryBrowserReason = "The provider has no repository browser service."

    let private noOpRepositoryBrowser: RepositoryBrowserService = {
        GetRepositoryWebUrl = fun _ -> async { return noOp repositoryBrowserReason None }
    }

    let private synchronizationReason = "The provider has no synchronization service."

    let private noOpSynchronization: SynchronizationService = {
        Refresh = fun _ -> async { return unsupported synchronizationReason }
        PreviewUpdate = fun _ -> async { return unsupported synchronizationReason }
        Update = fun _ _ -> async { return unsupported synchronizationReason }
        Publish = fun _ _ -> async { return unsupported synchronizationReason }
        Synchronize = fun _ _ -> async { return unsupported synchronizationReason }
    }

    let private conflictResolutionReason = "The provider has no conflict resolution service."

    let private noOpConflictResolution: ConflictResolutionService = {
        GetActiveSession = fun _ -> async { return noOp conflictResolutionReason None }
        Resolve = fun _ _ -> async { return unsupported conflictResolutionReason }
        Finalize = fun _ _ -> async { return unsupported conflictResolutionReason }
        Cancel = fun _ _ -> async { return unsupported conflictResolutionReason }
    }

    /// A core-only session: every optional service absent. Providers add the
    /// services they genuinely support.
    let createCoreOnly (descriptor: WorkspaceDescriptor) (core: CoreVersionControl) : WorkspaceSession = {
        Descriptor = descriptor
        Core = core
        Synchronization = None
        TextDiff = None
        ConflictResolution = None
        ObjectMaterialization = None
        StoragePolicy = None
        Maintenance = None
        RepositoryBrowser = None
        Close = fun () -> async.Return()
    }

    /// The session with every absent optional service filled by a fallback, for a
    /// consumer that wants one code path for every provider. Present services are kept.
    ///
    /// Each fallback that succeeds does nothing and carries a service_unavailable warning.
    /// ListObjects returns an empty array, GetSettings returns no threshold with
    /// MaterializeLargeObjects = true, the text diff reads return UnsupportedContent,
    /// and GetActiveSession and GetRepositoryWebUrl return None. Prune and Deduplicate
    /// return the reason as their report. Every synchronization operation and the
    /// conflict mutations Resolve, Finalize and Cancel fail as Unsupported with the
    /// service_unavailable code.
    let withFallbackServices (session: WorkspaceSession) : WorkspaceSession = {
        // Naming every field makes a newly added optional service fail compilation until this fallback handles it.
        Descriptor = session.Descriptor
        Core = session.Core
        Synchronization = Some(Option.defaultValue noOpSynchronization session.Synchronization)
        TextDiff = Some(Option.defaultValue noOpTextDiff session.TextDiff)
        ConflictResolution = Some(Option.defaultValue noOpConflictResolution session.ConflictResolution)
        ObjectMaterialization = Some(Option.defaultValue noOpObjectMaterialization session.ObjectMaterialization)
        StoragePolicy = Some(Option.defaultValue noOpStoragePolicy session.StoragePolicy)
        Maintenance = Some(Option.defaultValue noOpStorageMaintenance session.Maintenance)
        RepositoryBrowser = Some(Option.defaultValue noOpRepositoryBrowser session.RepositoryBrowser)
        Close = session.Close
    }

/// Provider entry point: probing, verification, provisioning, binding, and opening
/// per-workspace sessions. Registered per provider ID in an injected catalog.
type ProviderFactory = {
    Id: ProviderId
    /// Probes a directory for provider ownership. Must not throw; failures are ProbeFailed.
    Probe: string -> Async<ProbeResult>
    /// Verifies requested access intents against a repository location.
    VerifyLocation: VerifyLocationRequest -> OperationContext -> Async<OperationResult<AccessReport>>
    Initialize: InitializeRequest -> OperationContext -> Async<OperationResult<WorkspaceBinding>>
    Clone: CloneRequest -> OperationContext -> Async<OperationResult<WorkspaceBinding>>
    /// Registers an existing provider-owned workspace without cloning.
    Adopt: AdoptRequest -> OperationContext -> Async<OperationResult<WorkspaceBinding>>
    /// Attaches or re-targets an existing local workspace without cloning.
    Bind: BindRequest -> OperationContext -> Async<OperationResult<WorkspaceBinding>>
    Open: WorkspaceBinding -> OperationContext -> Async<OperationResult<WorkspaceSession>>
    /// Reports installed/missing/incompatible dependency components with remediation.
    CheckDependencies: OperationContext -> Async<OperationResult<DependencyStatus[]>>
    /// Attempts remediation for a component reported by CheckDependencies.
    InstallDependency: string -> OperationContext -> Async<OperationResult<DependencyStatus>>
}

module ProviderFactory =

    /// Returns a factory whose Open member fills successful and partial sessions
    /// with fallback services. Failed opens pass through unchanged. A composition root
    /// wraps its factories before ProviderResolver.tryCreateCatalog and then never
    /// checks optional service presence again.
    ///
    /// A wrapped factory never yields an unfilled session, and a filled session reports
    /// every service as present. A host that needs the availability report keeps the
    /// unwrapped factory, or applies WorkspaceSession.withFallbackServices itself after
    /// reading WorkspaceSession.availability.
    ///
    /// Only Open is wrapped. A future member that returns a session has to be added here.
    let withFallbackServices (factory: ProviderFactory) : ProviderFactory = {
        factory with
            Open =
                fun binding context ->
                    async {
                        let! result = factory.Open binding context

                        return
                            match result with
                            | Succeeded outcome ->
                                Succeeded { outcome with Value = WorkspaceSession.withFallbackServices outcome.Value }
                            | PartiallySucceeded(outcome, failure) ->
                                PartiallySucceeded(
                                    { outcome with Value = WorkspaceSession.withFallbackServices outcome.Value },
                                    failure
                                )
                            | Failed failure -> Failed failure
                    }
    }
