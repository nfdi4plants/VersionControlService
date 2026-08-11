module ExternalProvider.Provider

open VersionControlService.Abstractions

let private providerId =
    match ProviderId.tryCreate "sample.external" with
    | Ok value -> value
    | Error message -> failwith message

let private unsupported operation : OperationResult<'T> =
    OperationResult.failed (
        OperationFailure.create
            Unsupported
            "operation_not_supported"
            $"{operation} is not supported by the sample provider."
    )

let private createCore () : CoreVersionControl = {
    GetStatus =
        fun _ ->
            async {
                return
                    OperationResult.succeeded {
                        CurrentRef = None
                        WorkspaceVersion = "sample-read-only"
                        Changes = [||]
                        ActiveConflictSession = None
                        Synchronization = None
                    }
            }
    ListRefs = fun _ -> async { return OperationResult.succeeded [||] }
    CreateRef = fun _ _ -> async { return unsupported "Reference creation" }
    PreflightSwitchRef = fun _ _ -> async { return unsupported "Reference switching" }
    SwitchRef = fun _ _ -> async { return unsupported "Reference switching" }
    CreateRevision = fun _ _ -> async { return unsupported "Revision creation" }
    RestorePaths = fun _ _ -> async { return unsupported "Path restoration" }
    GetDiffSummary =
        fun _ ->
            async {
                return OperationResult.succeeded { Entries = [||] }
            }
}

let factory: ProviderFactory = {
    Id = providerId
    Probe = fun _ -> async { return NotDetected }
    VerifyLocation = fun _ _ -> async { return unsupported "Location verification" }
    Initialize = fun _ _ -> async { return unsupported "Workspace initialization" }
    Clone = fun _ _ -> async { return unsupported "Workspace cloning" }
    Adopt = fun _ _ -> async { return unsupported "Workspace adoption" }
    Bind = fun _ _ -> async { return unsupported "Workspace binding" }
    Open =
        fun binding _ ->
            async {
                let descriptor = {
                    ProviderId = binding.ProviderId
                    WorkspaceRoot = binding.WorkspaceRoot
                    Location = Some binding.Location
                }

                return
                    OperationResult.succeeded (
                        WorkspaceSession.createCoreOnly descriptor (createCore ())
                    )
            }
    CheckDependencies = fun _ -> async { return OperationResult.succeeded [||] }
    InstallDependency = fun _ _ -> async { return unsupported "Dependency installation" }
}
