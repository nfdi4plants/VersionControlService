/// lakeFS provider factory: probing (marker is a hint, never a binding),
/// access-intent verification against the API, and workspace bindings.
/// Sessions open over the workspace branch/index in the selected-revision task.
module VersionControlService.LakeFs.LakeFsProviderFactory

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.LakeFs.LakeFsTypes

module LakeFsApi = VersionControlService.LakeFs.LakeFsApi
module LakeFsCredentials = VersionControlService.LakeFs.LakeFsCredentials
module LakeFsWorkspaceIndex = VersionControlService.LakeFs.LakeFsWorkspaceIndex
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path

let lakeFsProviderId =
    match ProviderId.tryCreate "lakefs" with
    | Ok providerId -> providerId
    | Error message -> failwith message

let private connectionFailure (message: string) =
    OperationFailure.createRedacted Authentication "connection_profile_unresolved" message

let private unsupported (operation: string) : OperationResult<'T> =
    OperationResult.failed (
        OperationFailure.create
            Unsupported
            "operation_not_supported"
            $"{operation} is not supported by this provider."
    )

let private resolveLocation
    (credentials: LakeFsCredentials.LakeFsCredentialStrategy)
    (location: RepositoryLocation)
    : Async<Result<LakeFsConnection * LakeFsLocation, OperationFailure>> =
    async {
        match LakeFsLocation.tryParse location.ProviderLocation with
        | Error message -> return Error(OperationFailure.create Validation "invalid_location" message)
        | Ok parsed ->
            let! connection = credentials.ResolveConnection location.ConnectionProfileId

            match connection with
            | Error message -> return Error(connectionFailure message)
            | Ok resolved -> return Ok(resolved, parsed)
    }

let private bindingFor (workspaceRoot: string) (location: RepositoryLocation) : WorkspaceBinding = {
    SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
    ProviderId = lakeFsProviderId
    WorkspaceRoot = workspaceRoot
    ProviderStateRef = None
    Location = location
    ConnectionProfileId = location.ConnectionProfileId
}

/// Verifies requested intents against the server: reads through repository
/// lookup, writes through a transient probe branch (created and deleted).
let verifyAccess
    (credentials: LakeFsCredentials.LakeFsCredentialStrategy)
    (request: VerifyLocationRequest)
    (context: OperationContext)
    : Async<OperationResult<AccessReport>> =
    async {
        let! resolved = resolveLocation credentials request.Location

        match resolved with
        | Error failure -> return Failed failure
        | Ok(connection, location) ->
            let granted = ResizeArray<AccessIntent>()
            let denied = ResizeArray<AccessIntent>()

            let readIntents =
                request.Intents |> Array.filter (fun intent -> intent = ReadIntent)

            let writeIntents =
                request.Intents |> Array.filter (fun intent -> intent <> ReadIntent)

            if readIntents.Length > 0 then
                let! repository = LakeFsApi.getRepository connection location.Repository context

                match repository with
                | Ok _ -> granted.AddRange readIntents
                | Error _ -> denied.AddRange readIntents

            if writeIntents.Length > 0 then
                let probeBranch =
                    $"vcs-access-probe-{LakeFsWorkspaceIndex.createOwnershipToken ()}"

                let! created =
                    LakeFsApi.createBranch connection location.Repository probeBranch location.TargetRef context

                match created with
                | Ok() ->
                    granted.AddRange writeIntents
                    let! _ = LakeFsApi.deleteBranch connection location.Repository probeBranch context
                    ()
                | Error _ -> denied.AddRange writeIntents

            return
                OperationResult.succeeded {
                    Location = request.Location
                    GrantedIntents = granted.ToArray()
                    DeniedIntents = denied.ToArray()
                }
    }

/// Probes for the lakectl local marker. A marker is a hint for detection UIs —
/// binding authority always stays with the host's explicit binding store.
let probe (workspacePath: string) : Async<ProbeResult> =
    async {
        try
            let markerPath = NodePath.join [| workspacePath; ".lakefs_ref.yaml" |]

            if NodeFileSystem.existsSync markerPath then
                return Detected(workspacePath, 80, Some "lakefs local checkout marker")
            else
                return NotDetected
        with error ->
            return ProbeFailed(OperationFailure.createRedacted ProviderError "probe_exception" error.Message)
    }

let createFactory (credentials: LakeFsCredentials.LakeFsCredentialStrategy) : ProviderFactory = {
    Id = lakeFsProviderId
    Probe = probe
    VerifyLocation = fun request context -> verifyAccess credentials request context
    Initialize =
        fun request context -> async {
            match request.Location with
            | None ->
                return
                    Failed(
                        OperationFailure.create
                            Validation
                            "location_required"
                            "Initializing a lakeFS workspace requires a repository location."
                    )
            | Some location ->
                let! resolved = resolveLocation credentials location

                match resolved with
                | Error failure -> return Failed failure
                | Ok _ ->
                    if not (NodeFileSystem.existsSync request.TargetPath) then
                        NodeFileSystem.mkdirSync request.TargetPath (NodeFileSystem.MkdirOptions(recursive = true))

                    return OperationResult.succeeded (bindingFor request.TargetPath location)
        }
    Clone =
        fun request context -> async {
            let! resolved = resolveLocation credentials request.Location

            match resolved with
            | Error failure -> return Failed failure
            | Ok _ ->
                let targetExists = NodeFileSystem.existsSync request.TargetPath

                let targetNonEmpty =
                    targetExists
                    && (NodeFileSystem.readdirSync request.TargetPath).Length > 0

                if targetNonEmpty then
                    return
                        Failed(
                            OperationFailure.create
                                Validation
                                "target_not_empty"
                                "The clone target directory is not empty."
                        )
                else
                    if not targetExists then
                        NodeFileSystem.mkdirSync request.TargetPath (NodeFileSystem.MkdirOptions(recursive = true))

                    return OperationResult.succeeded (bindingFor request.TargetPath request.Location)
        }
    Adopt = fun _ _ -> async { return unsupported "Adoption" }
    Bind =
        fun request _ -> async {
            // Binding data comes exclusively from the request — provider markers
            // on disk are never read here.
            return OperationResult.succeeded (bindingFor request.WorkspaceRoot request.Location)
        }
    Open =
        fun _ _ -> async {
            return
                Failed(
                    OperationFailure.create
                        ProviderError
                        "lakefs_session_pending"
                        "The lakeFS workspace session arrives with the selected-revision task."
                )
        }
    CheckDependencies =
        fun _ -> async {
            // The lakeFS provider needs no local tooling: the API client is built in.
            return OperationResult.succeeded [||]
        }
    InstallDependency = fun _ _ -> async { return unsupported "Dependency installation" }
}
