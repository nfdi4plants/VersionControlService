/// Injected, instance-scoped provider resolution for the v2 session API.
/// Replaces module-level mutable registry state: the host constructs one catalog
/// at its composition root and passes it wherever resolution is needed.
module VersionControlService.Resolution.ProviderResolver

open System
open VersionControlService.Abstractions

/// Immutable provider catalog. Registering an external provider is adding its
/// factory here — never a library edit.
type ProviderCatalog = private ProviderCatalog of ProviderFactory[]

/// One probe detection candidate retained for ambiguity reporting.
type DetectionCandidate = {
    ProviderId: ProviderId
    Root: string
    Confidence: int
    BindingHint: string option
}

type WorkspaceResolution =
    /// The explicit host-stored binding won; probes were not consulted.
    | Bound of WorkspaceBinding * ProviderFactory
    /// Exactly one nearest owning root was detected.
    | ProbedWorkspace of ProviderFactory * DetectionCandidate
    /// Multiple providers claim the same nearest root; the host must bind explicitly.
    | AmbiguousWorkspace of DetectionCandidate[]
    | UnmanagedWorkspace

type ResolutionReport = {
    Resolution: WorkspaceResolution
    /// Structured diagnostics from probes that failed, threw, or returned Invalid.
    Diagnostics: OperationFailure[]
}

type ResolutionRequest = {
    WorkspacePath: string
    /// Explicit host-stored binding for this workspace root, if any. The host owns
    /// binding persistence; the resolver never reads or writes binding files.
    ExplicitBinding: WorkspaceBinding option
}

/// Normalizes a local path for root comparison only: forward slashes, no trailing
/// separator, ordinal-case-insensitive comparison happens separately.
let normalizePath (path: string) : string =
    let forward = (path |> Option.ofObj |> Option.defaultValue "").Replace('\\', '/')
    let trimmed = forward.TrimEnd '/'
    if trimmed.Length = 0 then "/" else trimmed

let private pathEquals (left: string) (right: string) =
    String.Equals(left, right, StringComparison.OrdinalIgnoreCase)

/// True when `root` owns `path` (path equals root or lives under it).
let private isOwnedBy (root: string) (path: string) =
    pathEquals root path
    || path.StartsWith(root + "/", StringComparison.OrdinalIgnoreCase)

let tryCreateCatalog (factories: ProviderFactory seq) : Result<ProviderCatalog, string> =
    let factoryArray = Seq.toArray factories

    let duplicateIds =
        factoryArray
        |> Array.countBy (fun factory -> ProviderId.value factory.Id)
        |> Array.filter (fun (_, count) -> count > 1)
        |> Array.map fst

    if duplicateIds.Length > 0 then
        Error $"""Duplicate provider registrations: {String.Join(", ", duplicateIds)}."""
    else
        Ok(ProviderCatalog factoryArray)

let factories (ProviderCatalog catalog) = Array.copy catalog

let tryGetFactory (ProviderCatalog catalog) (providerId: ProviderId) : ProviderFactory option =
    catalog
    |> Array.tryFind (fun factory -> ProviderId.value factory.Id = ProviderId.value providerId)

/// Runs one probe, converting every escape hatch (exception, Invalid, ProbeFailed)
/// into structured diagnostics instead of letting it break resolution.
let private runProbe
    (workspacePath: string)
    (factory: ProviderFactory)
    : Async<Result<DetectionCandidate option, OperationFailure>> =
    async {
        let providerName = ProviderId.value factory.Id

        try
            let! probeResult = factory.Probe workspacePath

            match probeResult with
            | NotDetected -> return Ok None
            | Detected(root, confidence, bindingHint) ->
                return
                    Ok(
                        Some {
                            ProviderId = factory.Id
                            Root = normalizePath root
                            Confidence = confidence
                            BindingHint = bindingHint
                        }
                    )
            | Invalid message ->
                return
                    Error(
                        OperationFailure.createRedacted
                            Validation
                            "probe_invalid"
                            $"Provider '{providerName}' reported an invalid workspace: {message}"
                    )
            | ProbeFailed failure -> return Error failure
        with error ->
            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "probe_exception"
                        $"Provider '{providerName}' probe threw: {error.Message}"
                )
    }

let resolve (catalog: ProviderCatalog) (request: ResolutionRequest) : Async<ResolutionReport> =
    async {
        match request.ExplicitBinding with
        | Some binding ->
            // An explicit host binding always wins; probes are not consulted.
            match tryGetFactory catalog binding.ProviderId with
            | Some factory ->
                return {
                    Resolution = Bound(binding, factory)
                    Diagnostics = [||]
                }
            | None ->
                return {
                    Resolution = UnmanagedWorkspace
                    Diagnostics = [|
                        OperationFailure.create
                            DependencyMissing
                            "provider_not_registered"
                            $"No factory is registered for provider '{ProviderId.value binding.ProviderId}'."
                    |]
                }
        | None ->
            let workspacePath = normalizePath request.WorkspacePath
            let diagnostics = ResizeArray<OperationFailure>()
            let candidates = ResizeArray<DetectionCandidate>()

            for factory in factories catalog do
                let! probeOutcome = runProbe workspacePath factory

                match probeOutcome with
                | Ok(Some candidate) ->
                    if isOwnedBy candidate.Root workspacePath then
                        candidates.Add candidate
                    else
                        diagnostics.Add(
                            OperationFailure.create
                                Validation
                                "probe_root_outside_workspace"
                                $"Provider '{ProviderId.value candidate.ProviderId}' detected root '{candidate.Root}' which does not own '{workspacePath}'."
                        )
                | Ok None -> ()
                | Error failure -> diagnostics.Add failure

            if candidates.Count = 0 then
                return {
                    Resolution = UnmanagedWorkspace
                    Diagnostics = diagnostics.ToArray()
                }
            else
                // The nearest owning root (longest normalized root) wins — but only
                // when it is unambiguous. Ties across providers report Ambiguous
                // instead of choosing by registration order.
                let deepestRootLength = candidates |> Seq.map (fun c -> c.Root.Length) |> Seq.max

                let nearest =
                    candidates
                    |> Seq.filter (fun candidate -> candidate.Root.Length = deepestRootLength)
                    |> Seq.toArray

                if nearest.Length = 1 then
                    let winner = nearest[0]

                    match tryGetFactory catalog winner.ProviderId with
                    | Some factory ->
                        return {
                            Resolution = ProbedWorkspace(factory, winner)
                            Diagnostics = diagnostics.ToArray()
                        }
                    | None ->
                        return {
                            Resolution = UnmanagedWorkspace
                            Diagnostics = diagnostics.ToArray()
                        }
                else
                    return {
                        Resolution = AmbiguousWorkspace nearest
                        Diagnostics = diagnostics.ToArray()
                    }
    }
