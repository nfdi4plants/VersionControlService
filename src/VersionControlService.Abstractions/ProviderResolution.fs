namespace VersionControlService.Abstractions

open System

/// The host-selected comparison semantics for local workspace paths.
type PathCaseSensitivity =
    | CaseSensitive
    | CaseInsensitive

/// Injected, instance-scoped provider resolution for the workspace-session API.
/// The host constructs one catalog at its composition root and passes it wherever
/// resolution is needed.
module ProviderResolver =

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
        /// The host filesystem's path-comparison semantics.
        PathCaseSensitivity: PathCaseSensitivity
    }

    /// Normalizes a local path for root comparison only: forward slashes and no
    /// redundant trailing separator. It deliberately preserves case and Unicode.
    let normalizePath (path: string) : string =
        let forward = (path |> Option.ofObj |> Option.defaultValue "").Replace('\\', '/')
        let trimmed = forward.TrimEnd '/'
        if trimmed.Length = 0 then "/" else trimmed

    let private comparison = function
        | CaseSensitive -> StringComparison.Ordinal
        | CaseInsensitive -> StringComparison.OrdinalIgnoreCase

    let private equals sensitivity (left: string) (right: string) =
        String.Equals(left, right, comparison sensitivity)

    /// True when `root` owns `path` (path equals root or lives under it).
    let private owns sensitivity (root: string) (path: string) =
        let descendantPrefix = if root = "/" then "/" else root + "/"

        equals sensitivity root path
        || path.StartsWith(descendantPrefix, comparison sensitivity)

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

    let private groupByRoot sensitivity (candidates: DetectionCandidate[]) : DetectionCandidate[][] =
        candidates
        |> Array.fold
            (fun (groups: DetectionCandidate[][]) candidate ->
                match groups |> Array.tryFindIndex (fun group -> equals sensitivity group[0].Root candidate.Root) with
                | Some index ->
                    groups
                    |> Array.mapi (fun groupIndex group ->
                        if groupIndex = index then Array.append group [| candidate |] else group)
                | None -> Array.append groups [| [| candidate |] |])
            [||]

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
                        if owns request.PathCaseSensitivity candidate.Root workspacePath then
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
                    let deepestRootLength = candidates |> Seq.map (fun candidate -> candidate.Root.Length) |> Seq.max

                    let nearestCandidates =
                        candidates
                        |> Seq.filter (fun candidate -> candidate.Root.Length = deepestRootLength)
                        |> Seq.toArray

                    let nearestRootGroups =
                        groupByRoot request.PathCaseSensitivity nearestCandidates

                    if nearestRootGroups.Length = 1 && nearestRootGroups[0].Length = 1 then
                        let winner = nearestRootGroups[0][0]

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
                            Resolution = AmbiguousWorkspace nearestCandidates
                            Diagnostics = diagnostics.ToArray()
                        }
        }
