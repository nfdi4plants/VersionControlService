/// Provider-owned lakeFS conflict-session state. The session handle is opaque and
/// versioned independently from the workspace version supplied with each mutation.
module VersionControlService.LakeFs.LakeFsConflictSession

open VersionControlService.Abstractions

module LakeFsIndex = VersionControlService.LakeFs.LakeFsWorkspaceIndex

type CandidateContent = {
    SourcePath: string
    Preview: ConflictPreview
}

type ResolvedContent =
    | ExistingFile of sourcePath: string
    | WorkspaceFile of path: RepositoryPath
    | SuppliedText of content: string

type ItemState = {
    ItemPath: string
    BaseContent: CandidateContent option
    WorkspaceContent: CandidateContent option
    TargetContent: CandidateContent option
    /// None means unresolved; Some None is a resolved deletion.
    mutable ResolvedContent: ResolvedContent option option
}

type State = {
    SessionId: string
    mutable HandleVersion: int
    Items: ItemState list
    TargetRevisionAtOpen: string
    WorkspaceRevisionAtOpen: string
    mutable PendingFinalizeRevision: string option
}

let private revisionId value =
    RevisionId.tryCreate value |> Result.defaultWith failwith

let create targetRevision workspaceRevision items = {
    SessionId = $"lakefs-conflict-{LakeFsIndex.createOwnershipToken ()}"
    HandleVersion = 1
    Items = items
    TargetRevisionAtOpen = targetRevision
    WorkspaceRevisionAtOpen = workspaceRevision
    PendingFinalizeRevision = None
}

let rejection () =
    {
        OperationFailure.create
            Concurrency
            "precondition_failed"
            "The conflict-session handle is stale, foreign, or closed." with
            RecoveryAction =
                Some {
                    Code = ConflictRecovery.RefreshConflictSession
                    Instructions = Some "Refresh the conflict session and deliberately retry with the live handle."
                }
    }

let validate
    (active: State option)
    (handle: ConflictSessionHandle)
    (currentWorkspaceVersion: string)
    (expectedWorkspaceVersion: string)
    =
    match active with
    | Some conflict when
        conflict.SessionId = handle.SessionId
        && string conflict.HandleVersion = handle.Version
        && currentWorkspaceVersion = expectedWorkspaceVersion
        ->
        Ok conflict
    | _ -> Error(rejection ())

let summary
    (baseRevision: RevisionId option)
    (workspaceRevision: RevisionId option)
    (conflict: State option)
    : ConflictSessionSummary option =
    conflict
    |> Option.map (fun active -> {
        Handle = {
            SessionId = active.SessionId
            Version = string active.HandleVersion
        }
        Items =
            active.Items
            |> List.filter (fun item -> item.ResolvedContent.IsNone)
            |> List.choose (fun item ->
                match RepositoryPath.tryCreate item.ItemPath with
                | Error _ -> None
                | Ok path ->
                    Some {
                        Path = path
                        Candidates = [|
                            {
                                CandidateId = "workspace"
                                Label = "Workspace version"
                                Revision = workspaceRevision
                                Preview = item.WorkspaceContent |> Option.map _.Preview
                            }
                            {
                                CandidateId = "target"
                                Label = "Target version"
                                Revision = Some(revisionId active.TargetRevisionAtOpen)
                                Preview = item.TargetContent |> Option.map _.Preview
                            }
                            yield!
                                match item.BaseContent with
                                | Some baseContent ->
                                    [|
                                        {
                                            CandidateId = "base"
                                            Label = "Base version"
                                            Revision = baseRevision
                                            Preview = Some baseContent.Preview
                                        }
                                    |]
                                | None -> [||]
                        |]
                        CombinedPreview = None
                        SupportsResolvedContent =
                            [ item.BaseContent; item.WorkspaceContent; item.TargetContent ]
                            |> List.choose id
                            |> List.forall (fun content ->
                                match content.Preview with
                                | TextPreview _ -> true
                                | UnsupportedPreview _ -> false)
                    })
            |> List.toArray
    })

let resolve (conflict: State) (path: RepositoryPath) (resolution: ConflictResolution) =
    let pathValue = RepositoryPath.value path

    match
        conflict.Items
        |> List.tryFind (fun item -> item.ItemPath = pathValue && item.ResolvedContent.IsNone)
    with
    | None ->
        Error(
            OperationFailure.create
                NotFound
                "conflict_item_not_found"
                "No unresolved conflict exists for the selected path."
        )
    | Some item ->
        let supportsResolvedContent =
            [ item.BaseContent; item.WorkspaceContent; item.TargetContent ]
            |> List.choose id
            |> List.forall (fun content ->
                match content.Preview with
                | TextPreview _ -> true
                | UnsupportedPreview _ -> false)

        let resolvedContent =
            match resolution with
            | SupplyResolvedContent content when supportsResolvedContent -> Some(Some(SuppliedText content))
            | SupplyResolvedContent _ -> None
            | PickCandidate "workspace" ->
                Some(item.WorkspaceContent |> Option.map (fun _ -> WorkspaceFile path))
            | PickCandidate "target" ->
                Some(item.TargetContent |> Option.map (fun value -> ExistingFile value.SourcePath))
            | PickCandidate "base" ->
                Some(item.BaseContent |> Option.map (fun value -> ExistingFile value.SourcePath))
            | PickCandidate _ -> None

        match resolvedContent with
        | None ->
            let code, message =
                if supportsResolvedContent then
                    "unknown_candidate", "The candidate ID is not part of this conflict item."
                else
                    "binary_resolution_required",
                    "Binary conflicts must be resolved by selecting an original candidate or editing the file manually."

            Error(
                OperationFailure.create
                    Validation
                    code
                    message
            )
        | Some content ->
            item.ResolvedContent <- Some content
            conflict.HandleVersion <- conflict.HandleVersion + 1
            Ok()

let hasUnresolvedItems (conflict: State) =
    conflict.Items |> List.exists (fun item -> item.ResolvedContent.IsNone)
