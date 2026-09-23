/// Provider-managed Git conflict sessions over the real merge state: candidate
/// content from index stages, literal-path resolution, and handle bookkeeping
/// helpers. The session module owns handle storage and validation.
module internal VersionControlService.Git.GitConflictSession

open VersionControlService.Abstractions

module NodeProcess = VersionControlService.Runtime.Node.Process

type GitRunner = string[] -> string option -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>
type StagePreviewReader = int -> string -> Async<Result<ConflictPreview option, OperationFailure>>
type CombinedPreviewReader = string -> Async<Result<ConflictPreview option, OperationFailure>>
type StagePointerReader = int -> string -> Async<Result<ConflictCandidateObject option, OperationFailure>>

/// Stale, foreign, or closed handles are rejected before provider state changes.
let handleRejection () =
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

let listUnmergedPaths (runGit: GitRunner) : Async<Result<string[], OperationFailure>> =
    async {
        let! output =
            runGit
                [|
                    "diff"
                    "--name-only"
                    "--diff-filter=U"
                    "-z"
                |]
                None

        match output with
        | Ok result when result.ExitCode = 0 ->
            return Ok(result.StdOut.Split '\000' |> Array.filter (fun entry -> entry <> ""))
        | Ok result ->
            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "git_failure"
                        $"Listing unmerged paths failed: {result.StdErr}"
                )
        | Error failure -> return Error failure
    }

/// Builds conflict items with provider-classified stage and combined previews.
let buildConflictItems
    (readStagePreview: StagePreviewReader)
    (readCombinedPreview: CombinedPreviewReader)
    (readStagePointer: StagePointerReader)
    (mergeHeadRevision: RevisionId option)
    (unmergedPaths: string[])
    : Async<Result<ConflictItem[], OperationFailure>> =
    async {
        let items = ResizeArray<ConflictItem>()
        let mutable previewFailure = None

        for pathValue in unmergedPaths do
            match previewFailure, RepositoryPath.tryCreate pathValue with
            | Some _, _
            | None, Error _ -> ()
            | None, Ok path ->
                let! basePreviewResult = readStagePreview 1 pathValue
                let! workspacePreviewResult = readStagePreview 2 pathValue
                let! targetPreviewResult = readStagePreview 3 pathValue
                let! combinedPreviewResult = readCombinedPreview pathValue
                let! basePointerResult = readStagePointer 1 pathValue
                let! workspacePointerResult = readStagePointer 2 pathValue
                let! targetPointerResult = readStagePointer 3 pathValue

                match
                    basePreviewResult,
                    workspacePreviewResult,
                    targetPreviewResult,
                    combinedPreviewResult,
                    basePointerResult,
                    workspacePointerResult,
                    targetPointerResult
                with
                | Error failure, _, _, _, _, _, _
                | _, Error failure, _, _, _, _, _
                | _, _, Error failure, _, _, _, _
                | _, _, _, Error failure, _, _, _
                | _, _, _, _, Error failure, _, _
                | _, _, _, _, _, Error failure, _
                | _, _, _, _, _, _, Error failure -> previewFailure <- Some failure
                | Ok basePreview, Ok workspacePreview, Ok targetPreview, Ok combinedPreview, Ok basePointer, Ok workspacePointer, Ok targetPointer ->
                    let isUnsupported = function
                        | Some(UnsupportedPreview _) -> true
                        | _ -> false

                    let requiresManualResolution =
                        isUnsupported basePreview
                        || isUnsupported workspacePreview
                        || isUnsupported targetPreview
                        || isUnsupported combinedPreview

                    let anyObject =
                        Option.isSome basePointer
                        || Option.isSome workspacePointer
                        || Option.isSome targetPointer

                    items.Add {
                        Path = path
                        Candidates = [|
                            {
                                CandidateId = "workspace"
                                Label = "Workspace version"
                                Revision = None
                                Preview = workspacePreview
                                Object = workspacePointer
                            }
                            {
                                CandidateId = "target"
                                Label = "Target version"
                                Revision = mergeHeadRevision
                                Preview = targetPreview
                                Object = targetPointer
                            }
                            yield!
                                match basePreview with
                                | Some preview ->
                                    [|
                                        {
                                            CandidateId = "base"
                                            Label = "Base version"
                                            Revision = None
                                            Preview = Some preview
                                            Object = basePointer
                                        }
                                    |]
                                | None -> [||]
                        |]
                        CombinedPreview = combinedPreview
                        SupportsResolvedContent = not requiresManualResolution && not anyObject
                    }

        match previewFailure with
        | Some failure -> return Error failure
        | None -> return Ok(items.ToArray())
    }
