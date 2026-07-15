/// Provider-managed Git conflict sessions over the real merge state: candidate
/// content from index stages, literal-path resolution, and handle bookkeeping
/// helpers. The session module owns handle storage and validation.
module VersionControlService.Git.GitConflictSession

open VersionControlService.Abstractions

module NodeProcess = VersionControlService.Runtime.Node.Process

type GitRunner = string[] -> string option -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>
type CombinedPreviewReader = string -> Async<Result<ConflictPreview option, OperationFailure>>

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

/// Content of one index stage (1 = base, 2 = workspace, 3 = target) for a path.
let readStageContent (runGit: GitRunner) (stage: int) (path: string) : Async<string option> =
    async {
        let! output = runGit [| "show"; $":{stage}:{path}" |] None

        match output with
        | Ok result when result.ExitCode = 0 -> return Some result.StdOut
        | _ -> return None
    }

/// Builds conflict items with workspace/target/base candidates and text previews.
let buildConflictItems
    (runGit: GitRunner)
    (readCombinedPreview: CombinedPreviewReader)
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
                let! baseContent = readStageContent runGit 1 pathValue
                let! workspaceContent = readStageContent runGit 2 pathValue
                let! targetContent = readStageContent runGit 3 pathValue
                let! combinedPreviewResult = readCombinedPreview pathValue

                match combinedPreviewResult with
                | Error failure -> previewFailure <- Some failure
                | Ok combinedPreview ->
                    items.Add {
                        Path = path
                        Candidates = [|
                            {
                                CandidateId = "workspace"
                                Label = "Workspace version"
                                Revision = None
                                Preview = workspaceContent |> Option.map TextPreview
                            }
                            {
                                CandidateId = "target"
                                Label = "Target version"
                                Revision = mergeHeadRevision
                                Preview = targetContent |> Option.map TextPreview
                            }
                            yield!
                                match baseContent with
                                | Some content ->
                                    [|
                                        {
                                            CandidateId = "base"
                                            Label = "Base version"
                                            Revision = None
                                            Preview = Some(TextPreview content)
                                        }
                                    |]
                                | None -> [||]
                        |]
                        CombinedPreview = combinedPreview
                        SupportsResolvedContent = true
                    }

        match previewFailure with
        | Some failure -> return Error failure
        | None -> return Ok(items.ToArray())
    }
