/// Isolated selected-revision transaction: a temporary index seeded from HEAD,
/// exact selected paths, commit-tree, and a compare-and-swap ref update that
/// never disturbs the real index or unrelated working-tree state.
module VersionControlService.Git.GitSelectedRevision

open System
open VersionControlService.Abstractions

module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path

/// Runs git in the workspace with optional stdin and extra environment.
type GitRunner =
    string[] -> string option -> (string * string)[] -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>

/// Invoked at named transaction points so tests can inject deterministic barriers.
type TransactionBarrier = string -> Async<unit>

let private failedRun (operation: string) (output: NodeProcess.ProcessOutput) =
    OperationFailure.createRedacted ProviderError "git_failure" $"{operation} failed: {output.StdErr}"

/// Metadata checks before the transaction commits: batched file sizes via stat,
/// then ONE `git check-attr --stdin -z filter` call carrying every oversized
/// selected path over NUL stdin — never per-path processes or argv path lists.
let private checkSelectedMetadata
    (runGit: GitRunner)
    (repoPath: string)
    (paths: RepositoryPath[])
    : Async<Result<OperationWarning[], OperationFailure>> =
    async {
        // Threshold config mirrors the extracted service's large-object policy key.
        let! thresholdOutput =
            runGit [| "config"; "--get"; "swate.lfs.autotrackthresholdmb" |] None [||]

        let thresholdMb =
            match thresholdOutput with
            | Ok output when output.ExitCode = 0 ->
                match Int32.TryParse(output.StdOut.Trim()) with
                | true, value when value > 0 -> value
                | _ -> 1
            | _ -> 1

        let thresholdBytes = float thresholdMb * 1024.0 * 1024.0

        let oversizedPaths =
            paths
            |> Array.map RepositoryPath.value
            |> Array.filter (fun pathValue ->
                let absolutePath = NodePath.join [| repoPath; pathValue |]

                try
                    NodeFileSystem.existsSync absolutePath
                    && (NodeFileSystem.statSync absolutePath).size > thresholdBytes
                with _ ->
                    false)

        if oversizedPaths.Length = 0 then
            return Ok [||]
        else
            let stdinPayload = String.concat "\000" oversizedPaths

            let! attrOutput =
                runGit [| "check-attr"; "--stdin"; "-z"; "filter" |] (Some stdinPayload) [||]

            match attrOutput with
            | Error attrFailure -> return Error attrFailure
            | Ok output when output.ExitCode <> 0 -> return Error(failedRun "git check-attr" output)
            | Ok output ->
                // -z output is NUL-delimited (path, attribute, value) triples.
                let fields = output.StdOut.Split '\000'

                let lfsTrackedPaths =
                    [|
                        for index in 0 .. 3 .. fields.Length - 3 do
                            if fields[index + 1] = "filter" && fields[index + 2] = "lfs" then
                                fields[index]
                    |]
                    |> Set.ofArray

                let warnings =
                    oversizedPaths
                    |> Array.filter (fun pathValue -> not (lfsTrackedPaths.Contains pathValue))
                    |> Array.map (fun pathValue -> {
                        Code = "oversized_object_not_tracked"
                        Message =
                            $"'{pathValue}' exceeds {thresholdMb} MB and is not tracked by large-object storage."
                    })

                return Ok warnings
    }

/// Creates a revision from exact selected paths without touching the real index
/// or unrelated working-tree state:
/// 1. validate and resolve the current branch/HEAD;
/// 2. seed a temporary index from HEAD (or empty for an unborn branch);
/// 3. `git add` the literal selection into the temporary index over NUL stdin;
/// 4. NoOp when the resulting tree equals HEAD's tree;
/// 5. `commit-tree`, then compare-and-swap `update-ref` with the expected old head;
/// 6. reconcile only the committed paths in the real index.
let createRevision
    (runGit: GitRunner)
    (barrier: TransactionBarrier)
    (repoPath: string)
    (message: string)
    (paths: RepositoryPath[])
    (context: OperationContext)
    : Async<OperationResult<RevisionId>> =
    async {
        // Validate before any mutation.
        let! branchOutput = runGit [| "symbolic-ref"; "-q"; "HEAD" |] None [||]

        match branchOutput with
        | Error failure -> return Failed failure
        | Ok branchResult when branchResult.ExitCode <> 0 ->
            return
                Failed(
                    OperationFailure.create
                        Validation
                        "detached_head"
                        "Selected revisions require a current branch; the workspace is detached."
                )
        | Ok branchResult ->
            let branchRef = branchResult.StdOut.Trim()

            let! headOutput = runGit [| "rev-parse"; "--verify"; "--quiet"; "HEAD" |] None [||]

            let expectedHead =
                match headOutput with
                | Ok output when output.ExitCode = 0 -> Some(output.StdOut.Trim())
                | _ -> None

            // Selected paths must exist in the worktree or in HEAD (deletions).
            let! headPathsOutput =
                match expectedHead with
                | Some _ ->
                    runGit
                        [| "ls-tree"; "-r"; "--name-only"; "-z"; "HEAD" |]
                        None
                        [||]
                | None -> async { return Ok { ExitCode = 0; StdOut = ""; StdErr = "" } }

            match headPathsOutput with
            | Error failure -> return Failed failure
            | Ok headPaths ->
                let headPathSet =
                    headPaths.StdOut.Split '\000'
                    |> Array.filter (fun entry -> entry <> "")
                    |> Set.ofArray

                let missing =
                    paths
                    |> Array.map RepositoryPath.value
                    |> Array.filter (fun pathValue ->
                        not (NodeFileSystem.existsSync (NodePath.join [| repoPath; pathValue |]))
                        && not (headPathSet.Contains pathValue))

                if missing.Length > 0 then
                    return
                        Failed {
                            OperationFailure.create
                                NotFound
                                "path_not_found"
                                "A selected path exists neither in the workspace nor in HEAD." with
                                AffectedPaths = missing
                        }
                else
                    let! metadataResult = checkSelectedMetadata runGit repoPath paths

                    match metadataResult with
                    | Error failure -> return Failed failure
                    | Ok warnings ->
                        // Isolated temporary index inside the resolved git dir.
                        let! gitDirOutput = runGit [| "rev-parse"; "--absolute-git-dir" |] None [||]

                        match gitDirOutput with
                        | Error failure -> return Failed failure
                        | Ok gitDirResult when gitDirResult.ExitCode <> 0 ->
                            return Failed(failedRun "git rev-parse --absolute-git-dir" gitDirResult)
                        | Ok gitDirResult ->
                            let gitDir = gitDirResult.StdOut.Trim()

                            let temporaryIndex =
                                NodePath.join [| gitDir; $"vcs-selected-index-{DateTime.Now.Ticks}" |]

                            let environment = [| "GIT_INDEX_FILE", temporaryIndex |]

                            let cleanupTemporaryIndex () =
                                try
                                    if NodeFileSystem.existsSync temporaryIndex then
                                        NodeFileSystem.unlinkSync temporaryIndex
                                with _ ->
                                    ()

                            try
                                // Seed the temporary index from HEAD (or leave empty when unborn).
                                let! seedResult =
                                    match expectedHead with
                                    | Some _ -> runGit [| "read-tree"; "HEAD" |] None environment
                                    | None -> runGit [| "read-tree"; "--empty" |] None environment

                                match seedResult with
                                | Error failure ->
                                    cleanupTemporaryIndex ()
                                    return Failed failure
                                | Ok seedOutput when seedOutput.ExitCode <> 0 ->
                                    cleanupTemporaryIndex ()
                                    return Failed(failedRun "git read-tree" seedOutput)
                                | Ok _ ->
                                    // Stage exactly the literal selection over NUL stdin.
                                    let stdinPayload = GitPathTransport.nulDelimitedLiteralPathspecs paths

                                    let! addResult =
                                        runGit
                                            [| "add"; yield! GitPathTransport.pathspecFromStdinArguments |]
                                            (Some stdinPayload)
                                            environment

                                    match addResult with
                                    | Error failure ->
                                        cleanupTemporaryIndex ()
                                        return Failed failure
                                    | Ok addOutput when addOutput.ExitCode <> 0 ->
                                        cleanupTemporaryIndex ()

                                        if addOutput.StdErr.Contains "did not match any files" then
                                            return
                                                Failed {
                                                    OperationFailure.create
                                                        NotFound
                                                        "path_not_found"
                                                        "A selected path does not exist in the workspace." with
                                                        AffectedPaths = paths |> Array.map RepositoryPath.value
                                                }
                                        else
                                            return Failed(failedRun "git add (temporary index)" addOutput)
                                    | Ok _ ->
                                        let! treeResult = runGit [| "write-tree" |] None environment

                                        match treeResult with
                                        | Error failure ->
                                            cleanupTemporaryIndex ()
                                            return Failed failure
                                        | Ok treeOutput when treeOutput.ExitCode <> 0 ->
                                            cleanupTemporaryIndex ()
                                            return Failed(failedRun "git write-tree" treeOutput)
                                        | Ok treeOutput ->
                                            let newTree = treeOutput.StdOut.Trim()

                                            let! headTree =
                                                match expectedHead with
                                                | Some _ -> runGit [| "rev-parse"; "HEAD^{tree}" |] None [||]
                                                | None ->
                                                    async {
                                                        return
                                                            Ok {
                                                                ExitCode = 0
                                                                StdOut = ""
                                                                StdErr = ""
                                                            }
                                                    }

                                            let headTreeValue =
                                                match headTree with
                                                | Ok output when output.ExitCode = 0 -> output.StdOut.Trim()
                                                | _ -> ""

                                            if newTree = headTreeValue then
                                                cleanupTemporaryIndex ()

                                                match expectedHead with
                                                | Some head ->
                                                    match RevisionId.tryCreate head with
                                                    | Ok revisionId ->
                                                        return
                                                            OperationResult.noOp
                                                                (Some "The selected paths are unchanged.")
                                                                revisionId
                                                    | Error errorMessage -> return failwith errorMessage
                                                | None ->
                                                    return
                                                        Failed(
                                                            OperationFailure.create
                                                                Validation
                                                                "nothing_to_commit"
                                                                "The selection contains no changes to commit."
                                                        )
                                            else
                                                let commitArguments = [|
                                                    "commit-tree"
                                                    newTree
                                                    yield!
                                                        (match expectedHead with
                                                         | Some head -> [| "-p"; head |]
                                                         | None -> [||])
                                                    "-m"
                                                    message
                                                |]

                                                let! commitResult = runGit commitArguments None [||]

                                                match commitResult with
                                                | Error failure ->
                                                    cleanupTemporaryIndex ()
                                                    return Failed failure
                                                | Ok commitOutput when commitOutput.ExitCode <> 0 ->
                                                    cleanupTemporaryIndex ()
                                                    return Failed(failedRun "git commit-tree" commitOutput)
                                                | Ok commitOutput ->
                                                    let newCommit = commitOutput.StdOut.Trim()

                                                    do! barrier "selected-revision-pre-update-ref"

                                                    // Compare-and-swap: the ref only moves if it
                                                    // still points at the expected old head.
                                                    let updateRefArguments = [|
                                                        "update-ref"
                                                        branchRef
                                                        newCommit
                                                        (expectedHead |> Option.defaultValue "")
                                                    |]

                                                    let! updateResult = runGit updateRefArguments None [||]

                                                    match updateResult with
                                                    | Error failure ->
                                                        cleanupTemporaryIndex ()
                                                        return Failed failure
                                                    | Ok updateOutput when updateOutput.ExitCode <> 0 ->
                                                        cleanupTemporaryIndex ()

                                                        return
                                                            Failed(
                                                                OperationFailure.create
                                                                    Concurrency
                                                                    "precondition_failed"
                                                                    "HEAD moved while the revision was being created."
                                                            )
                                                    | Ok _ ->
                                                        cleanupTemporaryIndex ()
                                                        do! barrier "selected-revision-post-update-ref"

                                                        // Reconcile only the committed paths in the
                                                        // real index (unrelated staged state untouched).
                                                        let! reconcileResult =
                                                            runGit
                                                                [|
                                                                    "reset"
                                                                    yield!
                                                                        GitPathTransport.pathspecFromStdinArguments
                                                                |]
                                                                (Some stdinPayload)
                                                                [||]

                                                        let revisionId =
                                                            match RevisionId.tryCreate newCommit with
                                                            | Ok value -> value
                                                            | Error errorMessage -> failwith errorMessage

                                                        match reconcileResult with
                                                        | Ok reconcileOutput when reconcileOutput.ExitCode = 0 ->
                                                            return
                                                                Succeeded {
                                                                    OperationOutcome.performed revisionId with
                                                                        AffectedPaths =
                                                                            paths |> Array.map RepositoryPath.value
                                                                        ResultingRevision = Some revisionId
                                                                        Warnings = warnings
                                                                        Publication = LocalOnly
                                                                }
                                                        | _ ->
                                                            // The revision exists; only reconciliation failed.
                                                            return
                                                                OperationResult.partiallySucceeded
                                                                    {
                                                                        OperationOutcome.performed revisionId with
                                                                            AffectedPaths =
                                                                                paths
                                                                                |> Array.map RepositoryPath.value
                                                                            ResultingRevision = Some revisionId
                                                                            Warnings = warnings
                                                                            Publication = LocalOnly
                                                                    }
                                                                    (OperationFailure.create
                                                                        ProviderError
                                                                        "index_reconciliation_failed"
                                                                        "The revision was created but the real index could not be reconciled.")
                                                                    {
                                                                        Code = "reconcile_index"
                                                                        Instructions =
                                                                            Some
                                                                                "Run git reset on the committed paths to refresh their index entries."
                                                                    }
                            finally
                                cleanupTemporaryIndex ()
    }
