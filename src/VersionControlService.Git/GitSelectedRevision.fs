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

/// Repository-relative roots of gitlink (submodule) entries in the index.
let listGitlinkRoots (runGit: GitRunner) : Async<Result<string[], OperationFailure>> =
    async {
        let! output = runGit [| "ls-files"; "-z"; "--stage" |] None [||]

        match output with
        | Error failure -> return Error failure
        | Ok result when result.ExitCode <> 0 -> return Error(failedRun "git ls-files --stage" result)
        | Ok result ->
            return
                result.StdOut.Split '\000'
                |> Array.filter (fun entry -> entry.StartsWith "160000 ")
                |> Array.choose (fun entry ->
                    match entry.Split '\t' with
                    | [| _; path |] -> Some path
                    | _ -> None)
                |> Ok
    }

/// Rejects selections that touch a submodule: paths inside a gitlink and the
/// gitlink itself are outside the selected-revision contract.
let private validateSubmoduleBoundary
    (gitlinkRoots: string[])
    (paths: RepositoryPath[])
    : Result<unit, OperationFailure> =
    let violations =
        paths
        |> Array.map RepositoryPath.value
        |> Array.filter (fun pathValue ->
            gitlinkRoots
            |> Array.exists (fun root -> pathValue = root || pathValue.StartsWith(root + "/")))

    if violations.Length > 0 then
        Error {
            OperationFailure.create
                Validation
                "submodule_internal_path"
                "Selected paths must not touch a submodule; gitlink entries are never created or modified." with
                AffectedPaths = violations
        }
    else
        Ok()

type private SelectedLfsPlan = {
    ThresholdBytes: float
    OversizedPaths: string[]
    GeneratedAttributesContent: string option
    AttributesOriginalIdentity: NodeFileSystem.Stats option
    AttributesOriginalContent: string
}

let private invalidThresholdFailure () =
    OperationFailure.create
        Validation
        "invalid_lfs_threshold"
        "The automatic LFS threshold must be a positive whole MiB value."

let private dependencyMissingFailure (detail: string) =
    OperationFailure.createRedacted
        DependencyMissing
        "git_lfs_missing"
        $"Git LFS is required before an oversized selected file can be committed: {detail}"

/// Computes every automatic-LFS decision before the selected-revision index or
/// ref is mutated. The plan owns exact literal rules and rejects unrelated
/// dirty attributes instead of merging consumer state implicitly.
let private checkSelectedMetadata
    (runGit: GitRunner)
    (repoPath: string)
    (paths: RepositoryPath[])
    : Async<Result<SelectedLfsPlan, OperationFailure>> =
    async {
        let! thresholdOutput =
            runGit [| "config"; "--get"; GitService.AutoTrackThresholdKey |] None [||]

        let thresholdResult =
            match thresholdOutput with
            | Ok output when output.ExitCode = 0 ->
                match Int32.TryParse(output.StdOut.Trim()) with
                | true, value when value > 0 -> Ok value
                | _ -> Error(invalidThresholdFailure ())
            | Ok output when output.ExitCode = 1 -> Ok GitService.DefaultAutoTrackThresholdMb
            | Ok output -> Error(failedRun "git config --get automatic LFS threshold" output)
            | Error failure -> Error failure

        match thresholdResult with
        | Error failure -> return Error failure
        | Ok thresholdMb ->
            let thresholdBytes = float thresholdMb * 1024.0 * 1024.0

            let oversizedPaths =
                paths
                |> Array.map RepositoryPath.value
                |> Array.filter (fun pathValue ->
                    let absolutePath = NodePath.join [| repoPath; pathValue |]

                    try
                        NodeFileSystem.existsSync absolutePath
                        && (NodeFileSystem.lstatSync absolutePath).isFile ()
                        && (NodeFileSystem.statSync absolutePath).size >= thresholdBytes
                    with _ ->
                        false)

            if oversizedPaths.Length = 0 then
                return
                    Ok {
                        ThresholdBytes = thresholdBytes
                        OversizedPaths = [||]
                        GeneratedAttributesContent = None
                        AttributesOriginalIdentity = None
                        AttributesOriginalContent = ""
                    }
            else
                let! lfsVersion = runGit [| "lfs"; "version" |] None [||]

                match lfsVersion with
                | Error failure -> return Error(dependencyMissingFailure failure.Message)
                | Ok output when output.ExitCode <> 0 ->
                    let detail =
                        if String.IsNullOrWhiteSpace output.StdErr then output.StdOut else output.StdErr

                    return Error(dependencyMissingFailure detail)
                | Ok _ ->
                    let attributesSelected =
                        paths
                        |> Array.exists (fun path -> RepositoryPath.value path = ".gitattributes")

                    let! attributesStatus =
                        runGit [| "status"; "--porcelain=v1"; "-z"; "--"; ".gitattributes" |] None [||]

                    match attributesStatus with
                    | Error failure -> return Error failure
                    | Ok output when output.ExitCode <> 0 ->
                        return Error(failedRun "git status -- .gitattributes" output)
                    | Ok output when output.StdOut <> "" && not attributesSelected ->
                        return
                            Error {
                                OperationFailure.create
                                    Validation
                                    "precondition_failed"
                                    "Automatic LFS tracking cannot modify an unrelated dirty .gitattributes file." with
                                    AffectedPaths = [| ".gitattributes" |]
                            }
                    | Ok _ ->
                        let attributesPath = NodePath.join [| repoPath; ".gitattributes" |]

                        let attributesReadResult =
                            try
                                Ok(GitLfsService.readAttributesNoFollow attributesPath)
                            with error ->
                                Error {
                                    OperationFailure.createRedacted
                                        ProviderError
                                        "attributes_read_failed"
                                        error.Message with
                                        AffectedPaths = [| ".gitattributes" |]
                                }

                        match attributesReadResult with
                        | Error failure -> return Error failure
                        | Ok(attributesContent, originalIdentity) ->
                            let updatedAttributes, generated =
                                GitLfsService.addLiteralTrackingRules attributesContent oversizedPaths

                            return
                                Ok {
                                    ThresholdBytes = thresholdBytes
                                    OversizedPaths = oversizedPaths
                                    GeneratedAttributesContent = if generated then Some updatedAttributes else None
                                    AttributesOriginalIdentity = originalIdentity
                                    AttributesOriginalContent = attributesContent
                                }
    }

let private tryPointerOid (pointerText: string) =
    pointerText.Replace("\r\n", "\n").Split '\n'
    |> Array.tryPick (fun line ->
        let prefix = "oid sha256:"

        if line.StartsWith(prefix, StringComparison.Ordinal) then
            let oid = line.Substring(prefix.Length).Trim()
            if oid.Length = 64 then Some oid else None
        else
            None)

let private prepareLfsPointerBlob
    (runGit: GitRunner)
    (repoPath: string)
    (commonGitDir: string)
    (relativePath: string)
    : Async<Result<string, OperationFailure>> =
    async {
        let sourcePath = NodePath.join [| repoPath; relativePath |]
        let snapshotPath = NodePath.join [| commonGitDir; $"vcs-lfs-snapshot-{DateTime.Now.Ticks}" |]

        let cleanupSnapshot () =
            try
                if NodeFileSystem.existsSync snapshotPath then
                    NodeFileSystem.unlinkSync snapshotPath
            with _ ->
                ()

        try
            return!
                async {
                    try
                        NodeFileSystem.copyFileSync sourcePath snapshotPath

                        let! pointerResult =
                            runGit [| "lfs"; "pointer"; $"--file={snapshotPath}" |] None [||]

                        match pointerResult with
                        | Error failure -> return Error(dependencyMissingFailure failure.Message)
                        | Ok output when output.ExitCode <> 0 ->
                            let detail =
                                if String.IsNullOrWhiteSpace output.StdErr then output.StdOut else output.StdErr

                            return Error(dependencyMissingFailure detail)
                        | Ok output ->
                            match tryPointerOid output.StdOut with
                            | None ->
                                return
                                    Error(
                                        OperationFailure.create
                                            ProviderError
                                            "lfs_pointer_invalid"
                                            "Git LFS did not return a canonical pointer for an oversized selected file."
                                    )
                            | Some oid ->
                                let objectDirectory =
                                    NodePath.join
                                        [|
                                            commonGitDir
                                            "lfs"
                                            "objects"
                                            oid.Substring(0, 2)
                                            oid.Substring(2, 2)
                                        |]

                                let objectPath = NodePath.join [| objectDirectory; oid |]
                                NodeFileSystem.mkdirSync objectDirectory (NodeFileSystem.MkdirOptions(recursive = true))

                                if NodeFileSystem.existsSync objectPath then
                                    cleanupSnapshot ()
                                else
                                    NodeFileSystem.renameSync snapshotPath objectPath

                                let! pointerBlob =
                                    runGit [| "hash-object"; "-w"; "--stdin" |] (Some output.StdOut) [||]

                                match pointerBlob with
                                | Error failure -> return Error failure
                                | Ok hashOutput when hashOutput.ExitCode <> 0 ->
                                    return Error(failedRun "git hash-object (LFS pointer)" hashOutput)
                                | Ok hashOutput -> return Ok(hashOutput.StdOut.Trim())
                    with error ->
                        return
                            Error(
                                OperationFailure.createRedacted
                                    ProviderError
                                    "lfs_object_prepare_failed"
                                    error.Message
                            )
                }
        finally
            cleanupSnapshot ()
    }

let private hashTextBlob (runGit: GitRunner) (content: string) =
    async {
        let! result = runGit [| "hash-object"; "-w"; "--stdin" |] (Some content) [||]

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 -> return Error(failedRun "git hash-object" output)
        | Ok output -> return Ok(output.StdOut.Trim())
    }

let private updateTemporaryIndexEntry
    (runGit: GitRunner)
    (environment: (string * string)[])
    (mode: string)
    (blobId: string)
    (path: string)
    =
    async {
        let indexInfo = $"{mode} {blobId}\t{path}\000"
        let! result = runGit [| "update-index"; "-z"; "--index-info" |] (Some indexInfo) environment

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 -> return Error(failedRun "git update-index" output)
        | Ok _ -> return Ok()
    }

type private TemporaryIndexEntry = {
    Mode: string
    BlobId: string
    Path: string
}

let private invalidTemporaryIndexEntry path =
    {
        OperationFailure.create
            ProviderError
            "temporary_index_entry_invalid"
            "Git returned malformed staged metadata for a selected file." with
            AffectedPaths = [| path |]
    }

let private listTemporaryIndexEntries
    (runGit: GitRunner)
    (environment: (string * string)[])
    (path: string)
    =
    async {
        let! result =
            runGit
                [| "--literal-pathspecs"; "ls-files"; "--stage"; "-z"; "--"; path |]
                None
                environment

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 -> return Error(failedRun "git ls-files --stage" output)
        | Ok output ->
            let mutable failure = None
            let entries = ResizeArray<TemporaryIndexEntry>()

            for value in output.StdOut.Split '\000' |> Array.filter (String.IsNullOrEmpty >> not) do
                match failure with
                | Some _ -> ()
                | None ->
                    let separatorIndex = value.IndexOf '\t'

                    if separatorIndex <= 0 || separatorIndex = value.Length - 1 then
                        failure <- Some(invalidTemporaryIndexEntry path)
                    else
                        let metadata = value.Substring(0, separatorIndex)
                        let entryPath = value.Substring(separatorIndex + 1)

                        match metadata.Split(' ', StringSplitOptions.RemoveEmptyEntries) with
                        | [| mode; blobId; "0" |] ->
                            entries.Add {
                                Mode = mode
                                BlobId = blobId
                                Path = entryPath
                            }
                        | _ -> failure <- Some(invalidTemporaryIndexEntry path)

            match failure with
            | Some currentFailure -> return Error currentFailure
            | None -> return Ok(entries.ToArray())
    }

let private temporaryIndexMode
    (runGit: GitRunner)
    (environment: (string * string)[])
    (path: string)
    =
    async {
        match! listTemporaryIndexEntries runGit environment path with
        | Error failure -> return Error failure
        | Ok entries ->
            match entries |> Array.tryFind (fun entry -> entry.Path = path) with
            | Some entry -> return Ok entry.Mode
            | None ->
                return
                    Error(
                        OperationFailure.create
                            ProviderError
                            "temporary_index_entry_missing"
                            "The oversized selected file was not present in the temporary index."
                    )
    }

let private validateLfsPlanAgainstTemporaryIndex
    (runGit: GitRunner)
    (environment: (string * string)[])
    (paths: RepositoryPath[])
    (plan: SelectedLfsPlan)
    =
    async {
        let mutable failure = None
        let plannedOversized = plan.OversizedPaths |> Set.ofArray
        let mutable newlyOversized = Set.empty

        for path in paths |> Array.map RepositoryPath.value do
            match failure with
            | Some _ -> ()
            | None ->
                match! listTemporaryIndexEntries runGit environment path with
                | Error currentFailure -> failure <- Some currentFailure
                | Ok entries ->
                    for entry in entries do
                        match failure with
                        | Some _ -> ()
                        | None when plannedOversized.Contains entry.Path -> ()
                        | None when entry.Mode.StartsWith("100", StringComparison.Ordinal) ->
                            let! sizeResult = runGit [| "cat-file"; "-s"; entry.BlobId |] None [||]

                            match sizeResult with
                            | Error currentFailure -> failure <- Some currentFailure
                            | Ok output when output.ExitCode <> 0 ->
                                failure <- Some(failedRun "git cat-file -s" output)
                            | Ok output ->
                                match Int64.TryParse(output.StdOut.Trim()) with
                                | true, size when float size >= plan.ThresholdBytes ->
                                    newlyOversized <- newlyOversized.Add entry.Path
                                | true, _ -> ()
                                | _ ->
                                    failure <-
                                        Some(
                                            OperationFailure.create
                                                ProviderError
                                                "staged_blob_size_invalid"
                                                "Git returned an invalid size for a staged selected file."
                                        )
                        | None -> ()

        match failure with
        | Some currentFailure -> return Error currentFailure
        | None when newlyOversized.IsEmpty -> return Ok()
        | None ->
            return
                Error {
                    OperationFailure.create
                        Concurrency
                        "selected_content_changed"
                        "A selected file crossed the automatic LFS threshold while the revision was being prepared." with
                        AffectedPaths = newlyOversized |> Set.toArray
                }
    }

let private applyLfsPlanToTemporaryIndex
    (runGit: GitRunner)
    (repoPath: string)
    (commonGitDir: string)
    (environment: (string * string)[])
    (plan: SelectedLfsPlan)
    =
    async {
        let mutable failure = None

        for relativePath in plan.OversizedPaths do
            match failure with
            | Some _ -> ()
            | None ->
                match! prepareLfsPointerBlob runGit repoPath commonGitDir relativePath with
                | Error currentFailure -> failure <- Some currentFailure
                | Ok pointerBlob ->
                    match! temporaryIndexMode runGit environment relativePath with
                    | Error currentFailure -> failure <- Some currentFailure
                    | Ok mode ->
                        match!
                            updateTemporaryIndexEntry
                                runGit
                                environment
                                mode
                                pointerBlob
                                relativePath
                        with
                        | Error currentFailure -> failure <- Some currentFailure
                        | Ok() -> ()

        match failure, plan.GeneratedAttributesContent with
        | Some currentFailure, _ -> return Error currentFailure
        | None, None -> return Ok()
        | None, Some attributesContent ->
            match! hashTextBlob runGit attributesContent with
            | Error currentFailure -> return Error currentFailure
            | Ok attributesBlob ->
                return!
                    updateTemporaryIndexEntry
                        runGit
                        environment
                        "100644"
                        attributesBlob
                        ".gitattributes"
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

                let! gitlinkRootsResult = listGitlinkRoots runGit

                match gitlinkRootsResult with
                | Error failure -> return Failed failure
                | Ok gitlinkRoots ->

                match validateSubmoduleBoundary gitlinkRoots paths with
                | Error failure -> return Failed failure
                | Ok() ->
                    let! metadataResult = checkSelectedMetadata runGit repoPath paths

                    match metadataResult with
                    | Error failure -> return Failed failure
                    | Ok lfsPlan ->
                        do! barrier "selected-revision-post-metadata-check"

                        // Isolated temporary index inside the resolved git dir.
                        let! gitDirOutput = runGit [| "rev-parse"; "--absolute-git-dir" |] None [||]

                        match gitDirOutput with
                        | Error failure -> return Failed failure
                        | Ok gitDirResult when gitDirResult.ExitCode <> 0 ->
                            return Failed(failedRun "git rev-parse --absolute-git-dir" gitDirResult)
                        | Ok gitDirResult ->
                            let gitDir = gitDirResult.StdOut.Trim()

                            let! commonGitDirOutput =
                                runGit
                                    [| "rev-parse"; "--path-format=absolute"; "--git-common-dir" |]
                                    None
                                    [||]

                            let commonGitDir =
                                match commonGitDirOutput with
                                | Ok output when output.ExitCode = 0 && not (String.IsNullOrWhiteSpace output.StdOut) ->
                                    output.StdOut.Trim()
                                | _ -> gitDir

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
                                        let! stagedPlanValidation =
                                            validateLfsPlanAgainstTemporaryIndex runGit environment paths lfsPlan

                                        let! preparedTree =
                                            match stagedPlanValidation with
                                            | Error failure -> async { return Error failure }
                                            | Ok() ->
                                                async {
                                                    match!
                                                        applyLfsPlanToTemporaryIndex
                                                            runGit
                                                            repoPath
                                                            commonGitDir
                                                            environment
                                                            lfsPlan
                                                    with
                                                    | Error failure -> return Error failure
                                                    | Ok() ->
                                                        let! treeResult = runGit [| "write-tree" |] None environment

                                                        match treeResult with
                                                        | Error failure -> return Error failure
                                                        | Ok treeOutput when treeOutput.ExitCode <> 0 ->
                                                            return Error(failedRun "git write-tree" treeOutput)
                                                        | Ok treeOutput -> return Ok(treeOutput.StdOut.Trim())
                                                }

                                        match preparedTree with
                                        | Error failure ->
                                            cleanupTemporaryIndex ()
                                            return Failed failure
                                        | Ok newTree ->

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

                                                        let attributesPath =
                                                            RepositoryPath.tryCreate ".gitattributes"
                                                            |> Result.defaultWith failwith

                                                        let reconciliationPaths =
                                                            match lfsPlan.GeneratedAttributesContent with
                                                            | Some _ when not (paths |> Array.contains attributesPath) ->
                                                                Array.append paths [| attributesPath |]
                                                            | _ -> paths

                                                        let affectedPaths =
                                                            reconciliationPaths |> Array.map RepositoryPath.value

                                                        let revisionId =
                                                            match RevisionId.tryCreate newCommit with
                                                            | Ok value -> value
                                                            | Error errorMessage -> failwith errorMessage

                                                        let outcome = {
                                                            OperationOutcome.performed revisionId with
                                                                AffectedPaths = affectedPaths
                                                                ResultingRevision = Some revisionId
                                                                Publication = LocalOnly
                                                        }

                                                        let partialReconciliation failure =
                                                            OperationResult.partiallySucceeded
                                                                outcome
                                                                {
                                                                    failure with
                                                                        AffectedPaths = affectedPaths
                                                                }
                                                                {
                                                                    Code = "reconcile_index"
                                                                    Instructions =
                                                                        Some
                                                                            "Merge the generated literal Git LFS rules into the current .gitattributes without discarding concurrent edits, then reconcile the affected paths in the index (for example with git reset)."
                                                                }

                                                        let! attributesWriteResult =
                                                            match lfsPlan.GeneratedAttributesContent with
                                                            | None -> async { return Ok() }
                                                            | Some attributesContent ->
                                                                async {
                                                                    let mutable replacement = None

                                                                    try
                                                                        let attributesFile =
                                                                            NodePath.join [| repoPath; ".gitattributes" |]

                                                                        let! prepared =
                                                                            GitLfsService.prepareAttributesReplacement
                                                                                attributesFile
                                                                                lfsPlan.AttributesOriginalIdentity
                                                                                lfsPlan.AttributesOriginalContent
                                                                                attributesContent

                                                                        replacement <- Some prepared
                                                                        do! barrier "selected-revision-attributes-replacement-ready"
                                                                        do! GitLfsService.applyAttributesReplacement prepared
                                                                        do! barrier "selected-revision-attributes-installed"
                                                                        do! GitLfsService.completeAttributesReplacement prepared
                                                                        replacement <- None
                                                                        return Ok()
                                                                    with error ->
                                                                        match replacement with
                                                                        | Some prepared ->
                                                                            try
                                                                                do! GitLfsService.abortAttributesReplacement prepared
                                                                            with _ ->
                                                                                ()
                                                                        | None -> ()

                                                                        return
                                                                            Error(
                                                                                OperationFailure.createRedacted
                                                                                    ProviderError
                                                                                    "attributes_reconciliation_failed"
                                                                                    error.Message
                                                                            )
                                                                }

                                                        // Reconcile only the committed paths in the
                                                        // real index (unrelated staged state untouched).
                                                        match attributesWriteResult with
                                                        | Error failure -> return partialReconciliation failure
                                                        | Ok() ->
                                                            let reconciliationPayload =
                                                                GitPathTransport.nulDelimitedLiteralPathspecs reconciliationPaths

                                                            let! reconcileResult =
                                                                runGit
                                                                    [|
                                                                        "reset"
                                                                        yield!
                                                                            GitPathTransport.pathspecFromStdinArguments
                                                                    |]
                                                                    (Some reconciliationPayload)
                                                                    [||]

                                                            match reconcileResult with
                                                            | Ok reconcileOutput when reconcileOutput.ExitCode = 0 ->
                                                                return Succeeded outcome
                                                            | Error failure ->
                                                                return
                                                                    partialReconciliation (
                                                                        OperationFailure.createRedacted
                                                                            ProviderError
                                                                            "index_reconciliation_failed"
                                                                            failure.Message
                                                                    )
                                                            | Ok reconcileOutput ->
                                                                // The revision exists; only reconciliation failed.
                                                                return
                                                                    partialReconciliation (
                                                                        OperationFailure.createRedacted
                                                                            ProviderError
                                                                            "index_reconciliation_failed"
                                                                            reconcileOutput.StdErr
                                                                    )
                            finally
                                cleanupTemporaryIndex ()
    }
