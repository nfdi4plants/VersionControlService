/// Isolated selected-revision transaction: a temporary index seeded from HEAD,
/// exact selected paths, commit-tree, and a compare-and-swap ref update that
/// never disturbs the real index or unrelated working-tree state.
module internal VersionControlService.Git.GitSelectedRevision

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions

module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodeInterop = VersionControlService.Runtime.Node.Interop

let private fileSystemDynamic: obj = importAll "node:fs"
let private maxLfsPointerProbeBytes = 1024.0

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
    ObservedOversizedPaths: string[]
    OversizedPaths: string[]
    InlinePaths: string[]
    GeneratedAttributesContent: string option
    AttributesOriginalIdentity: NodeFileSystem.Stats option
    AttributesOriginalContent: string
}

type private LfsPointerInfo = {
    Oid: string
    SizeInBytes: float
}

type private SelectedPathPolicy = {
    Path: string
    SizeInBytes: float
    IsLfsPointer: bool
    Policy: RevisionPathPolicy
}

let private tryParseLfsPointer (pointerText: string) =
    let lines =
        pointerText.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map _.Trim()
        |> Array.filter (String.IsNullOrWhiteSpace >> not)

    let oid =
        lines
        |> Array.tryPick (fun line ->
            let prefix = "oid sha256:"

            if line.StartsWith(prefix, StringComparison.Ordinal) && line.Length = prefix.Length + 64 then
                let value = line.Substring(prefix.Length)

                if value |> Seq.forall (fun character -> Char.IsDigit character || ('a' <= character && character <= 'f')) then
                    Some value
                else
                    None
            else
                None)

    let size =
        lines
        |> Array.tryPick (fun line ->
            let prefix = "size "

            if line.StartsWith(prefix, StringComparison.Ordinal) then
                let value = line.Substring(prefix.Length)

                if value.Length > 0 && value |> Seq.forall Char.IsDigit then
                    match Int64.TryParse value with
                    | true, parsed when parsed >= 0L -> Some(float parsed)
                    | _ -> None
                else
                    None
            else
                None)

    if
        lines.Length >= 3
        && lines.[0].Equals("version https://git-lfs.github.com/spec/v1", StringComparison.Ordinal)
    then
        match oid, size with
        | Some oidValue, Some sizeValue ->
            Some {
                Oid = oidValue
                SizeInBytes = sizeValue
            }
        | _ -> None
    else
        None

let private readLfsPointerProbe (path: string) (knownStats: NodeFileSystem.Stats option) =
    if knownStats |> Option.exists (fun stats -> stats.size > maxLfsPointerProbeBytes) then
        None
    else
        let descriptor, openedStats = NodeFileSystem.openReadOnlyNoFollowSync path

        try
            if openedStats.size > maxLfsPointerProbeBytes then
                None
            else
                let buffer = NodeInterop.bufferAlloc 1024
                let bytesRead: int =
                    fileSystemDynamic?readSync (descriptor, buffer, 0, 1024, null)
                    |> unbox

                if float bytesRead = openedStats.size then
                    Some(
                        NodeInterop.bufferSubarray buffer 0 bytesRead
                        |> NodeInterop.bufferToUtf8String
                    )
                else
                    None
        finally
            NodeFileSystem.closeFileDescriptorSync descriptor

let private invalidThresholdFailure () =
    OperationFailure.create
        Validation
        "invalid_lfs_threshold"
        GitService.InvalidLfsThresholdMessage

let private dependencyMissingFailure (detail: string) =
    OperationFailure.createRedacted
        DependencyMissing
        "git_lfs_missing"
        $"Git LFS is required before an oversized selected file can be committed: {detail}"

let private selectedFileStatFailure (path: string) (error: exn) =
    OperationFailure.createRedacted
        ProviderError
        "lfs_file_stat_failed"
        error.Message
    |> fun failure -> { failure with AffectedPaths = [| path |] }

let private listSelectedCandidateFiles
    (runGit: GitRunner)
    (environment: (string * string)[])
    (paths: RepositoryPath[])
    =
    async {
        let! outputResult =
            runGit
                [|
                    "ls-files"
                    "-z"
                    "--cached"
                    "--others"
                    "--exclude-standard"
                    "--"
                    yield! paths |> Array.map GitPathTransport.literalPathspec
                |]
                None
                environment

        match outputResult with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 ->
            return Error(failedRun "git ls-files selected LFS candidates" output)
        | Ok output ->
            return
                output.StdOut.Split '\000'
                |> Array.filter (fun path -> path <> "")
                |> Array.distinct
                |> Ok
    }

let private resolveSelectedPathPolicies
    (runGit: GitRunner)
    (barrier: TransactionBarrier)
    (repoPath: string)
    (paths: RepositoryPath[])
    (environment: (string * string)[])
    (revisionPolicy: RevisionPolicyStrategy)
    : Async<Result<SelectedPathPolicy[], OperationFailure>> =
    async {
        let selectedPolicies = ResizeArray<SelectedPathPolicy>()
        let mutable failure = None

        match! listSelectedCandidateFiles runGit environment paths with
        | Error currentFailure -> failure <- Some currentFailure
        | Ok candidates ->
            // A path deleted before planning is a valid selected deletion. Keep paths that existed before the barrier so a later lstat failure remains observable as a race.
            let candidatesPresentBeforeBarrier =
                candidates
                |> Array.filter (fun candidate ->
                    try
                        NodeFileSystem.tryLstatSync (NodePath.join [| repoPath; candidate |])
                        |> Option.isSome
                    with _ ->
                        true)

            do! barrier "selected-revision-lfs-candidates-listed"

            for candidate in candidatesPresentBeforeBarrier do
                match failure with
                | Some _ -> ()
                | None ->
                    let! fileStatsResult =
                        async {
                            try
                                let absolutePath = NodePath.join [| repoPath; candidate |]
                                let! fileStats = NodeFileSystem.lstatAsync absolutePath |> Async.AwaitPromise
                                return Ok fileStats
                            with error ->
                                return Error(selectedFileStatFailure candidate error)
                        }

                    match fileStatsResult with
                    | Error currentFailure -> failure <- Some currentFailure
                    | Ok fileStats when fileStats.isFile () ->
                        let pointerInfoResult =
                            try
                                let absolutePath = NodePath.join [| repoPath; candidate |]
                                Ok(
                                    readLfsPointerProbe absolutePath (Some fileStats)
                                    |> Option.bind tryParseLfsPointer
                                )
                            with error ->
                                Error(selectedFileStatFailure candidate error)

                        match pointerInfoResult with
                        | Error currentFailure -> failure <- Some currentFailure
                        | Ok pointerInfo ->
                            match RepositoryPath.tryCreate candidate with
                            | Error message ->
                                failure <-
                                    Some(
                                        OperationFailure.create Validation "invalid_path" message
                                        |> fun currentFailure -> {
                                            currentFailure with
                                                AffectedPaths = [| candidate |]
                                        }
                                    )
                            | Ok repositoryPath ->
                                let sizeInBytes = pointerInfo |> Option.map _.SizeInBytes |> Option.defaultValue fileStats.size

                                let policyResult =
                                    try
                                        Ok(
                                            revisionPolicy.ResolvePathPolicy {
                                                Path = repositoryPath
                                                SizeInBytes = sizeInBytes
                                            }
                                        )
                                    with error ->
                                        Error(
                                            OperationFailure.createRedacted
                                                ProviderError
                                                "revision_policy_failed"
                                                error.Message
                                            |> fun currentFailure -> {
                                                currentFailure with
                                                    AffectedPaths = [| candidate |]
                                            }
                                        )

                                match policyResult with
                                | Error currentFailure -> failure <- Some currentFailure
                                | Ok policy ->
                                    selectedPolicies.Add {
                                        Path = candidate
                                        SizeInBytes = sizeInBytes
                                        IsLfsPointer = pointerInfo.IsSome
                                        Policy = policy
                                    }
                    | Ok _ -> ()

        match failure with
        | Some currentFailure -> return Error currentFailure
        | None ->
            let inlinePointerPaths =
                selectedPolicies
                |> Seq.filter (fun selected ->
                    selected.Policy = RevisionPathPolicy.Inline
                    && selected.IsLfsPointer)
                |> Seq.map _.Path
                |> Seq.toArray

            if inlinePointerPaths.Length > 0 then
                return
                    Error {
                        OperationFailure.create
                            Validation
                            "inline_content_not_materialized"
                            "Materialize the affected files before creating an inline revision." with
                            AffectedPaths = inlinePointerPaths
                            RecoveryAction =
                                Some {
                                    Code = "retry_materialization"
                                    Instructions =
                                        Some "Materialize the affected files, refresh the status and create the revision again."
                                }
                    }
            else
                return Ok(selectedPolicies.ToArray())
    }

let private checkFilterAttributes
    (runGit: GitRunner)
    (environment: (string * string)[])
    (paths: string[])
    : Async<Result<Map<string, string>, OperationFailure>> =
    async {
        let repositoryPaths =
            paths
            |> Array.map (fun path ->
                RepositoryPath.tryCreate path
                |> Result.mapError (fun message ->
                    OperationFailure.create
                        Validation
                        "invalid_path"
                        message))

        match repositoryPaths |> Array.tryFind Result.isError with
        | Some(Error failure) -> return Error failure
        | _ ->
            let pathValues = repositoryPaths |> Array.choose Result.toOption
            let payload = GitPathTransport.nulDelimitedPaths pathValues
            let! outputResult =
                runGit
                    [| "check-attr"; "--cached"; "filter"; "--stdin"; "-z" |]
                    (Some payload)
                    environment

            match outputResult with
            | Error failure -> return Error failure
            | Ok output when output.ExitCode <> 0 ->
                return Error(failedRun "git check-attr" output)
            | Ok output ->
                return
                    GitLfsAdapter.parseFilterAttributes output.StdOut
                    |> Result.map Map.ofArray
                    |> Result.mapError (fun message ->
                        OperationFailure.create
                            ProviderError
                            "attributes_probe_failed"
                            message)
    }

/// Computes every automatic-LFS decision before the selected-revision index or
/// ref is mutated. The plan owns exact literal rules and rejects unrelated
/// dirty attributes instead of merging consumer state implicitly.
let private computeSelectedMetadata
    (runGit: GitRunner)
    (barrier: TransactionBarrier)
    (repoPath: string)
    (paths: RepositoryPath[])
    (environment: (string * string)[])
    (selectedPolicies: SelectedPathPolicy[])
    : Async<Result<SelectedLfsPlan, OperationFailure>> =
    async {
        let! thresholdOutput =
            runGit [| "config"; "--local"; "--get"; GitService.AutoTrackThresholdKey |] None [||]

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

            let observedOversizedPaths =
                selectedPolicies
                |> Array.filter (fun selected ->
                    selected.Policy <> RevisionPathPolicy.Inline
                    && selected.SizeInBytes >= thresholdBytes)
                |> Array.map _.Path

            let potentialPointerPaths =
                selectedPolicies
                |> Array.choose (fun selected ->
                    match selected.Policy with
                    | RevisionPathPolicy.LargeObject -> Some selected.Path
                    | RevisionPathPolicy.Automatic when selected.SizeInBytes >= thresholdBytes -> Some selected.Path
                    | _ -> None)

            let inlinePaths =
                selectedPolicies
                |> Array.filter (fun selected -> selected.Policy = RevisionPathPolicy.Inline)
                |> Array.map _.Path

            let pathsToCheck =
                Array.append potentialPointerPaths inlinePaths
                |> Array.distinct

            let! filterResult =
                if pathsToCheck.Length = 0 then
                    async { return Ok Map.empty }
                else
                    checkFilterAttributes runGit environment pathsToCheck

            match filterResult with
            | Error failure -> return Error failure
            | Ok filters ->
                let filterValue path = Map.tryFind path filters |> Option.defaultValue "unspecified"

                let pointerPaths =
                    selectedPolicies
                    |> Array.choose (fun selected ->
                        match selected.Policy with
                        | RevisionPathPolicy.LargeObject -> Some selected.Path
                        | RevisionPathPolicy.Automatic when
                            selected.SizeInBytes >= thresholdBytes
                            && filterValue selected.Path <> "unset" ->
                            Some selected.Path
                        | _ -> None)

                let pathsNeedingTracking =
                    pointerPaths |> Array.filter (fun path -> filterValue path <> "lfs")

                let pathsNeedingUntracking =
                    inlinePaths |> Array.filter (fun path -> filterValue path = "lfs")

                let! lfsDependencyResult =
                    if pointerPaths.Length = 0 then
                        async { return Ok() }
                    else
                        async {
                            let! lfsVersion = runGit [| "lfs"; "version" |] None [||]

                            match lfsVersion with
                            | Error failure -> return Error(dependencyMissingFailure failure.Message)
                            | Ok output when output.ExitCode <> 0 ->
                                let detail =
                                    if String.IsNullOrWhiteSpace output.StdErr then output.StdOut else output.StdErr

                                return Error(dependencyMissingFailure detail)
                            | Ok _ -> return Ok()
                        }

                match lfsDependencyResult with
                | Error failure -> return Error failure
                | Ok() when pathsNeedingTracking.Length = 0 && pathsNeedingUntracking.Length = 0 ->
                    return
                        Ok {
                            ThresholdBytes = thresholdBytes
                            ObservedOversizedPaths = observedOversizedPaths
                            OversizedPaths = pointerPaths
                            InlinePaths = inlinePaths
                            GeneratedAttributesContent = None
                            AttributesOriginalIdentity = None
                            AttributesOriginalContent = ""
                        }
                | Ok() ->
                    let attributesSelected =
                        paths
                        |> Array.exists (fun path -> RepositoryPath.value path = ".gitattributes")

                    let! attributesStatus =
                        runGit [| "status"; "--porcelain"; "-z"; "--"; ".gitattributes" |] None [||]

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
                                    "Generated LFS rules cannot modify an unrelated dirty .gitattributes file." with
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
                            let trackedAttributes, _ =
                                GitLfsService.addLiteralTrackingRules attributesContent pathsNeedingTracking

                            let updatedAttributes, _ =
                                GitLfsService.addLiteralUntrackingRules trackedAttributes pathsNeedingUntracking

                            return
                                Ok {
                                    ThresholdBytes = thresholdBytes
                                    ObservedOversizedPaths = observedOversizedPaths
                                    OversizedPaths = pointerPaths
                                    InlinePaths = inlinePaths
                                    GeneratedAttributesContent =
                                        if updatedAttributes <> attributesContent then Some updatedAttributes else None
                                    AttributesOriginalIdentity = originalIdentity
                                    AttributesOriginalContent = attributesContent
                                }
    }

let private checkSelectedMetadataWithPolicies
    (runGit: GitRunner)
    (barrier: TransactionBarrier)
    (repoPath: string)
    (paths: RepositoryPath[])
    (expectedHead: string option)
    (selectedPolicies: SelectedPathPolicy[])
    : Async<Result<SelectedLfsPlan, OperationFailure>> =
    async {
        let! gitDirOutput = runGit [| "rev-parse"; "--absolute-git-dir" |] None [||]

        match gitDirOutput with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 ->
            return Error(failedRun "git rev-parse --absolute-git-dir" output)
        | Ok output ->
            let temporaryIndex =
                NodePath.join [| output.StdOut.Trim(); $"vcs-selected-metadata-index-{DateTime.Now.Ticks}" |]

            let environment = [| "GIT_INDEX_FILE", temporaryIndex |]

            let cleanupTemporaryIndex () =
                try
                    if NodeFileSystem.existsSync temporaryIndex then
                        NodeFileSystem.unlinkSync temporaryIndex
                with _ ->
                    ()

            try
                let! seedResult =
                    match expectedHead with
                    | Some _ -> runGit [| "read-tree"; "HEAD" |] None environment
                    | None -> runGit [| "read-tree"; "--empty" |] None environment

                match seedResult with
                | Error failure -> return Error failure
                | Ok seedOutput when seedOutput.ExitCode <> 0 ->
                    return Error(failedRun "git read-tree" seedOutput)
                | Ok _ ->
                    let stdinPayload = GitPathTransport.nulDelimitedLiteralPathspecs paths
                    let! addResult =
                        runGit
                            [| "add"; yield! GitPathTransport.pathspecFromStdinArguments |]
                            (Some stdinPayload)
                            environment

                    match addResult with
                    | Error failure -> return Error failure
                    | Ok addOutput when addOutput.ExitCode <> 0 ->
                        if addOutput.StdErr.Contains "did not match any files" then
                            return
                                Error {
                                    OperationFailure.create
                                        NotFound
                                        "path_not_found"
                                        "A selected path does not exist in the workspace." with
                                        AffectedPaths = paths |> Array.map RepositoryPath.value
                                }
                        else
                            return Error(failedRun "git add (LFS metadata index)" addOutput)
                    | Ok _ ->
                        return!
                            computeSelectedMetadata
                                runGit
                                barrier
                                repoPath
                                paths
                                environment
                                selectedPolicies
            finally
                cleanupTemporaryIndex ()
    }

let private checkSelectedMetadata
    (runGit: GitRunner)
    (barrier: TransactionBarrier)
    (repoPath: string)
    (paths: RepositoryPath[])
    (expectedHead: string option)
    (revisionPolicy: RevisionPolicyStrategy)
    : Async<Result<SelectedLfsPlan, OperationFailure>> =
    async {
        let! selectedPoliciesResult =
            resolveSelectedPathPolicies runGit barrier repoPath paths [||] revisionPolicy

        match selectedPoliciesResult with
        | Error failure -> return Error failure
        | Ok selectedPolicies ->
            return!
                checkSelectedMetadataWithPolicies
                    runGit
                    barrier
                    repoPath
                    paths
                    expectedHead
                    selectedPolicies
    }

let private tryPointerOid (pointerText: string) =
    tryParseLfsPointer pointerText |> Option.map _.Oid

let private hashTextBlob (runGit: GitRunner) (content: string) =
    async {
        let! result = runGit [| "hash-object"; "-w"; "--stdin" |] (Some content) [||]

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 -> return Error(failedRun "git hash-object" output)
        | Ok output -> return Ok(output.StdOut.Trim())
    }

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
            let sourceContentResult : Result<string option, OperationFailure> =
                try
                    Ok(readLfsPointerProbe sourcePath None)
                with error ->
                    Error(
                        OperationFailure.createRedacted
                            ProviderError
                            "lfs_object_prepare_failed"
                            error.Message
                    )

            match sourceContentResult with
            | Error failure -> return Error failure
            | Ok(Some sourceContent) when tryParseLfsPointer sourceContent |> Option.isSome ->
                return! hashTextBlob runGit sourceContent
            | Ok _ ->
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
                            "The selected file was not present in the temporary index."
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
        // Entries the plan already decided are exempt, by entry path: a selected
        // directory expands to entries, and inline or forced pointer entries may sit
        // above the threshold by design.
        let plannedOversized = plan.ObservedOversizedPaths |> Set.ofArray
        let plannedPointers = plan.OversizedPaths |> Set.ofArray
        let inlinePaths = plan.InlinePaths |> Set.ofArray
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
                        | None when
                            plannedOversized.Contains entry.Path
                            || plannedPointers.Contains entry.Path
                            || inlinePaths.Contains entry.Path
                            ->
                            ()
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

        for relativePath in plan.InlinePaths do
            match failure with
            | Some _ -> ()
            | None ->
                match! temporaryIndexMode runGit environment relativePath with
                | Error currentFailure -> failure <- Some currentFailure
                | Ok mode ->
                    let absolutePath = NodePath.resolve [| repoPath; relativePath |]

                    // Inline keeps git's own conversions for the path (text normalization,
                    // other clean filters) and only neutralizes Git LFS, so the committed
                    // blob is what git status compares against afterwards. The process
                    // filter has to be cleared as well, because it takes precedence over
                    // clean and smudge.
                    let! blobResult =
                        runGit
                            [|
                                "-c"
                                "filter.lfs.process="
                                "-c"
                                "filter.lfs.clean=cat"
                                "-c"
                                "filter.lfs.smudge=cat"
                                "-c"
                                "filter.lfs.required=false"
                                "hash-object"
                                "-w"
                                "--path"
                                relativePath
                                "--"
                                absolutePath
                            |]
                            None
                            [||]

                    match blobResult with
                    | Error currentFailure -> failure <- Some currentFailure
                    | Ok output when output.ExitCode <> 0 ->
                        failure <- Some(failedRun "git hash-object (inline file)" output)
                    | Ok output ->
                        match!
                            updateTemporaryIndexEntry
                                runGit
                                environment
                                mode
                                (output.StdOut.Trim())
                                relativePath
                        with
                        | Error currentFailure -> failure <- Some currentFailure
                        | Ok() -> ()

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
    (identityArguments: string[])
    (revisionPolicy: RevisionPolicyStrategy)
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
                    let! metadataResult =
                        checkSelectedMetadata runGit barrier repoPath paths expectedHead revisionPolicy

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
                                                    yield! identityArguments
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

                                                        let partialReconciliation recoveryInstructions failure =
                                                            OperationResult.partiallySucceeded
                                                                outcome
                                                                {
                                                                    failure with
                                                                        AffectedPaths = affectedPaths
                                                                }
                                                                {
                                                                    Code = "reconcile_index"
                                                                    Instructions =
                                                                        Some recoveryInstructions
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
                                                        | Error failure ->
                                                            return
                                                                partialReconciliation
                                                                    "Merge the generated literal Git LFS rules into the current .gitattributes without discarding concurrent edits, then reconcile the affected paths in the index (for example with git reset)."
                                                                    failure
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
                                                                    partialReconciliation
                                                                        "Reconcile the affected paths in the index (for example with git reset)."
                                                                        (OperationFailure.createRedacted
                                                                            ProviderError
                                                                            "index_reconciliation_failed"
                                                                            failure.Message)
                                                            | Ok reconcileOutput ->
                                                                // The revision exists; only reconciliation failed.
                                                                return
                                                                    partialReconciliation
                                                                        "Reconcile the affected paths in the index (for example with git reset)."
                                                                        (OperationFailure.createRedacted
                                                                            ProviderError
                                                                            "index_reconciliation_failed"
                                                                            reconcileOutput.StdErr)
                            finally
                                cleanupTemporaryIndex ()
    }
