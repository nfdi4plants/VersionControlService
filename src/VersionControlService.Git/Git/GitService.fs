module internal VersionControlService.Git.GitService

open System
open System.IO
open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.Git.GitEngineTypes
open VersionControlService.Runtime.Node.Interop
open VersionControlService.Runtime.Node.FileSystem
open VersionControlService.Runtime.Node.Path
open VersionControlService.Bindings.SimpleGit
open VersionControlService.Git.GitLfsAdapter
open VersionControlService.Git.GitLfsService
open VersionControlService.Git.GitAuthAdapter
open VersionControlService.Git.GitInternals

module FileSystem = VersionControlService.Runtime.Node.FileSystem
module GitCredentialStrategy = VersionControlService.Git.GitCredentialStrategy

type GitFailure = {
    Kind: GitFailureKind
    Message: string
}

/// Internal result type returned by Git services before adapters map it to DTOs.
type GitResult<'T> = Result<'T, GitFailure>

type GitProgressCallback = GitInternals.GitProgressCallback

let private disallowedRemotePrefixes = [| "file://"; "ext::"; "fd::" |]

let private protocolOverridePattern =
    Regex("protocol\\.[^\\s=]+\\.(allow|deny)|(^|\\s)-c\\s+protocol\\.", RegexOptions.IgnoreCase)

let private remoteNamePattern = Regex("^[A-Za-z0-9._/-]+$")
let private scpRemotePattern = Regex(@"^[^\s@/:]+@[^\s/:]+:.+$")
let private invalidBranchCharactersPattern = Regex(@"[~^:?*\[\\\s]")
[<Literal>]
let AutoTrackThresholdKey = "versioncontrolservice.lfs.autotrackthresholdmb"

[<Literal>]
let MaterializeLargeObjectsKey = "versioncontrolservice.lfs.materializelargeobjects"

[<Literal>]
let DefaultAutoTrackThresholdMb = 1

[<Literal>]
let InvalidLfsThresholdMessage = "The automatic LFS threshold must be a positive whole MiB value."

let private gitLfsDefaultDownloadLargeFiles = false

let private normalizeOptionalGitRef (value: string option) =
    value
    |> Option.bind Option.ofObj
    |> Option.map _.Trim()
    |> Option.filter (fun item -> not (String.IsNullOrWhiteSpace item))

let private lfsInstallRequiredTokens = [|
    "git lfs is required for files larger than"
    "git lfs is required for this operation"
    "git: 'lfs' is not a git command"
    "git-lfs filter-process"
    "this repository is configured for git lfs but 'git-lfs' was not found"
    "external filter 'git-lfs filter-process' failed"
    "smudge filter lfs failed"
    "clean filter 'lfs' failed"
|]

/// Classifies git/simple-git/LFS error text into the shared failure taxonomy.
let classifyFailureKind (message: string) =
    let normalizedMessage =
        message
        |> Option.ofObj
        |> Option.defaultValue String.Empty
        |> fun text -> text.ToLowerInvariant()

    let containsAny (terms: string[]) =
        terms |> Array.exists normalizedMessage.Contains

    if containsAny lfsInstallRequiredTokens then
        GitFailureKind.LfsInstallRequired
    elif
        containsAny [|
            "has already been taken"
            "name already exists"
            "project already exists"
            "repository already exists"
        |]
    then
        GitFailureKind.RemoteProjectAlreadyExists
    // A bare "abort" token is not a cancellation marker: git prints "Aborting" when a
    // checkout or merge would overwrite local changes, and that is an ordinary failure.
    // Process kills are classified structurally before any text reaches this function.
    elif containsAny [| "cancelled"; "canceled" |] then
        GitFailureKind.Canceled
    elif containsAny [| "timed out"; "timeout"; "time out" |] then
        GitFailureKind.Timeout
    elif containsAny [| "forbidden"; "403" |] then
        GitFailureKind.Forbidden
    elif
        containsAny [|
            "unauthorized"
            "authentication failed"
            "401"
            "could not read username"
            "no access token available"
            "permission denied"
        |]
    then
        GitFailureKind.Unauthorized
    elif
        containsAny [|
            "network"
            "could not resolve host"
            "failed to connect"
            "connection reset"
            "connection refused"
            "unable to access"
        |]
    then
        GitFailureKind.Network
    else
        GitFailureKind.Unknown

/// Parses the numeric portion of Git's human-readable version output. Keeping
/// this tolerant makes dependency probing work for the platform-specific text
/// emitted by Git distributions without treating an unparseable version as
/// compatible.
let tryParseVersion (versionText: string) : (int * int * int) option =
    let matchResult = Regex.Match(versionText |> Option.ofObj |> Option.defaultValue String.Empty, @"(\d+)\.(\d+)(?:\.(\d+))?")

    if matchResult.Success then
        let patch =
            if matchResult.Groups.[3].Success then
                int matchResult.Groups.[3].Value
            else
                0

        Some(int matchResult.Groups.[1].Value, int matchResult.Groups.[2].Value, patch)
    else
        None

// GitService owns threshold formatting because the threshold is part of git workflow policy, not raw LFS command execution.
let private formatThresholdMb (thresholdMb: int) = $"{thresholdMb} MB"

// GitService builds the install prompt because this message is returned as part of git operation failures shown to the user.
let private buildLfsInstallPromptMessage (thresholdMb: int option) (details: string option) =
    let prompt =
        match thresholdMb with
        | Some value -> $"Git LFS is required for files larger than {formatThresholdMb value}. Install Git LFS now?"
        | None -> "Git LFS is required for this operation. Install Git LFS now?"

    match details |> Option.map _.Trim() with
    | Some value when not (String.IsNullOrWhiteSpace value) -> $"{prompt}\n\n{redactToken value}"
    | _ -> prompt

// GitService shapes the final failure because LFS install-required errors need workflow-specific wording.
let private createFailure kind (message: string) : GitFailure =
    match kind with
    | GitFailureKind.LfsInstallRequired ->
        let finalMessage =
            if message.IndexOf("Install Git LFS now?", StringComparison.OrdinalIgnoreCase) >= 0 then
                message
            else
                buildLfsInstallPromptMessage None (Some message)

        { Kind = kind; Message = finalMessage }
    | _ -> { Kind = kind; Message = message }

let private toFailure (error: exn) : GitFailure =
    GitInternals.toFailure classifyFailureKind createFailure error

let private errorResult (error: exn) : GitResult<'T> =
    GitInternals.errorResult classifyFailureKind createFailure error

// These helpers intentionally throw inside promise callbacks so the surrounding
// runSimpleGit/withLocalGit boundary can translate the failure into GitFailure.
let private abortGitPromise<'T> (message: string) : 'T = raise (exn message)

let private abortGitPromiseWith<'T> (error: exn) : 'T = raise error

let private tryGetNodeErrorCode (error: exn) : string option =
    try
        error?code |> unbox<string> |> Option.ofObj
    with _ ->
        None

let private valueOrEmptyArray (items: 'T[]) = if isNull items then [||] else items

// GitService validates the threshold because it owns the policy that decides when normal git actions must switch into LFS handling.
let validateLfsThresholdMb (thresholdMb: int) =
    if thresholdMb > 0 then
        Ok thresholdMb
    else
        Error(exn InvalidLfsThresholdMessage)

// GitService parses the stored threshold because the setting is interpreted by git workflow code here.
let private tryParseConfiguredThresholdMb (value: string option) =
    match value |> Option.bind Option.ofObj with
    | None -> None
    | Some text ->
        let success, parsed = Int32.TryParse(text.Trim())

        if success && parsed > 0 then
            Some parsed
        else
            None

// GitService parses the download preference because pull behavior is part of the git workflow owned here.
let private tryParseConfiguredDownloadLargeFiles (value: string option) =
    match value |> Option.bind Option.ofObj with
    | None -> None
    | Some text ->
        match text.Trim().ToLowerInvariant() with
        | "true"
        | "1"
        | "yes"
        | "on" -> Some true
        | "false"
        | "0"
        | "no"
        | "off" -> Some false
        | _ -> None

// GitService converts the threshold to bytes because file-size decisions happen during stage/commit orchestration here.
let private thresholdMbToBytes (thresholdMb: int) = int64 thresholdMb * 1024L * 1024L

// GitService formats the persisted threshold because it owns the config contract for this workflow setting.
let private formatThresholdConfigValue (thresholdMb: int) = string thresholdMb

// GitService formats the persisted download preference because it owns the config contract for this workflow setting.
let private formatDownloadLargeFilesConfigValue (downloadLargeFiles: bool) =
    if downloadLargeFiles then "true" else "false"

let private tryGetFileSizeInBytes (absolutePath: string) : JS.Promise<int64 option> = promise {
    try
        let! stats = statAsync absolutePath
        return Some(stats.size |> int64)
    with error ->
        match tryGetNodeErrorCode error with
        | Some "ENOENT" -> return None
        | _ -> return raise error
}

/// Validates local branch names and branch-like start points before passing them to git.
let ensureValidBranchLikeName (label: string) (value: string) =
    let trimmed = value.Trim()
    let containsControlCharacter = trimmed |> Seq.exists Char.IsControl

    if String.IsNullOrWhiteSpace trimmed then
        Error(exn $"{label} must not be empty.")
    elif containsControlCharacter then
        Error(exn $"{label} contains control characters.")
    elif trimmed.StartsWith("-") then
        Error(exn $"{label} must not start with '-'.")
    elif trimmed.StartsWith("/") then
        Error(exn $"{label} must not start with '/'.")
    elif
        trimmed.Split('/')
        |> Array.exists (fun segment -> segment.EndsWith(".lock", StringComparison.OrdinalIgnoreCase))
    then
        Error(exn $"{label} must not contain '.lock' path segments.")
    elif
        trimmed.Contains("..")
        || trimmed.EndsWith(".")
        || trimmed.Contains("@{")
        || trimmed.EndsWith("/")
    then
        Error(exn $"{label} contains invalid git ref segments.")
    elif invalidBranchCharactersPattern.IsMatch(trimmed) then
        Error(exn $"{label} contains invalid characters.")
    else
        Ok trimmed

/// Validates caller-provided pathspecs as repository-relative paths before file or git access.
let ensureValidPathspec (pathSpec: string) =
    let normalized = pathSpec.Replace("\\", "/").Trim()

    if String.IsNullOrWhiteSpace normalized then
        Error(exn "Pathspec must not be empty.")
    elif normalized.StartsWith("/") then
        Error(exn "Absolute pathspecs are not allowed.")
    elif Regex.IsMatch(normalized, "^[A-Za-z]:/") then
        Error(exn "Absolute pathspecs are not allowed.")
    elif
        normalized.Split('/')
        |> Array.exists (fun segment -> segment = "." || segment = "..")
    then
        Error(exn "Pathspec must not contain traversal segments.")
    elif normalized.Contains("\000") then
        Error(exn "Pathspec contains invalid null characters.")
    else
        Ok normalized

/// Validates a non-empty pathspec array and returns normalized pathspecs for git commands.
let validatePathspecs (pathSpecs: string[]) =
    if isNull pathSpecs || pathSpecs.Length = 0 then
        Error(exn "At least one pathspec is required.")
    else
        pathSpecs
        |> Array.map ensureValidPathspec
        |> Array.fold
            (fun state next ->
                match state, next with
                | Error e, _ -> Error e
                | _, Error e -> Error e
                | Ok acc, Ok value -> Ok(Array.append acc [| value |])
            )
            (Ok [||])

let private validateLiteralDiffPathspecs (pathSpecs: string[]) =
    if isNull pathSpecs || pathSpecs.Length = 0 then
        Error(exn "At least one pathspec is required.")
    else
        pathSpecs
        |> Array.map (fun pathSpec ->
            if String.IsNullOrWhiteSpace pathSpec then
                Error(exn "Pathspec must not be empty.")
            else
                match RepositoryPath.tryCreate pathSpec with
                | Ok path -> Ok path
                | Error message -> Error(exn message))
        |> Array.fold
            (fun state next ->
                match state, next with
                | Error e, _ -> Error e
                | _, Error e -> Error e
                | Ok acc, Ok value -> Ok(Array.append acc [| value |]))
            (Ok [||])

let private splitGitOutputLines (text: string) =
    text.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map _.Trim()
    |> Array.filter (String.IsNullOrWhiteSpace >> not)

let private withGitPathspecs (command: string[]) (pathSpecs: string[]) = [|
    yield! command
    yield "--"
    yield! pathSpecs
|]

/// Validates a remote name and defaults blank input to `origin`.
let validateRemoteName (remoteName: string) =
    let normalized =
        remoteName
        |> Option.ofObj
        |> Option.map _.Trim()
        |> Option.filter (fun x -> not (String.IsNullOrWhiteSpace x))
        |> Option.defaultValue "origin"

    if remoteNamePattern.IsMatch normalized then
        Ok normalized
    else
        Error(exn "Remote name contains unsupported characters.")

/// Enforces the service remote URL policy before clone/add-remote/auth lookup.
let ensureAllowedRemoteUrl (remoteUrl: string) =
    let normalized = remoteUrl.Trim()
    let isWindowsDrivePath =
        normalized.Length >= 3
        && Char.IsLetter normalized[0]
        && normalized[1] = ':'
        && (normalized[2] = '/' || normalized[2] = '\\')

    if String.IsNullOrWhiteSpace normalized then
        Error(exn "Remote URL is empty.")
    elif protocolOverridePattern.IsMatch normalized then
        Error(exn "Remote URL contains a protocol override attempt.")
    elif normalized.StartsWith("-", StringComparison.Ordinal) then
        Error(exn "Remote URL starts with an unsupported option.")
    elif
        disallowedRemotePrefixes
        |> Array.exists (fun prefix -> normalized.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
    then
        Error(exn "Remote URL uses a blocked protocol.")
    elif normalized.StartsWith("https://", StringComparison.OrdinalIgnoreCase) then
        Ok normalized
    elif normalized.StartsWith("ssh://", StringComparison.OrdinalIgnoreCase) then
        Ok normalized
    elif scpRemotePattern.IsMatch normalized then
        Ok normalized
    elif isWindowsDrivePath then
        Ok normalized
    elif normalized.Contains ':' then
        Error(exn "Remote URL contains an unsupported transport.")
    else
        Ok normalized

let private isLocalPathRemote (remoteUrl: string) =
    let normalized = remoteUrl.Trim()
    let isWindowsDrivePath =
        normalized.Length >= 3
        && Char.IsLetter normalized[0]
        && normalized[1] = ':'
        && (normalized[2] = '/' || normalized[2] = '\\')

    isAbsolute normalized || isWindowsDrivePath || not (normalized.Contains ':')

let private trimTrailingGitSuffix (path: string) =
    if path.EndsWith(".git", StringComparison.OrdinalIgnoreCase) && path.Length > 4 then
        path.[.. path.Length - 5]
    else
        path

/// Converts a validated git remote URL (https/ssh) into a browser-friendly repository URL.
let tryGetRepositoryWebUrlFromRemoteUrl (remoteUrl: string) : Result<string, exn> =
    match ensureAllowedRemoteUrl remoteUrl with
    | Error remoteUrlError -> Error remoteUrlError
    | Ok safeRemoteUrl ->
        let mutable uri = Unchecked.defaultof<Uri>

        if not (Uri.TryCreate(safeRemoteUrl, UriKind.Absolute, &uri)) then
            Error(exn $"Remote URL '{safeRemoteUrl}' is not a valid absolute URI.")
        elif String.IsNullOrWhiteSpace uri.Host then
            Error(exn "Remote URL is missing a host.")
        else
            let normalizedPath = uri.AbsolutePath.TrimEnd('/') |> trimTrailingGitSuffix

            if String.IsNullOrWhiteSpace normalizedPath || normalizedPath = "/" then
                Error(exn "Remote URL does not contain a repository path.")
            else
                Ok($"https://{uri.Host}{normalizedPath}".TrimEnd('/'))

let private unsupportedGitContentMessage (path: string) =
    $"Unsupported git content for '{path}'."

let private unsupportedGitContentResult<'T> (path: string) : GitResult<'T> =
    Error {
        Kind = GitFailureKind.Unknown
        Message = unsupportedGitContentMessage path
    }

/// Converts the service's unsupported-content sentinel into a DTO that callers can return without throwing.
let tryGetUnsupportedGitContent (requestedPath: string) (failure: GitFailure) : GitUnsupportedContentDto option =
    if String.Equals(failure.Message, unsupportedGitContentMessage requestedPath, StringComparison.Ordinal) then
        Some {
            Path = requestedPath
            Reason = Some failure.Message
        }
    else
        None

// Read file as a raw buffer for binary detection during merge conflict and diff view loading.
// Binary files cannot be displayed in the text-based diff/merge viewers and need to be routed
// to the unsupported content page instead.
let private readFileBufferAsync (absolutePath: string) : JS.Promise<obj> =
    FileSystem.readFileBufferAsync absolutePath

let private explicitlyUnsupportedExtensions =
    set [
        ".xlsx"
        ".xls"
        ".xlsm"
        ".xlsb"
        ".ods"
        ".zip"
        ".gz"
        ".png"
        ".jpg"
        ".jpeg"
        ".gif"
        ".bmp"
        ".ico"
        ".pdf"
        ".dll"
        ".exe"
        ".jar"
    ]

let internal isExplicitlyUnsupportedPath (path: string) =
    let extension = extname path

    not (String.IsNullOrWhiteSpace extension)
    && explicitlyUnsupportedExtensions.Contains(extension.ToLowerInvariant())

let internal isLikelyBinaryBuffer (buffer: obj) =
    let sampleLength = min (bufferLength buffer) 8192

    if not (bufferIsValidUtf8 buffer) then
        true
    elif sampleLength = 0 then
        false
    else
        let mutable nullByteDetected = false
        let mutable controlCount = 0

        for index in 0 .. sampleLength - 1 do
            let byteValue = bufferByteAt buffer index

            if byteValue = 0 then
                nullByteDetected <- true
            elif (byteValue < 7) || (byteValue > 13 && byteValue < 32) then
                controlCount <- controlCount + 1

        nullByteDetected || (float controlCount / float sampleLength) > 0.3

let private tryResolveArcRelativePath (arcPath: string) (requestedPath: string) =
    match ensureValidPathspec requestedPath with
    | Error validationError -> Error validationError
    | Ok safeRelativePath ->
        let arcRoot = resolve [| arcPath |]
        let absolutePath = resolve [| arcRoot; safeRelativePath |]

        let relativePath =
            relative arcRoot absolutePath |> fun value -> value.Replace("\\", "/")

        if
            String.IsNullOrWhiteSpace relativePath
            || relativePath = "."
            || relativePath = ".."
            || relativePath.StartsWith("../", StringComparison.Ordinal)
            || isAbsolute relativePath
        then
            Error(exn "Pathspec resolves outside the repository root.")
        else
            Ok(safeRelativePath, absolutePath)

let private createTemporaryLfsBackupPath (absolutePath: string) =
    $"{absolutePath}.vcs-lfs-backup-{Guid.NewGuid():N}"

let private restoreTemporaryLfsBackup backupPath absolutePath =
    if existsSync backupPath then
        if existsSync absolutePath then
            unlinkSync absolutePath

        renameSync backupPath absolutePath

let private removeTemporaryLfsBackup backupPath =
    if existsSync backupPath then
        unlinkSync backupPath

let private isPathCleanInStatus (status: StatusResult) (relativePath: string) =
    valueOrEmptyArray status.files
    |> Array.exists (fun fileStatus ->
        String.Equals(fileStatus.path, relativePath, StringComparison.Ordinal)
        || (fileStatus.``from``
            |> Option.exists (fun original -> String.Equals(original, relativePath, StringComparison.Ordinal)))
    )
    |> not

let private ensureBackupMatchesLfsOid (backupPath: string) (listing: GitLfsLsFileInfo) = promise {
    let! pointerTextResult =
        runGitCaptured {
            WorkingDirectory = None
            Arguments = [| "lfs"; "pointer"; "--file"; backupPath |]
            Environment = None
            StandardInput = None
            CancelCheck = None
            TimeoutMs = Some 30000
        }

    let generatedPointer =
        $"{pointerTextResult.StdoutText}\n{pointerTextResult.StderrText}"

    return
        if
            pointerTextResult.ExitCode = 0
            && generatedPointer.Contains($"oid sha256:{listing.oid}")
        then
            Ok()
        else
            Error(exn "The temporary LFS backup did not match the expected object. The original file was restored.")
}

let private isMergeInProgress (arcPath: string) =
    let mergeHeadPath = resolve [| arcPath; ".git"; "MERGE_HEAD" |]
    existsSync mergeHeadPath

let private toStatusDto (arcPath: string) (status: StatusResult) : GitStatusDto =
    let conflictedPaths = valueOrEmptyArray status.conflicted

    let fileStatuses =
        valueOrEmptyArray status.files
        |> Array.map (fun fileStatus -> {
            Path = fileStatus.path
            Index = fileStatus.index
            WorkingDir = fileStatus.working_dir
            OriginalPath = fileStatus.``from``
        })

    {
        Current = status.current
        Tracking = status.tracking
        Ahead = status.ahead
        Behind = status.behind
        IsClean = status.isClean ()
        Conflicted = conflictedPaths
        IsMergeInProgress = conflictedPaths.Length > 0 || isMergeInProgress arcPath
        Files = fileStatuses
    }

let private ensureCurrentlyConflictedPath (status: GitStatusDto) (requestedPath: string) =
    if
        status.Conflicted
        |> Array.exists (fun conflictedPath -> String.Equals(conflictedPath, requestedPath, StringComparison.Ordinal))
    then
        Ok()
    else
        Error(exn $"File '{requestedPath}' is not currently marked as conflicted.")

let private missingHeadContentMarkers = [|
    "does not exist in 'head'"
    "exists on disk, but not in 'head'"
    "bad revision 'head'"
    "invalid object name 'head'"
|]

let private isMissingHeadContentFailure (failure: GitFailure) =
    let message = failure.Message.ToLowerInvariant()
    missingHeadContentMarkers |> Array.exists message.Contains

let private readWorkingTreeTextIfPresent
    (arcPath: string)
    (requestedPath: string)
    : JS.Promise<GitResult<string option>> =
    promise {
        match tryResolveArcRelativePath arcPath requestedPath with
        | Error validationError -> return errorResult validationError
        | Ok(safePath, absolutePath) ->
            if isExplicitlyUnsupportedPath safePath then
                return unsupportedGitContentResult safePath
            elif not (existsSync absolutePath) then
                return Ok None
            else
                let! buffer = readFileBufferAsync absolutePath

                if isLikelyBinaryBuffer buffer then
                    return unsupportedGitContentResult safePath
                else
                    return Ok(Some(bufferToUtf8String buffer))
    }

let private readHeadTextIfAvailable
    (git: ISimpleGit)
    (requestedPath: string)
    (headPath: string)
    : JS.Promise<GitResult<string option>> =
    promise {
        match ensureValidPathspec requestedPath, ensureValidPathspec headPath with
        | Error validationError, _
        | _, Error validationError -> return errorResult validationError
        | Ok safeRequestedPath, Ok safeHeadPath ->
            if isExplicitlyUnsupportedPath safeRequestedPath || isExplicitlyUnsupportedPath safeHeadPath then
                return unsupportedGitContentResult safeRequestedPath
            else
                let! result =
                    GitInternals.runSimpleGit
                        toFailure
                        (fun currentGit -> currentGit.showBuffer(U2.Case1 $"HEAD:{safeHeadPath}"))
                        git

                match result with
                | Ok buffer when isLikelyBinaryBuffer buffer ->
                    return unsupportedGitContentResult safeRequestedPath
                | Ok buffer -> return Ok(Some(bufferToUtf8String buffer))
                | Error failure when isMissingHeadContentFailure failure -> return Ok None
                | Error failure -> return Error failure
}

let private quoteDiffPathToken (pathPrefix: string) (path: string option) =
    match path with
    | None -> "/dev/null"
    | Some value ->
        let escapedPath = value.Replace("\\", "\\\\").Replace("\"", "\\\"")

        $"\"{pathPrefix}{escapedPath}\""

let private buildSyntheticWordDiffText (previousPath: string option) (currentPath: string option) =
    let previousToken = quoteDiffPathToken "a/" previousPath
    let currentToken = quoteDiffPathToken "b/" currentPath

    [
        yield $"diff --git {previousToken} {currentToken}"

        match previousPath, currentPath with
        | None, Some _ -> yield "new file mode 100644"
        | Some _, None -> yield "deleted file mode 100644"
        | _ -> ()

        match previousPath, currentPath with
        | Some oldPath, Some currentPath when not (String.Equals(oldPath, currentPath, StringComparison.Ordinal)) ->
            yield $"rename from {oldPath}"
            yield $"rename to {currentPath}"
        | _ -> ()

        yield $"--- {previousToken}"
        yield $"+++ {currentToken}"
    ]
    |> String.concat "\n"

let private reconcileTrackingBranchForCheckout
    (remoteName: string)
    (branchName: string)
    (git: ISimpleGit)
    : JS.Promise<unit> =
    promise {
        let! remoteBranchText = git.raw [| "branch"; "-r"; "--no-color" |]
        let! status = git.status ()

        let currentTracking = normalizeOptionalGitRef status.tracking

        let remoteRefs =
            remoteBranchText.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map _.Trim()
            |> Array.filter (fun branchRef ->
                not (String.IsNullOrWhiteSpace branchRef) && not (branchRef.Contains " -> ")
            )

        let desiredUpstreamRef = $"{remoteName}/{branchName}"

        let desiredTracking =
            remoteRefs
            |> Array.tryFind (fun branchRef -> String.Equals(branchRef, desiredUpstreamRef, StringComparison.Ordinal))

        match desiredTracking, currentTracking with
        | Some desired, Some current when String.Equals(current, desired, StringComparison.Ordinal) -> return ()
        | Some desired, _ ->
            let! _ = git.raw [| "branch"; $"--set-upstream-to={desired}"; branchName |]
            return ()
        | None, Some _ ->
            let! _ = git.raw [| "branch"; "--unset-upstream"; branchName |]
            return ()
        | None, None -> return ()
    }

let private runSimpleGit (operation: ISimpleGit -> JS.Promise<'T>) (git: ISimpleGit) : JS.Promise<GitResult<'T>> =
    GitInternals.runSimpleGit toFailure operation git

let private exactTransferBytesPattern =
    Regex(
        @"(?<processed>\d+(?:\.\d+)?)\s*/\s*(?<total>\d+(?:\.\d+)?)\s*bytes",
        RegexOptions.IgnoreCase
    )

let private scaledTransferBytesPattern =
    Regex(
        @"(?<processed>\d+(?:\.\d+)?)\s*(?<processedUnit>KiB|MiB|GiB|KB|MB|GB|B)\s*/\s*(?<total>\d+(?:\.\d+)?)\s*(?<totalUnit>KiB|MiB|GiB|KB|MB|GB|B)",
        RegexOptions.IgnoreCase
    )

let private completedTransferBytesPattern =
    Regex(
        @"(?<bytes>\d+(?:\.\d+)?)\s*(?<unit>KiB|MiB|GiB|KB|MB|GB|B)\s*\|\s*\d+(?:\.\d+)?\s*(?:KiB|MiB|GiB|KB|MB|GB|B)/s",
        RegexOptions.IgnoreCase
    )

let private tryParseInvariantNumber (value: string) =
    match Double.TryParse value with
    | true, parsed -> Some parsed
    | false, _ -> None

let private transferUnitMultiplier (unitName: string) =
    match unitName.Trim().ToUpperInvariant() with
    | "KIB"
    | "KB" -> 1024.0
    | "MIB"
    | "MB" -> 1024.0 * 1024.0
    | "GIB"
    | "GB" -> 1024.0 * 1024.0 * 1024.0
    | _ -> 1.0

let private tryParseTransferBytes (text: string) =
    let lastMatch (pattern: Regex) =
        let matches = pattern.Matches text

        if matches.Count = 0 then
            None
        else
            Some matches[matches.Count - 1]

    match lastMatch exactTransferBytesPattern with
    | Some exact ->
        match
            tryParseInvariantNumber exact.Groups["processed"].Value,
            tryParseInvariantNumber exact.Groups["total"].Value
        with
        | Some processed, Some total -> Some(processed, total)
        | _ -> None
    | None ->
        match lastMatch scaledTransferBytesPattern with
        | Some scaled ->
            match
                tryParseInvariantNumber scaled.Groups["processed"].Value,
                tryParseInvariantNumber scaled.Groups["total"].Value
            with
            | Some processed, Some total ->
                Some(
                    processed * transferUnitMultiplier scaled.Groups["processedUnit"].Value,
                    total * transferUnitMultiplier scaled.Groups["totalUnit"].Value
                )
            | _ -> None
        | None ->
            match lastMatch completedTransferBytesPattern with
            | Some completed ->
                tryParseInvariantNumber completed.Groups["bytes"].Value
                |> Option.map (fun bytes ->
                    let transferred = bytes * transferUnitMultiplier completed.Groups["unit"].Value
                    transferred, transferred)
            | None -> None

let private runGitCapturedWithOutput progressCallback request =
    let pending = System.Text.StringBuilder()
    let mutable lastReported: (float * float) option = None

    let observeOutput (chunk: string) =
        GitInternals.reportOutputText progressCallback chunk
        pending.Append chunk |> ignore

        if pending.Length > 4096 then
            pending.Remove(0, pending.Length - 4096) |> ignore

        match tryParseTransferBytes (pending.ToString()) with
        | Some(processed, total) when lastReported <> Some(processed, total) ->
            lastReported <- Some(processed, total)

            progressCallback
            |> Option.iter (fun report ->
                GitInternals.createProgressDto
                    (Some "lfs")
                    (Some "upload")
                    None
                    (Some processed)
                    (Some total)
                    None
                |> report)
        | _ -> ()

    GitLfsAdapter.runGitCapturedWithOutput observeOutput request

// GitService reads the threshold because stage/commit need the value while deciding whether to enforce LFS automatically.
let private getConfiguredLfsThresholdMb (arcPath: string) : JS.Promise<GitResult<int>> = promise {
    let! result =
        runGitCaptured {
            WorkingDirectory = Some arcPath
            Arguments = [| "config"; "--local"; "--get-all"; AutoTrackThresholdKey |]
            Environment = None
            StandardInput = None
            CancelCheck = None
            TimeoutMs = Some 5000
        }

    let values =
        result.StdoutText.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map _.Trim()
        |> Array.filter (String.IsNullOrWhiteSpace >> not)

    if result.ExitCode = 1 && values.Length = 0 && String.IsNullOrWhiteSpace result.StderrText then
        return Ok DefaultAutoTrackThresholdMb
    elif result.ExitCode = 0 && not result.TimedOut then
        match values with
        | [| value |] ->
            match tryParseConfiguredThresholdMb (Some value) with
            | Some thresholdMb -> return Ok thresholdMb
            | None -> return Error(createFailure GitFailureKind.InvalidLfsThreshold InvalidLfsThresholdMessage)
        | _ -> return Error(createFailure GitFailureKind.InvalidLfsThreshold InvalidLfsThresholdMessage)
    else
        let detail =
            if result.TimedOut then
                "Reading the local automatic LFS threshold timed out."
            elif not (String.IsNullOrWhiteSpace result.StderrText) then
                result.StderrText
            else
                result.StdoutText

        return Error(createFailure (classifyFailureKind detail) detail)
}

// GitService reads the download preference because pull/sync need it while deciding whether to hydrate LFS content or keep pointers.
let private getConfiguredLfsDownloadLargeFiles (git: ISimpleGit) : JS.Promise<bool> = promise {
    try
        let! configResult = git.getConfig (MaterializeLargeObjectsKey, "local")

        return
            configResult.value
            |> tryParseConfiguredDownloadLargeFiles
            |> Option.defaultValue gitLfsDefaultDownloadLargeFiles
    with _ ->
        return gitLfsDefaultDownloadLargeFiles
}

// GitService creates this failure because install-required is surfaced as a git workflow error, not as a raw LFS command result.
let private createLfsInstallRequiredFailure (thresholdMb: int option) (details: string option) : GitFailure = {
    Kind = GitFailureKind.LfsInstallRequired
    Message = buildLfsInstallPromptMessage thresholdMb details
}

// GitService keeps this wrapper because automatic tracking must map LFS command failures into GitFailure values used by the workflow layer.
let private runGitLfsTrackCommand
    (arcPath: string)
    (relativePath: string)
    (thresholdMb: int)
    : JS.Promise<GitResult<unit>> =
    promise {
        let! result = trackLiteral arcPath relativePath

        return
            match result with
            | Ok() -> Ok()
            | Error message when classifyFailureKind message = GitFailureKind.LfsInstallRequired ->
                Error(createLfsInstallRequiredFailure (Some thresholdMb) (Some message))
            | Error message -> Error(createFailure GitFailureKind.Unknown message)
    }

let private getOversizedWorkingTreePaths
    (arcPath: string)
    (selectedPaths: string[])
    (thresholdBytes: int64)
    : JS.Promise<GitResult<string[]>> =
    promise {
        let rec regularFilesUnder (absolutePath: string) : JS.Promise<Result<string[], exn>> = promise {
            try
                let! stats = lstatAsync absolutePath

                if stats.isFile () then
                    return Ok [| absolutePath |]
                elif stats.isDirectory () then
                    let! entries = readdirWithTypesAsync absolutePath (ReaddirOptions(withFileTypes = true))
                    let files = ResizeArray<string>()
                    let mutable nestedFailure: exn option = None

                    for entry in entries do
                        if nestedFailure.IsNone && entry.isFile () then
                            files.Add(join [| absolutePath; entry.name |])
                        elif nestedFailure.IsNone && entry.isDirectory () then
                            let! nestedResult = regularFilesUnder (join [| absolutePath; entry.name |])

                            match nestedResult with
                            | Ok nested ->
                                for nestedPath in nested do
                                    files.Add nestedPath
                            | Error error -> nestedFailure <- Some error

                    match nestedFailure with
                    | Some error -> return Error error
                    | None -> return Ok(files.ToArray())
                else
                    return Ok [||]
            with error ->
                return Error error
        }

        let repositoryRoot = resolve [| arcPath |]
        let oversizedPaths = ResizeArray<string>()
        let mutable failure: GitFailure option = None

        for selectedPath in selectedPaths |> Array.distinct do
            match failure with
            | Some _ -> ()
            | None ->
                match tryResolveArcRelativePath arcPath selectedPath with
                | Error validationError -> failure <- Some(toFailure validationError)
                | Ok(_, absolutePath) ->
                    let! filesResult = regularFilesUnder absolutePath

                    match filesResult with
                    | Error error ->
                        match tryGetNodeErrorCode error with
                        | Some "ENOENT" -> ()
                        | _ -> failure <- Some(toFailure error)
                    | Ok files ->
                        for filePath in files do
                            match failure with
                            | Some _ -> ()
                            | None ->
                                try
                                    let! fileSizeOption = tryGetFileSizeInBytes filePath

                                    if fileSizeOption |> Option.exists (fun fileSize -> fileSize >= thresholdBytes) then
                                        let relativePath = relative repositoryRoot filePath |> fun value -> value.Replace("\\", "/")
                                        oversizedPaths.Add relativePath
                                with error ->
                                    failure <- Some(toFailure error)

        match failure with
        | Some failure -> return Error failure
        | None -> return Ok(oversizedPaths.ToArray() |> Array.distinct)
    }

// GitService keeps automatic LFS mutation in the explicit staging flow so only user-selected paths are re-staged.
let private enforceStageTimeLfsTrackingForPaths
    (arcPath: string)
    (selectedPaths: string[])
    (git: ISimpleGit)
    : JS.Promise<GitResult<unit>> =
    promise {
        match! getConfiguredLfsThresholdMb arcPath with
        | Error failure -> return Error failure
        | Ok thresholdMb ->
            let thresholdBytes = thresholdMbToBytes thresholdMb
            let! oversizedPathsResult = getOversizedWorkingTreePaths arcPath selectedPaths thresholdBytes

            match oversizedPathsResult with
            | Error failure -> return Error failure
            | Ok oversizedPaths when oversizedPaths.Length = 0 -> return Ok()
            | Ok oversizedPaths ->
                match! GitLfsAdapter.checkFilterAttributes arcPath oversizedPaths with
                | Error message ->
                    return
                        Error(
                            createFailure
                                (classifyFailureKind message)
                                $"Could not inspect automatic Git LFS attributes: {message}"
                        )
                | Ok attributes ->
                    let filters = attributes |> Map.ofArray
                    let pathsToTrack =
                        oversizedPaths
                        |> Array.filter (fun relativePath ->
                            match Map.tryFind relativePath filters |> Option.defaultValue "unspecified" with
                            | "lfs"
                            | "unset" -> false
                            | _ -> true)

                    let mutable failure: GitFailure option = None

                    for pathToTrack in pathsToTrack do
                        match failure with
                        | Some _ -> ()
                        | None ->
                            let! trackResult = runGitLfsTrackCommand arcPath pathToTrack thresholdMb

                            match trackResult with
                            | Ok() -> ()
                            | Error trackFailure -> failure <- Some trackFailure

                    match failure with
                    | Some failure -> return Error failure
                    | None ->
                        if pathsToTrack.Length = 0 then
                            return Ok()
                        else
                            let pathsToRestage = [| ".gitattributes"; yield! oversizedPaths |] |> Array.distinct

                            let! restageResult = runSimpleGit (fun currentGit -> currentGit.add pathsToRestage) git

                            match restageResult with
                            | Ok _ -> return Ok()
                            | Error failure when failure.Kind = GitFailureKind.LfsInstallRequired ->
                                return Error(createLfsInstallRequiredFailure (Some thresholdMb) None)
                            | Error failure -> return Error failure
    }

let private tryGetIndexedBlobId (git: ISimpleGit) (relativePath: string) : JS.Promise<GitResult<string option>> = promise {
    let! lsFilesResult =
        runSimpleGit (fun currentGit -> currentGit.raw [| "ls-files"; "--stage"; "--"; relativePath |]) git

    return
        lsFilesResult
        |> Result.bind (fun output ->
            output.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
            |> Array.tryPick (fun line ->
                let tabIndex = line.IndexOf('\t')

                if tabIndex <= 0 then
                    None
                else
                    let metadata =
                        line.Substring(0, tabIndex).Split(' ', StringSplitOptions.RemoveEmptyEntries)

                    if metadata.Length >= 3 && metadata.[2] = "0" then
                        Some metadata.[1]
                    else
                        None
            )
            |> Ok
        )
}

let private tryGetIndexedBlobSizeInBytes
    (git: ISimpleGit)
    (relativePath: string)
    : JS.Promise<GitResult<int64 option>> =
    promise {
        let! blobIdResult = tryGetIndexedBlobId git relativePath

        match blobIdResult with
        | Error failure -> return Error failure
        | Ok None -> return Ok None
        | Ok(Some blobId) ->
            let! sizeResult = runSimpleGit (fun currentGit -> currentGit.raw [| "cat-file"; "-s"; blobId |]) git

            return
                sizeResult
                |> Result.bind (fun text ->
                    let success, parsedSize = Int64.TryParse(text.Trim())

                    if success then
                        Ok(Some parsedSize)
                    else
                        errorResult (exn $"Could not determine staged blob size for '{relativePath}'.")
                )
    }

let private createCommitLfsValidationFailure (thresholdMb: int) (oversizedPaths: string[]) =
    let listedPaths = oversizedPaths |> Array.truncate 5 |> String.concat ", "

    let suffix = if oversizedPaths.Length > 5 then ", ..." else String.Empty

    {
        Kind = GitFailureKind.Unknown
        Message =
            $"Staged content larger than {formatThresholdMb thresholdMb} must already be staged as Git LFS pointers before commit. Re-stage the affected paths after tracking them with Git LFS or use the Git LFS-aware staging workflow. Affected paths: {listedPaths}{suffix}"
    }

module private GitStatusCode =

    let normalize (code: string) =
        let trimmed =
            code
            |> Option.ofObj
            |> Option.defaultValue String.Empty
            |> _.Trim()

        if String.IsNullOrWhiteSpace trimmed then "." else trimmed

    let isStagedIndexStatus (code: string) =
        let normalized = normalize code

        normalized <> "." && normalized <> "?"

let private validateCommitLfsPolicy (arcPath: string) (git: ISimpleGit) : JS.Promise<GitResult<unit>> = promise {
    match! getConfiguredLfsThresholdMb arcPath with
    | Error failure -> return Error failure
    | Ok thresholdMb ->
        let thresholdBytes = thresholdMbToBytes thresholdMb
        let! statusResult = runSimpleGit (fun currentGit -> currentGit.status ()) git

        match statusResult with
        | Error failure -> return Error failure
        | Ok status ->
            let stagedPaths =
                valueOrEmptyArray status.files
                |> Array.filter (fun fileStatus -> GitStatusCode.isStagedIndexStatus fileStatus.index)
                |> Array.map _.path
                |> Array.distinct

            let oversizedPaths = ResizeArray<string>()
            let mutable failure: GitFailure option = None

            for stagedPath in stagedPaths do
                match failure with
                | Some _ -> ()
                | None ->
                    let! stagedSizeResult = tryGetIndexedBlobSizeInBytes git stagedPath

                    match stagedSizeResult with
                    | Error stagedFailure -> failure <- Some stagedFailure
                    | Ok(Some stagedSize) when stagedSize >= thresholdBytes -> oversizedPaths.Add stagedPath
                    | Ok _ -> ()

            match failure with
            | Some failure -> return Error failure
            | None when oversizedPaths.Count = 0 -> return Ok()
            | None -> return Error(createCommitLfsValidationFailure thresholdMb (oversizedPaths.ToArray()))
}

// Worktree-populating commands skip implicit LFS downloads; explicit LFS hydration uses authenticated transfers.
let private applyLfsSkipSmudge (git: ISimpleGit) = git.env (GitLfsSkipSmudgeEnvKey, "1")

type private AuthenticatedGitSession = {
    Git: ISimpleGit
    CommandAuth: GitAuthAdapter.GitCommandAuthentication
}

let private createLocalGitSession arcPath progressCallback =
    let options = createOptions arcPath syncTimeout progressCallback

    {
        Git = createGit options |> withGitOutputProgress progressCallback
        CommandAuth = {
            ConfigArgs = [||]
            Environment = createNonInteractiveEnv ()
        }
    }

let private createAuthenticatedGitSession
    (arcPath: string)
    (remoteName: string)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (progressCallback: GitProgressCallback option)
    : JS.Promise<GitResult<AuthenticatedGitSession>> =
    promise {
        let probeOptions = createOptions arcPath standardTimeout None
        let probeGit = createGit probeOptions

        let! remoteResult =
            runSimpleGit
                (fun git -> promise {
                    // The effective fetch URL, with insteadOf applied and the first of several
                    // configured URLs, which is what fetch and the LFS endpoint use.
                    let! remoteUrl = git.raw [| "remote"; "get-url"; remoteName |]
                    return remoteUrl.Trim()
                })
                probeGit

        match remoteResult with
        | Error failure -> return Error failure
        | Ok remoteUrl ->
            match ensureAllowedRemoteUrl remoteUrl with
            | Error validationError -> return errorResult validationError
            | Ok allowedRemoteUrl ->
                match tryExtractHostFromRemoteUrl allowedRemoteUrl with
                | Error hostError -> return errorResult hostError
                | Ok host ->
                    let! credential = credentials.ResolveCredential host connectionProfileId |> Async.StartAsPromise

                    match credential with
                    | Some resolved when not (String.IsNullOrWhiteSpace resolved.Secret) ->
                        try
                            let operationOptions = createOptions arcPath syncTimeout progressCallback
                            let commandAuth =
                                GitCredentialStrategy.buildScopedHeaderAuthentication
                                    allowedRemoteUrl
                                    (Some resolved)

                            let git =
                                applyCommandAuthentication
                                    (fun options -> createGit options |> withGitOutputProgress progressCallback)
                                    operationOptions
                                    commandAuth

                            return Ok { Git = git; CommandAuth = commandAuth }
                        with error ->
                            return errorResult error
                    | _ ->
                        return
                            Error {
                                Kind = GitFailureKind.Unauthorized
                                Message = $"No access token available for remote host '{host}'."
                            }
    }

let private createOriginLfsRemoteSession
    (arcPath: string)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (progressCallback: GitProgressCallback option)
    : JS.Promise<GitResult<AuthenticatedGitSession>> =
    promise {
        let remoteName = "origin"
        let probeOptions = createOptions arcPath standardTimeout None
        let probeGit = createGit probeOptions

        let! remoteResult =
            runSimpleGit
                (fun git -> promise {
                    // The effective fetch URL, with insteadOf applied and the first of several
                    // configured URLs, which is what fetch and the LFS endpoint use.
                    let! remoteUrl = git.raw [| "remote"; "get-url"; remoteName |]
                    return remoteUrl.Trim()
                })
                probeGit

        match remoteResult with
        | Error failure -> return Error failure
        | Ok remoteUrl ->
            match ensureAllowedRemoteUrl remoteUrl with
            | Ok _ when isLocalPathRemote remoteUrl ->
                return Ok(createLocalGitSession arcPath progressCallback)
            | Ok _ ->
                let! sessionResult =
                    createAuthenticatedGitSession
                        arcPath
                        remoteName
                        credentials
                        connectionProfileId
                        progressCallback
                return sessionResult
            | Error _ when
                remoteUrl.StartsWith("file://", StringComparison.OrdinalIgnoreCase)
                || isAbsolute remoteUrl
                ->
                return Ok(createLocalGitSession arcPath progressCallback)
            | Error validationError -> return errorResult validationError
    }

let private ensureRepo (git: ISimpleGit) = promise {
    let! isRepo = git.checkIsRepo ()

    if isRepo then
        return Ok()
    else
        return Error(exn "The selected path is not a git repository.")
}

let private withLocalGitAndProgress
    (arcPath: string)
    (progressCallback: GitProgressCallback option)
    (operation: ISimpleGit -> JS.Promise<'T>)
    : JS.Promise<GitResult<'T>> =
    promise {
        let options = createOptions arcPath standardTimeout None
        let git = createGit options |> withGitOutputProgress progressCallback

        let! repoCheckResult = ensureRepo git

        match repoCheckResult with
        | Error repoError -> return errorResult repoError
        | Ok() -> return! runSimpleGit operation git
    }

let private withLocalGit (arcPath: string) (operation: ISimpleGit -> JS.Promise<'T>) : JS.Promise<GitResult<'T>> =
    withLocalGitAndProgress arcPath None operation

let private requireCleanWorkingTreeForLfsStorageAction (actionLabel: string) (git: ISimpleGit) = promise {
    let! status = git.status ()

    return
        if status.isClean () then
            Ok()
        else
            Error(exn $"{actionLabel} requires a clean working tree. Save, discard, or commit local changes first.")
}

let private rejectCustomLfsStorageForPrune (git: ISimpleGit) = promise {
    let! storageResult = runSimpleGit (fun currentGit -> currentGit.raw [| "config"; "--get"; "lfs.storage" |]) git

    return
        match storageResult with
        | Ok value when not (String.IsNullOrWhiteSpace(value.Trim())) ->
            Error(exn "Git LFS prune is disabled because this repository uses a custom lfs.storage directory.")
        | _ -> Ok()
}

/// Reads status for the active repository, including conflict metadata used by merge UI.
let getStatus (arcPath: string) : JS.Promise<GitResult<GitStatusDto>> =
    withLocalGit
        arcPath
        (fun git -> promise {
            let! status = git.status ()
            return toStatusDto arcPath status
        })

/// Lists local and remote branch refs.
let getBranches (arcPath: string) : JS.Promise<GitResult<GitBranchRefDto[]>> =
    withLocalGit
        arcPath
        (fun git -> promise {
            let! status = git.status ()
            let statusDto = toStatusDto arcPath status
            let! localBranchSummary = git.branchLocal ()
            let! remoteBranchText = git.raw [| "branch"; "-r"; "--no-color" |]

            let localRefs =
                valueOrEmptyArray localBranchSummary.all
                |> Array.map (fun branchName ->
                    let isCurrent = statusDto.Current = Some branchName

                    {
                        RefName = branchName
                        DisplayLabel = branchName
                        Kind = GitBranchRefKind.Local
                        IsCurrent = isCurrent
                        // Mark the active local branch when it already tracks an upstream so callers can represent the switched branch itself.
                        IsTracking = isCurrent && statusDto.Tracking.IsSome
                    }
                )

            let remoteRefs =
                remoteBranchText.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map _.Trim()
                |> Array.filter (fun branchName ->
                    not (String.IsNullOrWhiteSpace branchName) && not (branchName.Contains " -> ")
                )
                |> Array.map (fun branchName -> {
                    RefName = branchName
                    DisplayLabel = branchName
                    Kind = GitBranchRefKind.Remote
                    IsCurrent = false
                    IsTracking = statusDto.Tracking = Some branchName
                })

            return
                Array.append localRefs remoteRefs
                |> Array.distinctBy _.RefName
                |> Array.sortBy (fun branch ->
                    let kindOrder =
                        match branch.Kind with
                        | GitBranchRefKind.Local -> 0
                        | GitBranchRefKind.Remote -> 1

                    kindOrder,
                    (if branch.IsCurrent then 0 else 1),
                    (if branch.IsTracking then 0 else 1),
                    branch.DisplayLabel.ToLowerInvariant()
                )
        })

/// Exposes persisted LFS workflow settings.
let getLfsSettings (arcPath: string) : JS.Promise<GitResult<GitLfsSettingsDto>> = promise {
    match! getConfiguredLfsThresholdMb arcPath with
    | Error failure -> return Error failure
    | Ok thresholdMb ->
        return!
            withLocalGit
                arcPath
                (fun git -> promise {
                    let! downloadLargeFiles = getConfiguredLfsDownloadLargeFiles git

                    return {
                        AutoTrackThresholdMb = thresholdMb
                        DownloadLargeFiles = downloadLargeFiles
                    }
                })
}

/// Returns aggregate unstaged diff counts for the active repository.
let getDiffSummary (arcPath: string) : JS.Promise<GitResult<GitDiffSummaryDto>> =
    withLocalGit
        arcPath
        (fun git -> promise {
            let! diff = git.diffSummary ()

            return {
                Changed = diff.changed
                Insertions = diff.insertions
                Deletions = diff.deletions
            }
        })

/// Returns raw `git diff` text for validated pathspecs. Used by tests and lower-level consumers.
let getDiff (arcPath: string) (pathSpecs: string[]) : JS.Promise<GitResult<string>> = promise {
    match validateLiteralDiffPathspecs pathSpecs with
    | Error validationError -> return errorResult validationError
    | Ok literalPaths ->
        return!
            withLocalGit
                arcPath
                (fun git -> promise {
                    let diffArgs =
                        [|
                            "diff"
                            "--"
                            yield! literalPaths |> Array.map GitPathTransport.literalPathspec
                        |]
                    let! diff = git.raw diffArgs
                    return diff
                })
}

/// Returns porcelain word-diff text for validated pathspecs.
let getWordDiff (arcPath: string) (pathSpecs: string[]) : JS.Promise<GitResult<string>> = promise {
    match validateLiteralDiffPathspecs pathSpecs with
    | Error validationError -> return errorResult validationError
    | Ok literalPaths ->
        return!
            withLocalGit
                arcPath
                (fun git -> promise {
                    let diffArgs = [|
                        "diff"
                        "--word-diff=porcelain"
                        "-U0"
                        "--"
                        yield!
                            literalPaths
                            |> Array.map GitPathTransport.literalPathspec
                    |]

                    let! diff = git.raw diffArgs
                    return diff
                })
}

/// Loads previous/current text plus word-diff metadata for diff views.
/// Binary or explicitly unsupported files return the unsupported-content sentinel.
let getDiffViewData (arcPath: string) (requestedPath: string) : JS.Promise<GitResult<GitDiffViewDataDto>> = promise {
    match ensureValidPathspec requestedPath with
    | Error validationError -> return errorResult validationError
    | Ok safeRequestedPath ->
        if isExplicitlyUnsupportedPath safeRequestedPath then
            return unsupportedGitContentResult safeRequestedPath
        else
            return!
                withLocalGit
                    arcPath
                    (fun git -> promise {
                        let! status = git.status ()
                        let statusDto = toStatusDto arcPath status

                        match
                            statusDto.Files
                            |> Array.tryFind (fun file ->
                                String.Equals(file.Path, safeRequestedPath, StringComparison.Ordinal)
                            )
                        with
                        | None -> return abortGitPromise $"No git status entry found for '{safeRequestedPath}'."
                        | Some fileStatus ->
                            let previousPathCandidate =
                                fileStatus.OriginalPath |> Option.defaultValue safeRequestedPath

                            let! previousContentResult =
                                readHeadTextIfAvailable git safeRequestedPath previousPathCandidate

                            let previousContent =
                                match previousContentResult with
                                | Ok content -> content
                                | Error failure -> abortGitPromise failure.Message

                            let! currentContentResult = readWorkingTreeTextIfPresent arcPath safeRequestedPath

                            let currentContent =
                                match currentContentResult with
                                | Ok content -> content
                                | Error failure -> abortGitPromise failure.Message

                            if previousContent.IsNone && currentContent.IsNone then
                                return abortGitPromise $"No git diff content found for '{safeRequestedPath}'."
                            else
                                let diffPaths = [|
                                    yield safeRequestedPath

                                    match fileStatus.OriginalPath with
                                    | Some originalPath when
                                        not (String.Equals(originalPath, safeRequestedPath, StringComparison.Ordinal))
                                        ->
                                        yield originalPath
                                    | _ -> ()
                                |]

                                let! wordDiffResult =
                                    runSimpleGit
                                        (fun currentGit ->
                                            currentGit.raw [|
                                                "diff"
                                                "--word-diff=porcelain"
                                                "-U0"
                                                "--find-renames"
                                                "HEAD"
                                                "--"
                                                yield! diffPaths
                                            |]
                                        )
                                        git

                                let previousPathForMetadata =
                                    if previousContent.IsSome then
                                        Some previousPathCandidate
                                    else
                                        None

                                let currentPathForMetadata =
                                    if currentContent.IsSome then
                                        Some safeRequestedPath
                                    else
                                        None

                                let wordDiffText =
                                    match wordDiffResult with
                                    | Ok diff when not (String.IsNullOrWhiteSpace diff) -> diff
                                    | _ -> buildSyntheticWordDiffText previousPathForMetadata currentPathForMetadata

                                return {
                                    Path = safeRequestedPath
                                    PreviousContent = previousContent |> Option.defaultValue ""
                                    CurrentContent = currentContent |> Option.defaultValue ""
                                    WordDiffText = wordDiffText
                                }
                    })
}

/// Loads the current conflicted file content for the merge-resolution view.
let getMergeConflictViewData
    (arcPath: string)
    (requestedPath: string)
    : JS.Promise<GitResult<GitMergeConflictViewDataDto>> =
    promise {
        match ensureValidPathspec requestedPath with
        | Error validationError -> return errorResult validationError
        | Ok safeRequestedPath ->
            if isExplicitlyUnsupportedPath safeRequestedPath then
                return unsupportedGitContentResult safeRequestedPath
            else
                return!
                    withLocalGit
                        arcPath
                        (fun git -> promise {
                            let! status = git.status ()
                            let statusDto = toStatusDto arcPath status

                            match ensureCurrentlyConflictedPath statusDto safeRequestedPath with
                            | Error conflictError -> return abortGitPromiseWith conflictError
                            | Ok() ->
                                let! contentResult = readWorkingTreeTextIfPresent arcPath safeRequestedPath

                                match contentResult with
                                | Ok(Some content) ->
                                    return {
                                        Path = safeRequestedPath
                                        MergeConflictContent = content
                                    }
                                | Ok None ->
                                    return abortGitPromise $"Conflicted file '{safeRequestedPath}' no longer exists."
                                | Error failure -> return abortGitPromise failure.Message
                        })
    }

/// Plans and performs only the explicit LFS portion of a provider publish. The caller
/// owns the subsequent ref mutation and must set `GIT_LFS_SKIP_PUSH=1` when this
/// function returns `Ok true`. `fetchAuth` is scoped to the remote's fetch URL and only
/// serves the ls-remote that reads remote tips. `commandAuth` is scoped to the push URL
/// and serves the dry run and the upload.
let prepareExplicitLfsPush
    (arcPath: string)
    (remoteName: string)
    (branchName: string)
    (fetchAuth: GitCommandAuthentication)
    (commandAuth: GitCommandAuthentication)
    (progressCallback: GitProgressCallback option)
    (cancelCheck: unit -> bool)
    : JS.Promise<GitResult<bool>> =
    promise {
        if cancelCheck () then
            return Error(createFailure GitFailureKind.Canceled "Git LFS upload canceled.")
        else
            let options = createOptions arcPath syncTimeout progressCallback

            let git =
                applyCommandAuthentication
                    (fun currentOptions ->
                        createGit currentOptions
                        |> withGitOutputProgress progressCallback)
                    options
                    commandAuth

            let fetchGit =
                applyCommandAuthentication
                    (fun currentOptions ->
                        createGit currentOptions
                        |> withGitOutputProgress progressCallback)
                    options
                    fetchAuth

            let runSpawned request =
                runGitCapturedWithOutput
                    progressCallback
                    {
                        request with
                            CancelCheck = Some cancelCheck
                    }

            reportPhase progressCallback "lfs" "Checking Git LFS objects"

            let! planResult =
                planOutboundPush
                    runSimpleGit
                    runSpawned
                    toFailure
                    (fun currentGit -> runSimpleGit (fun gitInstance -> gitInstance.status ()) currentGit)
                    arcPath
                    remoteName
                    (Some branchName)
                    fetchGit
                    git

            match planResult with
            | Error failure -> return Error failure
            | Ok OutboundPushPlan.SkipLfsUpload when cancelCheck () ->
                return Error(createFailure GitFailureKind.Canceled "Git LFS upload canceled.")
            | Ok OutboundPushPlan.SkipLfsUpload -> return Ok false
            | Ok(OutboundPushPlan.UploadLfsObjects objectIds) ->
                if cancelCheck () then
                    return Error(createFailure GitFailureKind.Canceled "Git LFS upload canceled.")
                else
                    reportPhase progressCallback "lfs" "Uploading Git LFS objects"

                    let! uploadResult =
                        GitLfsService.uploadObjects
                            runSpawned
                            commandAuth
                            cancelCheck
                            arcPath
                            remoteName
                            branchName
                            objectIds

                    return
                        match uploadResult with
                        | Ok() -> Ok true
                        | Error error -> Error(toFailure error)
    }

/// Persists Git workflow LFS settings in local repository config.
let setLfsSettings (arcPath: string) (settings: GitLfsSettingsDto) : JS.Promise<GitResult<unit>> = promise {
    match validateLfsThresholdMb settings.AutoTrackThresholdMb with
    | Error validationError -> return errorResult validationError
    | Ok thresholdMb ->
        return!
            withLocalGit
                arcPath
                (fun git -> promise {
                    let! _ =
                        git.raw [|
                            "config"
                            "--local"
                            AutoTrackThresholdKey
                            formatThresholdConfigValue thresholdMb
                        |]

                    let! _ =
                        git.raw [|
                            "config"
                            "--local"
                            MaterializeLargeObjectsKey
                            formatDownloadLargeFilesConfigValue settings.DownloadLargeFiles
                        |]

                    return ()
                })
}

let pruneLfsCacheWithProgressAndCancellation
    (arcPath: string)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (progressCallback: GitProgressCallback option)
    (cancelCheck: unit -> bool)
    (onStarted: unit -> unit)
    : JS.Promise<GitResult<string>> =
    promise {
        let! localValidationResult =
            withLocalGitAndProgress
                arcPath
                progressCallback
                (fun git -> promise {
                    match! requireCleanWorkingTreeForLfsStorageAction "Cleaning the Git LFS cache" git with
                    | Error validationError -> return abortGitPromiseWith validationError
                    | Ok() ->
                        match! rejectCustomLfsStorageForPrune git with
                        | Error validationError -> return abortGitPromiseWith validationError
                        | Ok() -> return ()
                })

        match localValidationResult with
        | Error failure -> return Error failure
        | Ok() ->
            let! sessionResult =
                createOriginLfsRemoteSession
                    arcPath
                    credentials
                    connectionProfileId
                    progressCallback

            match sessionResult with
            | Error failure -> return Error failure
            | Ok session ->
                match!
                    GitLfsService.runAuthenticatedMaintenance
                        session.CommandAuth
                        arcPath
                        GitLfsService.storagePruneArgs
                        progressCallback
                        cancelCheck
                        onStarted
                with
                | Ok output -> return Ok output
                | Error error -> return errorResult error
    }

let pruneLfsCacheWithProgress
    (arcPath: string)
    (progressCallback: GitProgressCallback option)
    : JS.Promise<GitResult<string>> =
    pruneLfsCacheWithProgressAndCancellation
        arcPath
        GitCredentialStrategy.anonymous
        None
        progressCallback
        (fun () -> false)
        ignore

let pruneLfsCache (arcPath: string) : JS.Promise<GitResult<string>> = pruneLfsCacheWithProgress arcPath None

let dedupLfsStorageWithProgressAndCancellation
    (arcPath: string)
    (progressCallback: GitProgressCallback option)
    (cancelCheck: unit -> bool)
    (onStarted: unit -> unit)
    : JS.Promise<GitResult<string>> =
    withLocalGitAndProgress
        arcPath
        progressCallback
        (fun git -> promise {
            match! requireCleanWorkingTreeForLfsStorageAction "Reducing Git LFS duplicate storage" git with
            | Error validationError -> return abortGitPromiseWith validationError
            | Ok() ->
                let session = createLocalGitSession arcPath progressCallback

                match!
                    GitLfsService.runAuthenticatedMaintenance
                        session.CommandAuth
                        arcPath
                        GitLfsService.storageDedupArgs
                        progressCallback
                        cancelCheck
                        onStarted
                with
                | Ok output -> return output
                | Error error -> return abortGitPromiseWith error
        })

let dedupLfsStorageWithProgress
    (arcPath: string)
    (progressCallback: GitProgressCallback option)
    : JS.Promise<GitResult<string>> =
    dedupLfsStorageWithProgressAndCancellation arcPath progressCallback (fun () -> false) ignore

let dedupLfsStorage (arcPath: string) : JS.Promise<GitResult<string>> =
    dedupLfsStorageWithProgress arcPath None

/// Stages validated pathspecs and auto-tracks oversized selected files in Git LFS before restaging.
let stagePaths (arcPath: string) (pathSpecs: string[]) : JS.Promise<GitResult<unit>> = promise {
    match validatePathspecs pathSpecs with
    | Error validationError -> return errorResult validationError
    | Ok safePathSpecs ->
        return!
            withLocalGit
                arcPath
                (fun git -> promise {
                    let! _ = git.add safePathSpecs
                    let! lfsResult = enforceStageTimeLfsTrackingForPaths arcPath safePathSpecs git

                    match lfsResult with
                    | Ok() -> return ()
                    | Error failure -> return abortGitPromise failure.Message
                })
}

/// Unstages validated pathspecs with a mixed reset while leaving working tree files unchanged.
let unstagePaths (arcPath: string) (pathSpecs: string[]) : JS.Promise<GitResult<unit>> = promise {
    match validatePathspecs pathSpecs with
    | Error validationError -> return errorResult validationError
    | Ok safePathSpecs ->
        return!
            withLocalGit
                arcPath
                (fun git -> promise {
                    let resetOptions = [| yield "--"; yield! safePathSpecs |]

                    let! _ = git.reset ("mixed", !^resetOptions)
                    return ()
                })
}

let private hasHeadCommit (git: ISimpleGit) = promise {
    let! headResult = runSimpleGit (fun currentGit -> currentGit.raw [| "rev-parse"; "--verify"; "HEAD" |]) git
    return Result.isOk headResult
}

let private headPathsForPathspecs (git: ISimpleGit) (pathSpecs: string[]) = promise {
    let! headPathsResult =
        runSimpleGit
            (fun currentGit -> currentGit.raw (withGitPathspecs [| "ls-tree"; "-r"; "--name-only"; "HEAD" |] pathSpecs))
            git

    match headPathsResult with
    | Ok output -> return splitGitOutputLines output
    | Error failure -> return abortGitPromise failure.Message
}

let private discardPathspecsWithOriginals (arcPath: string) (status: StatusResult) (safePathSpecs: string[]) =
    let selectedPaths = safePathSpecs |> Set.ofArray
    let statusDto = toStatusDto arcPath status

    let originalPaths =
        statusDto.Files
        |> Array.choose (fun file ->
            if selectedPaths.Contains file.Path then
                file.OriginalPath
            else
                None
        )
        |> Array.choose (fun path ->
            match ensureValidPathspec path with
            | Ok safePath -> Some safePath
            | Error _ -> None
        )

    Array.append safePathSpecs originalPaths |> Array.distinct

let private canceledLfsResult<'T> () : GitResult<'T> =
    Error(createFailure GitFailureKind.Canceled "Git LFS operation canceled.")

let private runLfsRaw (args: string[]) git =
    runSimpleGit (fun currentGit -> currentGit.raw args) git

let private requireLfsListingForPath
    (arcPath: string)
    (safePath: string)
    (context: OperationContext)
    : JS.Promise<GitLfsLsFileInfo> = promise {
    if context.Cancellation.IsCancellationRequested() then
        return abortGitPromise "Git LFS operation canceled."
    else
        match! GitLfsService.tryFindListingForPath arcPath safePath context with
        | Ok listing -> return listing
        | Error message -> return abortGitPromise $"'{safePath}' {message}"
}

let private getCleanLfsListingForPath
    (arcPath: string)
    (safePath: string)
    (dirtyMessage: string)
    (context: OperationContext)
    : JS.Promise<GitResult<GitLfsLsFileInfo>> =
    if context.Cancellation.IsCancellationRequested() then
        promise { return canceledLfsResult () }
    else
        withLocalGit
            arcPath
            (fun git -> promise {
                let! status = git.status ()

                if context.Cancellation.IsCancellationRequested() then
                    return abortGitPromise "Git LFS operation canceled."
                elif not (isPathCleanInStatus status safePath) then
                    return abortGitPromise dirtyMessage
                else
                    return! requireLfsListingForPath arcPath safePath context
            })

let private getCleanLfsFileForAction
    (arcPath: string)
    (requestedPath: string)
    (dirtyMessageForSafePath: string -> string)
    (context: OperationContext)
    : JS.Promise<GitResult<string * string * GitLfsLsFileInfo>> = promise {
    if context.Cancellation.IsCancellationRequested() then
        return canceledLfsResult ()
    else
        match tryResolveArcRelativePath arcPath requestedPath with
        | Error validationError -> return errorResult validationError
        | Ok(safePath, absolutePath) ->
            match!
                getCleanLfsListingForPath
                    arcPath
                    safePath
                    (dirtyMessageForSafePath safePath)
                    context
            with
            | Error failure -> return Error failure
            | Ok listing -> return Ok(safePath, absolutePath, listing)
}

let private createMissingLfsAttributesFailure safePath actionDescription =
    exn
        $"'{safePath}' is listed by Git LFS, but the current .gitattributes does not register it. Restore Git LFS tracking for this path before {actionDescription}."

let private checkPathTrackedByAttributes arcPath safePath : JS.Promise<GitResult<bool>> = promise {
    match! GitLfsAdapter.checkFilterAttributes arcPath [| safePath |] with
    | Error message ->
        return
            Error(
                createFailure
                    (classifyFailureKind message)
                    $"Could not inspect Git LFS attributes for '{safePath}': {message}"
            )
    | Ok attributes ->
        return
            attributes
            |> Array.tryFind (fun (path, _) -> path = safePath)
            |> Option.exists (fun (_, value) -> value = "lfs")
            |> Ok
}

let private createLfsCheckoutFailure safePath trackedByAttributes =
    if not trackedByAttributes then
        createMissingLfsAttributesFailure safePath "downloading it"
    else
        exn $"Could not download '{safePath}' into the working tree."

let private failLfsCheckout arcPath safePath : JS.Promise<unit> = promise {
    match! checkPathTrackedByAttributes arcPath safePath with
    | Error failure -> return abortGitPromise failure.Message
    | Ok trackedByAttributes ->
        return abortGitPromiseWith (createLfsCheckoutFailure safePath trackedByAttributes)
}

let private requireDownloadedLfsFile
    arcPath
    safePath
    absolutePath
    (expectedListing: GitLfsLsFileInfo)
    (context: OperationContext)
    (git: ISimpleGit)
    =
    promise {
        let! finalListing = requireLfsListingForPath arcPath safePath context

        if context.Cancellation.IsCancellationRequested() then
            return abortGitPromise "Git LFS operation canceled."
        elif not finalListing.checkout then
            return! failLfsCheckout arcPath safePath
        else
            let! finalStats = statAsync absolutePath
            let expectedSize = int64 expectedListing.size
            let actualSize = int64 finalStats.size

            if actualSize <> expectedSize then
                return! failLfsCheckout arcPath safePath
            else
                let! finalStatus = git.status ()

                if context.Cancellation.IsCancellationRequested() then
                    return abortGitPromise "Git LFS operation canceled."
                elif not (isPathCleanInStatus finalStatus safePath) then
                    return! failLfsCheckout arcPath safePath
                else
                    return ()
    }

let private downloadMissingLfsFile
    arcPath
    safePath
    absolutePath
    listing
    credentials
    connectionProfileId
    context
    : JS.Promise<GitResult<unit>> = promise {
    if context.Cancellation.IsCancellationRequested() then
        return canceledLfsResult ()
    else
        match! createOriginLfsRemoteSession arcPath credentials connectionProfileId None with
        | Error failure -> return Error failure
        | Ok session ->
            match!
                GitLfsService.downloadObjectFromListing
                    arcPath
                    session.CommandAuth
                    safePath
                    listing
                    (Some context.Cancellation.IsCancellationRequested)
                    (fun () ->
                        context.ReportProgress {
                            PhaseCode = "lfs-materialize-transfer"
                            Item = Some safePath
                            Completed = Some 0.0
                            Total = Some listing.size
                            DisplayMessage = None
                        })
            with
            | Error _ when context.Cancellation.IsCancellationRequested() -> return canceledLfsResult ()
            | Error error -> return errorResult error
            | Ok() when context.Cancellation.IsCancellationRequested() -> return canceledLfsResult ()
            | Ok() ->
                let! checkoutResult = runLfsRaw (GitLfsService.buildCheckoutArgs safePath) session.Git

                match checkoutResult with
                | Error failure -> return Error failure
                | Ok _ when context.Cancellation.IsCancellationRequested() -> return canceledLfsResult ()
                | Ok _ ->
                    return!
                        runSimpleGit
                            (fun currentGit ->
                                requireDownloadedLfsFile
                                    arcPath
                                    safePath
                                    absolutePath
                                    listing
                                    context
                                    currentGit)
                            session.Git
}

let freeLocalLfsCopy
    (arcPath: string)
    (requestedPath: string)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (context: OperationContext)
    : JS.Promise<GitResult<unit>> = promise {
    let! lfsFileResult =
        getCleanLfsFileForAction
            arcPath
            requestedPath
            (fun safePath ->
                $"'{safePath}' has local changes. Save, discard, or commit them before freeing the local LFS copy.")
            context

    let! trackedResult =
        match lfsFileResult with
        | Ok(safePath, _, listing) when listing.checkout -> checkPathTrackedByAttributes arcPath safePath
        | _ -> promise { return Ok true }

    match lfsFileResult, trackedResult with
    | Error failure, _ -> return Error failure
    | _, Error failure -> return Error failure
    | Ok(safePath, _, listing), _ when not listing.checkout ->
        context.ReportProgress {
            PhaseCode = "lfs-dematerialize"
            Item = Some safePath
            Completed = Some 0.0
            Total = Some listing.size
            DisplayMessage = Some "Git LFS file is already dematerialized"
        }

        return Ok()
    | Ok(safePath, _, _), Ok false ->
        return errorResult (createMissingLfsAttributesFailure safePath "freeing the local LFS copy")
    | Ok(safePath, absolutePath, listing), Ok true ->
        context.ReportProgress {
            PhaseCode = "lfs-dematerialize"
            Item = Some safePath
            Completed = Some 0.0
            Total = Some listing.size
            DisplayMessage = Some "Preparing Git LFS dematerialization"
        }

        if context.Cancellation.IsCancellationRequested() then
            return canceledLfsResult ()
        else
            match! createOriginLfsRemoteSession arcPath credentials connectionProfileId None with
            | Error failure -> return Error failure
            | Ok session ->
                match!
                    GitLfsService.fetchRefetchForPath
                        arcPath
                        session.CommandAuth
                        safePath
                        (Some context.Cancellation.IsCancellationRequested)
                        (fun () ->
                            context.ReportProgress {
                                PhaseCode = "lfs-dematerialize-transfer"
                                Item = Some safePath
                                Completed = Some 0.0
                                Total = Some listing.size
                                DisplayMessage = None
                            })
                with
                | Error _ when context.Cancellation.IsCancellationRequested() -> return canceledLfsResult ()
                | Error error -> return errorResult error
                | Ok() when context.Cancellation.IsCancellationRequested() -> return canceledLfsResult ()
                | Ok() ->
                    let git = session.Git
                    let backupPath = createTemporaryLfsBackupPath absolutePath

                    try
                        renameSync absolutePath backupPath

                        if context.Cancellation.IsCancellationRequested() then
                            restoreTemporaryLfsBackup backupPath absolutePath
                            return canceledLfsResult ()
                        else
                            match! ensureBackupMatchesLfsOid backupPath listing with
                            | Error validationError ->
                                restoreTemporaryLfsBackup backupPath absolutePath

                                return errorResult validationError
                            | Ok() ->
                                let pointerGit = applyLfsSkipSmudge git

                                let! checkoutResult =
                                    runSimpleGit
                                        (fun currentGit -> currentGit.raw [| "checkout"; "HEAD"; "--"; safePath |])
                                        pointerGit

                                match checkoutResult with
                                | Error failure ->
                                    restoreTemporaryLfsBackup backupPath absolutePath

                                    return Error failure
                                | Ok _ when context.Cancellation.IsCancellationRequested() ->
                                    restoreTemporaryLfsBackup backupPath absolutePath
                                    return canceledLfsResult ()
                                | Ok _ ->
                                    let! finalStatusResult = runSimpleGit (fun currentGit -> currentGit.status ()) git

                                    match finalStatusResult with
                                    | Error failure ->
                                        restoreTemporaryLfsBackup backupPath absolutePath

                                        return Error failure
                                    | Ok finalStatus when not (isPathCleanInStatus finalStatus safePath) ->
                                        restoreTemporaryLfsBackup backupPath absolutePath

                                        return
                                            errorResult (
                                                exn
                                                    $"Could not replace '{safePath}' with an LFS pointer without changing Git status."
                                            )
                                    | Ok _ ->
                                        removeTemporaryLfsBackup backupPath

                                        context.ReportProgress {
                                            PhaseCode = "lfs-dematerialize"
                                            Item = Some safePath
                                            Completed = Some listing.size
                                            Total = Some listing.size
                                            DisplayMessage = Some "Git LFS file dematerialized"
                                        }

                                        return Ok()
                    with ex ->
                        restoreTemporaryLfsBackup backupPath absolutePath

                        return errorResult ex
}

let downloadLfsFile
    (arcPath: string)
    (requestedPath: string)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (context: OperationContext)
    : JS.Promise<GitResult<unit>> = promise {
    let! lfsFileResult =
        getCleanLfsFileForAction
            arcPath
            requestedPath
            (fun safePath ->
                $"'{safePath}' has local changes. Save, discard, or commit them before downloading the Git LFS file.")
            context

    match lfsFileResult with
    | Error failure -> return Error failure
    | Ok(safePath, _, listing) when listing.checkout ->
        context.ReportProgress {
            PhaseCode = "lfs-materialize"
            Item = Some safePath
            Completed = Some listing.size
            Total = Some listing.size
            DisplayMessage = Some "Git LFS file is already materialized"
        }

        return Ok()
    | Ok(safePath, absolutePath, listing) ->
        context.ReportProgress {
            PhaseCode = "lfs-materialize"
            Item = Some safePath
            Completed = Some 0.0
            Total = Some listing.size
            DisplayMessage = Some "Preparing Git LFS materialization"
        }

        if context.Cancellation.IsCancellationRequested() then
            return canceledLfsResult ()
        else
            let! result =
                downloadMissingLfsFile
                    arcPath
                    safePath
                    absolutePath
                    listing
                    credentials
                    connectionProfileId
                    context

            match result with
            | Ok() ->
                context.ReportProgress {
                    PhaseCode = "lfs-materialize"
                    Item = Some safePath
                    Completed = Some listing.size
                    Total = Some listing.size
                    DisplayMessage = Some "Git LFS file materialized"
                }

                return Ok()
            | Error failure -> return Error failure
}

/// Discards validated pathspecs by restoring tracked paths from HEAD and cleaning selected untracked files.
let discardPaths (arcPath: string) (pathSpecs: string[]) : JS.Promise<GitResult<unit>> = promise {
    match validatePathspecs pathSpecs with
    | Error validationError -> return errorResult validationError
    | Ok safePathSpecs ->
        return!
            withLocalGit
                arcPath
                (fun git -> promise {
                    let! status = git.status ()
                    let discardPathSpecs = discardPathspecsWithOriginals arcPath status safePathSpecs
                    let! hasHead = hasHeadCommit git

                    if hasHead then
                        let! resetResult =
                            runSimpleGit
                                (fun currentGit -> currentGit.raw (withGitPathspecs [| "reset" |] discardPathSpecs))
                                git

                        match resetResult with
                        | Error failure -> return abortGitPromise failure.Message
                        | Ok _ ->
                            let! headPaths = headPathsForPathspecs git discardPathSpecs

                            if headPaths.Length > 0 then
                                let restoreGit = applyLfsSkipSmudge git

                                let! restoreResult =
                                    runSimpleGit
                                        (fun currentGit ->
                                            currentGit.raw (withGitPathspecs [| "restore"; "--worktree" |] headPaths)
                                        )
                                        restoreGit

                                match restoreResult with
                                | Error failure -> return abortGitPromise failure.Message
                                | Ok _ -> ()
                    else
                        let! rmCachedResult =
                            runSimpleGit
                                (fun currentGit ->
                                    currentGit.raw (
                                        withGitPathspecs
                                            [| "rm"; "--cached"; "-r"; "--ignore-unmatch" |]
                                            discardPathSpecs
                                    )
                                )
                                git

                        match rmCachedResult with
                        | Error failure -> return abortGitPromise failure.Message
                        | Ok _ -> ()

                    let! cleanResult =
                        runSimpleGit
                            (fun currentGit -> currentGit.raw (withGitPathspecs [| "clean"; "-fd" |] discardPathSpecs))
                            git

                    match cleanResult with
                    | Error failure -> return abortGitPromise failure.Message
                    | Ok _ -> return ()
                })
}

/// Commits the current index after validating the commit message and staged LFS policy.
let commit (arcPath: string) (message: string) : JS.Promise<GitResult<string>> = promise {
    let normalizedMessage = message.Trim()

    if String.IsNullOrWhiteSpace normalizedMessage then
        return errorResult (exn "Commit message must not be empty.")
    else
        return!
            withLocalGit
                arcPath
                (fun git -> promise {
                            let! lfsResult = validateCommitLfsPolicy arcPath git

                    match lfsResult with
                    | Error failure -> return abortGitPromise failure.Message
                    | Ok() ->
                        let! result = git.commit (normalizedMessage)

                        return
                            if String.IsNullOrWhiteSpace result.commit then
                                "Committed changes."
                            else
                                result.commit
                })
}

/// Writes resolved merge-conflict content, stages it, and optionally commits when no conflicts remain.
/// The expected content guard prevents overwriting a file that changed since the caller opened the conflict view.
let confirmMergeResolution
    (arcPath: string)
    (requestedPath: string)
    (expectedConflictContent: string)
    (resolvedContent: string)
    (autoCommit: bool)
    : JS.Promise<GitResult<GitConfirmMergeResolutionResult>> =
    promise {
        match tryResolveArcRelativePath arcPath requestedPath with
        | Error validationError -> return errorResult validationError
        | Ok(safeRequestedPath, absolutePath) ->
            if isExplicitlyUnsupportedPath safeRequestedPath then
                return unsupportedGitContentResult safeRequestedPath
            else
                return!
                    withLocalGit
                        arcPath
                        (fun git -> promise {
                            let! statusBeforeWrite = git.status ()
                            let statusDtoBeforeWrite = toStatusDto arcPath statusBeforeWrite

                            match ensureCurrentlyConflictedPath statusDtoBeforeWrite safeRequestedPath with
                            | Error conflictError -> return abortGitPromiseWith conflictError
                            | Ok() ->
                                let! currentContentResult = readWorkingTreeTextIfPresent arcPath safeRequestedPath

                                let currentConflictContent =
                                    match currentContentResult with
                                    | Ok(Some content) -> content
                                    | Ok None ->
                                        abortGitPromise $"Conflicted file '{safeRequestedPath}' no longer exists."
                                    | Error failure -> abortGitPromise failure.Message

                                if
                                    not (
                                        String.Equals(
                                            currentConflictContent,
                                            expectedConflictContent,
                                            StringComparison.Ordinal
                                        )
                                    )
                                then
                                    return
                                        abortGitPromise
                                            $"Conflicted file '{safeRequestedPath}' changed on disk since it was opened. Reopen the merge conflict view and retry."
                                else
                                    try
                                        writeFileSync absolutePath resolvedContent TextEncoding.Utf8
                                    with error ->
                                        return
                                            abortGitPromise
                                                $"Failed to write resolved content for '{safeRequestedPath}': {error.Message}"

                                    let! addResult =
                                        runSimpleGit (fun currentGit -> currentGit.add [| safeRequestedPath |]) git

                                    match addResult with
                                    | Error failure ->
                                        return
                                            abortGitPromise
                                                $"Resolved content was written to '{safeRequestedPath}', but staging failed: {failure.Message}"
                                    | Ok _ ->
                                        let! statusAfterStage = git.status ()
                                        let updatedStatus = toStatusDto arcPath statusAfterStage

                                        if
                                            autoCommit
                                            && updatedStatus.Conflicted.Length = 0
                                            && updatedStatus.IsMergeInProgress
                                        then
                                            let! commitResult =
                                                runSimpleGit
                                                    (fun currentGit -> currentGit.commit "Resolve merge conflicts")
                                                    git

                                            match commitResult with
                                            | Error failure -> return abortGitPromise failure.Message
                                            | Ok _ ->
                                                let! statusAfterCommit = git.status ()
                                                let committedStatus = toStatusDto arcPath statusAfterCommit

                                                return {
                                                    UpdatedStatus = committedStatus
                                                    RemainingConflictedPaths = committedStatus.Conflicted
                                                    NextConflictedPath = committedStatus.Conflicted |> Array.tryHead
                                                }
                                        else
                                            return {
                                                UpdatedStatus = updatedStatus
                                                RemainingConflictedPaths = updatedStatus.Conflicted
                                                NextConflictedPath = updatedStatus.Conflicted |> Array.tryHead
                                            }
                        })
    }

/// Creates and checks out a new local branch, optionally from a validated start point.
/// Tracking is reconciled against `origin/<branch>` when that remote branch exists.
let createBranch (arcPath: string) (branchName: string) (startPoint: string option) : JS.Promise<GitResult<unit>> = promise {
    match ensureValidBranchLikeName "Branch name" branchName with
    | Error branchError -> return errorResult branchError
    | Ok safeBranchName ->
        match startPoint with
        | None ->
            return!
                withLocalGit
                    arcPath
                    (fun git -> promise {
                        let checkoutGit = applyLfsSkipSmudge git

                        let! _ = checkoutGit.checkoutLocalBranch (safeBranchName)
                        do! reconcileTrackingBranchForCheckout "origin" safeBranchName git
                        return ()
                    })
        | Some value ->
            match ensureValidBranchLikeName "Start point" value with
            | Error startPointError -> return errorResult startPointError
            | Ok safeStartPoint ->
                return!
                    withLocalGit
                        arcPath
                        (fun git -> promise {
                            let checkoutGit = applyLfsSkipSmudge git

                            let! _ = checkoutGit.checkoutBranch (safeBranchName, safeStartPoint)
                            do! reconcileTrackingBranchForCheckout "origin" safeBranchName git
                            return ()
                        })
}

/// Adds a new validated remote to the active repository.
let addRemote (arcPath: string) (remoteName: string) (remoteUrl: string) : JS.Promise<GitResult<unit>> = promise {
    match validateRemoteName remoteName with
    | Error remoteError -> return errorResult remoteError
    | Ok safeRemoteName ->
        match ensureAllowedRemoteUrl remoteUrl with
        | Error remoteUrlError -> return errorResult remoteUrlError
        | Ok safeRemoteUrl ->
            return!
                withLocalGit
                    arcPath
                    (fun git -> promise {
                        let! remoteList = git.getRemotes ()

                        let remoteExists =
                            match remoteList with
                            | U2.Case1 remotes ->
                                remotes
                                |> Array.exists (fun remote ->
                                    String.Equals(remote.name, safeRemoteName, StringComparison.Ordinal)
                                )
                            | U2.Case2 remotes ->
                                remotes
                                |> Array.exists (fun remote ->
                                    String.Equals(remote.name, safeRemoteName, StringComparison.Ordinal)
                                )

                        if remoteExists then
                            return abortGitPromise $"Remote '{safeRemoteName}' already exists."
                        else
                            let! _ = git.addRemote (safeRemoteName, safeRemoteUrl)
                            return ()
                    })
}

/// Resolves the origin remote to a browser-friendly repository URL if one exists and is openable.
let getOriginRepositoryWebUrl (arcPath: string) : JS.Promise<GitResult<string option>> =
    withLocalGit
        arcPath
        (fun git -> promise {
            let originName = "origin"

            let! verboseRemoteList = git.getRemotes true

            let configuredFromVerboseRemotes =
                match verboseRemoteList with
                | U2.Case2 remotes ->
                    remotes
                    |> Array.tryFind (fun remote -> String.Equals(remote.name, originName, StringComparison.Ordinal))
                    |> Option.bind (fun remote ->
                        remote.refs
                        |> Option.ofObj
                        |> Option.bind (fun refs ->
                            [| refs.push; refs.fetch |]
                            |> Array.choose Option.ofObj
                            |> Array.map _.Trim()
                            |> Array.tryFind (String.IsNullOrWhiteSpace >> not)
                        )
                    )
                | U2.Case1 _ -> None

            let! configuredRemoteUrlOption =
                match configuredFromVerboseRemotes with
                | Some configuredRemoteUrl -> promise { return Some configuredRemoteUrl }
                | None -> promise {
                    let! remoteList = git.getRemotes ()

                    let originExists =
                        match remoteList with
                        | U2.Case1 remotes ->
                            remotes
                            |> Array.exists (fun remote ->
                                String.Equals(remote.name, originName, StringComparison.Ordinal)
                            )
                        | U2.Case2 remotes ->
                            remotes
                            |> Array.exists (fun remote ->
                                String.Equals(remote.name, originName, StringComparison.Ordinal)
                            )

                    if not originExists then
                        return None
                    else
                        let! configuredRemoteUrlResult =
                            runSimpleGit
                                (fun currentGit -> currentGit.raw [| "config"; "--get"; "remote.origin.url" |])
                                git

                        match configuredRemoteUrlResult with
                        | Ok configuredRemoteUrl ->
                            let normalizedRemoteUrl = configuredRemoteUrl.Trim()

                            if String.IsNullOrWhiteSpace normalizedRemoteUrl then
                                return None
                            else
                                return Some normalizedRemoteUrl
                        | Error _ -> return None
                  }

            match configuredRemoteUrlOption with
            | None -> return None
            | Some configuredRemoteUrl ->
                match tryGetRepositoryWebUrlFromRemoteUrl configuredRemoteUrl with
                | Ok repositoryWebUrl -> return Some repositoryWebUrl
                | Error _ -> return None
        })

/// Checks out an existing local branch, or creates/checks out a local branch from StartPoint.
/// Renderer passes remote branch switches as Name=<local name>, StartPoint=<remote ref>.
let checkoutBranch (arcPath: string) (request: GitCheckoutBranchRequest) : JS.Promise<GitResult<unit>> = promise {
    match ensureValidBranchLikeName "Branch name" request.Name with
    | Error branchError -> return errorResult branchError
    | Ok safeBranchName ->
        match request.StartPoint with
        | Some startPoint ->
            match ensureValidBranchLikeName "Start point" startPoint with
            | Error startPointError -> return errorResult startPointError
            | Ok safeStartPoint ->
                return!
                    withLocalGit
                        arcPath
                        (fun git -> promise {
                            let checkoutGit = applyLfsSkipSmudge git

                            let! _ = checkoutGit.checkoutBranch (safeBranchName, safeStartPoint)
                            do! reconcileTrackingBranchForCheckout "origin" safeBranchName git
                            return ()
                        })
        | None ->
            return!
                withLocalGit
                    arcPath
                    (fun git -> promise {
                        let checkoutGit = applyLfsSkipSmudge git

                        let! localBranches = git.branchLocal ()
                        let branches = valueOrEmptyArray localBranches.all

                        let exists =
                            branches
                            |> Array.exists (fun existing ->
                                String.Equals(existing, safeBranchName, StringComparison.Ordinal)
                            )

                        if not exists then
                            return abortGitPromise $"Branch '{safeBranchName}' does not exist in the local repository."

                        let! _ = checkoutGit.checkout (safeBranchName)
                        do! reconcileTrackingBranchForCheckout "origin" safeBranchName git
                        return ()
                    })
}
