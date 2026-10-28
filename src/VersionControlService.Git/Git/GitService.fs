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
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeCancellation = VersionControlService.Runtime.Node.Cancellation
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
    "this repository is configured for git lfs but 'git-lfs' was not found"
|]

let private lfsFilterFailureTokens = [|
    "git-lfs filter-process"
    "external filter 'git-lfs filter-process' failed"
    "smudge filter lfs failed"
    "clean filter 'lfs' failed"
|]

let private lfsExecutableMissingTokens = [|
    "command not found"
    "is not recognized as an internal or external command"
    "no such file or directory"
    "cannot find the file"
    "git-lfs: not found"
    "git-lfs not found"
    "git-lfs was not found"
    "'git-lfs' was not found"
    "git-lfs' was not found"
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

    let gitLfsExecutableCouldNotStart =
        normalizedMessage.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.exists (fun line ->
            line.Contains("git-lfs")
            && (lfsExecutableMissingTokens |> Array.exists line.Contains)
        )

    if
        containsAny lfsInstallRequiredTokens
        || (containsAny lfsFilterFailureTokens && gitLfsExecutableCouldNotStart)
    then
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
    match GitInternals.tryIndexLockFailure message with
    | Some _ -> { Kind = kind; Message = message }
    | None ->
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

// Paths here are either validated with ensureValidPathspec or come from git output. RepositoryPath
// rejects a trailing slash that git accepts, so this function adds the prefix itself.
let private literalPathspecFromString (path: string) = ":(literal)" + path

let private literalPathspecsFromStrings (paths: string[]) =
    paths |> Array.map literalPathspecFromString

let private splitGitOutputLines (text: string) =
    text.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map _.Trim()
    |> Array.filter (String.IsNullOrWhiteSpace >> not)

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
        else
            match GitAuthAdapter.tryParseRemoteAuthority safeRemoteUrl with
            | None -> Error(exn "Remote URL is missing a host.")
            | Some(host, port) ->
                // An ssh port belongs to the ssh server, so the web URL never carries it.
                let webAuthority =
                    match port with
                    | Some value when
                        not (String.Equals(uri.Scheme, "ssh", StringComparison.OrdinalIgnoreCase))
                        && not (GitAuthAdapter.isDefaultHttpsPort value)
                        ->
                        $"{host}:{value}"
                    | _ -> host

                let normalizedPath = uri.AbsolutePath.TrimEnd('/') |> trimTrailingGitSuffix

                if String.IsNullOrWhiteSpace normalizedPath || normalizedPath = "/" then
                    Error(exn "Remote URL does not contain a repository path.")
                else
                    Ok($"https://{webAuthority}{normalizedPath}".TrimEnd('/'))

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
    absolutePath + ".vcs-lfs-backup-" + Guid.NewGuid().ToString("N")

/// Resolves the backup directory inside the Git directory and creates it when missing.
/// A backup there stays out of the working tree, so git status does not list it. The directory
/// is separate from Git LFS's lfs/tmp. git-lfs commands delete files in lfs/tmp whose modification
/// time is over an hour old, and a renamed file keeps its old modification time.
let private resolveLfsBackupDirectory (arcPath: string) : JS.Promise<GitResult<string>> = promise {
    let! result =
        runGitCaptured {
            WorkingDirectory = Some arcPath
            Arguments = [| "rev-parse"; "--git-path"; "vcs-lfs-backup" |]
            Environment = None
            StandardInput = None
            CancelCheck = None
            TimeoutMs = Some GitLfsService.DefaultTimeoutMs
        }

    let gitPath = result.StdoutText |> Option.ofObj |> Option.defaultValue String.Empty |> _.Trim()

    if result.ExitCode <> 0 || result.TimedOut || String.IsNullOrWhiteSpace gitPath then
        let diagnostic = result.StderrText |> Option.ofObj |> Option.defaultValue String.Empty |> _.Trim()
        return errorResult (exn $"Could not find the backup directory in the Git directory: {diagnostic}")
    else
        try
            // git prints the path relative to its working directory, which is the repository root here.
            let directory = resolve [| arcPath; gitPath |]
            mkdirSync directory (MkdirOptions(recursive = true))
            return Ok directory
        with error ->
            return errorResult error
}

/// Moves the file into the backup directory. When that directory is on another volume,
/// a rename cannot cross it, so the backup goes next to the file.
let private moveToLfsBackup (absolutePath: string) (backupDirectory: string) : string =
    let backupPath =
        join [| backupDirectory; "vcs-lfs-backup-" + Guid.NewGuid().ToString("N") |]

    try
        renameSync absolutePath backupPath
        backupPath
    with error when tryGetNodeErrorCode error = Some "EXDEV" ->
        let siblingPath = createTemporaryLfsBackupPath absolutePath
        renameSync absolutePath siblingPath
        siblingPath

type private LfsBackupRestoreOutcome =
    | LfsBackupRestored
    | LfsBackupRetained

let mutable internal freeLocalLfsCopyTestHook: (string -> string -> unit) option = None

// A Git LFS pointer is ASCII text, so equal size and equal decoded text mean equal bytes.
// Other bytes cannot decode to the same ASCII text.
let private isExpectedLfsPointerAtPath (absolutePath: string) (expectedPointer: string) (pointerCheckoutRan: bool) =
    if not pointerCheckoutRan then
        false
    else
        try
            match tryLstatSync absolutePath with
            | Some stats when stats.isFile () && stats.size = float expectedPointer.Length ->
                readFileSync absolutePath Utf8 = expectedPointer
            | _ -> false
        with _ ->
            false

let private restoreTemporaryLfsBackup
    (backupPath: string)
    (absolutePath: string)
    (expectedPointer: string)
    (pointerCheckoutRan: bool)
    : LfsBackupRestoreOutcome =
    if not (existsSync backupPath) then
        LfsBackupRestored
    else
        let pathEntry =
            try
                Some(tryLstatSync absolutePath)
            with _ ->
                None

        match pathEntry with
        | None -> LfsBackupRetained
        | Some None ->
            renameSync backupPath absolutePath
            LfsBackupRestored
        | Some(Some _) ->
            if isExpectedLfsPointerAtPath absolutePath expectedPointer pointerCheckoutRan then
                unlinkSync absolutePath
                renameSync backupPath absolutePath
                LfsBackupRestored
            else
                LfsBackupRetained

let private lfsBackupRetainedFailure safePath backupPath =
    createFailure
        GitFailureKind.LfsBackupRetained
        $"Freeing the local LFS copy of '{safePath}' stopped because the file changed. Its earlier content is kept at '{backupPath}'."

let private removeTemporaryLfsBackup backupPath =
    if existsSync backupPath then
        unlinkSync backupPath

let private canceledLfsFailure () : GitFailure =
    createFailure GitFailureKind.Canceled "Git LFS operation canceled."

let private canceledLfsResult<'T> () : GitResult<'T> = Error(canceledLfsFailure ())

let private ensureFileMatchesLfsOid
    (filePath: string)
    (listing: GitLfsLsFileInfo)
    (context: OperationContext)
    : JS.Promise<GitResult<unit>> =
    promise {
        let! pointerTextResult =
            runGitCaptured {
                WorkingDirectory = None
                Arguments = [| "lfs"; "pointer"; "--file"; filePath |]
                Environment = None
                StandardInput = None
                CancelCheck = Some context.Cancellation.IsCancellationRequested
                TimeoutMs = None
            }

        let generatedPointer =
            $"{pointerTextResult.StdoutText}\n{pointerTextResult.StderrText}"

        let processDiagnostic =
            [|
                pointerTextResult.StderrText |> Option.ofObj |> Option.defaultValue String.Empty |> _.Trim()
                pointerTextResult.StdoutText |> Option.ofObj |> Option.defaultValue String.Empty |> _.Trim()
            |]
            |> Array.filter (String.IsNullOrWhiteSpace >> not)
            |> String.concat "\n"

        return
            if context.Cancellation.IsCancellationRequested() then
                canceledLfsResult ()
            elif pointerTextResult.ExitCode <> 0 then
                let failureMessage =
                    if String.IsNullOrWhiteSpace processDiagnostic then
                        $"Git LFS pointer verification failed with exit code {pointerTextResult.ExitCode}"
                    else
                        processDiagnostic.TrimEnd('.')

                errorResult (exn $"Could not verify the file against its Git LFS object: {failureMessage}. The file stays in place.")
            elif
                pointerTextResult.ExitCode = 0
                && generatedPointer.Contains($"oid sha256:{listing.oid}")
            then
                Ok()
            else
                errorResult (exn "The file does not match its Git LFS object. The file stays in place.")
    }

/// Hashes the file where it is, then moves it into a backup. The hash of a large file takes a while,
/// and the file stays in the working tree during that time. The size and modification time recorded
/// before the hash reveal a write that happened while it ran.
let private moveVerifiedFileToLfsBackup
    (arcPath: string)
    (gitPath: string)
    (displayPath: string)
    (absolutePath: string)
    (listing: GitLfsLsFileInfo)
    (context: OperationContext)
    : JS.Promise<GitResult<string * Stats * string>> =
    promise {
        let! verifiedResult =
            promise {
                try
                    let statsBeforeHash = statSync absolutePath
                    let! verification = ensureFileMatchesLfsOid absolutePath listing context
                    return verification |> Result.map (fun () -> statsBeforeHash)
                with error ->
                    return errorResult error
            }

        match verifiedResult with
        | Error failure -> return Error failure
        | Ok _ when context.Cancellation.IsCancellationRequested() -> return canceledLfsResult ()
        | Ok statsBeforeHash ->
            let! pointerResult =
                runGitCaptured {
                    WorkingDirectory = Some arcPath
                    Arguments = [| "cat-file"; "blob"; "HEAD:" + gitPath |]
                    Environment = None
                    StandardInput = None
                    CancelCheck = Some context.Cancellation.IsCancellationRequested
                    TimeoutMs = Some GitLfsService.DefaultTimeoutMs
                }

            if context.Cancellation.IsCancellationRequested() then
                return canceledLfsResult ()
            elif pointerResult.ExitCode <> 0 || pointerResult.TimedOut then
                let diagnostic =
                    [| pointerResult.StderrText.Trim(); pointerResult.StdoutText.Trim() |]
                    |> Array.filter (String.IsNullOrWhiteSpace >> not)
                    |> String.concat "\n"

                let detail =
                    if String.IsNullOrWhiteSpace diagnostic then
                        "Git could not read the committed pointer."
                    else
                        diagnostic

                return errorResult (exn $"Could not read the committed LFS pointer for '{displayPath}': {detail}")
            else
                match! resolveLfsBackupDirectory arcPath with
                | Error failure -> return Error failure
                | Ok backupDirectory ->
                    let movedResult =
                        try
                            Ok(moveToLfsBackup absolutePath backupDirectory)
                        with error ->
                            errorResult error

                    match movedResult with
                    | Error failure -> return Error failure
                    | Ok backupPath ->
                        try
                            let backupStats = statSync backupPath

                            if
                                backupStats.size <> statsBeforeHash.size
                                || backupStats.mtimeMs <> statsBeforeHash.mtimeMs
                            then
                                match restoreTemporaryLfsBackup backupPath absolutePath pointerResult.StdoutText false with
                                | LfsBackupRetained -> return Error(lfsBackupRetainedFailure displayPath backupPath)
                                | LfsBackupRestored ->
                                    return errorResult (exn $"'{displayPath}' changed while it was being freed. Try again.")
                            else
                                return Ok(backupPath, statsBeforeHash, pointerResult.StdoutText)
                        with error ->
                            match restoreTemporaryLfsBackup backupPath absolutePath pointerResult.StdoutText false with
                            | LfsBackupRetained -> return Error(lfsBackupRetainedFailure displayPath backupPath)
                            | LfsBackupRestored -> return errorResult error
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

let private splitPorcelainV2BranchHeaders (statusStdOut: string) =
    let branchValues = Collections.Generic.Dictionary<string, string>()
    let mutable offset = 0
    let mutable readingBranchHeaders = true

    while readingBranchHeaders && offset < statusStdOut.Length do
        let lineEnd = statusStdOut.IndexOfAny([| '\000'; '\n' |], offset)

        if lineEnd < 0 then
            readingBranchHeaders <- false
        else
            let header = statusStdOut.Substring(offset, lineEnd - offset).TrimEnd('\r')

            if header.StartsWith("# branch.", StringComparison.Ordinal) then
                let separator = header.IndexOf(' ', 2)

                if separator > 2 then
                    branchValues[header.Substring(2, separator - 2)] <- header.Substring(separator + 1)

                offset <- lineEnd + 1
            else
                readingBranchHeaders <- false

    branchValues, statusStdOut.Substring(offset)

let stripPorcelainV2BranchHeaders (statusStdOut: string) =
    splitPorcelainV2BranchHeaders statusStdOut |> snd

let parsePorcelainV2StatusDto (arcPath: string) (statusStdOut: string) : GitStatusDto =
    let branchValues, statusBody = splitPorcelainV2BranchHeaders statusStdOut

    let current =
        match branchValues.TryGetValue "branch.head" with
        | true, "(detached)" -> Some "HEAD"
        | true, branch -> Some branch
        | false, _ -> None

    let tracking =
        branchValues.TryGetValue "branch.upstream"
        |> function
            | true, upstream when not (String.IsNullOrWhiteSpace upstream) -> Some upstream
            | _ -> None

    let ahead, behind =
        match tracking, branchValues.TryGetValue "branch.ab" with
        | Some _, (true, counts) ->
            let values = counts.Split(' ', StringSplitOptions.RemoveEmptyEntries)

            let parseCount prefix =
                values
                |> Array.tryFind (fun value -> value.StartsWith(prefix, StringComparison.Ordinal))
                |> Option.bind (fun value ->
                    match Int32.TryParse(value.Substring(prefix.Length)) with
                    | true, count -> Some count
                    | false, _ -> None)
                |> Option.defaultValue 0

            parseCount "+", parseCount "-"
        | _ -> 0, 0

    let statusRecords = statusBody.Split('\000')
    let files = ResizeArray<GitFileStatusDto>()
    let conflicted = ResizeArray<string>()

    let splitFieldsBeforePath fieldCount (record: string) =
        let fields = ResizeArray<string>()
        let mutable fieldStart = 0
        let mutable index = 0

        while fields.Count < fieldCount && index < record.Length do
            if record[index] = ' ' then
                fields.Add(record.Substring(fieldStart, index - fieldStart))
                fieldStart <- index + 1

            index <- index + 1

        if fields.Count = fieldCount then
            fields.Add(record.Substring(fieldStart))

        fields.ToArray()

    let statusCode value = if value = '.' then " " else string value

    let mutable recordIndex = 0

    while recordIndex < statusRecords.Length do
        // With -z, paths are verbatim, so a record is never trimmed (names can start or end with spaces).
        let record = statusRecords[recordIndex]

        if record.StartsWith("1 ", StringComparison.Ordinal) then
            let fields = splitFieldsBeforePath 8 record

            if fields.Length = 9 then
                files.Add {
                    Path = fields[8]
                    Index = statusCode (fields[1][0])
                    WorkingDir = statusCode (fields[1][1])
                    OriginalPath = None
                }
        elif record.StartsWith("2 ", StringComparison.Ordinal) then
            let fields = splitFieldsBeforePath 9 record

            if fields.Length = 10 then
                let originalPath =
                    if recordIndex + 1 < statusRecords.Length then
                        let sourcePath = statusRecords[recordIndex + 1]

                        // simple-git exposes the source path for rename codes only.
                        if fields[1][0] = 'R' || fields[1][1] = 'R' then
                            Some sourcePath
                        else
                            None
                    else
                        None

                files.Add {
                    Path = fields[9]
                    Index = statusCode (fields[1][0])
                    WorkingDir = statusCode (fields[1][1])
                    OriginalPath = originalPath
                }

                recordIndex <- recordIndex + 1
        elif record.StartsWith("u ", StringComparison.Ordinal) then
            let fields = splitFieldsBeforePath 10 record

            if fields.Length = 11 then
                let path = fields[10]
                conflicted.Add path
                files.Add {
                    Path = path
                    Index = statusCode (fields[1][0])
                    WorkingDir = statusCode (fields[1][1])
                    OriginalPath = None
                }
        elif record.StartsWith("? ", StringComparison.Ordinal) then
            files.Add {
                Path = record.Substring(2)
                Index = "?"
                WorkingDir = "?"
                OriginalPath = None
            }
        elif record.StartsWith("! ", StringComparison.Ordinal) then
            ()

        recordIndex <- recordIndex + 1

    let conflictedPaths = conflicted.ToArray()

    {
        Current = current
        Tracking = tracking
        Ahead = ahead
        Behind = behind
        IsClean = files.Count = 0
        Conflicted = conflictedPaths
        IsMergeInProgress = conflictedPaths.Length > 0 || isMergeInProgress arcPath
        Files = files.ToArray()
    }

let private ensureCurrentlyConflictedPath (status: GitStatusDto) (requestedPath: string) =
    if
        status.Conflicted
        |> Array.exists (fun conflictedPath -> String.Equals(conflictedPath, requestedPath, StringComparison.Ordinal))
    then
        Ok()
    else
        Error(exn $"File '{requestedPath}' is not currently marked as conflicted.")

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

let private runSimpleGitPathspecChunks
    (command: string[])
    (pathSpecs: string[])
    (git: ISimpleGit)
    : JS.Promise<GitResult<string>> =
    promise {
        let fixedArguments = Array.append command [| "--" |]
        let chunks = GitPathTransport.chunkArguments fixedArguments (Array.distinct pathSpecs)
        let outputs = ResizeArray<string>()
        let mutable failure = None

        for chunk in chunks do
            match failure with
            | Some _ -> ()
            | None ->
                let arguments = Array.append fixedArguments chunk
                let! result = runSimpleGit (fun currentGit -> currentGit.raw arguments) git

                match result with
                | Error currentFailure -> failure <- Some currentFailure
                | Ok output -> outputs.Add output

        match failure with
        | Some currentFailure -> return Error currentFailure
        | None -> return Ok(String.concat "" outputs)
    }

let private runGitCapturedWithOutput progressCallback request =
    let reportLine (line: string) =
        match NodeProcess.tryParseProgressMeter line with
        | Some meter ->
            let displayMessage = NodeProcess.formatProgressMeter meter |> Redaction.redact
            let completedTotal = NodeProcess.progressMeterCompletedTotal meter

            progressCallback
            |> Option.iter (fun report ->
                GitInternals.createProgressDto
                    (Some "lfs")
                    (Some "upload")
                    None
                    (completedTotal |> Option.map fst)
                    (completedTotal |> Option.map snd)
                    (Some displayMessage)
                |> report)
        | None -> ()

    let observeOutput, flushOutput = NodeProcess.createMeterObserver reportLine

    promise {
        let! result = GitLfsAdapter.runGitCapturedWithOutput observeOutput request
        flushOutput ()
        return result
    }

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

                            let! restageResult =
                                runSimpleGitPathspecChunks
                                    [| "add" |]
                                    (literalPathspecsFromStrings pathsToRestage)
                                    git

                            match restageResult with
                            | Ok _ -> return Ok()
                            | Error failure when failure.Kind = GitFailureKind.LfsInstallRequired ->
                                return Error(createLfsInstallRequiredFailure (Some thresholdMb) None)
                            | Error failure -> return Error failure
    }

let private tryGetIndexedBlobId (git: ISimpleGit) (relativePath: string) : JS.Promise<GitResult<string option>> = promise {
    let! lsFilesResult =
        runSimpleGit
            (fun currentGit ->
                currentGit.raw [| "ls-files"; "--stage"; "--"; literalPathspecFromString relativePath |])
            git

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
                                GitCredentialStrategy.buildScopedCommandAuthentication
                                    remoteName
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

let private withUntimedLocalGit
    (arcPath: string)
    (progressCallback: GitProgressCallback option)
    (operation: ISimpleGit -> JS.Promise<'T>)
    : JS.Promise<GitResult<'T>> =
    promise {
        let options = createUntimedOptions arcPath progressCallback None
        let git = createGit options |> withGitOutputProgress progressCallback

        let! repoCheckResult = ensureRepo git

        match repoCheckResult with
        | Error repoError -> return errorResult repoError
        | Ok() -> return! runSimpleGit operation git
    }

let private withCancellableLocalGit
    (arcPath: string)
    (cancellation: OperationCancellation)
    (operation: ISimpleGit -> JS.Promise<'T>)
    : JS.Promise<GitResult<'T>> =
    promise {
        let abortSignal = NodeCancellation.toAbortSignal cancellation
        let options = createUntimedOptions arcPath None (Some abortSignal)
        let git = createGit options
        let canceledResult: GitResult<'T> = Error(createFailure GitFailureKind.Canceled "The Git command was canceled.")
        let mapCancellation result =
            match result with
            | Error _ when cancellation.IsCancellationRequested() -> canceledResult
            | _ -> result

        // An abort during the repository probe rejects the promise, so it is caught here and mapped like
        // any other failure.
        let! repoCheckResult =
            promise {
                try
                    return! ensureRepo git
                with error ->
                    return Error error
            }

        match repoCheckResult with
        | Error repoError -> return mapCancellation (errorResult repoError)
        | Ok() ->
            let! operationResult = runSimpleGit operation git
            return mapCancellation operationResult
    }

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
let getStatus (arcPath: string) (cancellation: OperationCancellation) : JS.Promise<GitResult<GitStatusDto>> =
    withCancellableLocalGit
        arcPath
        cancellation
        (fun git -> promise {
            let! status = git.status ()
            return toStatusDto arcPath status
        })

/// Lists local and remote branch refs.
let getBranches (arcPath: string) : JS.Promise<GitResult<GitBranchRefDto[]>> =
    withLocalGit
        arcPath
        (fun git -> promise {
            let! localBranchSummary = git.branchLocal ()
            let! localBranchUpstreamText =
                git.raw [| "for-each-ref"; "--format=%(refname:short)%09%(upstream:short)"; "refs/heads" |]
            let! remoteBranchText = git.raw [| "branch"; "-r"; "--no-color" |]

            let currentBranch =
                if localBranchSummary.detached then
                    None
                else
                    normalizeOptionalGitRef (Option.ofObj localBranchSummary.current)

            let localBranchUpstreams =
                localBranchUpstreamText.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
                |> Array.choose (fun branchLine ->
                    let separatorIndex = branchLine.IndexOf('\t')

                    if separatorIndex < 0 then
                        None
                    else
                        let branchName = branchLine.Substring(0, separatorIndex)
                        let upstream = normalizeOptionalGitRef (Some(branchLine.Substring(separatorIndex + 1)))
                        Some(branchName, upstream)
                )
                |> Map.ofArray

            let currentTracking =
                currentBranch
                |> Option.bind (fun branchName ->
                    Map.tryFind branchName localBranchUpstreams |> Option.flatten
                )

            let localRefs =
                valueOrEmptyArray localBranchSummary.all
                |> Array.map (fun branchName ->
                    let isCurrent = currentBranch = Some branchName

                    {
                        RefName = branchName
                        DisplayLabel = branchName
                        Kind = GitBranchRefKind.Local
                        IsCurrent = isCurrent
                        // Mark the active local branch when it already tracks an upstream so callers can represent the switched branch itself.
                        IsTracking = isCurrent && currentTracking.IsSome
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
                    IsTracking = currentTracking = Some branchName
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
            withUntimedLocalGit
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
    withUntimedLocalGit
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
                    let! stageResult =
                        runSimpleGitPathspecChunks [| "add" |] (literalPathspecsFromStrings safePathSpecs) git

                    match stageResult with
                    | Error failure -> return abortGitPromise failure.Message
                    | Ok _ ->
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
                    let! resetResult =
                        runSimpleGitPathspecChunks
                            [| "reset"; "--mixed" |]
                            (literalPathspecsFromStrings safePathSpecs)
                            git

                    match resetResult with
                    | Ok _ -> return ()
                    | Error failure -> return abortGitPromise failure.Message
                })
}

let private hasHeadCommit (git: ISimpleGit) = promise {
    let! headResult = runSimpleGit (fun currentGit -> currentGit.raw [| "rev-parse"; "--verify"; "HEAD" |]) git
    return Result.isOk headResult
}

let private headPathsForPathspecsWithGit (git: ISimpleGit) (pathSpecs: string[]) = promise {
    let! headPathsResult =
        runSimpleGitPathspecChunks
            [| "ls-tree"; "-r"; "-z"; "--name-only"; "HEAD" |]
            (literalPathspecsFromStrings pathSpecs)
            git

    match headPathsResult with
    | Ok output ->
        return
            output.Split('\000', StringSplitOptions.RemoveEmptyEntries)
            |> Array.distinct
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

let private createMissingLfsAttributesFailure safePath actionDescription =
    exn
        $"'{safePath}' is listed by Git LFS, but the current .gitattributes does not register it. Restore Git LFS tracking for this path before {actionDescription}."

let private createLfsCheckoutFailure safePath trackedByAttributes =
    if not trackedByAttributes then
        createMissingLfsAttributesFailure safePath "downloading it"
    else
        exn $"Could not download '{safePath}' into the working tree."

let private lfsFailure (message: string) : GitFailure = toFailure (exn message)

/// Reads which of `paths` have changes, with one `git status` per chunk of literal pathspecs.
/// The set holds the path of every status record and the source path of a rename, the names
/// that isPathCleanInStatus compares. The read has no timeout and ends when the context is canceled.
let private readDirtyPaths
    (arcPath: string)
    (paths: string[])
    (context: OperationContext)
    : JS.Promise<GitResult<Set<string>>> =
    withCancellableLocalGit
        arcPath
        context.Cancellation
        (fun git -> promise {
            let fixedArguments =
                [| "status"; "--porcelain=v2"; "-z"; "--untracked-files=all"; "--" |]

            let chunks =
                GitPathTransport.chunkArguments fixedArguments (literalPathspecsFromStrings paths |> Array.distinct)

            let dirtyPaths = ResizeArray<string>()

            for chunk in chunks do
                let! output = git.raw (Array.append fixedArguments chunk)

                if context.Cancellation.IsCancellationRequested() then
                    abortGitPromise "Git LFS operation canceled."

                for file in (parsePorcelainV2StatusDto arcPath output).Files do
                    dirtyPaths.Add file.Path

                    match file.OriginalPath with
                    | Some original -> dirtyPaths.Add original
                    | None -> ()

            return Set.ofSeq dirtyPaths
        })

/// Reads which of `paths` the current .gitattributes registers with the lfs filter.
/// The error is the text of the failed check.
let private readLfsTrackedPaths (arcPath: string) (paths: string[]) : JS.Promise<Result<Set<string>, string>> = promise {
    match! GitLfsAdapter.checkFilterAttributes arcPath paths with
    | Error message -> return Error message
    | Ok attributes ->
        return
            attributes
            |> Array.filter (fun (_, value) -> value = "lfs")
            |> Array.map fst
            |> Set.ofArray
            |> Ok
}

/// Resolves the directory that holds the local Git LFS objects.
let private resolveLfsMediaDirectory (arcPath: string) : JS.Promise<GitResult<string>> = promise {
    let runGit (arguments: string[]) : Async<Result<NodeProcess.ProcessOutput, OperationFailure>> = async {
        let! result =
            runGitCaptured {
                WorkingDirectory = Some arcPath
                Arguments = arguments
                Environment = None
                StandardInput = None
                CancelCheck = None
                TimeoutMs = Some GitLfsService.DefaultTimeoutMs
            }
            |> Async.AwaitPromise

        let output: NodeProcess.ProcessOutput = {
            ExitCode = result.ExitCode
            StdOut = result.StdoutText
            StdErr = result.StderrText
        }

        return Ok output
    }

    let! mediaDirectory = GitLfsObjects.resolveLocalMediaDirectory runGit arcPath |> Async.StartAsPromise

    match mediaDirectory with
    | Ok directory -> return Ok directory
    | Error failure -> return errorResult (exn failure.Message)
}

let private tryReadFileSize (absolutePath: string) : JS.Promise<float option> = promise {
    try
        let! stats = statAsync absolutePath
        return Some stats.size
    with _ ->
        return None
}

/// Tells whether the file at the path still holds a Git LFS pointer. A pointer has the size of a small
/// object, so a size check alone cannot tell the two apart.
let internal isLfsPointerFile (absolutePath: string) : bool =
    try
        (readFileSync absolutePath Utf8).StartsWith("version https://git-lfs", StringComparison.Ordinal)
    with _ ->
        false

/// What a download did to one path.
type LfsPathEffect =
    | Changed
    | AlreadyInState

/// The result of a download for one requested path. `Path` is the path as the caller passed it.
type LfsPathOutcome = {
    Path: string
    Result: Result<LfsPathEffect, GitFailure>
}

/// A requested path that still needs its content. `Key` is the spelling of the Git LFS listing,
/// which git uses for status and for the fetch and checkout patterns. `SafePath` is the validated spelling of the
/// request, which messages and progress reports use.
type private LfsPathCandidate = {
    Requested: string
    SafePath: string
    Key: string
    AbsolutePath: string
    Listing: GitLfsLsFileInfo
}

type private LfsPathResults = Collections.Generic.Dictionary<string, Result<LfsPathEffect, GitFailure>>

let private groupLfsCandidatesByPattern (candidates: LfsPathCandidate[]) =
    let groups = Collections.Generic.Dictionary<string, ResizeArray<LfsPathCandidate>>()

    for candidate in candidates do
        let pattern = GitLfsService.lfsPathPattern candidate.Key

        match groups.TryGetValue pattern with
        | true, group -> group.Add candidate
        | false, _ ->
            let group = ResizeArray<LfsPathCandidate>()
            group.Add candidate
            groups[pattern] <- group

    groups

/// Downloads the objects of the candidates into the local cache. A download that fails does not
/// stop the others, because git-lfs still fetches the rest of its objects. `missingOnRemote` collects
/// the candidates whose object the remote lacks and `downloadErrors` the error of every other failed
/// download. The result is an error only when the operation was canceled.
let private downloadLfsObjects
    (arcPath: string)
    (session: AuthenticatedGitSession)
    (exactCandidates: LfsPathCandidate[])
    (fetchCandidates: LfsPathCandidate[])
    (missingOnRemote: Collections.Generic.HashSet<string>)
    (downloadErrors: Collections.Generic.Dictionary<string, GitFailure>)
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    : JS.Promise<GitResult<unit>> = promise {
    let cancelCheck = Some context.Cancellation.IsCancellationRequested
    let mutable canceled = false

    let reportTransfer (item: string option) () =
        reportProgress {
            PhaseCode = "lfs-materialize-transfer"
            Item = item
            Completed = None
            Total = None
            DisplayMessage = None
        }

    let recordFailure (candidates: LfsPathCandidate[]) (error: exn) =
        let missingObjectIds = GitLfsService.missingOnRemoteObjectIds error.Message
        // A failure of one path that names the missing content without an object id still belongs to that path.
        let onlyPathMissing = candidates.Length = 1 && GitLfsService.isMissingOnRemote error.Message

        for candidate in candidates do
            if onlyPathMissing || missingObjectIds.Contains candidate.Listing.oid then
                missingOnRemote.Add candidate.Requested |> ignore
            else
                downloadErrors[candidate.Requested] <- toFailure error

    // A fetch of the path pattern also downloads files whose names differ only at special
    // characters. A candidate with such a file takes only its own object. The checkout later still runs
    // with the pattern, so it replaces that file's pointer when the file's own object is already cached.
    for candidate in exactCandidates do
        if not canceled then
            let! result =
                GitLfsService.downloadObjects
                    arcPath
                    session.CommandAuth
                    (GitLfsService.SmudgeObject(candidate.Key, candidate.Listing))
                    cancelCheck
                    (reportTransfer (Some candidate.SafePath))

            match result with
            | _ when context.Cancellation.IsCancellationRequested() -> canceled <- true
            | Ok() -> ()
            | Error error -> recordFailure [| candidate |] error

    if fetchCandidates.Length > 0 then
        let groups = groupLfsCandidatesByPattern fetchCandidates

        let fixedArguments =
            Array.append session.CommandAuth.ConfigArgs (GitLfsService.buildDownloadObjectArgs [||])

        let patternChunks = GitPathTransport.chunkArguments fixedArguments (Seq.toArray groups.Keys)

        for patterns in patternChunks do
            if not canceled then
                let chunkCandidates =
                    patterns |> Array.collect (fun pattern -> groups[pattern].ToArray())

                let item =
                    if chunkCandidates.Length = 1 then
                        Some chunkCandidates[0].SafePath
                    else
                        None

                let! result =
                    GitLfsService.downloadObjects
                        arcPath
                        session.CommandAuth
                        (GitLfsService.FetchPaths(chunkCandidates |> Array.map (fun candidate -> candidate.Key)))
                        cancelCheck
                        (reportTransfer item)

                match result with
                | _ when context.Cancellation.IsCancellationRequested() -> canceled <- true
                | Ok() -> ()
                | Error error -> recordFailure chunkCandidates error

    if canceled then
        return canceledLfsResult ()
    else
        return Ok()
}

/// Runs `git lfs checkout` for the candidates, one command per chunk of patterns. The result holds
/// the candidates of the chunks that succeeded. A chunk that fails settles its candidates with the
/// failure. A cancellation before the first chunk is an error, and a later one settles the candidates
/// of the chunks that did not run.
let private checkoutLfsCandidates
    (arcPath: string)
    (candidates: LfsPathCandidate[])
    (results: LfsPathResults)
    (context: OperationContext)
    : JS.Promise<GitResult<LfsPathCandidate[]>> = promise {
    let groups = groupLfsCandidatesByPattern candidates

    let chunks =
        GitPathTransport.chunkArguments GitLfsService.checkoutArgsPrefix (Seq.toArray groups.Keys)

    let checkedOut = ResizeArray<LfsPathCandidate>()
    let mutable chunkIndex = 0
    let mutable stopped = false
    let mutable canceledBeforeChange = false

    for patterns in chunks do
        let chunkCandidates = patterns |> Array.collect (fun pattern -> groups[pattern].ToArray())

        if stopped then
            for candidate in chunkCandidates do
                results[candidate.Requested] <- Error(canceledLfsFailure ())
        elif context.Cancellation.IsCancellationRequested() then
            stopped <- true

            if chunkIndex = 0 then
                canceledBeforeChange <- true
            else
                for candidate in chunkCandidates do
                    results[candidate.Requested] <- Error(canceledLfsFailure ())
        else
            let! checkoutResult = GitLfsService.checkoutPatterns arcPath patterns

            match checkoutResult with
            | Error error ->
                for candidate in chunkCandidates do
                    results[candidate.Requested] <- Error(toFailure error)
            | Ok() -> checkedOut.AddRange chunkCandidates

        chunkIndex <- chunkIndex + 1

    if canceledBeforeChange then
        return canceledLfsResult ()
    else
        return Ok(checkedOut.ToArray())
}

/// Confirms that each checked-out candidate holds its content and settles it. A file passes when its
/// size equals the listed size and its path-limited status is clean. The files have changed by now,
/// so the checks ignore cancellation.
let private verifyLfsCandidates
    (arcPath: string)
    (candidates: LfsPathCandidate[])
    (results: LfsPathResults)
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    : JS.Promise<unit> = promise {
    if candidates.Length > 0 then
        let uncancelableContext = { context with Cancellation = OperationCancellation.none }

        let! dirtyResult =
            readDirtyPaths arcPath (candidates |> Array.map (fun candidate -> candidate.Key)) uncancelableContext

        match dirtyResult with
        | Error failure ->
            for candidate in candidates do
                results[candidate.Requested] <- Error failure
        | Ok dirtyPaths ->
            let failed = ResizeArray<LfsPathCandidate>()

            for candidate in candidates do
                let! size = tryReadFileSize candidate.AbsolutePath

                let pointerLeftInPlace =
                    candidate.Listing.size <= 1024.0 && isLfsPointerFile candidate.AbsolutePath

                if
                    size = Some candidate.Listing.size
                    && not pointerLeftInPlace
                    && not (dirtyPaths.Contains candidate.Key)
                then
                    results[candidate.Requested] <- Ok Changed

                    reportProgress {
                        PhaseCode = "lfs-materialize"
                        Item = Some candidate.SafePath
                        Completed = Some candidate.Listing.size
                        Total = Some candidate.Listing.size
                        DisplayMessage = Some "Git LFS file materialized"
                    }
                else
                    failed.Add candidate

            if failed.Count > 0 then
                let! trackedResult =
                    readLfsTrackedPaths arcPath (failed |> Seq.map (fun candidate -> candidate.Key) |> Seq.toArray)

                for candidate in failed do
                    match trackedResult with
                    | Error message ->
                        results[candidate.Requested] <-
                            Error(
                                lfsFailure
                                    $"Could not inspect Git LFS attributes for '{candidate.SafePath}': {message}"
                            )
                    | Ok trackedPaths ->
                        results[candidate.Requested] <-
                            Error(
                                toFailure (
                                    createLfsCheckoutFailure candidate.SafePath (trackedPaths.Contains candidate.Key)
                                )
                            )
}

/// Downloads and checks out the candidates. The error is a failure of the whole operation. A
/// failure of one candidate goes into `results`.
let private downloadLfsCandidates
    (arcPath: string)
    (listing: Collections.Generic.Dictionary<string, GitLfsLsFileInfo>)
    (candidates: LfsPathCandidate[])
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    (results: LfsPathResults)
    : JS.Promise<GitResult<unit>> = promise {
    for candidate in candidates do
        reportProgress {
            PhaseCode = "lfs-materialize"
            Item = Some candidate.SafePath
            Completed = Some 0.0
            Total = Some candidate.Listing.size
            DisplayMessage = Some "Preparing Git LFS materialization"
        }

    if context.Cancellation.IsCancellationRequested() then
        return canceledLfsResult ()
    else
        match! createOriginLfsRemoteSession arcPath credentials connectionProfileId None with
        | Error failure -> return Error failure
        | Ok session ->
            let candidateKeys =
                Collections.Generic.HashSet<string>(candidates |> Array.map (fun candidate -> candidate.Key))

            let otherListedIndex =
                GitLfsService.createListedPathIndex (listing.Keys |> Seq.filter (fun key -> not (candidateKeys.Contains key)))

            let exactCandidates, fetchCandidates =
                candidates
                |> Array.partition (fun candidate ->
                    GitLfsService.pathPatternMatchesIndexedPath otherListedIndex candidate.Key)

            let missingOnRemote = Collections.Generic.HashSet<string>()
            let downloadErrors = Collections.Generic.Dictionary<string, GitFailure>()

            match!
                downloadLfsObjects
                    arcPath
                    session
                    exactCandidates
                    fetchCandidates
                    missingOnRemote
                    downloadErrors
                    reportProgress
                    context
            with
            | Error failure -> return Error failure
            | Ok() ->
                match! resolveLfsMediaDirectory arcPath with
                | Error failure -> return Error failure
                | Ok mediaDirectory ->
                    let downloaded = ResizeArray<LfsPathCandidate>()

                    for candidate in candidates do
                        // git-lfs treats an empty object as present without a file in the object directory.
                        let isAvailable =
                            candidate.Listing.size = 0.0
                            || GitLfsObjects.isObjectLocallyAvailable
                                mediaDirectory
                                candidate.Listing.oid
                                candidate.Listing.size

                        if missingOnRemote.Contains candidate.Requested then
                            results[candidate.Requested] <-
                                Error(
                                    lfsFailure
                                        $"The remote does not have the content of '{candidate.SafePath}'. Upload it from the computer that saved it."
                                )
                        elif isAvailable then
                            downloaded.Add candidate
                        else
                            match downloadErrors.TryGetValue candidate.Requested with
                            | true, failure -> results[candidate.Requested] <- Error failure
                            | false, _ ->
                                results[candidate.Requested] <-
                                    Error(lfsFailure $"Git LFS did not download '{candidate.SafePath}'.")

                    // A file that left the working tree during the download stays gone.
                    // The checkout would create it again from the object.
                    let present = ResizeArray<LfsPathCandidate>()

                    for candidate in downloaded do
                        if (tryLstatSync candidate.AbsolutePath).IsNone then
                            results[candidate.Requested] <-
                                Error(
                                    lfsFailure
                                        $"'{candidate.SafePath}' has local changes. Save, discard, or commit them before downloading the Git LFS file."
                                )
                        else
                            present.Add candidate

                    if present.Count = 0 then
                        return Ok()
                    else
                        match! checkoutLfsCandidates arcPath (present.ToArray()) results context with
                        | Error failure -> return Error failure
                        | Ok checkedOut ->
                            do! verifyLfsCandidates arcPath checkedOut results reportProgress context
                            return Ok()
}

/// Reads the Git LFS listing once. It settles the paths that need no action and fails the paths with
/// changes or without a listing entry. The rest are the candidates. A request that names an
/// already listed path again, for example in another case, is dropped from `order`, because a
/// duplicate counts once.
let private readLfsCandidates
    (arcPath: string)
    (resolvedPaths: (string * string)[])
    (dirtyMessage: string -> string)
    (isAlreadyInState: GitLfsLsFileInfo -> bool)
    (reportAlreadyInState: string -> GitLfsLsFileInfo -> unit)
    (context: OperationContext)
    (results: LfsPathResults)
    (order: ResizeArray<string>)
    : JS.Promise<GitResult<Collections.Generic.Dictionary<string, GitLfsLsFileInfo> * LfsPathCandidate[]>> = promise {
    let isCanceled () = context.Cancellation.IsCancellationRequested()
    let! listingResult = GitLfsService.readLsFilesByRelativePath arcPath context

    match listingResult with
    | Error _ when isCanceled () -> return canceledLfsResult ()
    | Error message ->
        // A request for one path keeps the message of the single-file action, which names the path.
        let pathPrefix =
            if resolvedPaths.Length = 1 then
                $"'{snd resolvedPaths[0]}' "
            else
                String.Empty

        return errorResult (exn $"{pathPrefix}Could not read Git LFS file metadata: {message}")
    | Ok _ when isCanceled () -> return canceledLfsResult ()
    | Ok listing ->
        let lookup = GitLfsService.createListingLookup listing
        let seenKeys = Collections.Generic.HashSet<string>()
        let uniqueEntries = ResizeArray<string * string * GitLfsLsFileInfo option>()

        for requested, safePath in resolvedPaths do
            let info = lookup safePath

            match info with
            | Some listed when not (seenKeys.Add listed.name) -> order.Remove requested |> ignore
            | _ -> uniqueEntries.Add((requested, safePath, info))

        let entries = uniqueEntries.ToArray()

        // A path the listing lacks is still checked for changes, because a changed file reports
        // that before it reports that Git LFS does not list it.
        let statusPaths =
            entries
            |> Array.map (fun (_, safePath, info) ->
                match info with
                | Some listed -> listed.name
                | None -> safePath)
            |> Array.distinct

        match! readDirtyPaths arcPath statusPaths context with
        | Error _ when isCanceled () -> return canceledLfsResult ()
        | Error failure -> return Error failure
        | Ok _ when isCanceled () -> return canceledLfsResult ()
        | Ok dirtyPaths ->
            let candidates = ResizeArray<LfsPathCandidate>()

            for requested, safePath, info in entries do
                let statusPath =
                    match info with
                    | Some listed -> listed.name
                    | None -> safePath

                if dirtyPaths.Contains statusPath then
                    results[requested] <- Error(lfsFailure (dirtyMessage safePath))
                else
                    match info with
                    | None ->
                        results[requested] <-
                            Error(
                                lfsFailure $"'{safePath}' The file is not listed by Git LFS in the current checkout."
                            )
                    | Some listed when isAlreadyInState listed ->
                        results[requested] <- Ok AlreadyInState
                        reportAlreadyInState safePath listed
                    | Some listed ->
                        candidates.Add {
                            Requested = requested
                            SafePath = safePath
                            Key = listed.name
                            AbsolutePath = resolve [| arcPath; listed.name |]
                            Listing = listed
                        }

            return Ok(listing, candidates.ToArray())
}

/// Reads the Git LFS listing once, then downloads the paths that need it.
let private runLfsDownload
    (arcPath: string)
    (resolvedPaths: (string * string)[])
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    (results: LfsPathResults)
    (order: ResizeArray<string>)
    : JS.Promise<GitResult<unit>> = promise {
    match!
        readLfsCandidates
            arcPath
            resolvedPaths
            (fun safePath ->
                $"'{safePath}' has local changes. Save, discard, or commit them before downloading the Git LFS file.")
            (fun listed -> listed.checkout)
            (fun safePath listed ->
                reportProgress {
                    PhaseCode = "lfs-materialize"
                    Item = Some safePath
                    Completed = Some listed.size
                    Total = Some listed.size
                    DisplayMessage = Some "Git LFS file is already materialized"
                })
            context
            results
            order
    with
    | Error failure -> return Error failure
    | Ok(_, candidates) when candidates.Length = 0 -> return Ok()
    | Ok(listing, candidates) ->
        return!
            downloadLfsCandidates
                arcPath
                listing
                candidates
                credentials
                connectionProfileId
                reportProgress
                context
                results
}

/// Validates and deduplicates the requested paths for a download or a free, and runs `run` on the
/// valid ones. A path that fails validation settles at once. The result names each requested path
/// once, in request order, and `run` may drop a request from `order` when it duplicates another.
/// The error is a failure of the whole operation.
let private runLfsPaths
    (arcPath: string)
    (requestedPaths: string[])
    (context: OperationContext)
    (run: (string * string)[] -> LfsPathResults -> ResizeArray<string> -> JS.Promise<GitResult<unit>>)
    : JS.Promise<GitResult<LfsPathOutcome[]>> = promise {
    if context.Cancellation.IsCancellationRequested() then
        return canceledLfsResult ()
    else
        try
            let results = LfsPathResults()
            let order = ResizeArray<string>()
            let resolvedPaths = ResizeArray<string * string>()
            let seenPaths = Collections.Generic.HashSet<string>()

            for requested in requestedPaths do
                match tryResolveArcRelativePath arcPath requested with
                | Error validationError ->
                    if seenPaths.Add requested then
                        order.Add requested
                        results[requested] <- Error(toFailure validationError)
                | Ok(safePath, _) ->
                    if seenPaths.Add safePath then
                        order.Add requested
                        resolvedPaths.Add((requested, safePath))

            let! runResult =
                if resolvedPaths.Count = 0 then
                    promise { return Ok() }
                else
                    run (resolvedPaths.ToArray()) results order

            match runResult with
            | Error failure -> return Error failure
            | Ok() ->
                return
                    order
                    |> Seq.map (fun requested -> ({ Path = requested; Result = results[requested] }: LfsPathOutcome))
                    |> Seq.toArray
                    |> Ok
        with error ->
            return errorResult error
}

/// Downloads the content of Git LFS files that sit in the working tree as pointers. One listing and
/// one remote session serve the whole request, and the objects come in one fetch per chunk of paths.
/// The result names each requested path once, in request order. A path whose request failed does not
/// stop the others. The error is a failure of the whole operation, such as a cancellation before any
/// file changed.
let downloadLfsFiles
    (arcPath: string)
    (requestedPaths: string[])
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    : JS.Promise<GitResult<LfsPathOutcome[]>> =
    runLfsPaths arcPath requestedPaths context (fun resolvedPaths results order ->
        runLfsDownload
            arcPath
            resolvedPaths
            credentials
            connectionProfileId
            reportProgress
            context
            results
            order)

/// The answer of the remote check for one path of a free.
type private LfsConfirmation =
    | RemoteHasObject
    | RemoteLacksCoverage
    | ConfirmationFailed of exn
    | ConfirmationCanceled

let private lfsConfirmationFailureMessage (safePath: string) (error: exn) =
    if GitLfsService.isMissingOnRemote error.Message then
        $"The remote does not have '{safePath}' yet. Push it before freeing its local copy."
    else
        $"Could not confirm that the remote has '{safePath}': {error.Message}"

/// Asks the remote about one path. The dry run must list the object, then a remote without
/// the batch API must complete a refetch.
let private confirmLfsPath
    (arcPath: string)
    (session: AuthenticatedGitSession)
    (usesServer: bool)
    (candidate: LfsPathCandidate)
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    : JS.Promise<LfsConfirmation> = promise {
    let cancelCheck = Some context.Cancellation.IsCancellationRequested

    let onTransferStarted () =
        reportProgress {
            PhaseCode = "lfs-dematerialize-transfer"
            Item = Some candidate.SafePath
            Completed = Some 0.0
            Total = Some candidate.Listing.size
            DisplayMessage = None
        }

    let! confirmationResult =
        promise {
            let! dryRunResult =
                GitLfsService.fetchRefetchForPaths
                    arcPath
                    session.CommandAuth
                    [| candidate.Key |]
                    true
                    cancelCheck
                    onTransferStarted

            match dryRunResult with
            | Error error -> return Error error
            | Ok output when not (GitLfsService.dryRunCoversObject output candidate.Listing.oid) -> return Ok None
            | Ok _ when usesServer -> return Ok(Some String.Empty)
            | Ok _ ->
                let! refetchResult =
                    GitLfsService.fetchRefetchForPaths
                        arcPath
                        session.CommandAuth
                        [| candidate.Key |]
                        false
                        cancelCheck
                        onTransferStarted

                return Result.map Some refetchResult
        }

    match confirmationResult with
    | Error _ when context.Cancellation.IsCancellationRequested() -> return ConfirmationCanceled
    | Error error -> return ConfirmationFailed error
    | Ok _ when context.Cancellation.IsCancellationRequested() -> return ConfirmationCanceled
    | Ok None -> return RemoteLacksCoverage
    | Ok _ -> return RemoteHasObject
}

/// Asks the remote about each path on its own, in order.
let private confirmLfsPathsOneByOne
    (arcPath: string)
    (session: AuthenticatedGitSession)
    (usesServer: bool)
    (paths: LfsPathCandidate[])
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    : JS.Promise<LfsConfirmation[]> = promise {
    let answers = ResizeArray<LfsConfirmation>()

    for candidate in paths do
        if context.Cancellation.IsCancellationRequested() then
            answers.Add ConfirmationCanceled
        else
            let! answer = confirmLfsPath arcPath session usesServer candidate reportProgress context
            answers.Add answer

    return answers.ToArray()
}

/// Asks the remote about the paths of one chunk with a single dry run. The chunk counts as
/// confirmed when the dry run succeeds and, without the batch API, so does the refetch. When either
/// fails, its output may name the objects that the remote lacks. Those paths settle with the message of
/// a single-file free, and the rest of the chunk asks again once. A failure that names no object, or a
/// second failure, sends each path to ask on its own, so every path keeps the message of a single-file free.
let rec private confirmLfsChunk
    (arcPath: string)
    (session: AuthenticatedGitSession)
    (usesServer: bool)
    (chunk: LfsPathCandidate[])
    (mayRetry: bool)
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    : JS.Promise<LfsConfirmation[]> = promise {
    let isCanceled () = context.Cancellation.IsCancellationRequested()

    if chunk.Length = 1 then
        let! confirmation = confirmLfsPath arcPath session usesServer chunk[0] reportProgress context
        return [| confirmation |]
    else
        let cancelCheck = Some context.Cancellation.IsCancellationRequested
        let keys = chunk |> Array.map (fun candidate -> candidate.Key)

        let onTransferStarted () =
            reportProgress {
                PhaseCode = "lfs-dematerialize-transfer"
                Item = None
                Completed = Some 0.0
                Total = Some(chunk |> Array.sumBy (fun candidate -> candidate.Listing.size))
                DisplayMessage = None
            }

        let canceledAnswers = chunk |> Array.map (fun _ -> ConfirmationCanceled)

        let answerAfterFailure (error: exn) = promise {
            let missingObjectIds = GitLfsService.missingOnRemoteObjectIds error.Message
            let lacksObject = chunk |> Array.map (fun candidate -> missingObjectIds.Contains candidate.Listing.oid)

            if mayRetry && Array.exists id lacksObject then
                let remaining =
                    chunk |> Array.filter (fun candidate -> not (missingObjectIds.Contains candidate.Listing.oid))

                let! remainingAnswers =
                    if remaining.Length = 0 then
                        promise { return [||] }
                    else
                        confirmLfsChunk arcPath session usesServer remaining false reportProgress context

                let answers = ResizeArray<LfsConfirmation>()
                let mutable next = 0

                for lacks in lacksObject do
                    if lacks then
                        answers.Add(ConfirmationFailed error)
                    else
                        answers.Add remainingAnswers[next]
                        next <- next + 1

                return answers.ToArray()
            else
                return! confirmLfsPathsOneByOne arcPath session usesServer chunk reportProgress context
        }

        let! dryRunResult =
            GitLfsService.fetchRefetchForPaths arcPath session.CommandAuth keys true cancelCheck onTransferStarted

        match dryRunResult with
        | Error _ when isCanceled () -> return canceledAnswers
        | Error error -> return! answerAfterFailure error
        | Ok output ->
            let isCovered =
                chunk
                |> Array.map (fun candidate -> GitLfsService.dryRunCoversObject output candidate.Listing.oid)

            // The refetch only matters for an object that the dry run lists.
            let! refetchResult =
                if usesServer || not (Array.exists id isCovered) then
                    promise { return Ok String.Empty }
                else
                    GitLfsService.fetchRefetchForPaths
                        arcPath
                        session.CommandAuth
                        keys
                        false
                        cancelCheck
                        onTransferStarted

            match refetchResult with
            | Error _ when isCanceled () -> return canceledAnswers
            | Error error -> return! answerAfterFailure error
            | Ok _ when isCanceled () -> return canceledAnswers
            | Ok _ ->
                return
                    isCovered
                    |> Array.map (fun covered -> if covered then RemoteHasObject else RemoteLacksCoverage)
}

/// Asks the remote about every candidate, one dry run per chunk of patterns. A candidate that the
/// remote lacks settles with its failure. The result holds the confirmed candidates in request order.
/// The error is a cancellation.
let private confirmLfsCandidates
    (arcPath: string)
    (session: AuthenticatedGitSession)
    (usesServer: bool)
    (candidates: LfsPathCandidate[])
    (results: LfsPathResults)
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    : JS.Promise<GitResult<LfsPathCandidate[]>> = promise {
    let groups = groupLfsCandidatesByPattern candidates

    let fixedArguments =
        Array.append session.CommandAuth.ConfigArgs (GitLfsService.buildFetchRefetchArgsForPaths true [||])

    let patternChunks = GitPathTransport.chunkArguments fixedArguments (Seq.toArray groups.Keys)
    let confirmed = ResizeArray<LfsPathCandidate>()
    let mutable canceled = false

    for patterns in patternChunks do
        if not canceled then
            let chunk = patterns |> Array.collect (fun pattern -> groups[pattern].ToArray())
            let! answers = confirmLfsChunk arcPath session usesServer chunk true reportProgress context

            for index in 0 .. chunk.Length - 1 do
                let candidate = chunk[index]

                match answers[index] with
                | ConfirmationCanceled -> canceled <- true
                | RemoteHasObject -> confirmed.Add candidate
                | RemoteLacksCoverage ->
                    results[candidate.Requested] <-
                        Error(
                            lfsFailure
                                $"Could not check whether the remote has '{candidate.SafePath}', so its local copy stays."
                        )
                | ConfirmationFailed error ->
                    results[candidate.Requested] <-
                        Error(lfsFailure (lfsConfirmationFailureMessage candidate.SafePath error))

    if canceled then
        return canceledLfsResult ()
    else
        let confirmedKeys =
            Collections.Generic.HashSet<string>(confirmed |> Seq.map (fun candidate -> candidate.Requested))

        return Ok(candidates |> Array.filter (fun candidate -> confirmedKeys.Contains candidate.Requested))
}

/// Replaces one file with its pointer. The file moves into a backup first and returns from it when
/// anything goes wrong. A failure that leaves the backup behind has the kind LfsBackupRetained.
let private freeLfsCandidate
    (arcPath: string)
    (git: ISimpleGit)
    (candidate: LfsPathCandidate)
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    : JS.Promise<GitResult<unit>> = promise {
    let gitPath = candidate.Key
    let displayPath = candidate.SafePath
    let absolutePath = candidate.AbsolutePath
    let listing = candidate.Listing
    let! backupResult = moveVerifiedFileToLfsBackup arcPath gitPath displayPath absolutePath listing context

    match backupResult with
    | Error failure -> return Error failure
    | Ok(backupPath, statsBeforeHash, expectedPointer) ->
        let mutable pointerCheckoutRan = false

        try
            freeLocalLfsCopyTestHook
            |> Option.iter (fun hook -> hook "after-backup-move" absolutePath)

            if context.Cancellation.IsCancellationRequested() then
                match restoreTemporaryLfsBackup backupPath absolutePath expectedPointer pointerCheckoutRan with
                | LfsBackupRetained -> return Error(lfsBackupRetainedFailure displayPath backupPath)
                | LfsBackupRestored -> return canceledLfsResult ()
            else
                let pointerGit = applyLfsSkipSmudge git

                let pathExistsBeforeCheckout =
                    try
                        tryLstatSync absolutePath |> Option.isSome
                    with _ ->
                        true

                if pathExistsBeforeCheckout then
                    return Error(lfsBackupRetainedFailure displayPath backupPath)
                else
                    pointerCheckoutRan <- true

                    let! checkoutResult =
                        runSimpleGit
                            (fun currentGit ->
                                currentGit.raw [| "checkout"; "HEAD"; "--"; literalPathspecFromString gitPath |])
                            pointerGit

                    freeLocalLfsCopyTestHook
                    |> Option.iter (fun hook -> hook "after-pointer-checkout" absolutePath)

                    match checkoutResult with
                    | Error failure ->
                        match restoreTemporaryLfsBackup backupPath absolutePath expectedPointer pointerCheckoutRan with
                        | LfsBackupRetained -> return Error(lfsBackupRetainedFailure displayPath backupPath)
                        | LfsBackupRestored -> return Error failure
                    | Ok _ when context.Cancellation.IsCancellationRequested() ->
                        match restoreTemporaryLfsBackup backupPath absolutePath expectedPointer pointerCheckoutRan with
                        | LfsBackupRetained -> return Error(lfsBackupRetainedFailure displayPath backupPath)
                        | LfsBackupRestored -> return canceledLfsResult ()
                    | Ok _ ->
                        // The file has changed, so the final status ignores cancellation.
                        let! finalStatusResult =
                            readDirtyPaths
                                arcPath
                                [| gitPath |]
                                { context with
                                    Cancellation = OperationCancellation.none }

                        freeLocalLfsCopyTestHook
                        |> Option.iter (fun hook -> hook "after-final-status" absolutePath)

                        match finalStatusResult with
                        | Error failure ->
                            match
                                restoreTemporaryLfsBackup backupPath absolutePath expectedPointer pointerCheckoutRan
                            with
                            | LfsBackupRetained -> return Error(lfsBackupRetainedFailure displayPath backupPath)
                            | LfsBackupRestored -> return Error failure
                        | Ok dirtyPaths when dirtyPaths.Contains gitPath ->
                            match
                                restoreTemporaryLfsBackup backupPath absolutePath expectedPointer pointerCheckoutRan
                            with
                            | LfsBackupRetained -> return Error(lfsBackupRetainedFailure displayPath backupPath)
                            | LfsBackupRestored ->
                                return
                                    errorResult (
                                        exn
                                            $"Could not replace '{displayPath}' with an LFS pointer without changing Git status."
                                    )
                        | Ok _ ->
                            if not (isExpectedLfsPointerAtPath absolutePath expectedPointer pointerCheckoutRan) then
                                return Error(lfsBackupRetainedFailure displayPath backupPath)
                            else
                                let backupStats = statSync backupPath

                                if
                                    backupStats.size <> statsBeforeHash.size
                                    || backupStats.mtimeMs <> statsBeforeHash.mtimeMs
                                then
                                    match
                                        restoreTemporaryLfsBackup
                                            backupPath
                                            absolutePath
                                            expectedPointer
                                            pointerCheckoutRan
                                    with
                                    | LfsBackupRetained -> return Error(lfsBackupRetainedFailure displayPath backupPath)
                                    | LfsBackupRestored ->
                                        return
                                            errorResult (exn $"'{displayPath}' changed while it was being freed. Try again.")
                                else
                                    removeTemporaryLfsBackup backupPath

                                    reportProgress {
                                        PhaseCode = "lfs-dematerialize"
                                        Item = Some candidate.SafePath
                                        Completed = Some listing.size
                                        Total = Some listing.size
                                        DisplayMessage = Some "Git LFS file dematerialized"
                                    }

                                    return Ok()
        with ex ->
            match restoreTemporaryLfsBackup backupPath absolutePath expectedPointer pointerCheckoutRan with
            | LfsBackupRetained -> return Error(lfsBackupRetainedFailure displayPath backupPath)
            | LfsBackupRestored -> return errorResult ex
}

/// Frees the confirmed candidates one after the other. A cancellation stops the next paths, which
/// settle as canceled, and the paths already freed stay freed. A cancellation before the first path
/// is an error of the whole operation.
let private freeLfsCandidates
    (arcPath: string)
    (session: AuthenticatedGitSession)
    (confirmed: LfsPathCandidate[])
    (results: LfsPathResults)
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    : JS.Promise<GitResult<unit>> = promise {
    let mutable index = 0
    let mutable stopped = false
    let mutable canceledBeforeChange = false

    for candidate in confirmed do
        if stopped then
            results[candidate.Requested] <- Error(canceledLfsFailure ())
        elif context.Cancellation.IsCancellationRequested() then
            stopped <- true

            if index = 0 then
                canceledBeforeChange <- true
            else
                results[candidate.Requested] <- Error(canceledLfsFailure ())
        else
            let! freeResult = freeLfsCandidate arcPath session.Git candidate reportProgress context

            match freeResult with
            | Ok() -> results[candidate.Requested] <- Ok Changed
            | Error failure -> results[candidate.Requested] <- Error failure

        index <- index + 1

    if canceledBeforeChange then
        return canceledLfsResult ()
    else
        return Ok()
}

/// Reads the Git LFS listing once, then frees the paths that need it.
let private runLfsFree
    (arcPath: string)
    (resolvedPaths: (string * string)[])
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    (results: LfsPathResults)
    (order: ResizeArray<string>)
    : JS.Promise<GitResult<unit>> = promise {
    match!
        readLfsCandidates
            arcPath
            resolvedPaths
            (fun safePath ->
                $"'{safePath}' has local changes. Save, discard, or commit them before freeing the local LFS copy.")
            (fun listed -> not listed.checkout)
            (fun safePath listed ->
                reportProgress {
                    PhaseCode = "lfs-dematerialize"
                    Item = Some safePath
                    Completed = Some 0.0
                    Total = Some listed.size
                    DisplayMessage = Some "Git LFS file is already dematerialized"
                })
            context
            results
            order
    with
    | Error failure -> return Error failure
    | Ok(_, candidates) when candidates.Length = 0 -> return Ok()
    | Ok(_, candidates) ->
        let! trackedResult = readLfsTrackedPaths arcPath (candidates |> Array.map (fun candidate -> candidate.Key))
        let tracked = ResizeArray<LfsPathCandidate>()

        for candidate in candidates do
            match trackedResult with
            | Error message ->
                results[candidate.Requested] <-
                    Error(
                        createFailure
                            (classifyFailureKind message)
                            $"Could not inspect Git LFS attributes for '{candidate.SafePath}': {message}"
                    )
            | Ok trackedPaths when trackedPaths.Contains candidate.Key -> tracked.Add candidate
            | Ok _ ->
                results[candidate.Requested] <-
                    Error(
                        toFailure (createMissingLfsAttributesFailure candidate.SafePath "freeing the local LFS copy")
                    )

        if tracked.Count = 0 then
            return Ok()
        else
            for candidate in tracked do
                reportProgress {
                    PhaseCode = "lfs-dematerialize"
                    Item = Some candidate.SafePath
                    Completed = Some 0.0
                    Total = Some candidate.Listing.size
                    DisplayMessage = Some "Preparing Git LFS dematerialization"
                }

            if context.Cancellation.IsCancellationRequested() then
                return canceledLfsResult ()
            else
                match! createOriginLfsRemoteSession arcPath credentials connectionProfileId None with
                | Error failure -> return Error failure
                | Ok session ->
                    let! endpoint = GitLfsService.tryGetLfsEndpoint arcPath
                    let! standaloneAgent = GitLfsService.tryHasStandaloneTransferAgent arcPath
                    let usesServer = GitLfsService.usesBatchApi endpoint && not standaloneAgent

                    match!
                        confirmLfsCandidates
                            arcPath
                            session
                            usesServer
                            (tracked.ToArray())
                            results
                            reportProgress
                            context
                    with
                    | Error failure -> return Error failure
                    | Ok confirmed ->
                        if confirmed.Length = 0 then
                            return Ok()
                        else
                            return! freeLfsCandidates arcPath session confirmed results reportProgress context
}

/// Replaces the content of Git LFS files with their pointers, after the remote has confirmed that it
/// holds the objects. One listing and one remote session serve the whole request, with one remote check
/// per chunk of paths. The result names each requested path once, in request order. A path whose request
/// failed does not stop the others. The error is a failure of the whole operation, such as a cancellation
/// before any file changed. The objects in the local cache stay, and the caller removes those of the
/// freed paths.
let freeLfsFiles
    (arcPath: string)
    (requestedPaths: string[])
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (reportProgress: OperationProgress -> unit)
    (context: OperationContext)
    : JS.Promise<GitResult<LfsPathOutcome[]>> =
    runLfsPaths arcPath requestedPaths context (fun resolvedPaths results order ->
        runLfsFree arcPath resolvedPaths credentials connectionProfileId reportProgress context results order)

/// Frees one file. The result is false when the file already was a pointer.
let freeLocalLfsCopy
    (arcPath: string)
    (requestedPath: string)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (context: OperationContext)
    : JS.Promise<GitResult<bool>> = promise {
    match! freeLfsFiles arcPath [| requestedPath |] credentials connectionProfileId context.ReportProgress context with
    | Error failure -> return Error failure
    | Ok outcomes ->
        match outcomes[0].Result with
        | Ok Changed -> return Ok true
        | Ok AlreadyInState -> return Ok false
        | Error failure -> return Error failure
}

let downloadLfsFile
    (arcPath: string)
    (requestedPath: string)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (context: OperationContext)
    : JS.Promise<GitResult<unit>> = promise {
    match! downloadLfsFiles arcPath [| requestedPath |] credentials connectionProfileId context.ReportProgress context with
    | Error failure -> return Error failure
    | Ok outcomes ->
        match outcomes[0].Result with
        | Ok _ -> return Ok()
        | Error failure -> return Error failure
}

/// Discards validated pathspecs by restoring tracked paths from HEAD and cleaning selected untracked files.
let discardPaths (arcPath: string) (pathSpecs: string[]) : JS.Promise<GitResult<string[]>> = promise {
    match validatePathspecs pathSpecs with
    | Error validationError -> return errorResult validationError
    | Ok safePathSpecs ->
        return!
            withUntimedLocalGit
                arcPath
                None
                (fun git -> promise {
                    let! status = git.status ()
                    let discardPathSpecs = discardPathspecsWithOriginals arcPath status safePathSpecs
                    let! hasHead = hasHeadCommit git
                    let mutable restoredHeadPaths = [||]

                    if hasHead then
                        let! resetResult =
                            runSimpleGitPathspecChunks
                                [| "reset" |]
                                (literalPathspecsFromStrings discardPathSpecs)
                                git

                        match resetResult with
                        | Error failure -> return abortGitPromise failure.Message
                        | Ok _ ->
                            let! headPaths = headPathsForPathspecsWithGit git discardPathSpecs
                            restoredHeadPaths <- headPaths

                            if headPaths.Length > 0 then
                                let restoreGit = applyLfsSkipSmudge git

                                let! restoreResult =
                                    runSimpleGitPathspecChunks
                                        [| "restore"; "--worktree" |]
                                        (literalPathspecsFromStrings headPaths)
                                        restoreGit

                                match restoreResult with
                                | Error failure -> return abortGitPromise failure.Message
                                | Ok _ -> ()
                    else
                        let! rmCachedResult =
                            runSimpleGitPathspecChunks
                                [| "rm"; "--cached"; "-r"; "--ignore-unmatch" |]
                                (literalPathspecsFromStrings discardPathSpecs)
                                git

                        match rmCachedResult with
                        | Error failure -> return abortGitPromise failure.Message
                        | Ok _ -> ()

                    let! cleanResult =
                        runSimpleGitPathspecChunks
                            [| "clean"; "-fd" |]
                            (literalPathspecsFromStrings discardPathSpecs)
                            git

                    match cleanResult with
                    | Error failure -> return abortGitPromise failure.Message
                    | Ok _ -> return restoredHeadPaths
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
                                        runSimpleGit
                                            (fun currentGit ->
                                                currentGit.raw [|
                                                    "add"
                                                    "--"
                                                    literalPathspecFromString safeRequestedPath
                                                |])
                                            git

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
