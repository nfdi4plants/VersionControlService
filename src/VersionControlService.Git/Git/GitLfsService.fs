module internal VersionControlService.Git.GitLfsService

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Support
open VersionControlService.Git.GitEngineTypes
open VersionControlService.Runtime.Node.Interop
open VersionControlService.Bindings.SimpleGit
open VersionControlService.Git.GitLfsAdapter
open VersionControlService.Git.GitAuthAdapter

module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path

/// Default timeout for interactive Git LFS commands launched by the service process.
[<Literal>]
let DefaultTimeoutMs = 30000

/// Describes whether an outbound Git push needs a separate LFS object upload before the git ref push.
[<RequireQualifiedAccess>]
type OutboundPushPlan =
    | SkipLfsUpload
    | UploadLfsObjects of lfsObjectIds: string[]

let private maxLfsPointerProbeBytes = 1024L
let mutable private cachedSystemInstalled = false

[<Literal>]
let private lfsLsFilesTimeoutMs = 15000

/// Parses Git LFS's platform-specific version banner (`git-lfs/3.6.1`,
/// `git lfs 3.6.1`, ...). A successful executable probe without a version is
/// still installed, but is not considered compatible for dependent workflows.
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

let parseLsFiles (stdoutText: string) : GitLfsLsFileInfo[] =
    try
        match Thoth.Json.JavaScript.Decode.fromString JsonDecoder.lsFilesResponseDecoder stdoutText with
        | Ok files -> files
        | Error message -> failwith message
    with ex ->
        let detail =
            if String.IsNullOrWhiteSpace ex.Message then
                "Unknown decoding error."
            else
                ex.Message

        raise (Exception($"Failed to parse git lfs ls-files JSON: {detail}", ex))

let private indexUsingRelativePath (files: GitLfsLsFileInfo[]) : Dictionary<string, GitLfsLsFileInfo> =
    let filesByRelativePath = Dictionary<string, GitLfsLsFileInfo>()

    files
    |> Array.iter (fun info ->
        if not (String.IsNullOrWhiteSpace info.name) then
            let relativePath = PathHelpers.normalizeSeparators info.name
            filesByRelativePath.[relativePath] <- { info with name = relativePath }
    )

    filesByRelativePath

let tryFindLsFileInfoByRelativePath (filesByRelativePath: Dictionary<string, GitLfsLsFileInfo>) (relativePath: string) =
    let normalizedPath = PathHelpers.normalizeSeparators relativePath

    match filesByRelativePath.TryGetValue normalizedPath with
    | true, info -> Some info
    | false, _ ->
        filesByRelativePath
        |> Seq.tryPick (fun entry ->
            if PathHelpers.pathsEqual entry.Key normalizedPath then
                Some entry.Value
            else
                None
        )

let buildLsFilesJsonArgs () = [| "lfs"; "ls-files"; "-j" |]

/// Chooses the most useful text from a Git LFS adapter result for user-facing errors.
let extractFailureMessage (result: GitLfsResult) =
    let errorText =
        result.Error |> Option.ofObj |> Option.defaultValue String.Empty |> _.Trim()

    let outputText =
        result.Output |> Option.ofObj |> Option.defaultValue String.Empty |> _.Trim()

    if not (String.IsNullOrWhiteSpace errorText) then
        errorText
    elif not (String.IsNullOrWhiteSpace outputText) then
        outputText
    else
        "Git LFS command failed."

let private redactDiagnosticText (text: string) =
    if String.IsNullOrWhiteSpace text then
        text
    else
        redactToken text

/// Builds a GitLfsRequest with a generated request id and default timeout.
let createRequest
    (repoPath: string)
    (command: GitLfsCommand)
    (filePath: string option)
    (timeoutMs: int option)
    : GitLfsRequest =
    let effectiveTimeout =
        match command with
        | Pull
        | Fetch -> None
        | _ -> Some(defaultArg timeoutMs DefaultTimeoutMs)

    {
        RequestId = Guid.NewGuid().ToString()
        RepoPath = repoPath
        Command = command
        FilePath = filePath
        TimeoutMs = effectiveTimeout
    }

/// Runs a Git LFS adapter request and normalizes unsuccessful adapter results to Result.Error.
let run
    (request: GitLfsRequest)
    (onProgress: string -> unit)
    (cancelCheck: unit -> bool)
    : JS.Promise<Result<GitLfsResult, exn>> =
    promise {
        try
            let! result = gitLfs.Run request onProgress cancelCheck

            if result.Success then
                return Ok result
            else
                return Error(exn (extractFailureMessage result))
        with ex ->
            return Error ex
    }

/// Runs Git LFS without progress or cancellation hooks.
let runSilently (request: GitLfsRequest) : JS.Promise<Result<GitLfsResult, exn>> = run request ignore (fun () -> false)

let private requireGitLfsForLiteralPolicy (repoPath: string) : JS.Promise<Result<unit, string>> = promise {
    let! result =
        runGitCaptured {
            WorkingDirectory = Some repoPath
            Arguments = [| "lfs"; "version" |]
            Environment = None
            StandardInput = None
            CancelCheck = None
            TimeoutMs = Some DefaultTimeoutMs
        }

    return
        if result.ExitCode = 0 && not result.TimedOut then
            Ok()
        elif not (String.IsNullOrWhiteSpace result.StderrText) then
            Error result.StderrText
        elif not (String.IsNullOrWhiteSpace result.StdoutText) then
            Error result.StdoutText
        else
            Error "Git LFS literal path tracking failed."
}

let private literalAttributePattern (relativePath: string) =
    let globPattern = System.Text.StringBuilder("/")

    for character in relativePath do
        match character with
        | '['
        | ']'
        | '*'
        | '?' -> globPattern.Append('\\').Append(character) |> ignore
        | _ -> globPattern.Append(character) |> ignore

    let quoted = System.Text.StringBuilder("\"")

    for character in globPattern.ToString() do
        match character with
        | '\\' -> quoted.Append("\\\\") |> ignore
        | '"' -> quoted.Append("\\\"") |> ignore
        | '\u0007' -> quoted.Append("\\a") |> ignore
        | '\b' -> quoted.Append("\\b") |> ignore
        | '\t' -> quoted.Append("\\t") |> ignore
        | '\n' -> quoted.Append("\\n") |> ignore
        | '\u000b' -> quoted.Append("\\v") |> ignore
        | '\u000c' -> quoted.Append("\\f") |> ignore
        | '\r' -> quoted.Append("\\r") |> ignore
        | value when int value < 32 || int value = 127 ->
            quoted.Append("\\").Append(Convert.ToString(int value, 8).PadLeft(3, '0'))
            |> ignore
        | _ -> quoted.Append(character) |> ignore

    quoted.Append('"').ToString()

let literalTrackingRule relativePath =
    $"{literalAttributePattern relativePath} filter=lfs diff=lfs merge=lfs -text"

let private removeExactAttributeRule (rule: string) (content: string) =
    let rulePattern = $"^{Regex.Escape(rule)}(?:\\r?\\n|$)"
    Regex(rulePattern, RegexOptions.Multiline).Replace(content, String.Empty)

let private sameFileIdentity (left: NodeFileSystem.Stats) (right: NodeFileSystem.Stats) =
    left.dev = right.dev && left.ino = right.ino

[<Emit("$0?.code")>]
let private nodeErrorCode (_error: exn) : string = jsNative

let private isHardLinkUnavailable (error: exn) =
    match nodeErrorCode error |> Option.ofObj with
    | Some "EPERM"
    | Some "EACCES"
    | Some "ENOTSUP"
    | Some "EOPNOTSUPP"
    | Some "ENOSYS"
    | Some "EXDEV" -> true
    | _ -> false

let private ensurePathHasNoLinks (path: string) =
    let rec check current =
        let parent = NodePath.dirname current

        if parent <> current then
            check parent

        if NodeFileSystem.existsSync current && (NodeFileSystem.lstatSync current).isSymbolicLink () then
            invalidOp $"Refusing to update Git attributes through the symbolic-link path '{current}'."

    check (NodePath.resolve [| path |])

let readAttributesNoFollow (attributesPath: string) =
    if NodeFileSystem.existsSync attributesPath then
        ensurePathHasNoLinks attributesPath
        let pathStats = NodeFileSystem.lstatSync attributesPath
        let content, openedStats = NodeFileSystem.readUtf8FileNoFollowSync attributesPath
        let afterStats = NodeFileSystem.lstatSync attributesPath

        if not (sameFileIdentity pathStats openedStats && sameFileIdentity pathStats afterStats) then
            invalidOp "The Git attributes file changed while its policy was being read."

        content, Some pathStats
    else
        ensurePathHasNoLinks (NodePath.dirname attributesPath)
        "", None

let replaceAttributesAtomically
    (attributesPath: string)
    (originalIdentity: NodeFileSystem.Stats option)
    (originalContent: string)
    (content: string)
    =
    let tempPath = attributesPath + $".vcs-{Guid.NewGuid():N}.tmp"

    try
        NodeFileSystem.writeUtf8FileExclusiveAndFlushSync tempPath content
        ensurePathHasNoLinks (NodePath.dirname attributesPath)

        match originalIdentity with
        | Some expected ->
            if not (NodeFileSystem.existsSync attributesPath) then
                invalidOp "The Git attributes file disappeared before its policy could be replaced."

            let currentContent, currentIdentity = readAttributesNoFollow attributesPath

            if
                currentContent <> originalContent
                || (currentIdentity |> Option.exists (sameFileIdentity expected) |> not)
            then
                invalidOp "The Git attributes file changed before its policy could be replaced."
        | None when NodeFileSystem.existsSync attributesPath ->
            invalidOp "The Git attributes file appeared before its policy could be created."
        | None -> ()

        NodeFileSystem.renameSync tempPath attributesPath
    finally
        if NodeFileSystem.existsSync tempPath then
            NodeFileSystem.unlinkSync tempPath

type AttributesReplacement = private {
    AttributesPath: string
    TempPath: string
    TempIdentity: NodeFileSystem.Stats
    OriginalContent: string
    GeneratedContent: string
    AppendContent: string option
    mutable OriginalHandle: NodeFileSystem.FileHandle option
    mutable InstalledIdentity: NodeFileSystem.Stats option
    mutable InstalledContent: string option
}

let private readHandleUtf8FromStart (handle: NodeFileSystem.FileHandle) =
    async {
        let chunkSize = 64 * 1024
        let decoder = createUtf8StringDecoder ()
        let content = System.Text.StringBuilder()
        let mutable position = 0.0
        let mutable finished = false

        while not finished do
            let buffer = bufferAlloc chunkSize
            let! readResult = handle.read(buffer, 0, chunkSize, position) |> Async.AwaitPromise

            if readResult.bytesRead = 0 then
                finished <- true
            else
                content.Append(
                    decodeUtf8Chunk
                        decoder
                        (bufferSubarray readResult.buffer 0 readResult.bytesRead)
                )
                |> ignore

                position <- position + float readResult.bytesRead

        content.Append(finishUtf8Decoding decoder) |> ignore
        return content.ToString()
    }

let private cleanupAttributesReplacement (replacement: AttributesReplacement) =
    async {
        let mutable cleanupError: exn option = None

        match replacement.OriginalHandle with
        | Some handle ->
            replacement.OriginalHandle <- None

            try
                do! handle.close () |> Async.AwaitPromise
            with error ->
                cleanupError <- Some error
        | None -> ()

        if
            not (
                NodeFileSystem.removeFileIfIdentityMatchesSync
                    replacement.TempPath
                    replacement.TempIdentity
            )
            && NodeFileSystem.existsSync replacement.TempPath
            && cleanupError.IsNone
        then
            cleanupError <-
                Some(
                    InvalidOperationException(
                        "The Git attributes transaction did not remove a temporary path it no longer owned."
                    )
                )

        match cleanupError with
        | Some error -> return raise error
        | None -> return ()
    }

let abortAttributesReplacement (replacement: AttributesReplacement) =
    cleanupAttributesReplacement replacement

let prepareAttributesReplacement
    (attributesPath: string)
    (originalIdentity: NodeFileSystem.Stats option)
    (originalContent: string)
    (content: string)
    =
    async {
        let appendContent =
            match originalIdentity with
            | Some _
                when content.StartsWith(originalContent, StringComparison.Ordinal)
                     && content.Length > originalContent.Length ->
                Some(content.Substring(originalContent.Length))
            | Some _ ->
                invalidOp "The generated Git attributes content did not preserve the validated original prefix."
            | None -> None

        let tempPath = attributesPath + $".vcs-{Guid.NewGuid():N}.tmp"
        let mutable tempIdentity = None

        try
            ensurePathHasNoLinks (NodePath.dirname attributesPath)
            let createdIdentity =
                NodeFileSystem.writeUtf8FileExclusiveAndFlushWithIdentitySync tempPath content

            tempIdentity <- Some createdIdentity

            match originalIdentity with
            | Some expected ->
                if not (NodeFileSystem.existsSync attributesPath) then
                    invalidOp "The Git attributes file disappeared before its policy could be replaced."

                ensurePathHasNoLinks attributesPath
                let! handle = NodeFileSystem.openReadNoFollowAsync attributesPath |> Async.AwaitPromise

                try
                    let! openedStats = handle.stat () |> Async.AwaitPromise
                    let! currentContent = readHandleUtf8FromStart handle
                    let afterStats = NodeFileSystem.lstatSync attributesPath

                    if
                        currentContent <> originalContent
                        || not (sameFileIdentity expected openedStats)
                        || not (sameFileIdentity expected afterStats)
                    then
                        invalidOp "The Git attributes file changed before its policy could be replaced."

                    return {
                        AttributesPath = attributesPath
                        TempPath = tempPath
                        TempIdentity = createdIdentity
                        OriginalContent = originalContent
                        GeneratedContent = content
                        AppendContent = appendContent
                        OriginalHandle = Some handle
                        InstalledIdentity = None
                        InstalledContent = None
                    }
                with error ->
                    do! handle.close () |> Async.AwaitPromise
                    return raise error
            | None when NodeFileSystem.existsSync attributesPath ->
                return invalidOp "The Git attributes file appeared before its policy could be created."
            | None ->
                return {
                    AttributesPath = attributesPath
                    TempPath = tempPath
                    TempIdentity = createdIdentity
                    OriginalContent = originalContent
                    GeneratedContent = content
                    AppendContent = appendContent
                    OriginalHandle = None
                    InstalledIdentity = None
                    InstalledContent = None
                }
        with error ->
            match tempIdentity with
            | Some createdIdentity ->
                NodeFileSystem.removeFileIfIdentityMatchesSync tempPath createdIdentity |> ignore
            | None -> ()

            return raise error
    }

let private validateOriginalAttributesPath
    (replacement: AttributesReplacement)
    (handle: NodeFileSystem.FileHandle)
    =
    async {
        if not (NodeFileSystem.existsSync replacement.AttributesPath) then
            invalidOp "The Git attributes file disappeared before its policy could be reconciled."

        ensurePathHasNoLinks replacement.AttributesPath
        let! openedStats = handle.stat () |> Async.AwaitPromise
        let! currentContent = readHandleUtf8FromStart handle
        let pathStats = NodeFileSystem.lstatSync replacement.AttributesPath

        if
            currentContent <> replacement.OriginalContent
            || not (sameFileIdentity openedStats pathStats)
        then
            invalidOp "The Git attributes file changed before its policy could be reconciled."

        return openedStats
    }

let applyAttributesReplacement (replacement: AttributesReplacement) =
    async {
        match replacement.OriginalHandle, replacement.AppendContent with
        | Some originalHandle, Some appendContent ->
            let! originalStats = validateOriginalAttributesPath replacement originalHandle
            let! appendHandle =
                NodeFileSystem.openAppendNoFollowAsync replacement.AttributesPath
                |> Async.AwaitPromise

            let mutable appendError: exn option = None

            try
                ensurePathHasNoLinks replacement.AttributesPath
                let! appendStats = appendHandle.stat () |> Async.AwaitPromise
                let pathStats = NodeFileSystem.lstatSync replacement.AttributesPath

                if
                    not (sameFileIdentity originalStats appendStats)
                    || not (sameFileIdentity originalStats pathStats)
                then
                    invalidOp "The Git attributes file changed before its policy could be appended."

                do! appendHandle.writeFile (box appendContent) |> Async.AwaitPromise
                do! appendHandle.sync () |> Async.AwaitPromise
            with error ->
                appendError <- Some error

            try
                do! appendHandle.close () |> Async.AwaitPromise
            with error ->
                if appendError.IsNone then
                    appendError <- Some error

            match appendError with
            | Some error -> return raise error
            | None ->
                if not (NodeFileSystem.existsSync replacement.AttributesPath) then
                    invalidOp "The Git attributes file disappeared while its policy was being appended."

                ensurePathHasNoLinks replacement.AttributesPath
                let! installedIdentity = originalHandle.stat () |> Async.AwaitPromise
                let! installedContent = readHandleUtf8FromStart originalHandle
                let pathStats = NodeFileSystem.lstatSync replacement.AttributesPath

                if
                    installedContent <> replacement.GeneratedContent
                    || not (sameFileIdentity installedIdentity pathStats)
                then
                    invalidOp
                        "The Git attributes file changed while its policy was being appended; concurrent bytes were preserved."

                replacement.InstalledIdentity <- Some installedIdentity
                replacement.InstalledContent <- Some installedContent
        | None, None ->
            ensurePathHasNoLinks (NodePath.dirname replacement.AttributesPath)

            if NodeFileSystem.existsSync replacement.AttributesPath then
                invalidOp "The Git attributes file appeared before its policy could be created."

            let mutable useExclusiveCreate = false

            try
                NodeFileSystem.linkSync replacement.TempPath replacement.AttributesPath
            with error when isHardLinkUnavailable error ->
                useExclusiveCreate <- true

            if useExclusiveCreate then
                let! createdHandle =
                    NodeFileSystem.openAppendExclusiveAsync replacement.AttributesPath
                    |> Async.AwaitPromise

                let mutable createError: exn option = None
                let mutable createdIdentity = None

                try
                    let! identity = createdHandle.stat () |> Async.AwaitPromise
                    createdIdentity <- Some identity
                    do! createdHandle.writeFile (box replacement.GeneratedContent) |> Async.AwaitPromise
                    do! createdHandle.sync () |> Async.AwaitPromise
                    let! createdContent = readHandleUtf8FromStart createdHandle

                    if createdContent <> replacement.GeneratedContent then
                        invalidOp
                            "The Git attributes file changed while its policy was being created; concurrent bytes were preserved."
                with error ->
                    createError <- Some error

                try
                    do! createdHandle.close () |> Async.AwaitPromise
                with error ->
                    if createError.IsNone then
                        createError <- Some error

                match createError, createdIdentity with
                | Some error, _ -> return raise error
                | None, Some identity ->
                    ensurePathHasNoLinks replacement.AttributesPath
                    let pathIdentity = NodeFileSystem.lstatSync replacement.AttributesPath

                    if not (sameFileIdentity identity pathIdentity) then
                        invalidOp
                            "The Git attributes file changed while its policy was being created; concurrent bytes were preserved."

                    replacement.InstalledIdentity <- Some identity
                    replacement.InstalledContent <- Some replacement.GeneratedContent
                | None, None -> invalidOp "The Git attributes file identity was unavailable after creation."
            else
                let installedContent, installedIdentity = readAttributesNoFollow replacement.AttributesPath

                match installedIdentity with
                | Some identity when installedContent = replacement.GeneratedContent ->
                    replacement.InstalledIdentity <- Some identity
                    replacement.InstalledContent <- Some installedContent
                | _ ->
                    invalidOp
                        "The Git attributes file changed while its policy was being created; concurrent bytes were preserved."
        | _ -> invalidOp "The Git attributes replacement transaction was invalid."
    }

let completeAttributesReplacement (replacement: AttributesReplacement) =
    async {
        let mutable completionError: exn option = None

        try
            match replacement.InstalledIdentity, replacement.InstalledContent with
            | Some expectedIdentity, Some expectedContent ->
                let currentContent, currentIdentity = readAttributesNoFollow replacement.AttributesPath

                if
                    currentContent <> expectedContent
                    || (currentIdentity |> Option.exists (sameFileIdentity expectedIdentity) |> not)
                then
                    invalidOp
                        "The Git attributes file changed after its policy was installed; concurrent bytes were preserved."
            | _ -> invalidOp "The Git attributes replacement transaction was not installed."
        with error ->
            completionError <- Some error

        try
            do! cleanupAttributesReplacement replacement
        with cleanupError ->
            if completionError.IsNone then
                completionError <- Some cleanupError

        match completionError with
        | Some error -> return raise error
        | None -> return ()
    }

let addLiteralTrackingRules (content: string) (relativePaths: string[]) =
    let lineEnding = if content.Contains("\r\n") then "\r\n" else "\n"
    let mutable updated = content
    let mutable added = false

    for relativePath in relativePaths |> Array.distinct do
        let rule = literalTrackingRule relativePath
        let rulePattern = $"^{Regex.Escape(rule)}(?:\r?$)"

        if not (Regex.IsMatch(updated, rulePattern, RegexOptions.Multiline)) then
            updated <-
                if String.IsNullOrEmpty updated then
                    rule + lineEnding
                elif updated.EndsWith("\n") then
                    updated + rule + lineEnding
                else
                    updated + lineEnding + rule + lineEnding

            added <- true

    updated, added

let private rewriteLiteralTrackingRule repoPath relativePath enabled =
    try
        let attributesPath = NodePath.resolve [| repoPath; ".gitattributes" |]
        let content, originalIdentity = readAttributesNoFollow attributesPath
        let canonicalRule = literalTrackingRule relativePath

        let updated =
            if enabled then
                addLiteralTrackingRules content [| relativePath |] |> fst
            else
                removeExactAttributeRule canonicalRule content

        if updated <> content then
            replaceAttributesAtomically attributesPath originalIdentity content updated

        Ok()
    with error ->
        Error $"Could not update the literal Git LFS path policy: {error.Message}"

/// Tracks one exact repository-relative path and anchors the generated rule at the repository root.
let trackLiteral (repoPath: string) (relativePath: string) : JS.Promise<Result<unit, string>> = promise {
    match! requireGitLfsForLiteralPolicy repoPath with
    | Error error -> return Error error
    | Ok() -> return rewriteLiteralTrackingRule repoPath relativePath true
}

/// Tracks a repository-relative path through git-lfs's literal filename mode.
/// Provider storage-policy calls use trackLiteral to add repository-root anchoring.
let track (repoPath: string) (relativePath: string) : JS.Promise<Result<unit, string>> =
    promise {
        let! result = createRequest repoPath Track (Some relativePath) None |> runSilently

        return
            match result with
            | Ok _ -> Ok()
            | Error exn -> Error exn.Message
    }

/// Removes the exact literal-filename rule emitted by `track`.
/// git-lfs has no corresponding literal-filename switch for `untrack`.
let untrackLiteral (repoPath: string) (relativePath: string) : JS.Promise<Result<unit, string>> = promise {
    match! requireGitLfsForLiteralPolicy repoPath with
    | Error error -> return Error error
    | Ok() -> return rewriteLiteralTrackingRule repoPath relativePath false
}

/// Runs `git lfs install` for a specific repository.
let install (repoPath: string) : JS.Promise<Result<unit, string>> = promise {
    let! result = createRequest repoPath Install None None |> runSilently

    return
        match result with
        | Ok _ -> Ok()
        | Error exn -> Error exn.Message
}

/// Runs system-level Git LFS install and updates the installation cache on success.
let installSystem () : JS.Promise<Result<unit, string>> = promise {
    try
        let! result = gitLfs.Install (Some DefaultTimeoutMs) ignore (fun () -> false)

        return
            if result.Success then
                cachedSystemInstalled <- true
                Ok()
            else
                Error(extractFailureMessage result)
    with ex ->
        return Error ex.Message
}

/// Probes whether `git lfs` is available on PATH. The positive result is cached for the process lifetime.
let isSystemInstalled () : JS.Promise<bool> = promise {
    if cachedSystemInstalled then
        return true
    else
        let! output = tryExecGitText None DefaultTimeoutMs [| "lfs"; "version" |]

        let isInstalled =
            output |> Option.exists (fun text -> not (String.IsNullOrWhiteSpace text))

        if isInstalled then
            cachedSystemInstalled <- true

        return isInstalled
}

/// Checks whether `.gitattributes` marks a path for Git LFS.
let isTrackedByAttributes (repoRoot: string) (relativePath: string) =
    gitLfs.IsTrackedByAttributes repoRoot relativePath

let private extractLsFilesFailureMessage (result: GitSpawnResult) =
    let stderrText =
        result.StderrText
        |> Option.ofObj
        |> Option.defaultValue String.Empty
        |> _.Trim()

    let stdoutText =
        result.StdoutText
        |> Option.ofObj
        |> Option.defaultValue String.Empty
        |> _.Trim()

    let message =
        if result.TimedOut then
            if not (String.IsNullOrWhiteSpace stderrText) then
                stderrText
            else
                "Git LFS metadata command timed out."
        elif not (String.IsNullOrWhiteSpace stderrText) then
            stderrText
        elif not (String.IsNullOrWhiteSpace stdoutText) then
            stdoutText
        else
            "Git LFS metadata command failed."

    redactToken message

let readLsFilesByRelativePath
    (repoRoot: string)
    : JS.Promise<Result<Dictionary<string, GitLfsLsFileInfo>, string>> =
    promise {
        let normalizedRepoRoot = PathHelpers.normalizePath repoRoot

        try
            let! commandResult =
                runGitCaptured {
                    WorkingDirectory = Some normalizedRepoRoot
                    Arguments = buildLsFilesJsonArgs ()
                    Environment = None
                    StandardInput = None
                    CancelCheck = None
                    TimeoutMs = Some lfsLsFilesTimeoutMs
                }

            if commandResult.ExitCode <> 0 || commandResult.TimedOut then
                return Error(extractLsFilesFailureMessage commandResult)
            else
                let stdoutText =
                    commandResult.StdoutText
                    |> Option.ofObj
                    |> Option.defaultValue String.Empty
                    |> _.Trim()

                if String.IsNullOrWhiteSpace stdoutText then
                    return Ok(Dictionary<string, GitLfsLsFileInfo>())
                else
                    try
                        return stdoutText |> parseLsFiles |> indexUsingRelativePath |> Ok
                    with parseError ->
                        let reason =
                            if String.IsNullOrWhiteSpace parseError.Message then
                                "Unknown decoding error."
                            else
                                parseError.Message

                        Browser.Dom.console.warn $"Git LFS ls-files parse warning: {reason}"
                        return Error reason
        with error ->
            let message =
                if String.IsNullOrWhiteSpace error.Message then
                    "Unknown Git LFS metadata error."
                else
                    error.Message

            return Error(redactToken message)
    }

/// Tries to read `git lfs ls-files -j` metadata keyed by repository-relative path.
/// Fail-open behavior: command or parse failures yield an empty dictionary.
let tryGetLsFilesByRelativePath (repoRoot: string) : JS.Promise<Dictionary<string, GitLfsLsFileInfo>> = promise {
    match! readLsFilesByRelativePath repoRoot with
    | Ok filesByRelativePath -> return filesByRelativePath
    | Error message ->
        Browser.Dom.console.warn $"Git LFS ls-files warning: {message}"
        return Dictionary<string, GitLfsLsFileInfo>()
}

let tryFindListingForPath (repoRoot: string) (relativePath: string) : JS.Promise<Result<GitLfsLsFileInfo, string>> = promise {
    try
        match! readLsFilesByRelativePath repoRoot with
        | Error message -> return Error $"Could not read Git LFS file metadata: {message}"
        | Ok filesByRelativePath ->
            return
                match tryFindLsFileInfoByRelativePath filesByRelativePath relativePath with
                | Some listing -> Ok listing
                | None -> Error "The file is not listed by Git LFS in the current checkout."
    with error ->
        return Error $"Could not read Git LFS file metadata: {error.Message}"
}

let storagePruneArgs = [|
    "lfs"
    "prune"
    "--verify-remote"
    "--verify-unreachable"
    "--when-unverified=halt"
|]

let storageDedupArgs = [| "lfs"; "dedup" |]

let buildFetchRefetchArgs (relativePath: string) = [|
    "lfs"
    "fetch"
    "--refetch"
    $"--include={relativePath}"
    "origin"
    "HEAD"
|]

let private buildSmudgePointerArgs (relativePath: string) = [|
    "-c"
    "lfs.fetchinclude="
    "-c"
    "lfs.fetchexclude="
    "lfs"
    "smudge"
    "--"
    relativePath
|]

let buildCheckoutArgs (relativePath: string) = [| "lfs"; "checkout"; "--"; relativePath |]

let private formatDiagnosticsSection (title: string) (content: string option) =
    match content |> Option.map _.Trim() with
    | Some value when not (String.IsNullOrWhiteSpace value) -> Some $"{title}:\n{redactDiagnosticText value}"
    | _ -> None

let private tryRunRawDiagnosticCommand
    (runSimpleGitRaw: (ISimpleGit -> JS.Promise<string>) -> ISimpleGit -> JS.Promise<Result<string, 'Failure>>)
    (getFailureMessage: 'Failure -> string option)
    (git: ISimpleGit)
    (args: string[])
    : JS.Promise<string option> =
    promise {
        let! result = runSimpleGitRaw (fun currentGit -> currentGit.raw args) git

        return
            match result with
            | Ok output when not (String.IsNullOrWhiteSpace output) -> Some output
            | Ok _ -> None
            | Error failure ->
                getFailureMessage failure
                |> Option.filter (fun message -> not (String.IsNullOrWhiteSpace message))
                |> Option.map (fun message -> $"Unavailable: {message}")
    }

/// Collects redacted Git LFS diagnostics after an LFS upload failure so push errors are actionable.
let collectPushDiagnostics
    (runSimpleGitRaw: (ISimpleGit -> JS.Promise<string>) -> ISimpleGit -> JS.Promise<Result<string, 'Failure>>)
    (getFailureMessage: 'Failure -> string option)
    (remoteName: string)
    (git: ISimpleGit)
    : JS.Promise<string option> =
    promise {
        let! pushRemoteUrl =
            tryRunRawDiagnosticCommand runSimpleGitRaw getFailureMessage git [|
                "remote"
                "get-url"
                "--push"
                remoteName
            |]

        let! lfsVersion = tryRunRawDiagnosticCommand runSimpleGitRaw getFailureMessage git [| "lfs"; "version" |]

        let! lfsEnv = tryRunRawDiagnosticCommand runSimpleGitRaw getFailureMessage git [| "lfs"; "env" |]

        let! lfsLogsLast = tryRunRawDiagnosticCommand runSimpleGitRaw getFailureMessage git [| "lfs"; "logs"; "last" |]

        return
            [
                formatDiagnosticsSection "Git Push Remote" pushRemoteUrl
                formatDiagnosticsSection "Git LFS Version" lfsVersion
                formatDiagnosticsSection "Git LFS Env" lfsEnv
                formatDiagnosticsSection "Git LFS Logs Last" lfsLogsLast
            ]
            |> List.choose id
            |> function
                | [] -> None
                | sections -> Some(String.concat "\n\n" sections)
    }

/// Appends optional LFS diagnostic sections to the original push failure message.
let appendPushDiagnostics (message: string) (diagnostics: string option) =
    match diagnostics |> Option.map _.Trim() with
    | Some value when not (String.IsNullOrWhiteSpace value) -> $"{message}\n\nLFS diagnostics:\n{value}"
    | _ -> message

let private resolvePushRefSpec
    (runStatus: ISimpleGit -> JS.Promise<Result<StatusResult, 'Failure>>)
    (remoteBranchName: string option)
    (git: ISimpleGit)
    : JS.Promise<string> =
    promise {
        match remoteBranchName with
        | Some branchName -> return branchName
        | None ->
            let! statusResult = runStatus git

            match statusResult with
            | Ok status ->
                let currentBranch =
                    status.current
                    |> Option.bind Option.ofObj
                    |> Option.map _.Trim()
                    |> Option.filter (fun branch -> not (String.IsNullOrWhiteSpace branch))
                    |> Option.defaultValue "HEAD"

                return currentBranch
            | Error _ -> return "HEAD"
    }

let private splitNonEmptyLines (text: string) =
    text.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map _.Trim()
    |> Array.filter (fun line -> not (String.IsNullOrWhiteSpace line))

let private shouldSkipLfsUploadFromPushDryRun (output: string) =
    let statusLines =
        output
        |> splitNonEmptyLines
        |> Array.filter (fun line ->
            not (line.StartsWith("To ", StringComparison.Ordinal))
            && not (line.Equals("Done", StringComparison.OrdinalIgnoreCase))
        )

    statusLines.Length = 0
    || statusLines
       |> Array.forall (fun line -> line.StartsWith("=", StringComparison.Ordinal))
    // Rejected dry runs still mean there is no successful outbound push to upload LFS objects for.
    || statusLines
       |> Array.exists (fun line -> line.StartsWith("!", StringComparison.Ordinal))

let private getKnownRemoteRefs
    (runSimpleGitRaw: (ISimpleGit -> JS.Promise<string>) -> ISimpleGit -> JS.Promise<Result<string, 'Failure>>)
    (remoteName: string)
    (git: ISimpleGit)
    : JS.Promise<Result<string[], 'Failure>> =
    promise {
        let! result =
            runSimpleGitRaw
                (fun currentGit ->
                    currentGit.raw [|
                        "for-each-ref"
                        "--format=%(refname)"
                        $"refs/remotes/{remoteName}"
                    |]
                )
                git

        return
            result
            |> Result.map (fun output ->
                output
                |> splitNonEmptyLines
                |> Array.filter (fun refName -> not (refName.EndsWith("/HEAD", StringComparison.Ordinal)))
            )
    }

let private getOutboundObjectIds
    (runSimpleGitRaw: (ISimpleGit -> JS.Promise<string>) -> ISimpleGit -> JS.Promise<Result<string, 'Failure>>)
    (refSpec: string)
    (remoteRefs: string[])
    (git: ISimpleGit)
    : JS.Promise<Result<string[], 'Failure>> =
    promise {
        let args = [|
            "rev-list"
            "--objects"
            refSpec
            yield! remoteRefs |> Array.map (fun remoteRef -> $"^{remoteRef}")
        |]

        let! result = runSimpleGitRaw (fun currentGit -> currentGit.raw args) git

        return
            result
            |> Result.map (fun output ->
                output
                |> splitNonEmptyLines
                |> Array.choose (fun line ->
                    let separatorIndex = line.IndexOf(' ')

                    if separatorIndex <= 0 then
                        None
                    else
                        Some(line.Substring(0, separatorIndex))
                )
                |> Array.distinct
            )
    }

let private parseLsRemoteTipIds (output: string) =
    output
    |> splitNonEmptyLines
    |> Array.choose (fun line ->
        let columns = line.Split('\t', 2, StringSplitOptions.None)
        if columns.Length = 2 then Some columns.[0] else None
    )
    |> Array.distinct

let private buildRevListStandardInput (refSpec: string) (remoteTipIds: string[]) =
    if remoteTipIds.Length = 0 then
        $"{refSpec}\n"
    else
        String.concat "\n" [|
            yield refSpec
            yield "--not"
            yield! remoteTipIds
            yield ""
        |]

let private tryParsePointerOid (content: string) =
    let lines =
        content.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map _.Trim()
        |> Array.filter (fun line -> not (String.IsNullOrWhiteSpace line))

    let tryGetOidLine () =
        lines
        |> Array.tryPick (fun line ->
            let prefix = "oid sha256:"

            if
                line.StartsWith(prefix, StringComparison.Ordinal)
                && line.Length = prefix.Length + 64
            then
                let oid = line.Substring(prefix.Length)

                let isLowerHex =
                    oid |> Seq.forall (fun c -> Char.IsDigit c || ('a' <= c && c <= 'f'))

                if isLowerHex then Some oid else None
            else
                None
        )

    let hasSizeLine =
        lines
        |> Array.exists (fun line ->
            let prefix = "size "

            line.StartsWith(prefix, StringComparison.Ordinal)
            && line.Length > prefix.Length
            && (line.Substring(prefix.Length) |> Seq.forall Char.IsDigit)
        )

    if
        lines.Length >= 3
        && lines.[0].Equals("version https://git-lfs.github.com/spec/v1", StringComparison.Ordinal)
        && hasSizeLine
    then
        tryGetOidLine ()
    else
        None

let private tryReadBatchPointerOids (stdoutBuffer: obj) =
    let totalLength = bufferLength stdoutBuffer

    let findNewlineIndex startIndex =
        let mutable currentIndex = startIndex
        let mutable foundIndex = -1

        while foundIndex < 0 && currentIndex < totalLength do
            if bufferByteAt stdoutBuffer currentIndex = 10 then
                foundIndex <- currentIndex
            else
                currentIndex <- currentIndex + 1

        foundIndex

    let pointerOids = ResizeArray<string>()
    let mutable index = 0
    let mutable malformed = false

    while not malformed && index < totalLength do
        let headerEnd = findNewlineIndex index

        if headerEnd < 0 then
            malformed <- true
        else
            let header = bufferSubarray stdoutBuffer index headerEnd |> bufferToUtf8String
            let parts = header.Split(' ', 3, StringSplitOptions.RemoveEmptyEntries)

            if parts.Length <> 3 || not (parts.[1].Equals("blob", StringComparison.Ordinal)) then
                malformed <- true
            else
                let success, contentByteLength = Int32.TryParse(parts.[2])
                let contentStart = headerEnd + 1
                let contentEnd = contentStart + contentByteLength

                if not success || contentByteLength < 0 || contentEnd > totalLength then
                    malformed <- true
                else
                    let content =
                        bufferSubarray stdoutBuffer contentStart contentEnd |> bufferToUtf8String

                    match tryParsePointerOid content with
                    | Some pointerOid -> pointerOids.Add pointerOid
                    | None -> ()

                    index <- contentEnd

                    if index < totalLength && bufferByteAt stdoutBuffer index = 10 then
                        index <- index + 1

    if malformed then
        None
    else
        Some(pointerOids.ToArray() |> Array.distinct)

let private extractSpawnFailureMessage (result: GitSpawnResult) =
    let stderrText =
        result.StderrText
        |> Option.ofObj
        |> Option.defaultValue String.Empty
        |> _.Trim()

    let stdoutText =
        result.StdoutText
        |> Option.ofObj
        |> Option.defaultValue String.Empty
        |> _.Trim()

    if result.TimedOut then
        if not (String.IsNullOrWhiteSpace stderrText) then
            stderrText
        else
            "Git command timed out."
    elif not (String.IsNullOrWhiteSpace stderrText) then
        stderrText
    elif not (String.IsNullOrWhiteSpace stdoutText) then
        stdoutText
    else
        "Git command failed."

let private spawnFailure (result: GitSpawnResult) = exn (extractSpawnFailureMessage result)

let private isHttpExtraHeaderConfigArg (configArg: string) =
    let trimmed = configArg.Trim()
    let equalsIndex = trimmed.IndexOf("=")

    let key =
        if equalsIndex >= 0 then
            trimmed.Substring(0, equalsIndex)
        else
            trimmed

    let normalizedKey = key.Trim().ToLowerInvariant()

    normalizedKey.Equals("http.extraheader", StringComparison.Ordinal)
    || (normalizedKey.StartsWith("http.", StringComparison.Ordinal)
        && normalizedKey.EndsWith(".extraheader", StringComparison.Ordinal))

let private lfsTransferConfigArgs (configArgs: string[]) =
    let filtered = ResizeArray<string>()
    let mutable index = 0

    while index < configArgs.Length do
        if
            configArgs.[index] = "-c"
            && index + 1 < configArgs.Length
            && isHttpExtraHeaderConfigArg configArgs.[index + 1]
        then
            index <- index + 2
        else
            filtered.Add configArgs.[index]
            index <- index + 1

    filtered.ToArray()

let runAuthenticatedTransferWith
    (runSpawnedGit: GitSpawnRequest -> JS.Promise<GitSpawnResult>)
    (commandAuth: GitCommandAuthentication)
    (repoPath: string)
    (arguments: string[])
    (cancelCheck: (unit -> bool) option)
    : JS.Promise<Result<unit, exn>> =
    promise {
        let! result =
            runSpawnedGit {
                WorkingDirectory = Some repoPath
                Arguments = [|
                    yield! lfsTransferConfigArgs commandAuth.ConfigArgs
                    yield! arguments
                |]
                Environment = Some commandAuth.Environment
                StandardInput = None
                CancelCheck = cancelCheck
                TimeoutMs = None
            }

        return
            if result.ExitCode = 0 && not result.TimedOut then
                Ok()
            else
                Error(exn (extractSpawnFailureMessage result))
    }

let private maintenanceProgressPattern =
    Regex(@"(?<progress>\d+(?:\.\d+)?)%\s+\((?<processed>\d+(?:\.\d+)?)/(?<total>\d+(?:\.\d+)?)(?:\s+bytes)?\)")

let private tryParseInvariantFloat (value: string) =
    match Double.TryParse value with
    | true, parsed -> Some parsed
    | false, _ -> None

let private createMaintenanceOutputObserver (progressCallback: (GitProgressDto -> unit) option) =
    let pending = System.Text.StringBuilder()

    let reportLine (line: string) =
        progressCallback
        |> Option.iter (fun report ->
            if not (String.IsNullOrWhiteSpace line) then
                let matched = maintenanceProgressPattern.Match line

                let progress, processed, total =
                    if matched.Success then
                        tryParseInvariantFloat matched.Groups["progress"].Value,
                        tryParseInvariantFloat matched.Groups["processed"].Value,
                        tryParseInvariantFloat matched.Groups["total"].Value
                    else
                        None, None, None

                report {
                    Method = Some "lfs"
                    Stage = Some "maintenance"
                    Progress = progress
                    Processed = processed
                    Total = total
                    Output = Some(Redaction.redact line)
                })

    let observe (chunk: string) =
        pending.Append chunk |> ignore
        let mutable text = pending.ToString()
        let mutable newline = text.IndexOf '\n'

        while newline >= 0 do
            reportLine (text.Substring(0, newline).TrimEnd '\r')
            text <- text.Substring(newline + 1)
            newline <- text.IndexOf '\n'

        pending.Clear() |> ignore
        pending.Append text |> ignore

    let flush () =
        if pending.Length > 0 then
            reportLine (pending.ToString().TrimEnd '\r')
            pending.Clear() |> ignore

    observe, flush

/// Runs a maintenance command with authentication and streams its real process progress.
/// `onStarted` fires only after the child process can observe cancellation.
let internal runAuthenticatedMaintenance
    (commandAuth: GitCommandAuthentication)
    (repoPath: string)
    (arguments: string[])
    (progressCallback: (GitProgressDto -> unit) option)
    (cancelCheck: unit -> bool)
    (onStarted: unit -> unit)
    : JS.Promise<Result<string, exn>> =
    promise {
        let observeOutput, flushOutput = createMaintenanceOutputObserver progressCallback
        let! result =
            runGitCapturedWithStartedAndOutput
                onStarted
                observeOutput
                {
                    WorkingDirectory = Some repoPath
                    Arguments = [|
                        yield! lfsTransferConfigArgs commandAuth.ConfigArgs
                        yield! arguments
                    |]
                    Environment = Some commandAuth.Environment
                    StandardInput = None
                    CancelCheck = Some cancelCheck
                    TimeoutMs = None
                }

        flushOutput ()

        return
            if result.ExitCode = 0 && not result.TimedOut then
                Ok result.StdoutText
            else
                Error(exn (extractSpawnFailureMessage result))
    }

let pullAll
    (repoPath: string)
    (commandAuth: GitCommandAuthentication)
    (cancelCheck: (unit -> bool) option)
    : JS.Promise<Result<unit, exn>> =
    runAuthenticatedTransferWith runGitDiscardingStdout commandAuth repoPath [| "lfs"; "pull" |] cancelCheck

let fetchRefetchForPath
    (repoPath: string)
    (commandAuth: GitCommandAuthentication)
    (relativePath: string)
    (cancelCheck: (unit -> bool) option)
    : JS.Promise<Result<unit, exn>> =
    runAuthenticatedTransferWith
        runGitDiscardingStdout
        commandAuth
        repoPath
        (buildFetchRefetchArgs relativePath)
        cancelCheck

let private buildPointerInput (listing: GitLfsLsFileInfo) =
    let sizeText = listing.size |> int64 |> string

    $"version {listing.version}\noid {listing.``oid_type``}:{listing.oid}\nsize {sizeText}\n"

/// Downloads the exact LFS object described by `listing` without path include filtering.
/// `git lfs smudge` reads the pointer OID from stdin and stores the object locally; stdout is discarded.
let downloadObjectFromListing
    (repoPath: string)
    (commandAuth: GitCommandAuthentication)
    (relativePath: string)
    (listing: GitLfsLsFileInfo)
    : JS.Promise<Result<unit, exn>> =
    promise {
        let! result =
            runGitDiscardingStdout {
                WorkingDirectory = Some repoPath
                Arguments = [|
                    yield! lfsTransferConfigArgs commandAuth.ConfigArgs
                    yield! buildSmudgePointerArgs relativePath
                |]
                Environment = Some commandAuth.Environment
                StandardInput = Some(buildPointerInput listing)
                CancelCheck = None
                TimeoutMs = None
            }

        return
            if result.ExitCode = 0 && not result.TimedOut then
                Ok()
            else
                Error(exn (extractSpawnFailureMessage result))
    }

let private isUnsupportedOptionFailure (result: GitSpawnResult) =
    let diagnosticText = $"{result.StdoutText}\n{result.StderrText}".ToLowerInvariant()

    result.ExitCode <> 0
    && (diagnosticText.Contains("unknown option")
        || diagnosticText.Contains("unknown flag")
        || diagnosticText.Contains("unrecognized option")
        || diagnosticText.Contains("invalid option"))

let private filterResolvableRemoteTipIds
    (runSpawnedGit: GitSpawnRequest -> JS.Promise<GitSpawnResult>)
    (repoPath: string)
    (remoteTipIds: string[])
    : JS.Promise<Result<string[], exn>> =
    promise {
        let distinctRemoteTipIds = remoteTipIds |> Array.distinct

        if distinctRemoteTipIds.Length = 0 then
            return Ok [||]
        else
            let! batchCheckResult =
                runSpawnedGit {
                    WorkingDirectory = Some repoPath
                    Arguments = [| "cat-file"; "--batch-check" |]
                    Environment = None
                    StandardInput = Some(String.concat "\n" [| yield! distinctRemoteTipIds; yield "" |])
                    CancelCheck = None
                    TimeoutMs = Some DefaultTimeoutMs
                }

            if batchCheckResult.ExitCode <> 0 then
                return Error(spawnFailure batchCheckResult)
            else
                let lines = batchCheckResult.StdoutText |> splitNonEmptyLines
                let statusByObjectId = System.Collections.Generic.Dictionary<string, bool>()
                let mutable malformed = false

                for line in lines do
                    if not malformed then
                        let parts = line.Split(' ', 3, StringSplitOptions.RemoveEmptyEntries)

                        if parts.Length < 2 then
                            malformed <- true
                        else
                            let isMissing = parts.[1].Equals("missing", StringComparison.Ordinal)
                            statusByObjectId.[parts.[0]] <- not isMissing

                if
                    malformed
                    || (distinctRemoteTipIds
                        |> Array.exists (fun objectId -> not (statusByObjectId.ContainsKey objectId)))
                then
                    return Error(exn "Failed to parse git cat-file --batch-check output while filtering remote tips.")
                else
                    return
                        distinctRemoteTipIds
                        |> Array.filter (fun objectId -> statusByObjectId.[objectId])
                        |> Ok
    }

let private getLfsObjectIdsFromObjectIds
    (runSimpleGitRaw: (ISimpleGit -> JS.Promise<string>) -> ISimpleGit -> JS.Promise<Result<string, 'Failure>>)
    (objectIds: string[])
    (git: ISimpleGit)
    : JS.Promise<Result<string[], 'Failure>> =
    promise {
        let lfsObjectIds = ResizeArray<string>()
        let mutable failure: 'Failure option = None

        for objectId in objectIds do
            if failure.IsNone then
                let! objectTypeResult =
                    runSimpleGitRaw (fun currentGit -> currentGit.raw [| "cat-file"; "-t"; objectId |]) git

                match objectTypeResult with
                | Error currentFailure -> failure <- Some currentFailure
                | Ok objectType when objectType.Trim().Equals("blob", StringComparison.Ordinal) ->
                    // These object IDs come from outbound history, not working tree paths, so query the git object store directly.
                    let! objectSizeResult =
                        runSimpleGitRaw (fun currentGit -> currentGit.raw [| "cat-file"; "-s"; objectId |]) git

                    match objectSizeResult with
                    | Error currentFailure -> failure <- Some currentFailure
                    | Ok objectSizeText ->
                        let success, objectSize = Int64.TryParse(objectSizeText.Trim())

                        if success && objectSize <= maxLfsPointerProbeBytes then
                            let! contentResult =
                                runSimpleGitRaw (fun currentGit -> currentGit.raw [| "cat-file"; "-p"; objectId |]) git

                            match contentResult with
                            | Error currentFailure -> failure <- Some currentFailure
                            | Ok content ->
                                match tryParsePointerOid content with
                                | Some lfsObjectId -> lfsObjectIds.Add lfsObjectId
                                | None -> ()
                | Ok _ -> ()

        match failure with
        | Some currentFailure -> return Error currentFailure
        | None -> return Ok(lfsObjectIds.ToArray() |> Array.distinct)
    }

let private tryGetOptimizedLfsObjectIds
    (runSpawnedGit: GitSpawnRequest -> JS.Promise<GitSpawnResult>)
    (repoPath: string)
    (refSpec: string)
    (remoteTipIds: string[])
    : JS.Promise<Result<string[] option, exn>> =
    promise {
        let! revListResult =
            runSpawnedGit {
                WorkingDirectory = Some repoPath
                Arguments = [|
                    "rev-list"
                    "--objects"
                    "--no-object-names"
                    "--stdin"
                    "--ignore-missing"
                    $"--filter=blob:limit={maxLfsPointerProbeBytes}"
                    "--filter=object:type=blob"
                    "--filter-provided-objects"
                |]
                Environment = None
                StandardInput = Some(buildRevListStandardInput refSpec remoteTipIds)
                CancelCheck = None
                TimeoutMs = Some DefaultTimeoutMs
            }

        if revListResult.ExitCode <> 0 then
            return
                if isUnsupportedOptionFailure revListResult then
                    Ok None
                else
                    Error(spawnFailure revListResult)
        else
            let candidateBlobIds =
                revListResult.StdoutText |> splitNonEmptyLines |> Array.distinct

            if candidateBlobIds.Length = 0 then
                return Ok(Some [||])
            else
                let! catFileResult =
                    runSpawnedGit {
                        WorkingDirectory = Some repoPath
                        Arguments = [| "cat-file"; "--batch"; "--buffer" |]
                        Environment = None
                        StandardInput = Some(String.concat "\n" [| yield! candidateBlobIds; yield "" |])
                        CancelCheck = None
                        TimeoutMs = Some DefaultTimeoutMs
                    }

                if catFileResult.ExitCode <> 0 then
                    return
                        if isUnsupportedOptionFailure catFileResult then
                            Ok None
                        else
                            Error(spawnFailure catFileResult)
                else
                    return
                        match tryReadBatchPointerOids catFileResult.StdoutBuffer with
                        | Some pointerOids -> Ok(Some pointerOids)
                        | None ->
                            Error(exn "Failed to parse git cat-file --batch output while planning Git LFS upload.")
    }

let private getOutboundObjectIdsFromRemoteTips
    (runSpawnedGit: GitSpawnRequest -> JS.Promise<GitSpawnResult>)
    (repoPath: string)
    (refSpec: string)
    (remoteTipIds: string[])
    : JS.Promise<Result<string[] option, exn>> =
    promise {
        let! revListResult =
            runSpawnedGit {
                WorkingDirectory = Some repoPath
                Arguments = [|
                    "rev-list"
                    "--objects"
                    "--no-object-names"
                    "--stdin"
                    "--ignore-missing"
                |]
                Environment = None
                StandardInput = Some(buildRevListStandardInput refSpec remoteTipIds)
                CancelCheck = None
                TimeoutMs = Some DefaultTimeoutMs
            }

        if revListResult.ExitCode = 0 then
            return Ok(Some(revListResult.StdoutText |> splitNonEmptyLines |> Array.distinct))
        else
            return
                if isUnsupportedOptionFailure revListResult then
                    Ok None
                else
                    Error(spawnFailure revListResult)
    }

/// Determines whether an upcoming git push references LFS pointer objects that must be uploaded explicitly.
/// Tests call this directly because the planning logic has several fallbacks for older git versions.
let planOutboundPush
    (runSimpleGitRaw: (ISimpleGit -> JS.Promise<string>) -> ISimpleGit -> JS.Promise<Result<string, 'Failure>>)
    (runSpawnedGit: GitSpawnRequest -> JS.Promise<GitSpawnResult>)
    (mapSpawnFailure: exn -> 'Failure)
    (runStatus: ISimpleGit -> JS.Promise<Result<StatusResult, 'Failure>>)
    (repoPath: string)
    (remoteName: string)
    (branchName: string option)
    (git: ISimpleGit)
    : JS.Promise<Result<OutboundPushPlan, 'Failure>> =
    promise {
        let! refSpec = resolvePushRefSpec runStatus branchName git

        let! dryRunResult =
            runSimpleGitRaw
                (fun currentGit ->
                    let dryRunGit = currentGit.env ("GIT_LFS_SKIP_PUSH", "1")

                    dryRunGit.raw [|
                        "push"
                        "--porcelain"
                        "--dry-run"
                        remoteName
                        refSpec
                    |]
                )
                git

        match dryRunResult with
        | Error failure -> return Error failure
        | Ok dryRunOutput when shouldSkipLfsUploadFromPushDryRun dryRunOutput ->
            return Ok OutboundPushPlan.SkipLfsUpload
        | Ok _ ->
            let! remoteTipIdsResult =
                runSimpleGitRaw (fun currentGit -> currentGit.raw [| "ls-remote"; "--refs"; remoteName |]) git

            match remoteTipIdsResult with
            | Error failure -> return Error failure
            | Ok remoteTipOutput ->
                let remoteTipIds = parseLsRemoteTipIds remoteTipOutput
                let! resolvableRemoteTipIdsResult = filterResolvableRemoteTipIds runSpawnedGit repoPath remoteTipIds

                match resolvableRemoteTipIdsResult with
                | Error failure -> return Error(mapSpawnFailure failure)
                | Ok remoteTipIds ->
                    let! optimizedLfsObjectIdsResult =
                        tryGetOptimizedLfsObjectIds runSpawnedGit repoPath refSpec remoteTipIds

                    match optimizedLfsObjectIdsResult with
                    | Error failure -> return Error(mapSpawnFailure failure)
                    | Ok(Some [||]) -> return Ok OutboundPushPlan.SkipLfsUpload
                    | Ok(Some lfsObjectIds) -> return Ok(OutboundPushPlan.UploadLfsObjects lfsObjectIds)
                    | Ok None ->
                        let! remoteTruthObjectIdsResult =
                            getOutboundObjectIdsFromRemoteTips runSpawnedGit repoPath refSpec remoteTipIds

                        match remoteTruthObjectIdsResult with
                        | Error failure -> return Error(mapSpawnFailure failure)
                        | Ok(Some [||]) -> return Ok OutboundPushPlan.SkipLfsUpload
                        | Ok(Some objectIds) ->
                            let! lfsObjectIdsResult = getLfsObjectIdsFromObjectIds runSimpleGitRaw objectIds git

                            return
                                lfsObjectIdsResult
                                |> Result.map (fun lfsObjectIds ->
                                    if lfsObjectIds.Length = 0 then
                                        OutboundPushPlan.SkipLfsUpload
                                    else
                                        OutboundPushPlan.UploadLfsObjects lfsObjectIds
                                )
                        | Ok None ->
                            let! remoteRefsResult = getKnownRemoteRefs runSimpleGitRaw remoteName git

                            match remoteRefsResult with
                            | Error failure -> return Error failure
                            | Ok remoteRefs ->
                                let! outboundObjectIdsResult =
                                    getOutboundObjectIds runSimpleGitRaw refSpec remoteRefs git

                                match outboundObjectIdsResult with
                                | Error failure -> return Error failure
                                | Ok [||] -> return Ok OutboundPushPlan.SkipLfsUpload
                                | Ok objectIds ->
                                    let! lfsObjectIdsResult = getLfsObjectIdsFromObjectIds runSimpleGitRaw objectIds git

                                    return
                                        lfsObjectIdsResult
                                        |> Result.map (fun lfsObjectIds ->
                                            if lfsObjectIds.Length = 0 then
                                                OutboundPushPlan.SkipLfsUpload
                                            else
                                                OutboundPushPlan.UploadLfsObjects lfsObjectIds
                                        )
    }

/// Uploads the exact LFS object ids needed for a push, falling back to refspec upload when the local git-lfs is older.
/// Authentication is supplied by GitAuthAdapter and is passed only for this spawned command.
let uploadObjects
    (runSpawnedGit: GitSpawnRequest -> JS.Promise<GitSpawnResult>)
    (commandAuth: GitCommandAuthentication)
    (cancelCheck: unit -> bool)
    (repoPath: string)
    (remoteName: string)
    (refSpec: string)
    (lfsObjectIds: string[])
    : JS.Promise<Result<unit, exn>> =
    promise {
        if lfsObjectIds.Length = 0 then
            return Ok()
        elif cancelCheck () then
            return Error(exn "Git LFS upload cancelled.")
        else
            let! exactUploadResult =
                runSpawnedGit {
                    WorkingDirectory = Some repoPath
                    Arguments = [|
                        yield! lfsTransferConfigArgs commandAuth.ConfigArgs
                        "lfs"
                        "push"
                        "--object-id"
                        remoteName
                        "--stdin"
                    |]
                    Environment = Some commandAuth.Environment
                    StandardInput = Some(String.concat "\n" [| yield! lfsObjectIds; yield "" |])
                    CancelCheck = Some cancelCheck
                    TimeoutMs = None
                }

            if exactUploadResult.ExitCode = 0 then
                return Ok()
            elif isUnsupportedOptionFailure exactUploadResult then
                if cancelCheck () then
                    return Error(exn "Git LFS upload cancelled.")
                else
                    let! fallbackResult =
                        runSpawnedGit {
                            WorkingDirectory = Some repoPath
                            Arguments = [|
                                yield! lfsTransferConfigArgs commandAuth.ConfigArgs
                                "lfs"
                                "push"
                                remoteName
                                refSpec
                            |]
                            Environment = Some commandAuth.Environment
                            StandardInput = None
                            CancelCheck = Some cancelCheck
                            TimeoutMs = None
                        }

                    return
                        if fallbackResult.ExitCode = 0 then
                            Ok()
                        else
                            Error(exn (extractSpawnFailureMessage fallbackResult))
            else
                return Error(exn (extractSpawnFailureMessage exactUploadResult))
    }
