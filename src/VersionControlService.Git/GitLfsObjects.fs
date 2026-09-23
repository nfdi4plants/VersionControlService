module internal VersionControlService.Git.GitLfsObjects

open System
open Fable.Core
open VersionControlService.Abstractions

module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodeInterop = VersionControlService.Runtime.Node.Interop

type internal LfsPointerInfo = {
    Oid: string
    SizeInBytes: float
}

/// Parses a validated Git LFS pointer and returns its content identity and size.
let internal tryParseLfsPointer (pointerText: string) =
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

let internal readWorktreePointer
    (repoPath: string)
    (relativePath: string)
    : Async<Result<LfsPointerInfo option, OperationFailure>> =
    async {
        let absolutePath = NodePath.resolve [| repoPath; relativePath |]

        try
            match NodeFileSystem.tryLstatSync absolutePath with
            | None -> return Ok None
            | Some pathStats when not (pathStats.isFile ()) || pathStats.size > 1024.0 -> return Ok None
            | Some pathStats ->
                let! handle = NodeFileSystem.openReadNoFollowAsync absolutePath |> Async.AwaitPromise

                try
                    let! handleStats = handle.stat () |> Async.AwaitPromise

                    if
                        not (handleStats.isFile ())
                        || handleStats.dev <> pathStats.dev
                        || handleStats.ino <> pathStats.ino
                    then
                        return
                            Error(
                                OperationFailure.createRedacted
                                    ProviderError
                                    "worktree_read_failed"
                                    "The worktree file changed while its storage policy was being checked."
                            )
                    elif handleStats.size > 1024.0 then
                        return Ok None
                    else
                        let buffer = NodeInterop.bufferAlloc 1024
                        let! readResult = handle.read(buffer, 0, 1024, 0.0) |> Async.AwaitPromise

                        let content =
                            readResult.buffer
                            |> fun bytes -> NodeInterop.bufferSubarray bytes 0 readResult.bytesRead
                            |> NodeInterop.bufferToUtf8String

                        return Ok(tryParseLfsPointer content)
                finally
                    NodeInterop.observePromise (handle.close ()) ignore ignore
        with _ ->
            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "worktree_read_failed"
                        "The worktree file could not be inspected before changing its storage policy."
                )
    }

let private runCommonDirectoryFallback
    (runGit: string[] -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>)
    (repoPath: string)
    : Async<Result<string, OperationFailure>> =
    async {
        let! result = runGit [| "rev-parse"; "--git-common-dir" |]

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 ->
            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "git_failure"
                        $"Resolving Git common directory failed: {output.StdErr}"
                )
        | Ok output ->
            let commonDirectory = output.StdOut.Trim()

            if String.IsNullOrWhiteSpace commonDirectory then
                return
                    Error(
                        OperationFailure.createRedacted
                            ProviderError
                            "invalid_git_output"
                            "Git returned an empty common directory."
                    )
            else
                let absoluteCommonDirectory =
                    if NodePath.isAbsolute commonDirectory then commonDirectory else NodePath.resolve [| repoPath; commonDirectory |]

                return Ok(NodePath.join [| absoluteCommonDirectory; "lfs"; "objects" |])
    }

/// Resolves Git LFS media storage, falling back to the repository's common Git directory.
let resolveLocalMediaDirectory
    (runGit: string[] -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>)
    (repoPath: string)
    : Async<Result<string, OperationFailure>> =
    async {
        let! lfsEnvironment = runGit [| "lfs"; "env" |]

        let localMediaDirectory =
            match lfsEnvironment with
            | Ok output when output.ExitCode = 0 ->
                output.StdOut.Split '\n'
                |> Array.tryPick (fun line ->
                    let trimmed = line.TrimEnd '\r'
                    let prefix = "LocalMediaDir="

                    if trimmed.StartsWith(prefix, StringComparison.Ordinal) then
                        Some(trimmed.Substring(prefix.Length).Trim())
                    else
                        None)
            | _ -> None

        match localMediaDirectory with
        | Some directory when not (String.IsNullOrWhiteSpace directory) ->
            if NodePath.isAbsolute directory then
                return Ok directory
            else
                let! gitDirectoryResult = runGit [| "rev-parse"; "--absolute-git-dir" |]

                match gitDirectoryResult with
                | Error failure -> return Error failure
                | Ok output when output.ExitCode <> 0 ->
                    return
                        Error(
                            OperationFailure.createRedacted
                                ProviderError
                                "git_failure"
                                $"Resolving the Git directory failed: {output.StdErr}"
                        )
                | Ok output ->
                    let gitDirectory = output.StdOut.Trim()

                    if String.IsNullOrWhiteSpace gitDirectory then
                        return
                            Error(
                                OperationFailure.createRedacted
                                    ProviderError
                                    "invalid_git_output"
                                    "Git returned an empty absolute Git directory."
                            )
                    else
                        let absoluteGitDirectory =
                            if NodePath.isAbsolute gitDirectory then
                                gitDirectory
                            else
                                NodePath.resolve [| repoPath; gitDirectory |]

                        return Ok(NodePath.resolve [| absoluteGitDirectory; directory |])
        | _ -> return! runCommonDirectoryFallback runGit repoPath
    }

/// Builds the content-addressed Git LFS object path for a SHA-256 identity.
let objectPath (mediaDirectory: string) (oid: string) =
    NodePath.join [| mediaDirectory; oid.Substring(0, 2); oid.Substring(2, 2); oid |]

/// Checks for a regular local LFS object file with the expected size.
let isObjectLocallyAvailable (mediaDirectory: string) (oid: string) (sizeBytes: float) : bool =
    try
        match NodeFileSystem.tryLstatSync (objectPath mediaDirectory oid) with
        | Some stats -> stats.isFile () && stats.size = sizeBytes
        | None -> false
    with _ ->
        false

/// Reads and verifies a locally stored Git LFS object within the requested size limit.
let tryReadLocalObject (mediaDirectory: string) (oid: string) (sizeBytes: float) (maximumBytes: float) : obj option =
    try
        if sizeBytes <= maximumBytes && isObjectLocallyAvailable mediaDirectory oid sizeBytes then
            let bytes, _ = NodeFileSystem.readBufferNoFollowSync (objectPath mediaDirectory oid)

            if NodeInterop.sha256Buffer bytes = oid then
                Some bytes
            else
                None
        else
            None
    with _ ->
        None

/// Escapes the glob characters git-lfs would otherwise expand in a path argument.
let internal escapeLfsPathspec (path: string) =
    path
    |> Seq.map (fun character ->
        if character = '*' || character = '?' || character = '[' || character = ']' || character = '{' || character = '}' then
            "\\" + string character
        else
            string character)
    |> String.concat ""

/// Root-anchored `git lfs checkout` pattern for one repository path. git-lfs reads its
/// arguments as gitignore-style patterns, so an unanchored name also matches in subfolders.
let internal lfsCheckoutPattern (path: string) = "/" + escapeLfsPathspec path
