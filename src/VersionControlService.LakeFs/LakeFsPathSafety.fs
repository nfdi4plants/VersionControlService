module VersionControlService.LakeFs.LakeFsPathSafety

open System
open System.Text
open Fable.Core
open VersionControlService.Abstractions

module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodePath = VersionControlService.Runtime.Node.Path

exception WorkspacePathFailure of OperationFailure

type InspectedFile = {
    AbsolutePath: string
    Sha256: string
    Size: float
    Identity: NodeFileSystem.Stats
}

let private normalizeNfc (value: string) = value.Normalize NormalizationForm.FormC

let private pathFailure code message path =
    {
        OperationFailure.create Validation code message with
            AffectedPaths = [| path |]
    }

let private normalizedRelative workspaceRoot candidatePath =
    NodePath.relative (NodePath.resolve [| workspaceRoot |]) (NodePath.resolve [| candidatePath |])

let private isContained workspaceRoot candidatePath =
    let relative = normalizedRelative workspaceRoot candidatePath

    relative = ""
    || (not (NodePath.isAbsolute relative)
        && relative <> ".."
        && not (relative.StartsWith("../", StringComparison.Ordinal))
        && not (relative.StartsWith("..\\", StringComparison.Ordinal)))

let rejectLinksInChain
    (workspaceRoot: string)
    (candidatePath: string)
    : Result<unit, OperationFailure> =
    let normalizedRoot = NodePath.resolve [| workspaceRoot |]
    let normalizedCandidate = NodePath.resolve [| candidatePath |]
    let affectedPath =
        normalizedRelative normalizedRoot normalizedCandidate
        |> fun value -> value.Replace('\\', '/')

    if not (isContained normalizedRoot normalizedCandidate) then
        Error(
            pathFailure
                "unsafe_repository_path"
                "The repository path escapes the managed workspace."
                affectedPath
        )
    else
        let segments =
            affectedPath.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries)

        let candidates =
            [|
                yield normalizedRoot

                let mutable current = normalizedRoot

                for segment in segments do
                    current <- NodePath.join [| current; segment |]
                    yield current
            |]

        let mutable failure: OperationFailure option = None

        for current in candidates do
            if failure.IsNone then
                try
                    match NodeFileSystem.tryLstatSync current with
                    | Some stats when stats.isSymbolicLink() ->
                        failure <-
                            Some(
                                pathFailure
                                    "symlink_not_supported"
                                    "Symbolic links, junctions, and reparse-point paths are not supported in lakeFS workspaces."
                                    affectedPath
                            )
                    | _ -> ()
                with error ->
                    failure <-
                        Some(
                            OperationFailure.createRedacted
                                ProviderError
                                "path_inspection_failed"
                                $"Inspecting a workspace path failed: {error.Message}"
                        )

        match failure with
        | Some value -> Error value
        | None -> Ok()

let resolveWorkspacePath
    (workspaceRoot: string)
    (path: RepositoryPath)
    : Result<string, OperationFailure> =
    let pathValue = RepositoryPath.value path
    let candidate = NodePath.resolve [| workspaceRoot; pathValue |]

    if not (isContained workspaceRoot candidate) then
        Error(
            pathFailure
                "unsafe_repository_path"
                "The repository path escapes the managed workspace."
                pathValue
        )
    else
        rejectLinksInChain workspaceRoot candidate |> Result.map (fun () -> candidate)

let resolveRemotePath
    (workspaceRoot: string)
    (prefix: string)
    (remoteKey: string)
    : Result<RepositoryPath option, OperationFailure> =
    let relative =
        if String.IsNullOrEmpty prefix then
            Some remoteKey
        elif remoteKey.StartsWith(prefix + "/", StringComparison.Ordinal) then
            Some(remoteKey.Substring(prefix.Length + 1))
        else
            None

    match relative with
    | None -> Ok None
    | Some value ->
        match RepositoryPath.tryCreate value with
        | Error message ->
            Error(pathFailure "unsafe_repository_path" message value)
        | Ok path ->
            resolveWorkspacePath workspaceRoot path
            |> Result.map (fun _ -> Some path)

let private windowsReservedBaseNames =
    set [
        "con"; "prn"; "aux"; "nul"
        "com1"; "com2"; "com3"; "com4"; "com5"; "com6"; "com7"; "com8"; "com9"
        "lpt1"; "lpt2"; "lpt3"; "lpt4"; "lpt5"; "lpt6"; "lpt7"; "lpt8"; "lpt9"
    ]

let private isWindowsInvalidName (path: string) =
    path.Split '/'
    |> Array.exists (fun segment ->
        let baseName = (segment.Split '.').[0].ToLowerInvariant()

        windowsReservedBaseNames.Contains baseName
        || segment.EndsWith "."
        || segment.EndsWith " "
        || segment |> Seq.exists (fun character -> int character < 32 || "<>:\"|?*".Contains(string character)))

let validateMaterializationPathsForPlatform
    (platform: string)
    (paths: RepositoryPath[])
    : Result<unit, OperationFailure> =
    let aliasKey (path: RepositoryPath) =
        let value = RepositoryPath.value path

        match platform with
        | "win32" -> value.ToLowerInvariant()
        | "darwin" -> normalizeNfc value |> _.ToLowerInvariant()
        | _ -> value

    let collisions =
        paths
        |> Array.groupBy aliasKey
        |> Array.filter (fun (_, group) -> group.Length > 1)
        |> Array.collect snd

    if collisions.Length > 0 then
        Error {
            OperationFailure.create
                Validation
                "path_collision"
                "Distinct repository paths alias to one local file on this filesystem." with
                AffectedPaths = collisions |> Array.map RepositoryPath.value
        }
    elif platform = "win32" then
        let invalid =
            paths
            |> Array.map RepositoryPath.value
            |> Array.filter isWindowsInvalidName

        if invalid.Length > 0 then
            Error {
                OperationFailure.create
                    Validation
                    "unrepresentable_path"
                    "A repository path cannot be represented on the local filesystem." with
                    AffectedPaths = invalid
            }
        else
            Ok()
    else
        Ok()

let validateMaterializationPaths paths =
    validateMaterializationPathsForPlatform (NodeInterop.processPlatform ()) paths

let private identityMatches (first: NodeFileSystem.Stats) (second: NodeFileSystem.Stats) =
    first.dev = second.dev && first.ino = second.ino

let private changedPathFailure path =
    pathFailure
        "symlink_not_supported"
        "The workspace path changed while it was being accessed. Refresh and retry."
        (RepositoryPath.value path)

let private changedFileFailure path =
    {
        OperationFailure.create
            Concurrency
            "workspace_path_changed"
            "The workspace file changed while the operation was preparing it. Refresh and retry." with
            Retryable = true
            AffectedPaths = [| RepositoryPath.value path |]
    }

let private attributeToRequestedPath
    (requestedPath: RepositoryPath)
    (failure: OperationFailure)
    : OperationFailure =
    if failure.Code = "symlink_not_supported" || failure.Code = "unsafe_repository_path" then
        { failure with AffectedPaths = [| RepositoryPath.value requestedPath |] }
    else
        failure

let inspectFile
    (workspaceRoot: string)
    (path: RepositoryPath)
    : Result<InspectedFile option, OperationFailure> =
    match resolveWorkspacePath workspaceRoot path with
    | Error failure -> Error(attributeToRequestedPath path failure)
    | Ok absolute ->
        try
            match NodeFileSystem.tryLstatSync absolute with
            | None -> Ok None
            | Some initialStats
                when initialStats.isSymbolicLink() || not (initialStats.isFile()) ->
                Error(changedPathFailure path)
            | Some initialStats ->
                let hashed = NodeFileSystem.hashFileNoFollowSync absolute

                match resolveWorkspacePath workspaceRoot path with
                | Error failure -> Error(attributeToRequestedPath path failure)
                | Ok verifiedAbsolute ->
                    let currentStats = NodeFileSystem.lstatSync verifiedAbsolute

                    if
                        not (hashed.Stats.isFile())
                        || currentStats.isSymbolicLink()
                        || not (currentStats.isFile())
                        || not (identityMatches initialStats hashed.Stats)
                        || not (identityMatches hashed.Stats currentStats)
                    then
                        Error(changedFileFailure path)
                    else
                        Ok(
                            Some {
                                AbsolutePath = verifiedAbsolute
                                Sha256 = hashed.Sha256
                                Size = hashed.Stats.size
                                Identity = hashed.Stats
                            }
                        )
        with error ->
            Error {
                OperationFailure.createRedacted
                    ProviderError
                    "workspace_inspection_failed"
                    $"Inspecting a workspace file failed: {error.Message}" with
                    AffectedPaths = [| RepositoryPath.value path |]
            }

let validateOpenedFile
    (workspaceRoot: string)
    (path: RepositoryPath)
    (expectedStats: NodeFileSystem.Stats)
    (openedStats: NodeFileSystem.Stats)
    : Result<unit, OperationFailure> =
    match resolveWorkspacePath workspaceRoot path with
    | Error failure -> Error(attributeToRequestedPath path failure)
    | Ok absolute ->
        try
            let currentStats = NodeFileSystem.lstatSync absolute

            if
                currentStats.isSymbolicLink()
                || not (currentStats.isFile())
                || not (expectedStats.isFile())
                || not (openedStats.isFile())
                || not (identityMatches expectedStats openedStats)
                || not (identityMatches openedStats currentStats)
            then
                Error(changedFileFailure path)
            else
                Ok()
        with error ->
            Error(
                OperationFailure.createRedacted
                    ProviderError
                    "path_inspection_failed"
                    $"Inspecting an opened workspace file failed: {error.Message}"
            )

let readUtf8File
    (workspaceRoot: string)
    (path: RepositoryPath)
    : Result<string option, OperationFailure> =
    match resolveWorkspacePath workspaceRoot path with
    | Error failure -> Error failure
    | Ok absolute when not (NodeFileSystem.existsSync absolute) -> Ok None
    | Ok absolute ->
        try
            let content, openedStats = NodeFileSystem.readUtf8FileNoFollowSync absolute

            match resolveWorkspacePath workspaceRoot path with
            | Error failure -> Error(attributeToRequestedPath path failure)
            | Ok verifiedAbsolute ->
                let currentStats = NodeFileSystem.lstatSync verifiedAbsolute

                if currentStats.isSymbolicLink() || not (identityMatches openedStats currentStats) then
                    Error(changedFileFailure path)
                else
                    Ok(Some content)
        with error ->
            Error(
                OperationFailure.createRedacted
                    ProviderError
                    "workspace_read_failed"
                    $"Reading a workspace file failed: {error.Message}"
            )

let readBuffer
    (workspaceRoot: string)
    (path: RepositoryPath)
    : Result<obj option, OperationFailure> =
    match resolveWorkspacePath workspaceRoot path with
    | Error failure -> Error failure
    | Ok absolute when not (NodeFileSystem.existsSync absolute) -> Ok None
    | Ok absolute ->
        try
            let content, openedStats = NodeFileSystem.readBufferNoFollowSync absolute

            match resolveWorkspacePath workspaceRoot path with
            | Error failure -> Error(attributeToRequestedPath path failure)
            | Ok verifiedAbsolute ->
                let currentStats = NodeFileSystem.lstatSync verifiedAbsolute

                if currentStats.isSymbolicLink() || not (identityMatches openedStats currentStats) then
                    Error(changedFileFailure path)
                else
                    Ok(Some content)
        with error ->
            Error(
                OperationFailure.createRedacted
                    ProviderError
                    "workspace_read_failed"
                    $"Reading a workspace file failed: {error.Message}"
            )

let private ensureParentDirectories (workspaceRoot: string) (path: RepositoryPath) =
    let pathValue = RepositoryPath.value path
    let segments = pathValue.Split '/'
    let parents = segments |> Array.take (max 0 (segments.Length - 1))
    let mutable relative = ""
    let mutable failure: OperationFailure option = None

    for segment in parents do
        if failure.IsNone then
            relative <- if relative = "" then segment else $"{relative}/{segment}"

            match RepositoryPath.tryCreate relative with
            | Error message ->
                failure <- Some(pathFailure "unsafe_repository_path" message pathValue)
            | Ok parentPath ->
                match resolveWorkspacePath workspaceRoot parentPath with
                | Error value -> failure <- Some(attributeToRequestedPath path value)
                | Ok absolute ->
                    if not (NodeFileSystem.existsSync absolute) then
                        try
                            NodeFileSystem.mkdirSync absolute (NodeFileSystem.MkdirOptions(recursive = false))
                        with error ->
                            failure <-
                                Some(
                                    OperationFailure.createRedacted
                                        ProviderError
                                        "workspace_directory_create_failed"
                                        $"Creating a workspace directory failed: {error.Message}"
                                )

                    if failure.IsNone then
                        match rejectLinksInChain workspaceRoot absolute with
                        | Error value -> failure <- Some(attributeToRequestedPath path value)
                        | Ok() -> ()

    match failure with
    | Some value -> Error value
    | None -> Ok()

let writeUtf8File
    (workspaceRoot: string)
    (path: RepositoryPath)
    (content: string)
    : Result<unit, OperationFailure> =
    match ensureParentDirectories workspaceRoot path with
    | Error failure -> Error failure
    | Ok() ->
        match resolveWorkspacePath workspaceRoot path with
        | Error failure -> Error failure
        | Ok absolute ->
            let pathValue = RepositoryPath.value path
            let nonce = Guid.NewGuid().ToString "N"
            let temporaryValue = $"{pathValue}.vcs-{nonce}.tmp"

            match RepositoryPath.tryCreate temporaryValue with
            | Error message -> Error(pathFailure "unsafe_repository_path" message pathValue)
            | Ok temporaryPath ->
                match resolveWorkspacePath workspaceRoot temporaryPath with
                | Error failure -> Error failure
                | Ok temporaryAbsolute ->
                    try
                        NodeFileSystem.writeUtf8FileExclusiveAndFlushSync temporaryAbsolute content

                        match resolveWorkspacePath workspaceRoot path with
                        | Error failure ->
                            NodeFileSystem.unlinkSync temporaryAbsolute
                            Error failure
                        | Ok verifiedAbsolute ->
                            NodeFileSystem.renameSync temporaryAbsolute verifiedAbsolute

                            match resolveWorkspacePath workspaceRoot path with
                            | Error failure -> Error failure
                            | Ok _ -> Ok()
                    with error ->
                        if NodeFileSystem.existsSync temporaryAbsolute then
                            try
                                NodeFileSystem.unlinkSync temporaryAbsolute
                            with _ ->
                                ()

                        Error(
                            OperationFailure.createRedacted
                                ProviderError
                                "workspace_write_failed"
                                $"Writing a workspace file failed: {error.Message}"
                        )

let removeFile
    (workspaceRoot: string)
    (path: RepositoryPath)
    : Result<unit, OperationFailure> =
    match resolveWorkspacePath workspaceRoot path with
    | Error failure -> Error failure
    | Ok absolute ->
        try
            if NodeFileSystem.existsSync absolute then
                let stats = NodeFileSystem.lstatSync absolute

                if stats.isSymbolicLink() then
                    Error(changedPathFailure path)
                else
                    NodeFileSystem.unlinkSync absolute
                    Ok()
            else
                Ok()
        with error ->
            Error(
                OperationFailure.createRedacted
                    ProviderError
                    "workspace_delete_failed"
                    $"Deleting a workspace file failed: {error.Message}"
            )

let replaceFileFromTemporary
    (workspaceRoot: string)
    (path: RepositoryPath)
    (temporaryPath: string)
    : Result<unit, OperationFailure> =
    match ensureParentDirectories workspaceRoot path with
    | Error failure -> Error failure
    | Ok() ->
        match resolveWorkspacePath workspaceRoot path with
        | Error failure -> Error failure
        | Ok targetPath ->
            try
                let temporaryStats = NodeFileSystem.lstatSync temporaryPath

                if temporaryStats.isSymbolicLink() || not (temporaryStats.isFile()) then
                    Error(changedPathFailure path)
                else
                    NodeFileSystem.renameSync temporaryPath targetPath

                    match resolveWorkspacePath workspaceRoot path with
                    | Error failure -> Error failure
                    | Ok _ -> Ok()
            with error ->
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "workspace_replace_failed"
                        $"Replacing a workspace file failed: {error.Message}"
                )

let walkFiles (workspaceRoot: string) : Result<RepositoryPath[], OperationFailure> =
    let files = ResizeArray<RepositoryPath>()
    let mutable failure: OperationFailure option = None

    let rec walk relative =
        if failure.IsNone then
            let currentResult =
                if relative = "" then
                    rejectLinksInChain workspaceRoot workspaceRoot
                    |> Result.map (fun () -> NodePath.resolve [| workspaceRoot |])
                else
                    RepositoryPath.tryCreate relative
                    |> Result.mapError (fun message -> pathFailure "unsafe_repository_path" message relative)
                    |> Result.bind (resolveWorkspacePath workspaceRoot)

            match currentResult with
            | Error value -> failure <- Some value
            | Ok current when not (NodeFileSystem.existsSync current) -> ()
            | Ok current ->
                try
                    for name in NodeFileSystem.readdirSync current do
                        if failure.IsNone then
                            let childRelative = if relative = "" then name else $"{relative}/{name}"

                            match RepositoryPath.tryCreate childRelative with
                            | Error message ->
                                failure <- Some(pathFailure "unsafe_repository_path" message childRelative)
                            | Ok childPath ->
                                match resolveWorkspacePath workspaceRoot childPath with
                                | Error value -> failure <- Some value
                                | Ok childAbsolute ->
                                    let stats = NodeFileSystem.lstatSync childAbsolute

                                    if stats.isSymbolicLink() then
                                        failure <- Some(changedPathFailure childPath)
                                    elif stats.isDirectory() then
                                        walk childRelative
                                    elif stats.isFile() then
                                        files.Add childPath
                with error ->
                    failure <-
                        Some(
                            OperationFailure.createRedacted
                                ProviderError
                                "workspace_walk_failed"
                                $"Walking workspace files failed: {error.Message}"
                        )

    walk ""

    match failure with
    | Some value -> Error value
    | None -> Ok(files.ToArray())

let orRaise result =
    match result with
    | Ok value -> value
    | Error failure -> raise (WorkspacePathFailure failure)

let guard (operation: Async<OperationResult<'T>>) : Async<OperationResult<'T>> =
    async {
        try
            return! operation
        with WorkspacePathFailure failure ->
            return Failed failure
    }
