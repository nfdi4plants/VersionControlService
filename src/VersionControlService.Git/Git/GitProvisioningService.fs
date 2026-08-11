module internal VersionControlService.Git.GitProvisioningService

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Git.GitEngineTypes
open VersionControlService.Runtime.Node.FileSystem
open VersionControlService.Runtime.Node.Path
open VersionControlService.Bindings.SimpleGit
open VersionControlService.Git.GitInternals

type ExistingPathKind =
    | Directory
    | File
    | Symlink
    | Other

let private tryGetNodeErrorCode (error: exn) : string option =
    try
        error?code |> unbox<string> |> Option.ofObj
    with _ ->
        None

let private createPathAccessError (pathValue: string) (error: exn) =
    let code =
        match tryGetNodeErrorCode error with
        | Some codeValue -> codeValue
        | None -> "UNKNOWN"

    exn $"Path '{pathValue}' could not be accessed ({code}): {error.Message}"

let private classifyStatsObject (stats: Stats) : ExistingPathKind =
    try
        if stats.isSymbolicLink () then ExistingPathKind.Symlink
        elif stats.isDirectory () then ExistingPathKind.Directory
        elif stats.isFile () then ExistingPathKind.File
        else ExistingPathKind.Other
    with _ ->
        ExistingPathKind.Other

let private tryGetExistingPathKindWithStats
    (getStatsAsync: string -> JS.Promise<Stats>)
    (pathValue: string)
    : JS.Promise<Result<ExistingPathKind option, exn>> =
    promise {
        try
            let! stats = getStatsAsync pathValue
            return Ok(Some(classifyStatsObject stats))
        with error ->
            match tryGetNodeErrorCode error with
            | Some "ENOENT" -> return Ok None
            | _ -> return Error(createPathAccessError pathValue error)
    }

let private tryGetExistingPathKind (pathValue: string) : JS.Promise<Result<ExistingPathKind option, exn>> = promise {
    let! statResult = tryGetExistingPathKindWithStats statAsync pathValue

    match statResult with
    | Ok None ->
        // `stat` follows symbolic links; for broken links this yields ENOENT even though the entry exists.
        // Probe with `lstat` to correctly detect a link at the path.
        let! lstatResult = tryGetExistingPathKindWithStats lstatAsync pathValue

        match lstatResult with
        | Ok(Some ExistingPathKind.Symlink) -> return Ok(Some ExistingPathKind.Symlink)
        | _ -> return lstatResult
    | _ -> return statResult
}

/// Validates a user-selected repository path and normalizes it with the supplied resolver.
/// Kept injectable so validation tests can avoid depending on OS path resolution.
let validateAndNormalizeTargetPathWithResolver
    (resolvePath: string -> string)
    (targetPath: string)
    : Result<string, exn> =
    let candidate =
        targetPath
        |> Option.ofObj
        |> Option.map _.Trim()
        |> Option.defaultValue String.Empty

    if String.IsNullOrWhiteSpace candidate then
        Error(exn "Target path must not be empty.")
    elif candidate.Contains("\u0000") then
        Error(exn "Target path contains invalid null characters.")
    else
        try
            Ok(resolvePath candidate)
        with error ->
            Error(exn $"Target path could not be normalized: {error.Message}")

let private createFailure kind message : GitService.GitFailure = { Kind = kind; Message = message }

let private toFailure (error: exn) : GitService.GitFailure =
    GitInternals.toFailure GitService.classifyFailureKind createFailure error

let private errorResult (error: exn) : GitService.GitResult<'T> =
    GitInternals.errorResult GitService.classifyFailureKind createFailure error

let private runSimpleGit
    (operation: ISimpleGit -> JS.Promise<'T>)
    (git: ISimpleGit)
    : JS.Promise<GitService.GitResult<'T>> =
    GitInternals.runSimpleGit toFailure operation git

let private resolveAbsolutePath (pathValue: string) = resolve [| pathValue |]

let private mkdirRecursiveAsync (directoryPath: string) : JS.Promise<unit> = promise {
    let! _ = mkdirAsync directoryPath (MkdirOptions(recursive = true))
    return ()
}

let private createExistingRepositoryFailure () : GitService.GitResult<'T> =
    Error {
        Kind = GitFailureKind.Unknown
        Message = "Target path is already a git repository."
    }

/// Initializes a Git repository at a user-selected path.
/// This is path-driven provisioning: it does not require application state and returns the normalized repository path.
let initRepository (targetPath: string) : JS.Promise<GitService.GitResult<string>> = promise {
    match validateAndNormalizeTargetPathWithResolver resolveAbsolutePath targetPath with
    | Error validationError -> return errorResult validationError
    | Ok normalizedTargetPath ->
        try
            let! preInitResult = promise {
                let! kindResult = tryGetExistingPathKind normalizedTargetPath

                match kindResult with
                | Error statError -> return errorResult statError
                | Ok(Some ExistingPathKind.Directory) ->
                    let existingGit =
                        createOptions normalizedTargetPath standardTimeout None |> createGit

                    let! existingRepoResult = runSimpleGit (fun git -> git.checkIsRepo ()) existingGit

                    match existingRepoResult with
                    | Error failure -> return Error failure
                    | Ok true -> return createExistingRepositoryFailure ()
                    | Ok false -> return Ok()
                | Ok(Some ExistingPathKind.Symlink) ->
                    return errorResult (exn "Target path exists and is a symbolic link/junction (not a directory).")
                | Ok(Some ExistingPathKind.File)
                | Ok(Some ExistingPathKind.Other) ->
                    return errorResult (exn "Target path exists and is not a directory.")
                | Ok None ->
                    do! mkdirRecursiveAsync normalizedTargetPath
                    return Ok()
            }

            match preInitResult with
            | Error failure -> return Error failure
            | Ok() ->
                let initGit = createOptions normalizedTargetPath standardTimeout None |> createGit

                let! initResult =
                    runSimpleGit
                        (fun git -> promise {
                            let! _ = git.init (U2.Case1 [| "--initial-branch=main" |])
                            return normalizedTargetPath
                        })
                        initGit

                return initResult
        with error ->
            return errorResult error
}
