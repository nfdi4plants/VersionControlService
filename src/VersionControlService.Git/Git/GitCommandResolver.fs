module internal VersionControlService.Git.GitCommandResolver

[<assembly: System.Runtime.CompilerServices.InternalsVisibleTo("VersionControlService.Tests")>]
do ()

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Runtime.Node.Interop

module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path

let private macGitToolDirectories = [|
    "/opt/homebrew/bin"
    "/usr/local/bin"
    "/opt/local/bin"
    "/usr/bin"
    "/bin"
|]

let private linuxGitToolDirectories = [|
    "/home/linuxbrew/.linuxbrew/bin"
    "/usr/local/bin"
    "/usr/bin"
    "/bin"
    "/snap/bin"
|]

let private supportsFallback (platform: string) =
    platform = "darwin" || platform = "linux"

let private fallbackDirectories (platform: string) =
    match platform with
    | "darwin" -> macGitToolDirectories
    | "linux" -> linuxGitToolDirectories
    | _ -> [||]

let private pathSeparator (platform: string) =
    if platform.StartsWith("win", StringComparison.OrdinalIgnoreCase) then
        ';'
    else
        ':'

let private distinctPathEntries (separator: char) (fallbackEntries: string[]) (currentPath: string) =
    let entries = ResizeArray<string>()
    let seen = HashSet<string>()

    let addEntry (entry: string) =
        let normalizedEntry =
            entry |> Option.ofObj |> Option.defaultValue String.Empty |> _.Trim()

        if not (String.IsNullOrWhiteSpace normalizedEntry) && seen.Add normalizedEntry then
            entries.Add normalizedEntry

    fallbackEntries |> Array.iter addEntry

    currentPath.Split([| separator |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.iter addEntry

    entries.ToArray() |> String.concat (string separator)

/// Returns the inherited PATH when the current environment can run `git lfs --version`.
/// On macOS/Linux desktop launches, the process may miss shell startup PATH entries; in that case
/// prepend common package-manager Git locations while preserving the original PATH tail.
let resolveGitToolPath (canResolveGitLfs: string -> bool) (platform: string) (currentPath: string) =
    let normalizedPlatform =
        platform
        |> Option.ofObj
        |> Option.defaultValue String.Empty
        |> _.Trim()
        |> _.ToLowerInvariant()

    let existingPath = currentPath |> Option.ofObj |> Option.defaultValue String.Empty

    if supportsFallback normalizedPlatform && not (canResolveGitLfs existingPath) then
        distinctPathEntries (pathSeparator normalizedPlatform) (fallbackDirectories normalizedPlatform) existingPath
    else
        existingPath

[<Emit("process.platform")>]
let private getProcessPlatform () : string = jsNative

[<Emit("$0?.PATH || $0?.Path || $0?.path || ''")>]
let private getEnvironmentPath (_environment: obj) : string = jsNative

[<Emit("{ ...$0, PATH: $1 }")>]
let private withEnvironmentPath (_environment: obj) (_pathValue: string) : obj = jsNative

type private FileConstants =
    abstract X_OK: int

[<Import("constants", "fs")>]
let private fileConstants: FileConstants = jsNative

[<Import("accessSync", "fs")>]
let private accessSync (path: string) (mode: int) : unit = jsNative

let private isRegularFile (path: string) =
    try
        NodeFileSystem.statSync(path).isFile ()
    with _ ->
        false

let private isExecutableFile (path: string) =
    isRegularFile path
    && (try
            accessSync path fileConstants.X_OK
            true
        with _ ->
            false)

/// Looks the name "git" up in `pathValue` the way Node's spawn does. On Windows each PATH entry is tried
/// with the extensions .com and .exe in that order. On other platforms the first entry with an executable
/// file named git wins. The result is an absolute path, or None when no entry has git. Empty PATH entries
/// are skipped, although POSIX reads them as the current directory. On Windows libuv also looks in the
/// working directory of the child before the PATH, which this lookup leaves out.
let findGitOnPath (platform: string) (pathValue: string) : string option =
    let isWindows = platform.StartsWith("win", StringComparison.OrdinalIgnoreCase)

    let fileNames =
        if isWindows then
            [| "git.com"; "git.exe" |]
        else
            [| "git" |]

    let isMatch = if isWindows then isRegularFile else isExecutableFile

    pathValue.Split([| pathSeparator platform |], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.collect (fun entry -> fileNames |> Seq.map (fun name -> NodePath.resolve [| entry; name |]))
    |> Seq.tryFind isMatch

/// `findGitOnPath` for the platform of this process and the PATH of a child environment.
let findGitInEnvironment (environment: obj) : string option =
    findGitOnPath (getProcessPlatform ()) (getEnvironmentPath environment)

let private canRunGitLfsVersionWithPath (executable: string) (baseEnvironment: obj) (pathValue: string) =
    try
        let environment = withEnvironmentPath baseEnvironment pathValue

        childProcessDynamic?execFileSync (
            executable,
            [| "lfs"; "--version" |],
            createObj [
                "encoding" ==> "utf8"
                "stdio" ==> "pipe"
                "shell" ==> false
                "env" ==> environment
            ]
        )
        |> ignore

        true
    with _ ->
        false

/// The probe result holds for one executable and one PATH, so the cache key has both. A resolved PATH
/// resolves to itself, so its own entry is stored too and a call that starts from the result of an
/// earlier call needs no new probe.
let private cachedGitToolPaths = Dictionary<string, string>()

let private cacheKey (executable: string) (pathValue: string) = executable + "\u0000" + pathValue

/// Forgets the cached probe results.
let resetCache () = cachedGitToolPaths.Clear()

/// Puts the directory of an explicitly chosen git executable first in the PATH of a copy of `environment`.
/// Helpers that ship beside git, such as git-lfs, then win over other installs on the PATH.
let prependExecutableDirectory (directory: string) (environment: obj) : obj =
    let currentPath = getEnvironmentPath environment

    withEnvironmentPath
        environment
        (distinctPathEntries (pathSeparator (getProcessPlatform ())) [| directory |] currentPath)

/// `ensureGitToolPath` with the platform and the probe of `git lfs --version` given by the caller.
/// The probe receives the executable, the environment and the PATH to try.
let internal ensureGitToolPathWith
    (canRunGitLfs: string -> obj -> string -> bool)
    (platform: string)
    (executable: string)
    (environment: obj)
    =
    let currentPath = getEnvironmentPath environment
    let key = cacheKey executable currentPath

    let resolvedPath =
        match cachedGitToolPaths.TryGetValue key with
        | true, path -> path
        | _ ->
            let path =
                resolveGitToolPath (canRunGitLfs executable environment) platform currentPath

            cachedGitToolPaths[key] <- path
            cachedGitToolPaths[cacheKey executable path] <- path
            path

    if String.Equals(currentPath, resolvedPath, StringComparison.Ordinal) then
        environment
    else
        withEnvironmentPath environment resolvedPath

/// Applies a resolved PATH to an environment object only when the inherited environment cannot
/// run `git lfs --version` with `executable` on macOS/Linux.
let ensureGitToolPath (executable: string) (environment: obj) =
    ensureGitToolPathWith canRunGitLfsVersionWithPath (getProcessPlatform ()) executable environment
