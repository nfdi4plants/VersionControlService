/// Environment helpers for hosts that launch Git beside the provider library.
module VersionControlService.Git.GitExecution

open System
open Fable.Core
open Fable.Core.JsInterop

[<Emit("Object.assign({}, process.env)")>]
let private copyProcessEnvironment () : obj = jsNative

[<Emit("$0?.PATH || $0?.Path || $0?.path || ''")>]
let private environmentPath (_environment: obj) : string = jsNative

[<Emit("Object.assign({}, $0)")>]
let private copyEnvironment (_environment: obj) : obj = jsNative

[<Emit("Object.assign($0, { LC_ALL: 'C' })")>]
let forceEnglishDiagnostics (_environment: obj) : obj = jsNative

/// One sample of the selection, taken for a start of git or for the instance of a simple-git operation.
/// The executable and the environment come from the same sample, so they cannot disagree when the
/// selection changes between two samples.
type internal GitInvocation = {
    Executable: string
    /// True when the provider chose the executable. False when the name "git" is resolved through PATH.
    Explicit: bool
}

let private automaticDiscovery () : string option = None

let mutable private executableProvider: unit -> string option = automaticDiscovery

/// Installs the function that selects the git executable. The selection is shared by every factory
/// in this process, and the provider must not throw. simple-git captures its binary and environment when an
/// operation creates its instance, so for simple-git a changed answer applies from the next operation.
/// Process requests and the Git LFS adapter ask the provider at each start. The text diff supervisor asks
/// it each time it launches git. For all of them a changed answer applies from the next start. None or a
/// blank string selects automatic discovery, which resolves the name "git" through PATH. A selected path
/// has to be absolute, because a relative path would run relative to the working directory of each command.
let setGitExecutableProvider (provider: unit -> string option) : unit = executableProvider <- provider

/// Restores automatic discovery for every factory in this process and clears the cached probe of
/// the Git tool PATH.
let resetGitExecutableProvider () : unit =
    executableProvider <- automaticDiscovery
    GitCommandResolver.resetCache ()

/// Samples the provider once. A direct launch of git calls this at each start. A simple-git operation calls
/// it when it creates its instance, and the dependency check calls it once for all its commands. The result
/// goes to every function that needs the executable or the environment of that sample.
let internal resolveInvocation () : GitInvocation =
    let selected =
        executableProvider ()
        |> Option.filter (fun value -> not (String.IsNullOrWhiteSpace value))

    {
        Executable = defaultArg selected "git"
        Explicit = selected.IsSome
    }

/// The executable the next start of git runs. This is the path the provider selects, or "git"
/// for automatic discovery.
let internal gitExecutable () : string = (resolveInvocation ()).Executable

/// Turns a base environment into the child environment of one start. The base itself stays as it is.
/// The macOS and Linux fallback covers desktop launches that miss the shell PATH and goes in front of
/// the inherited entries. An explicit executable then puts its directory in front of all of them.
/// Git output is parsed in English, so LC_ALL=C is set last. Running it again on its own result changes nothing.
let internal completeEnvironment (invocation: GitInvocation) (environment: obj) : obj =
    let withFallback =
        copyEnvironment environment
        |> GitCommandResolver.ensureGitToolPath invocation.Executable

    let withExecutableDirectory =
        if invocation.Explicit then
            match VersionControlService.Runtime.Node.Path.dirname invocation.Executable with
            | "." -> withFallback
            | directory -> GitCommandResolver.prependExecutableDirectory directory withFallback
        else
            withFallback

    forceEnglishDiagnostics withExecutableDirectory

/// The child environment of one start. It begins as a fresh copy of process.env, which is never modified.
let internal resolvedEnvironmentFor (invocation: GitInvocation) : obj =
    copyProcessEnvironment () |> completeEnvironment invocation

/// The git executable one start would run, as an absolute path when it can be resolved. An explicit
/// selection is reported as selected. Automatic discovery resolves the name "git" against the PATH
/// of the child environment of that start, and gives None when the PATH has no git.
let internal probeExecutablePath (invocation: GitInvocation) : string option =
    if invocation.Explicit then
        Some invocation.Executable
    else
        GitCommandResolver.findGitInEnvironment (resolvedEnvironmentFor invocation)

/// Names why an explicit selection cannot start git, in words that follow the path. None means the
/// selection is automatic or names a regular file. Symbolic links to files count as files. Only the
/// dependency check reports a problem. Operations do not validate the selection, so the host has to
/// provide an absolute path.
let internal explicitSelectionProblem (invocation: GitInvocation) : string option =
    let path = invocation.Executable

    if not invocation.Explicit then
        None
    elif not (VersionControlService.Runtime.Node.Path.isAbsolute path) then
        Some "is not an absolute path"
    elif path.Contains "\u0000" then
        Some "contains a NUL character"
    else
        try
            if VersionControlService.Runtime.Node.FileSystem.statSync(path).isFile () then
                None
            else
                Some "is not a regular file"
        with error ->
            match error?code |> unbox<string> |> Option.ofObj with
            | Some "ENOENT"
            | Some "ENOTDIR" -> Some "does not exist"
            | _ -> Some "cannot be read"

/// Git output is parsed in English, so every child process receives LC_ALL=C.
/// The environment starts from a fresh copy of process.env. Git tool PATH resolution
/// covers macOS and Linux desktop launches that miss the shell PATH. process.env itself
/// is never modified. An explicitly selected executable puts its directory first in PATH.
let resolvedEnvironment () : obj =
    resolvedEnvironmentFor (resolveInvocation ())

/// The environment of one start as name and value pairs, suited to request records that carry
/// environment overrides. It starts from a fresh copy of process.env, which is never modified.
let internal environmentOverridesFor (invocation: GitInvocation) : (string * string)[] =
    let inherited = copyProcessEnvironment ()
    let resolved = completeEnvironment invocation inherited

    let pathOverrides =
        if not (String.Equals(environmentPath inherited, environmentPath resolved, StringComparison.Ordinal)) then
            [| "PATH", environmentPath resolved |]
        else
            [||]

    Array.append [| "LC_ALL", "C" |] pathOverrides

/// The same resolution as name and value pairs, suited to request records that carry
/// environment overrides. It resolves PATH for macOS and Linux desktop launches that miss
/// the shell PATH and never modifies process.env. Every returned child environment receives LC_ALL=C.
let environmentOverrides () : (string * string)[] =
    environmentOverridesFor (resolveInvocation ())
