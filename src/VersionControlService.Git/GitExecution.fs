/// Environment helpers for hosts that launch Git beside the provider library.
module VersionControlService.Git.GitExecution

open System
open Fable.Core
open Fable.Core.JsInterop

[<Emit("Object.assign({}, process.env)")>]
let private copyProcessEnvironment () : obj = jsNative

[<Emit("$0?.PATH || $0?.Path || $0?.path || ''")>]
let private environmentPath (_environment: obj) : string = jsNative

[<Emit("Object.assign($0, { LC_ALL: 'C' })")>]
let forceEnglishDiagnostics (_environment: obj) : obj = jsNative

/// Git output is parsed in English, so every child process receives LC_ALL=C.
/// The environment starts from a fresh copy of process.env. Git tool PATH resolution
/// covers macOS and Linux desktop launches that miss the shell PATH. process.env itself
/// is never modified.
let resolvedEnvironment () : obj =
    copyProcessEnvironment ()
    |> GitCommandResolver.ensureGitToolPath
    |> forceEnglishDiagnostics

/// The same resolution as name and value pairs, suited to request records that carry
/// environment overrides. It starts from a fresh copy of process.env, resolves PATH
/// for macOS and Linux desktop launches that miss the shell PATH, and never modifies
/// process.env. Every returned child environment receives LC_ALL=C.
let environmentOverrides () : (string * string)[] =
    let inherited = copyProcessEnvironment ()
    let resolved =
        GitCommandResolver.ensureGitToolPath inherited
        |> forceEnglishDiagnostics

    let pathOverrides =
        if not (String.Equals(environmentPath inherited, environmentPath resolved, StringComparison.Ordinal)) then
            [| "PATH", environmentPath resolved |]
        else
            [||]

    Array.append [| "LC_ALL", "C" |] pathOverrides
