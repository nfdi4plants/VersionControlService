/// Environment helpers for hosts that launch Git beside the provider library.
module VersionControlService.Git.GitExecution

open System
open Fable.Core
open Fable.Core.JsInterop

[<Emit("Object.assign({}, process.env)")>]
let private copyProcessEnvironment () : obj = jsNative

[<Emit("$0?.PATH || $0?.Path || $0?.path || ''")>]
let private environmentPath (_environment: obj) : string = jsNative

[<Emit("Object.assign($0, { LC_ALL: 'C', LANGUAGE: 'C' })")>]
let private forceEnglishDiagnostics (_environment: obj) : obj = jsNative

/// Git output is parsed in English, so every child process receives fixed diagnostic locales.
let resolvedEnvironment () : obj =
    copyProcessEnvironment ()
    |> GitCommandResolver.ensureGitToolPath
    |> forceEnglishDiagnostics

/// The same resolution as name and value pairs, suited to request records that carry
/// environment overrides.
let environmentOverrides () : (string * string)[] =
    let inherited = copyProcessEnvironment ()
    let resolved =
        GitCommandResolver.ensureGitToolPath inherited
        |> forceEnglishDiagnostics

    [|
        "LC_ALL", "C"
        "LANGUAGE", "C"

        if not (String.Equals(environmentPath inherited, environmentPath resolved, StringComparison.Ordinal)) then
            "PATH", environmentPath resolved
    |]
