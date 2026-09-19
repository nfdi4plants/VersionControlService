/// Environment helpers for hosts that launch Git beside the provider library.
module VersionControlService.Git.GitExecution

open System
open Fable.Core
open Fable.Core.JsInterop

[<Emit("Object.assign({}, process.env)")>]
let private copyProcessEnvironment () : obj = jsNative

[<Emit("$0?.PATH || $0?.Path || $0?.path || ''")>]
let private environmentPath (_environment: obj) : string = jsNative

/// The environment child processes that run git should receive. It is process.env with
/// the PATH resolved the way the library resolves it for its own git and git-lfs runs
/// (macOS and Linux desktop launches often miss the shell PATH). The result is a fresh
/// object each call and process.env itself is never modified.
let resolvedEnvironment () : obj =
    copyProcessEnvironment ()
    |> GitCommandResolver.ensureGitToolPath

/// The same resolution as name and value pairs, empty when PATH needs no change. Suited
/// to request records that carry an environment override list.
let environmentOverrides () : (string * string)[] =
    let inherited = copyProcessEnvironment ()
    let resolved = GitCommandResolver.ensureGitToolPath inherited

    if String.Equals(environmentPath inherited, environmentPath resolved, StringComparison.Ordinal) then
        [||]
    else
        [| "PATH", environmentPath resolved |]
