module VersionControlService.Runtime.Node.Path

open Fable.Core

[<Import("join", "path")>]
let join ([<ParamSeq>] paths: string[]) : string = jsNative

[<Import("resolve", "path")>]
let resolve ([<ParamSeq>] paths: string[]) : string = jsNative

[<Import("sep", "path")>]
let private separator: string = jsNative

/// The stored form of a workspace root: absolute, without a trailing separator, and with
/// forward slashes on Windows, which is the form git reports for a repository root.
let normalizeWorkspaceRoot (path: string) =
    let resolved = resolve [| path |]

    if separator = "\\" then resolved.Replace('\\', '/') else resolved

[<Import("relative", "path")>]
let relative (fromPath: string) (toPath: string) : string = jsNative

[<Import("isAbsolute", "path")>]
let isAbsolute (path: string) : bool = jsNative

[<Import("dirname", "path")>]
let dirname (path: string) : string = jsNative

[<Import("basename", "path")>]
let basename (path: string) : string = jsNative

[<Import("extname", "path")>]
let extname (path: string) : string = jsNative
