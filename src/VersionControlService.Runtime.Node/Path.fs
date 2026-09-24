module VersionControlService.Runtime.Node.Path

open Fable.Core

[<Import("join", "path")>]
let join ([<ParamSeq>] paths: string[]) : string = jsNative

[<Import("resolve", "path")>]
let resolve ([<ParamSeq>] paths: string[]) : string = jsNative

[<Import("sep", "path")>]
let private separator: string = jsNative

type private RealpathSync =
    abstract member ``native``: path: string -> string

[<Import("realpathSync", "fs")>]
let private realpathSync: RealpathSync = jsNative

[<Import("existsSync", "fs")>]
let private existsSync (path: string) : bool = jsNative

let private isFilesystemRoot (path: string) =
    path = "/" || (path.Length = 3 && path.[1] = ':' && path.[2] = '/')

/// Returns the stored form of a workspace root. Existing paths use the file system's case.
/// Windows paths use forward slashes. A drive root and `/` keep their trailing slash.
let normalizeWorkspaceRoot (path: string) =
    let resolved = resolve [| path |]
    let canonical = if existsSync resolved then realpathSync.``native`` resolved else resolved
    let normalized = if separator = "\\" then canonical.Replace('\\', '/') else canonical

    if isFilesystemRoot normalized then normalized else normalized.TrimEnd('/')

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
