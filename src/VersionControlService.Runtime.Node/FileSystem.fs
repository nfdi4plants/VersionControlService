module VersionControlService.Runtime.Node.FileSystem

open Fable.Core
open Fable.Core.JsInterop

[<JS.PojoAttribute>]
type MkdirOptions(?recursive: bool) =
    member val recursive: bool option = recursive with get, set

[<JS.PojoAttribute>]
type RmOptions(?recursive: bool, ?force: bool) =
    member val recursive: bool option = recursive with get, set
    member val force: bool option = force with get, set

[<JS.PojoAttribute>]
type ReaddirOptions(?withFileTypes: bool) =
    member val withFileTypes: bool option = withFileTypes with get, set

[<StringEnum(CaseRules.LowerFirst)>]
type TextEncoding = | Utf8

type Stats =
    abstract member isDirectory: unit -> bool
    abstract member isFile: unit -> bool
    abstract member isSymbolicLink: unit -> bool
    abstract member size: float
    abstract member dev: float
    abstract member ino: float

type FileReadResult =
    abstract member bytesRead: int
    abstract member buffer: obj

type FileHandle =
    abstract member read: buffer: obj * offset: int * length: int * position: float -> JS.Promise<FileReadResult>
    abstract member stat: unit -> JS.Promise<Stats>
    abstract member close: unit -> JS.Promise<unit>

type Dirent =
    abstract member name: string
    abstract member isDirectory: unit -> bool
    abstract member isFile: unit -> bool
    abstract member isSymbolicLink: unit -> bool

[<Import("mkdirSync", "fs")>]
let mkdirSync (path: string) (options: MkdirOptions) : unit = jsNative

[<Import("existsSync", "fs")>]
let existsSync (path: string) : bool = jsNative

[<Import("statSync", "fs")>]
let statSync (path: string) : Stats = jsNative

[<Import("readFileSync", "fs")>]
let readFileSync (path: string) (encoding: TextEncoding) : string = jsNative

[<Import("writeFileSync", "fs")>]
let writeFileSync (path: string) (content: string) (encoding: TextEncoding) : unit = jsNative

[<Import("renameSync", "fs")>]
let renameSync (oldPath: string) (newPath: string) : unit = jsNative

[<Import("unlinkSync", "fs")>]
let unlinkSync (path: string) : unit = jsNative

[<Import("readdirSync", "fs")>]
let readdirSync (path: string) : string array = jsNative

[<Import("mkdir", "fs/promises")>]
let mkdirAsync (path: string) (options: MkdirOptions) : JS.Promise<obj> = jsNative

[<Import("readFile", "fs/promises")>]
let readFileAsync (path: string) (encoding: TextEncoding) : JS.Promise<string> = jsNative

[<Import("readFile", "fs/promises")>]
let readFileBufferAsync (path: string) : JS.Promise<obj> = jsNative

[<Import("writeFile", "fs/promises")>]
let writeFileAsync (path: string) (content: string) (encoding: TextEncoding) : JS.Promise<unit> = jsNative

[<Import("rename", "fs/promises")>]
let renameAsync (oldPath: string) (newPath: string) : JS.Promise<unit> = jsNative

[<Import("rm", "fs/promises")>]
let rmAsync (path: string) (options: RmOptions) : JS.Promise<unit> = jsNative

[<Import("stat", "fs/promises")>]
let statAsync (path: string) : JS.Promise<Stats> = jsNative

[<Import("lstat", "fs/promises")>]
let lstatAsync (path: string) : JS.Promise<Stats> = jsNative

let private fileSystemDynamic: obj = importAll "node:fs"
let private fileSystemPromisesDynamic: obj = importAll "node:fs/promises"

[<Emit("$0 == null")>]
let private isNullish (_value: obj) : bool = jsNative

/// Opens a read handle without following a leaf symlink on platforms where
/// Node exposes O_NOFOLLOW. Windows does not expose that flag, so callers must
/// additionally compare handle/path identity after validating parent components.
let openReadNoFollowAsync (path: string) : JS.Promise<FileHandle> =
    let noFollow: obj = fileSystemDynamic?constants?O_NOFOLLOW

    let flags: obj =
        if isNullish noFollow then
            box "r"
        else
            let readOnly: int = unbox fileSystemDynamic?constants?O_RDONLY
            box (readOnly ||| unbox<int> noFollow)

    fileSystemPromisesDynamic?``open`` (path, flags) |> unbox<JS.Promise<FileHandle>>

[<Import("readdir", "fs/promises")>]
let readdirAsync (path: string) : JS.Promise<string[]> = jsNative

[<Import("readdir", "fs/promises")>]
let readdirWithTypesAsync (path: string) (options: ReaddirOptions) : JS.Promise<Dirent[]> = jsNative
