module VersionControlService.Runtime.Node.FileSystem

open System
open Fable.Core
open Fable.Core.JsInterop

[<JS.PojoAttribute>]
type MkdirOptions(?recursive: bool) =
    member val recursive: bool option = recursive with get, set

[<JS.PojoAttribute>]
type RmOptions(?recursive: bool, ?force: bool, ?maxRetries: int, ?retryDelay: int) =
    member val recursive: bool option = recursive with get, set
    member val force: bool option = force with get, set
    /// Retries on EBUSY, EPERM and friends. Windows keeps handles on files a killed
    /// child process just wrote, so removal right after a kill needs a few attempts.
    member val maxRetries: int option = maxRetries with get, set
    member val retryDelay: int option = retryDelay with get, set

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
    abstract member mtimeMs: float

type FileReadResult =
    abstract member bytesRead: int
    abstract member buffer: obj

type FileHandle =
    abstract member read: buffer: obj * offset: int * length: int * position: float -> JS.Promise<FileReadResult>
    abstract member writeFile: buffer: obj -> JS.Promise<unit>
    abstract member sync: unit -> JS.Promise<unit>
    abstract member stat: unit -> JS.Promise<Stats>
    abstract member close: unit -> JS.Promise<unit>

type HashedFile = {
    Sha256: string
    Stats: Stats
}

[<AllowNullLiteral>]
type private NodeError =
    abstract member message: string

[<AllowNullLiteral>]
type private ReadStream =
    abstract member on: eventName: string * listener: (obj -> unit) -> ReadStream

[<AllowNullLiteral>]
type private CryptoHash =
    abstract member update: data: obj -> CryptoHash
    abstract member digest: encoding: string -> string

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

[<Import("lstatSync", "fs")>]
let lstatSync (path: string) : Stats = jsNative

[<Import("utimesSync", "fs")>]
let utimesSync (path: string) (atime: DateTime) (mtime: DateTime) : unit = jsNative

let tryLstatSync (path: string) : Stats option =
    try
        Some(lstatSync path)
    with error ->
        let code: string = error?code |> unbox

        if code = "ENOENT" then
            None
        else
            raise error

[<Import("readFileSync", "fs")>]
let readFileSync (path: string) (encoding: TextEncoding) : string = jsNative

[<Import("writeFileSync", "fs")>]
let writeFileSync (path: string) (content: string) (encoding: TextEncoding) : unit = jsNative

[<Import("copyFileSync", "fs")>]
let copyFileSync (sourcePath: string) (destinationPath: string) : unit = jsNative

[<Import("renameSync", "fs")>]
let renameSync (oldPath: string) (newPath: string) : unit = jsNative

[<Import("linkSync", "fs")>]
let linkSync (existingPath: string) (newPath: string) : unit = jsNative

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

[<Import("writeFile", "fs/promises")>]
let writeFileBufferAsync (path: string) (buffer: obj) : JS.Promise<unit> = jsNative

[<Import("rename", "fs/promises")>]
let renameAsync (oldPath: string) (newPath: string) : JS.Promise<unit> = jsNative

[<Import("rm", "fs/promises")>]
let rmAsync (path: string) (options: RmOptions) : JS.Promise<unit> = jsNative

[<Import("stat", "fs/promises")>]
let statAsync (path: string) : JS.Promise<Stats> = jsNative

[<Import("createReadStream", "fs")>]
let private openHashReadStream (path: string) : ReadStream = jsNative

[<Import("createHash", "node:crypto")>]
let private createHash (algorithm: string) : CryptoHash = jsNative

let hashFileSha256Async (path: string) : Async<HashedFile> =
    async {
        let! stats = statAsync path |> Async.AwaitPromise

        return!
            Async.FromContinuations(fun (resolve, reject, _) ->
                let hash = createHash "sha256"
                let stream = openHashReadStream path

                stream.on("data", fun data -> hash.update data |> ignore) |> ignore
                stream.on("error", fun error -> reject (Exception((unbox<NodeError> error).message)))
                |> ignore

                stream.on(
                    "end",
                    fun _ ->
                        resolve {
                            Sha256 = hash.digest "hex"
                            Stats = stats
                        }
                )
                |> ignore)
    }

[<Import("lstat", "fs/promises")>]
let lstatAsync (path: string) : JS.Promise<Stats> = jsNative

let private fileSystemDynamic: obj = importAll "node:fs"
let private fileSystemPromisesDynamic: obj = importAll "node:fs/promises"
let private pathDynamic: obj = importAll "node:path"
let private cryptoDynamic: obj = importAll "node:crypto"
let private bufferDynamic: obj = importAll "node:buffer"

[<Emit("$0 == null")>]
let private isNullish (_value: obj) : bool = jsNative

let realpathSync (path: string) : string =
    let absolutePath: string = pathDynamic?resolve path |> unbox

    let rec resolveExisting current suffix =
        try
            let resolved: string = fileSystemDynamic?realpathSync current |> unbox

            suffix
            |> List.fold
                (fun current segment -> pathDynamic?join (current, segment) |> unbox<string>)
                resolved
        with error ->
            let code =
                try
                    error?code |> unbox<string>
                with _ ->
                    ""

            if code <> "ENOENT" then
                raise error

            let parent: string = pathDynamic?dirname current |> unbox

            if parent = current then
                raise error

            let name: string = pathDynamic?basename current |> unbox
            resolveExisting parent (name :: suffix)

    resolveExisting absolutePath []

let openReadOnlyNoFollowSync (path: string) : int * Stats =
    let noFollow: obj = fileSystemDynamic?constants?O_NOFOLLOW
    let readOnly: int = unbox fileSystemDynamic?constants?O_RDONLY
    let flags = if isNullish noFollow then readOnly else readOnly ||| unbox<int> noFollow
    let descriptor: int = fileSystemDynamic?openSync (path, flags) |> unbox

    try
        let stats: Stats = fileSystemDynamic?fstatSync descriptor |> unbox
        descriptor, stats
    with error ->
        fileSystemDynamic?closeSync descriptor |> ignore
        raise error

let closeFileDescriptorSync (descriptor: int) =
    fileSystemDynamic?closeSync descriptor |> ignore

let hashFileNoFollowSync (path: string) : HashedFile =
    let descriptor, stats = openReadOnlyNoFollowSync path

    try
        let chunk: obj = bufferDynamic?Buffer?allocUnsafe (1024 * 1024)
        let hash: obj = cryptoDynamic?createHash "sha256"
        let mutable reading = true

        while reading do
            let bytesRead: int =
                fileSystemDynamic?readSync (descriptor, chunk, 0, 1024 * 1024, null)
                |> unbox

            if bytesRead = 0 then
                reading <- false
            else
                hash?update (chunk?subarray (0, bytesRead)) |> ignore

        {
            Sha256 = hash?digest "hex" |> unbox
            Stats = stats
        }
    finally
        closeFileDescriptorSync descriptor

/// Reads UTF-8 through a descriptor opened with O_NOFOLLOW where supported and
/// returns the identity of the opened object for a caller-side lstat comparison.
let readUtf8FileNoFollowSync (path: string) : string * Stats =
    let descriptor, stats = openReadOnlyNoFollowSync path

    try
        let content: string = fileSystemDynamic?readFileSync (descriptor, "utf8") |> unbox
        content, stats
    finally
        closeFileDescriptorSync descriptor

/// Reads raw bytes through a descriptor opened with O_NOFOLLOW where supported.
let readBufferNoFollowSync (path: string) : obj * Stats =
    let descriptor, stats = openReadOnlyNoFollowSync path

    try
        let content: obj = fileSystemDynamic?readFileSync descriptor |> unbox
        content, stats
    finally
        closeFileDescriptorSync descriptor

let removeFileIfIdentityMatchesSync (path: string) (expected: Stats) : bool =
    try
        let current: Stats = fileSystemDynamic?lstatSync path |> unbox

        if current.isFile () && current.dev = expected.dev && current.ino = expected.ino then
            fileSystemDynamic?unlinkSync path |> ignore
            true
        else
            false
    with _ ->
        false

/// Creates a new UTF-8 file exclusively, flushes its bytes, closes it, and
/// returns the identity of the exact inode created by this operation.
/// O_EXCL prevents an existing link from being followed at the temporary path.
let writeUtf8FileExclusiveAndFlushWithIdentitySync (path: string) (content: string) : Stats =
    let flags: int =
        (unbox<int> fileSystemDynamic?constants?O_WRONLY)
        ||| (unbox<int> fileSystemDynamic?constants?O_CREAT)
        ||| (unbox<int> fileSystemDynamic?constants?O_EXCL)

    let descriptor: int = fileSystemDynamic?openSync (path, flags, 384) |> unbox
    let mutable identity: Stats option = None

    try
        let created: Stats = fileSystemDynamic?fstatSync descriptor |> unbox
        identity <- Some created

        try
            fileSystemDynamic?writeFileSync (descriptor, content, "utf8") |> ignore
            fileSystemDynamic?fsyncSync (descriptor) |> ignore
        finally
            fileSystemDynamic?closeSync (descriptor) |> ignore

        created
    with error ->
        match identity with
        | Some created -> removeFileIfIdentityMatchesSync path created |> ignore
        | None ->
            try
                fileSystemDynamic?closeSync (descriptor) |> ignore
            with _ ->
                ()

        raise error

let writeUtf8FileExclusiveAndFlushSync (path: string) (content: string) : unit =
    writeUtf8FileExclusiveAndFlushWithIdentitySync path content |> ignore

/// Copies a regular file into a new destination inode, never replacing an
/// existing path, and flushes the copied bytes before returning its identity.
let copyFileExclusiveAndFlushWithIdentitySync (sourcePath: string) (destinationPath: string) : Stats =
    let sourceDescriptor, sourceStats = openReadOnlyNoFollowSync sourcePath

    try
        if not (sourceStats.isFile ()) || sourceStats.isSymbolicLink () then
            invalidOp "The prepared materialization source is not a regular file."

        let flags: int =
            (unbox<int> fileSystemDynamic?constants?O_WRONLY)
            ||| (unbox<int> fileSystemDynamic?constants?O_CREAT)
            ||| (unbox<int> fileSystemDynamic?constants?O_EXCL)

        let destinationDescriptor: int =
            fileSystemDynamic?openSync (destinationPath, flags, 384) |> unbox

        let mutable destinationClosed = false
        let mutable createdIdentity: Stats option = None

        try
            let created: Stats = fileSystemDynamic?fstatSync destinationDescriptor |> unbox
            createdIdentity <- Some created
            let chunk: obj = bufferDynamic?Buffer?allocUnsafe (1024 * 1024)
            let mutable copying = true

            while copying do
                let bytesRead: int =
                    fileSystemDynamic?readSync (sourceDescriptor, chunk, 0, 1024 * 1024, null)
                    |> unbox

                if bytesRead = 0 then
                    copying <- false
                else
                    let mutable written = 0

                    while written < bytesRead do
                        let bytesWritten: int =
                            fileSystemDynamic?writeSync (
                                destinationDescriptor,
                                chunk,
                                written,
                                bytesRead - written,
                                null
                            )
                            |> unbox

                        if bytesWritten <= 0 then
                            invalidOp "Copying the prepared materialization object made no progress."

                        written <- written + bytesWritten

            fileSystemDynamic?fsyncSync destinationDescriptor |> ignore
            fileSystemDynamic?closeSync destinationDescriptor |> ignore
            destinationClosed <- true
            created
        with error ->
            if not destinationClosed then
                try
                    fileSystemDynamic?closeSync destinationDescriptor |> ignore
                with _ ->
                    ()

            createdIdentity
            |> Option.iter (fun created ->
                removeFileIfIdentityMatchesSync destinationPath created |> ignore)

            raise error
    finally
        closeFileDescriptorSync sourceDescriptor

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

/// Opens a write handle whose writes are forced to the end of the opened file,
/// without following a leaf symlink on platforms where O_NOFOLLOW is available.
let openAppendNoFollowAsync (path: string) : JS.Promise<FileHandle> =
    let noFollow: obj = fileSystemDynamic?constants?O_NOFOLLOW
    let writeOnly: int = unbox fileSystemDynamic?constants?O_WRONLY
    let append: int = unbox fileSystemDynamic?constants?O_APPEND

    let flags =
        if isNullish noFollow then
            writeOnly ||| append
        else
            writeOnly ||| append ||| unbox<int> noFollow

    fileSystemPromisesDynamic?``open`` (path, flags) |> unbox<JS.Promise<FileHandle>>

/// Creates a new readable file exclusively and forces every write to append.
/// The exclusive open never follows or replaces an appearing path.
let openAppendExclusiveAsync (path: string) : JS.Promise<FileHandle> =
    fileSystemPromisesDynamic?``open`` (path, "ax+", 384) |> unbox<JS.Promise<FileHandle>>

/// Opens a new file exclusively so an existing path or link cannot be followed.
let openWriteExclusiveAsync (path: string) : JS.Promise<FileHandle> =
    fileSystemPromisesDynamic?``open`` (path, "wx", 384) |> unbox<JS.Promise<FileHandle>>

/// Creates a readable byte stream. The raw Node object remains internal to the runtime package.
let internal createReadStream (path: string) : obj = fileSystemDynamic?createReadStream (path)

/// Creates an exclusive writable byte stream. The raw Node object remains internal to the runtime package.
let internal createWriteStreamExclusive (path: string) : obj =
    fileSystemDynamic?createWriteStream (path, createObj [ "flags" ==> "wx" ])

let internal removeFileIfExistsSync (path: string) =
    try
        fileSystemDynamic?rmSync (path, createObj [ "force" ==> true ]) |> ignore
    with _ ->
        ()

[<Import("readdir", "fs/promises")>]
let readdirAsync (path: string) : JS.Promise<string[]> = jsNative

[<Import("readdir", "fs/promises")>]
let readdirWithTypesAsync (path: string) (options: ReaddirOptions) : JS.Promise<Dirent[]> = jsNative
