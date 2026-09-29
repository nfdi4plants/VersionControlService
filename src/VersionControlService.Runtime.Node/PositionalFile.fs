module VersionControlService.Runtime.Node.PositionalFile

open System
open Fable.Core
open Fable.Core.JsInterop

type PositionalFileStats = {
    Size: int64
    MtimeNs: string
    Ino: string
    Dev: string
    IsFile: bool
    IsSymbolicLink: bool
    IsDirectory: bool
}

[<AllowNullLiteral>]
type private NodeError =
    abstract member code: string

[<AllowNullLiteral>]
type private BigIntStats =
    abstract member size: int64
    abstract member mtimeNs: int64
    abstract member ino: int64
    abstract member dev: int64
    abstract member isFile: unit -> bool
    abstract member isSymbolicLink: unit -> bool
    abstract member isDirectory: unit -> bool

type private FileSystem =
    abstract member ``open``:
        path: string * flags: string * callback: Action<NodeError, int> -> unit

    abstract member read:
        fd: int *
        buffer: byte[] *
        offset: int *
        length: int *
        position: int64 *
        callback: Action<NodeError, int, byte[]> -> unit

    abstract member write:
        fd: int *
        buffer: byte[] *
        offset: int *
        length: int *
        position: float *
        callback: Action<NodeError, int, byte[]> -> unit

    abstract member fstat:
        fd: int * options: obj * callback: Action<NodeError, BigIntStats> -> unit

    abstract member lstat:
        path: string * options: obj * callback: Action<NodeError, BigIntStats> -> unit

    abstract member close: fd: int * callback: Action<NodeError> -> unit
    abstract member mkdir: path: string * options: obj * callback: Action<NodeError> -> unit
    abstract member rm: path: string * options: obj * callback: Action<NodeError> -> unit

[<ImportAll("node:fs")>]
let private fileSystem: FileSystem = jsNative

[<Import("setTimeout", "node:timers")>]
let private setTimeout (callback: unit -> unit) (delayMs: int) : obj = jsNative

[<Emit("$0.toString(10)")>]
let private bigintToDecimalString (_value: int64) : string = jsNative

[<Emit("Number($0)")>]
let private int64ToNumber (_value: int64) : float = jsNative

let private maxSafeFilePosition = 9007199254740991L

let private rejectNodeError (reject: exn -> unit) (error: NodeError) =
    reject (unbox<exn> error)

let private openWithFlags path flags : JS.Promise<int> =
    JS.Constructors.Promise.Create(fun resolve reject ->
        try
            fileSystem.``open``(
                path,
                flags,
                Action<NodeError, int>(fun error fd ->
                    if isNull error then
                        resolve fd
                    else
                        rejectNodeError reject error)
            )
        with error ->
            reject error)

let openRead (path: string) : JS.Promise<int> =
    openWithFlags path "r"

let openCreateExclusive (path: string) : JS.Promise<int> =
    openWithFlags path "wx"

/// Opens a new file for both reading and writing, failing if it already exists.
let openCreateExclusiveReadWrite (path: string) : JS.Promise<int> =
    openWithFlags path "wx+"

let readAt
    (fd: int)
    (buffer: byte[])
    (offset: int)
    (length: int)
    (position: int64)
    : JS.Promise<int> =
    JS.Constructors.Promise.Create(fun resolve reject ->
        try
            fileSystem.read(
                fd,
                buffer,
                offset,
                length,
                position,
                Action<NodeError, int, byte[]>(fun error bytesRead _ ->
                    if isNull error then
                        resolve bytesRead
                    else
                        rejectNodeError reject error)
            )
        with error ->
            reject error)

let writeAt
    (fd: int)
    (buffer: byte[])
    (offset: int)
    (length: int)
    (position: int64)
    : JS.Promise<int> =
    JS.Constructors.Promise.Create(fun resolve reject ->
        if position < 0L || position > maxSafeFilePosition then
            reject (Exception("File position must be between zero and JavaScript's maximum safe integer."))
        else
            try
                fileSystem.write(
                    fd,
                    buffer,
                    offset,
                    length,
                    int64ToNumber position,
                    Action<NodeError, int, byte[]>(fun error bytesWritten _ ->
                        if isNull error then
                            resolve bytesWritten
                        else
                            rejectNodeError reject error)
                )
            with error ->
                reject error)

let private toFileStats (stats: BigIntStats) = {
    Size = stats.size
    MtimeNs = bigintToDecimalString stats.mtimeNs
    Ino = bigintToDecimalString stats.ino
    Dev = bigintToDecimalString stats.dev
    IsFile = stats.isFile ()
    IsSymbolicLink = stats.isSymbolicLink ()
    IsDirectory = stats.isDirectory ()
}

let fstat (fd: int) : JS.Promise<PositionalFileStats> =
    JS.Constructors.Promise.Create(fun resolve reject ->
        try
            fileSystem.fstat(
                fd,
                createObj [ "bigint" ==> true ],
                Action<NodeError, BigIntStats>(fun error stats ->
                    if isNull error then
                        resolve (toFileStats stats)
                    else
                        rejectNodeError reject error)
            )
        with error ->
            reject error)

let lstat (path: string) : JS.Promise<PositionalFileStats> =
    JS.Constructors.Promise.Create(fun resolve reject ->
        try
            fileSystem.lstat(
                path,
                createObj [ "bigint" ==> true ],
                Action<NodeError, BigIntStats>(fun error stats ->
                    if isNull error then
                        resolve (toFileStats stats)
                    else
                        rejectNodeError reject error)
            )
        with error ->
            reject error)

let close (fd: int) : JS.Promise<unit> =
    JS.Constructors.Promise.Create(fun resolve reject ->
        try
            fileSystem.close(
                fd,
                Action<NodeError>(fun error ->
                    if isNull error then
                        resolve ()
                    else
                        rejectNodeError reject error)
            )
        with error ->
            reject error)

let mkdirRecursive (path: string) : JS.Promise<unit> =
    JS.Constructors.Promise.Create(fun resolve reject ->
        try
            fileSystem.mkdir(
                path,
                createObj [ "recursive" ==> true ],
                Action<NodeError>(fun error ->
                    if isNull error then
                        resolve ()
                    else
                        rejectNodeError reject error)
            )
        with error ->
            reject error)

let removeRecursive (path: string) : JS.Promise<unit> =
    JS.Constructors.Promise.Create(fun resolve reject ->
        try
            fileSystem.rm(
                path,
                createObj [ "recursive" ==> true; "force" ==> true ],
                Action<NodeError>(fun error ->
                    if isNull error then
                        resolve ()
                    else
                        rejectNodeError reject error)
            )
        with error ->
            reject error)

let private isRetryableRemovalError (error: NodeError) =
    error.code = "EBUSY" || error.code = "EPERM" || error.code = "ENOTEMPTY"

let removeWithRetry (path: string) (attempts: int) (delayMs: int) : JS.Promise<unit> =
    JS.Constructors.Promise.Create(fun resolve reject ->
        let rec remove remaining =
            try
                fileSystem.rm(
                    path,
                    createObj [ "recursive" ==> true; "force" ==> true ],
                    Action<NodeError>(fun error ->
                        if isNull error then
                            resolve ()
                        elif remaining > 1 && isRetryableRemovalError error then
                            setTimeout (fun () -> remove (remaining - 1)) (max 0 delayMs)
                            |> ignore
                        else
                            rejectNodeError reject error)
                )
            with error ->
                reject error

        remove (max 1 attempts))
