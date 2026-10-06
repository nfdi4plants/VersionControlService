module VersionControlService.Git.TextDiff.TextDiffSources

open System
open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.TextDiff
open VersionControlService.Git.TextDiff.TextDiffSourceResolver

module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodePath = VersionControlService.Runtime.Node.Path
module NodePositionalFile = VersionControlService.Runtime.Node.PositionalFile
module NodeProcess = VersionControlService.Runtime.Node.Process

[<Emit("$0?.code ?? ''")>]
let private errorCode (_error: obj) : string = jsNative

type TextDiffSourceFailure = {
    Code: string
    Message: string
}

type TextDiffSourceFailureCallback = TextDiffSourceFailure -> unit

[<Sealed>]
type TextDiffSourceException(code: string, message: string) =
    inherit Exception(message)

    member _.Code = code

let private changedFailure path = {
    Code = TextDiffFailureCodes.SourceChanged
    Message = $"The source changed while reading {path}."
}

let private readFailure message = {
    Code = TextDiffFailureCodes.ReadFailed
    Message = message
}

let private notify (callback: TextDiffSourceFailureCallback) (failure: TextDiffSourceFailure) =
    try
        callback failure
    with _ -> ()

let private raiseFailure (failure: TextDiffSourceFailure) =
    raise (TextDiffSourceException(failure.Code, failure.Message))

let private identityOfStats (stats: NodePositionalFile.PositionalFileStats) : FileIdentity = {
    Size = stats.Size
    MtimeNs = stats.MtimeNs
    Ino = stats.Ino
    Dev = stats.Dev
}

let private sameIdentity (expected: FileIdentity) (actual: FileIdentity) =
    expected.Size = actual.Size
    && expected.MtimeNs = actual.MtimeNs
    && expected.Ino = actual.Ino
    && expected.Dev = actual.Dev

let private completedChild (exit: NodeProcess.ChildExit) =
    exit.ExitCode = Some 0
    && exit.Signal.IsNone
    && exit.SpawnError.IsNone

let private childFailure (path: string) (exit: NodeProcess.ChildExit) (expected: int64) (actual: int64 option) =
    let detail =
        match exit.SpawnError, exit.Stderr, actual with
        | Some error, _, _ -> error
        | None, stderr, _ when not (String.IsNullOrWhiteSpace stderr) -> stderr.Trim()
        | None, _, Some size -> $"Git wrote {size} bytes, expected {expected}."
        | None, _, None -> "The Git blob process did not complete successfully."

    readFailure $"The Git blob for {path} could not be read. {detail}"

type private FileSourceCore(
    path: string,
    knownLength: int64,
    expectedPathIdentity: FileIdentity,
    descriptor: int,
    descriptorIdentity: FileIdentity,
    pointerPath: string option,
    pointerIdentity: FileIdentity option,
    callback: TextDiffSourceFailureCallback
) =
    let mutable disposed = false
    let mutable terminalFailure: TextDiffSourceFailure option = None
    let mutable closePromise: JS.Promise<unit> option = None

    let report failure =
        match terminalFailure with
        | Some existing -> existing
        | None ->
            terminalFailure <- Some failure
            notify callback failure
            failure

    let ensureOpen () =
        if disposed then
            raise (ObjectDisposedException("The text diff source has been disposed."))

    let validateIdentities () = async {
        ensureOpen ()

        match terminalFailure with
        | Some failure -> return raiseFailure failure
        | None ->
            try
                let! pathStats = NodePositionalFile.lstat path |> Async.AwaitPromise
                let! descriptorStats = NodePositionalFile.fstat descriptor |> Async.AwaitPromise
                let pathIdentity = identityOfStats pathStats
                let descriptorIdentityNow = identityOfStats descriptorStats

                let! pointerMatches =
                    async {
                        match pointerPath, pointerIdentity with
                        | Some path, Some expected ->
                            try
                                let! stats = NodePositionalFile.lstat path |> Async.AwaitPromise
                                return sameIdentity expected (identityOfStats stats)
                            with _ ->
                                return false
                        | _ -> return true
                    }

                if not (sameIdentity expectedPathIdentity pathIdentity)
                   || not (sameIdentity descriptorIdentity descriptorIdentityNow)
                   || not pointerMatches then
                    return raiseFailure (report (changedFailure path))
            with
            | :? TextDiffSourceException as error -> return raise error
            | error when errorCode (box error) = "ENOENT" || errorCode (box error) = "ENOTDIR" ->
                return raiseFailure (report (changedFailure path))
            | error ->
                return raiseFailure (report (readFailure (NodeInterop.errorMessage (box error))))
    }

    member _.Path = path

    member _.Failure = terminalFailure

    member _.IsComplete() = not disposed && terminalFailure.IsNone

    member _.CheckIdentity() = validateIdentities ()

    member this.ReadAt(position: int64) (buffer: byte[]) (offset: int) (count: int) = async {
        if position < 0L then invalidArg (nameof position) "The position cannot be negative."
        if isNull buffer then nullArg (nameof buffer)
        if offset < 0 || count < 0 || offset > buffer.Length - count then
            invalidArg (nameof offset) "The destination range is invalid."

        ensureOpen ()

        // The identity checks run once per request in the worker. A source that already failed keeps failing.
        match terminalFailure with
        | Some failure -> return raiseFailure failure
        | None ->
            if count = 0 then
                return ReadOutcome.Bytes 0
            elif position >= knownLength then
                return ReadOutcome.EndOfSource
            else
                let wanted = int (min (int64 count) (knownLength - position))

                try
                    let! amount = NodePositionalFile.readAt descriptor buffer offset wanted position |> Async.AwaitPromise

                    if amount > 0 then
                        return ReadOutcome.Bytes amount
                    else
                        // A file saved shorter in place reads zero bytes. Report that as a changed source.
                        do! validateIdentities ()
                        return raiseFailure (report (readFailure $"Reading {path} made no progress."))
                with
                | :? TextDiffSourceException as error -> return raise error
                | error ->
                    do! validateIdentities ()
                    return raiseFailure (report (readFailure (NodeInterop.errorMessage (box error))))
    }

    member this.Dispose() : JS.Promise<unit> =
        match closePromise with
        | Some pending -> pending
        | None ->
            disposed <- true

            let pending =
                async {
                    try
                        do! NodePositionalFile.close descriptor |> Async.AwaitPromise
                    with _ -> ()
                }
                |> Async.StartAsPromise

            closePromise <- Some pending
            pending

    static member Open(
        path: string,
        knownLength: int64,
        expectedPathIdentity: FileIdentity,
        pointerPath: string option,
        pointerIdentity: FileIdentity option,
        callback: TextDiffSourceFailureCallback
    ) : JS.Promise<FileSourceCore> =
        async {
            if knownLength < 0L then invalidArg (nameof knownLength) "The source length cannot be negative."

            let! descriptor = NodePositionalFile.openRead path |> Async.AwaitPromise
            let mutable transferred = false

            try
                let! descriptorStats = NodePositionalFile.fstat descriptor |> Async.AwaitPromise
                let! pathStats = NodePositionalFile.lstat path |> Async.AwaitPromise
                let descriptorIdentity = identityOfStats descriptorStats
                let pathIdentity = identityOfStats pathStats

                let! pointerMatches =
                    async {
                        match pointerPath, pointerIdentity with
                        | Some pointer, Some expected ->
                            try
                                let! stats = NodePositionalFile.lstat pointer |> Async.AwaitPromise
                                return sameIdentity expected (identityOfStats stats)
                            with _ ->
                                return false
                        | _ -> return true
                    }

                if not (sameIdentity expectedPathIdentity pathIdentity)
                   || not (sameIdentity expectedPathIdentity descriptorIdentity)
                   || not pointerMatches then
                    let failure = changedFailure path
                    notify callback failure
                    return raiseFailure failure
                else
                    let source =
                        FileSourceCore(
                            path,
                            knownLength,
                            expectedPathIdentity,
                            descriptor,
                            descriptorIdentity,
                            pointerPath,
                            pointerIdentity,
                            callback
                        )

                    transferred <- true
                    return source
            with error ->
                if not transferred then
                    try
                        do! NodePositionalFile.close descriptor |> Async.AwaitPromise
                    with _ -> ()

                return raise error
        }
        |> Async.StartAsPromise

    interface IByteSource with
        member _.IsComplete() = not disposed && terminalFailure.IsNone
        member this.ReadAt position buffer offset count = this.ReadAt position buffer offset count

/// Reads a local file positionally and checks that the file is unchanged. For an LFS object it also checks
/// that the pointer file is unchanged. ReadAt rethrows a failure that was already reported. Callers check the
/// file (and for an LFS object its pointer) with CheckIdentity before and after each request.
type FileSource private (core: FileSourceCore) =
    static member OpenWorkingFile(
        path: string,
        expectedIdentity: FileIdentity,
        ?onFailure: TextDiffSourceFailureCallback
    ) : JS.Promise<FileSource> =
        let callback = defaultArg onFailure (fun _ -> ())

        async {
            let! source =
                FileSourceCore.Open(
                    path,
                    expectedIdentity.Size,
                    expectedIdentity,
                    None,
                    None,
                    callback
                )
                |> Async.AwaitPromise

            return FileSource(source)
        }
        |> Async.StartAsPromise

    static member OpenLfsObject(
        objectPath: string,
        expectedLength: int64,
        objectIdentity: FileIdentity,
        pointerPath: string option,
        pointerIdentity: FileIdentity option,
        ?onFailure: TextDiffSourceFailureCallback
    ) : JS.Promise<FileSource> =
        let callback = defaultArg onFailure (fun _ -> ())

        async {
            let! source =
                FileSourceCore.Open(
                    objectPath,
                    expectedLength,
                    objectIdentity,
                    pointerPath,
                    pointerIdentity,
                    callback
                )
                |> Async.AwaitPromise

            return FileSource(source)
        }
        |> Async.StartAsPromise

    member _.Path = core.Path

    member _.IsComplete() = core.IsComplete()

    member this.ReadAt position buffer offset count = core.ReadAt position buffer offset count

    member _.Failure = core.Failure

    /// Raises the source_changed failure when the file (or the LFS pointer) differs from the opened state, and
    /// rethrows a failure that was already reported.
    member _.CheckIdentity() = core.CheckIdentity()

    member _.Dispose() = core.Dispose()

    interface IByteSource with
        member _.IsComplete() = core.IsComplete()
        member _.ReadAt position buffer offset count = core.ReadAt position buffer offset count

type SpoolSource private (
    path: string,
    expectedLength: int64,
    descriptor: int,
    initialLength: int64,
    retained: byte[] option,
    childClosed: JS.Promise<NodeProcess.ChildExit>,
    callback: TextDiffSourceFailureCallback,
    onWait: unit -> unit,
    readAt: int -> byte[] -> int -> int -> int64 -> JS.Promise<int>
) =
    let mutable disposed = false
    let mutable available = initialLength
    let mutable cached = 0L
    let mutable complete = false
    let mutable terminalFailure: TextDiffSourceFailure option = None
    let mutable closePromise: JS.Promise<unit> option = None
    let mutable childExit: NodeProcess.ChildExit option = None

    let report failure =
        match terminalFailure with
        | Some existing -> existing
        | None ->
            terminalFailure <- Some failure
            notify callback failure
            failure

    let ensureOpen () =
        if disposed then
            raise (ObjectDisposedException("The text diff spool has been disposed."))

    let notYetAvailable () =
        onWait ()
        ReadOutcome.NotYetAvailable

    let validateChild exit actualLength =
        if completedChild exit && actualLength = expectedLength then
            complete <- true
        else
            report (childFailure path exit expectedLength (Some actualLength)) |> ignore

    let fillRetained memory availableLength = async {
        let limit = min availableLength expectedLength
        let mutable stalled = false

        // Overlapping calls can read the same range, so cached only moves forward.
        while cached < limit && not stalled do
            let start = cached

            let! amount =
                readAt descriptor memory (int start) (int (min (limit - start) 262144L)) start
                |> Async.AwaitPromise

            if amount <= 0 then
                // The size already observed covers this offset, so an empty read means the file shrank underneath the spool.
                // Without a failure the read would answer NotYetAvailable for bytes that never arrive.
                stalled <- true

                report (
                    readFailure
                        $"The Git blob for {path} could not be read. The spool file returned no bytes at offset {start} although {availableLength} bytes were visible."
                )
                |> ignore
            else
                cached <- max cached (start + int64 amount)
    }

    let refresh () = async {
        ensureOpen ()

        match terminalFailure with
        | Some failure -> return raiseFailure failure
        | None ->
            try
                let exitBeforeStat = childExit
                let! stats = NodePositionalFile.fstat descriptor |> Async.AwaitPromise
                available <- stats.Size

                match retained with
                | Some memory -> do! fillRetained memory available
                | None -> ()

                match childExit with
                | Some exit when exitBeforeStat.IsNone ->
                    let! afterExit = NodePositionalFile.fstat descriptor |> Async.AwaitPromise
                    available <- afterExit.Size
                    match retained with
                    | Some memory -> do! fillRetained memory available
                    | None -> ()
                    validateChild exit available
                | Some exit -> validateChild exit available
                | None -> ()

                match terminalFailure with
                | Some failure -> return raiseFailure failure
                | None -> return available
            with
            | :? TextDiffSourceException as error -> return raise error
            | error ->
                return raiseFailure (report (readFailure (NodeInterop.errorMessage (box error))))
    }

    do
        NodeInterop.observePromise
            childClosed
            (fun exit ->
                childExit <- Some exit
                NodeInterop.observePromise (refresh () |> Async.StartAsPromise) ignore ignore)
            (fun error ->
                report (readFailure $"The Git blob process for {path} failed. {NodeInterop.errorMessage error}")
                |> ignore)

    member _.Path = path

    member _.Failure = terminalFailure

    member _.IsComplete() = not disposed && complete && terminalFailure.IsNone

    /// The bytes the spool still has to receive. It is 0 once the spool is complete or failed. The size seen at the
    /// last read can be stale, which overstates the rest and so errs on the safe side for a space check.
    member _.RemainingBytes: float =
        if complete || terminalFailure.IsSome then 0.0 else float (max 0L (expectedLength - available))

    member _.Dispose() : JS.Promise<unit> =
        match closePromise with
        | Some pending -> pending
        | None ->
            disposed <- true

            let pending =
                async {
                    try
                        do! NodePositionalFile.close descriptor |> Async.AwaitPromise
                    with _ -> ()
                }
                |> Async.StartAsPromise

            closePromise <- Some pending
            pending

    static member Open(
        path: string,
        expectedLength: int64,
        retained: byte[] option,
        childClosed: JS.Promise<NodeProcess.ChildExit>,
        ?onFailure: TextDiffSourceFailureCallback,
        ?onWait: unit -> unit,
        ?readAt: (int -> byte[] -> int -> int -> int64 -> JS.Promise<int>)
    ) : JS.Promise<SpoolSource> =
        let callback = defaultArg onFailure (fun _ -> ())
        let waitCallback = defaultArg onWait ignore
        let readAt =
            defaultArg
                readAt
                (fun file buffer offset count position -> NodePositionalFile.readAt file buffer offset count position)

        async {
            if expectedLength < 0L then invalidArg (nameof expectedLength) "The source length cannot be negative."

            let! descriptor = NodePositionalFile.openRead path |> Async.AwaitPromise

            try
                let! stats = NodePositionalFile.fstat descriptor |> Async.AwaitPromise
                return SpoolSource(path, expectedLength, descriptor, stats.Size, retained, childClosed, callback, waitCallback, readAt)
            with error ->
                try
                    do! NodePositionalFile.close descriptor |> Async.AwaitPromise
                with _ -> ()

                return raise error
        }
        |> Async.StartAsPromise

    member this.ReadAt(position: int64) (buffer: byte[]) (offset: int) (count: int) = async {
        if position < 0L then invalidArg (nameof position) "The position cannot be negative."
        if isNull buffer then nullArg (nameof buffer)
        if offset < 0 || count < 0 || offset > buffer.Length - count then
            invalidArg (nameof offset) "The destination range is invalid."

        let! length = refresh ()

        if count = 0 then
            if complete then return ReadOutcome.Bytes 0
            else return notYetAvailable ()
        elif position >= expectedLength then
            if complete && (retained.IsNone || cached >= expectedLength) then return ReadOutcome.EndOfSource
            else return notYetAvailable ()
        elif retained.IsSome then
            let memory = retained.Value

            if position >= cached then
                if complete && cached >= expectedLength then return ReadOutcome.EndOfSource
                else return notYetAvailable ()
            else
                let amount = int (min (int64 count) (cached - position))
                Array.blit memory (int position) buffer offset amount
                return ReadOutcome.Bytes amount
        else
            let readable = min (int64 count) (min (length - position) (expectedLength - position))

            if readable <= 0L then
                if complete then return ReadOutcome.EndOfSource
                else return notYetAvailable ()
            else
                let! result =
                    async {
                        try
                            let! amount =
                                readAt descriptor buffer offset (int readable) position |> Async.AwaitPromise

                            return Choice1Of2 amount
                        with error ->
                            return Choice2Of2 error
                    }

                let! after =
                    async {
                        try
                            let! _ = refresh ()
                            return Choice1Of2 ()
                        with error ->
                            return Choice2Of2 error
                    }

                match after with
                | Choice2Of2 error -> return raise error
                | Choice1Of2 () ->
                    match result with
                    | Choice2Of2 error ->
                        return raiseFailure (report (readFailure (NodeInterop.errorMessage (box error))))
                    | Choice1Of2 amount when amount > 0 -> return ReadOutcome.Bytes amount
                    | Choice1Of2 _ ->
                        if complete then return ReadOutcome.EndOfSource
                        else return notYetAvailable ()
    }

    interface IByteSource with
        member _.IsComplete() = not disposed && complete && terminalFailure.IsNone
        member this.ReadAt position buffer offset count = this.ReadAt position buffer offset count

/// A Git blob that the worker read whole into memory. It is complete from the start and has no file to check.
type MemoryBlobSource(bytes: byte[]) =
    do
        if isNull bytes then nullArg (nameof bytes)

    interface IByteSource with
        member _.IsComplete() = true

        member _.ReadAt position buffer offset count = async {
            if position < 0L then invalidArg (nameof position) "The position cannot be negative."
            if isNull buffer then nullArg (nameof buffer)

            if offset < 0 || count < 0 || offset > buffer.Length - count then
                invalidArg (nameof offset) "The destination range is invalid."

            if count = 0 then
                return ReadOutcome.Bytes 0
            elif position >= int64 bytes.Length then
                return ReadOutcome.EndOfSource
            else
                let amount = int (min (int64 count) (int64 bytes.Length - position))
                Array.blit bytes (int position) buffer offset amount
                return ReadOutcome.Bytes amount
        }

/// Fable turns a ResizeArray of bytes into a plain JavaScript array, and Node writes only typed arrays.
[<Emit("($0 instanceof Uint8Array ? $0 : Uint8Array.from($0))")>]
let private asTypedBytes (_buffer: byte[]) : byte[] = jsNative

type NodeTempStore private (path: string, descriptor: int, initialLength: int64) =
    let mutable disposed = false
    let mutable length = initialLength
    let mutable disposePromise: JS.Promise<unit> option = None

    let ensureOpen () =
        if disposed then
            raise (ObjectDisposedException("The text diff temporary store has been disposed."))

    let validateBuffer (buffer: byte[]) offset count =
        ensureOpen ()
        if isNull buffer then nullArg (nameof buffer)
        if offset < 0 || count < 0 || offset > buffer.Length - count then
            invalidArg (nameof offset) "The buffer range is invalid."

    let writeFully position (buffer: byte[]) offset count = async {
        let typed = asTypedBytes buffer
        let mutable written = 0

        while written < count do
            let! current =
                NodePositionalFile.writeAt descriptor typed (offset + written) (count - written) (position + int64 written)
                |> Async.AwaitPromise

            if current <= 0 then
                return raise (InvalidOperationException("Writing the text diff temporary store made no progress."))

            written <- written + current
    }

    member _.Path = path

    member _.Append(buffer: byte[]) (offset: int) (count: int) = async {
        validateBuffer buffer offset count

        if int64 count > Int64.MaxValue - length then
            invalidArg (nameof count) "The temporary store is too large."

        let start = length
        do! writeFully start buffer offset count
        length <- length + int64 count
        return start
    }

    member _.WriteAt(position: int64) (buffer: byte[]) (offset: int) (count: int) = async {
        validateBuffer buffer offset count
        if position < 0L || position > Int64.MaxValue - int64 count then
            invalidArg (nameof position) "The write position is outside the supported range."

        do! writeFully position buffer offset count
        length <- max length (position + int64 count)
    }

    member _.ReadAt(position: int64) (buffer: byte[]) (offset: int) (count: int) = async {
        validateBuffer buffer offset count
        if position < 0L then invalidArg (nameof position) "The read position cannot be negative."
        if count = 0 || position >= length then return 0
        else
            let wanted = int (min (int64 count) (length - position))
            let mutable read = 0
            let mutable stopped = false

            while read < wanted && not stopped do
                let! current =
                    NodePositionalFile.readAt descriptor buffer (offset + read) (wanted - read) (position + int64 read)
                    |> Async.AwaitPromise

                if current <= 0 then
                    stopped <- true
                else
                    read <- read + current

            return read
    }

    member _.Length() = length

    member this.Dispose() : Async<unit> =
        async {
            match disposePromise with
            | Some pending -> do! pending |> Async.AwaitPromise
            | None ->
                disposed <- true

                let pending =
                    async {
                        try
                            do! NodePositionalFile.close descriptor |> Async.AwaitPromise
                        with _ -> ()

                        try
                            do! NodeFileSystem.rmAsync path (NodeFileSystem.RmOptions(recursive = true, force = true, maxRetries = 4, retryDelay = 40)) |> Async.AwaitPromise
                        with _ -> ()
                    }
                    |> Async.StartAsPromise

                disposePromise <- Some pending
                do! pending |> Async.AwaitPromise
        }

    static member Open(path: string) : JS.Promise<NodeTempStore> =
        async {
            let! descriptor = NodePositionalFile.openCreateExclusiveReadWrite path |> Async.AwaitPromise

            try
                let! stats = NodePositionalFile.fstat descriptor |> Async.AwaitPromise
                return NodeTempStore(path, descriptor, stats.Size)
            with error ->
                try
                    do! NodePositionalFile.close descriptor |> Async.AwaitPromise
                with _ -> ()

                try
                    do! NodeFileSystem.rmAsync path (NodeFileSystem.RmOptions(recursive = true, force = true, maxRetries = 4, retryDelay = 40)) |> Async.AwaitPromise
                with _ -> ()

                return raise error
        }
        |> Async.StartAsPromise

    static member Create(directory: string, fileName: string) : JS.Promise<NodeTempStore> =
        if String.IsNullOrWhiteSpace fileName then
            invalidArg (nameof fileName) "The temporary store name cannot be empty."

        if fileName = "." || fileName = ".." || fileName.Contains("/") || fileName.Contains("\\") then
            invalidArg (nameof fileName) "The temporary store name must identify a file."

        let safeFileName =
            fileName
            |> Seq.map (fun character ->
                if
                    (character >= 'A' && character <= 'Z')
                    || (character >= 'a' && character <= 'z')
                    || (character >= '0' && character <= '9')
                    || character = '.'
                    || character = '_'
                    || character = '-'
                then
                    character
                else
                    '-')
            |> Seq.toArray
            |> String

        NodeTempStore.Open(NodePath.join [| directory; safeFileName |])

    interface ITempStore with
        member this.Append buffer offset count = this.Append buffer offset count
        member this.WriteAt position buffer offset count = this.WriteAt position buffer offset count
        member this.ReadAt position buffer offset count = this.ReadAt position buffer offset count
        member _.Length() = length
        member this.Dispose() = this.Dispose()
