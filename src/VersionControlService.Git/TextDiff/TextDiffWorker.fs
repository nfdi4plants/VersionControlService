/// Entry points for a host-owned worker file. The default handler opens Git diff sessions on the paged text
/// diff engine and serves the calls on their handles.
module VersionControlService.Git.TextDiff.TextDiffWorker

open System
open System.Collections.Generic
open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.TextDiff
open VersionControlService.Git.TextDiff.TextDiffProtocol
open VersionControlService.Git.TextDiff.TextDiffWorkerDispatcher
open VersionControlService.Git.TextDiff.TextDiffSourceResolver
open VersionControlService.Git.TextDiff.TextDiffPreparation
open VersionControlService.Git.TextDiff.TextDiffSources
open VersionControlService.Git.TextDiff.TextDiffStorage

module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodePositionalFile = VersionControlService.Runtime.Node.PositionalFile
module NodeWorkerThreads = VersionControlService.Runtime.Node.WorkerThreads

/// Progress messages go out about four times per second.
[<Literal>]
let private ProgressIntervalMs = 225

[<Literal>]
let private SampleLength = 65536

/// A seekable source up to this size is classified from its whole content.
[<Literal>]
let private WholeSampleLimit = 196608L

/// Bytes on each side of a sample, so a character split at the sample border is still visible.
[<Literal>]
let private SamplePadding = 4L

[<Literal>]
let private RetainedBlobLimit = 2097152L

[<Literal>]
let private DefaultIdleMs = 900000.0

[<Literal>]
let private SpoolPollMs = 5

/// How long a write check reuses an answer of the free space of the temp drive.
[<Literal>]
let private FreeSpaceCacheMs = 1000.0

/// Room in a page envelope for values the engine adds after it sized the parts.
[<Literal>]
let private EnvelopeSlack = 1024

[<Emit("$0?.code ?? ''")>]
let private errorCode (_error: obj) : string = jsNative

[<Emit("performance.now()")>]
let private performanceNow () : float = jsNative

[<Emit("(() => { const timer = setInterval($0, $1); if (timer && timer.unref) timer.unref(); return timer; })()")>]
let private startInterval (_callback: unit -> unit) (_milliseconds: int) : obj = jsNative

[<Emit("clearInterval($0)")>]
let private stopInterval (_timer: obj) : unit = jsNative

[<Emit("new Promise(resolve => setTimeout(resolve, $0))")>]
let private delay (_milliseconds: int) : JS.Promise<unit> = jsNative

let private sleep (milliseconds: int) : Async<unit> = delay milliseconds |> Async.AwaitPromise

let private lstat (path: string) : Async<StatResult> = async {
    try
        let! stats = NodePositionalFile.lstat path |> Async.AwaitPromise

        return
            StatResult.Stat {
                Size = stats.Size
                IsFile = stats.IsFile
                IsSymbolicLink = stats.IsSymbolicLink
                IsDirectory = stats.IsDirectory
                MtimeNs = stats.MtimeNs
                Ino = stats.Ino
                Dev = stats.Dev
            }
    with error ->
        match errorCode error with
        | "ENOENT"
        | "ENOTDIR" -> return StatResult.Missing
        | _ -> return raise error
}

let private readPrefix (path: string) (count: int) : Async<byte[]> =
    promise {
        let! descriptor = NodePositionalFile.openRead path
        let buffer: byte[] = Array.zeroCreate count
        let mutable total = 0
        let mutable ended = false
        let mutable failure = None

        try
            while not ended && total < count do
                let! read = NodePositionalFile.readAt descriptor buffer total (count - total) (int64 total)

                if read <= 0 then ended <- true else total <- total + read
        with error ->
            failure <- Some error

        do! NodePositionalFile.close descriptor

        match failure with
        | Some error -> return raise error
        | None -> return Array.sub buffer 0 total
    }
    |> Async.AwaitPromise

/// A resolver host that runs Git through the given runner and reads files on the calling thread.
let localFileHostWithCancellation (runGit: string[] -> Async<GitShort>) (isCanceled: unit -> bool) : IResolverHost =
    { new IResolverHost with
        member _.RunGit arguments = runGit arguments
        member _.Lstat path = lstat path
        member _.IsCanceled() = isCanceled ()
        member _.ReadPrefix path count = readPrefix path count
        member _.Realpath path = async { return NodeFileSystem.realpathSync path }
    }

let localFileHost (runGit: string[] -> Async<GitShort>) : IResolverHost =
    localFileHostWithCancellation runGit (fun () -> false)


let private categoryOf (code: string) =
    match code with
    | TextDiffFailureCodes.ContentNotText
    | TextDiffFailureCodes.EncodingMismatch
    | TextDiffFailureCodes.SessionClosed
    | TextDiffFailureCodes.ContinuationMismatch
    | TextDiffFailureCodes.PreparationMismatch -> Validation
    | TextDiffFailureCodes.SourceChanged -> Concurrency
    | _ -> ProviderError

let private failure (code: string) (message: string) = OperationFailure.create (categoryOf code) code message

let private sourceFailure (value: TextDiffSourceFailure) = failure value.Code value.Message

let private closedFailure () =
    failure TextDiffFailureCodes.SessionClosed "The diff session is closed."

let private describeError (sourceFailureOf: TextDiffSourceFailure option) (error: exn) : OperationFailure =
    match sourceFailureOf with
    | Some value -> sourceFailure value
    | None ->
        match error with
        | :? TextDiffSourceException as source -> failure source.Code source.Message
        | _ ->
            let code =
                if errorCode (box error) <> "" then TextDiffFailureCodes.ReadFailed else TextDiffFailureCodes.WorkerFailed

            failure code (NodeInterop.errorMessage (box error))


/// The encoding of one classified side.
type private SideClass = { Enc: TextEncoding; BomLen: int; Chosen: bool }

type private ClassOutcome =
    | Settled of SideClass
    | Evidence of string

/// The Git child that fills the spool of one blob side.
type private SpoolStart = {
    SpoolPath: string
    Closed: JS.Promise<NodeProcess.ChildExit>
    Retained: byte[] option
    BlobSize: int64
    SpawnFailure: string option ref
}

/// One source of a session, with what the worker needs to check and release it. A source that is classified from
/// its first 64 KiB (a spool or a blob in memory) has IsSpool set. SpoolRemaining is the number of bytes that a
/// still growing spool has yet to receive, and 0 for every other source.
type private OpenedSide = {
    Resolved: ResolvedSide
    SideLength: int64
    Src: IByteSource option
    IsSpool: bool
    Check: unit -> Async<unit>
    Release: unit -> JS.Promise<unit>
    SpoolRemaining: unit -> float
}

type private SideState =
    | NotOpened
    | Starting of SpoolStart
    | SideOpen of OpenedSide

type private SlotPhase =
    | Preparing
    | Active
    | Invalid of OperationFailure
    | Ended

/// Everything the worker holds for one pool session generation.
[<AllowNullLiteral>]
type private Slot(generation: int, owner: TextDiffOwner, request: OpenDiffRequest, host: WorkerHost) =
    member val Generation = generation
    member val Version = NodeInterop.randomUuid().Replace("-", "")
    member val Owner = owner
    /// The Open request without its continuation.
    member val Request = request
    member val Host = host with get, set
    member val Phase = SlotPhase.Preparing with get, set
    member val Busy = false with get, set
    member val LastUsed = performanceNow () with get, set
    member val Continuation: string option = None with get, set
    member val Resolved: ResolvedSources option = None with get, set
    member val Binding: PreparationBinding option = None with get, set
    member val Previous = SideState.NotOpened with get, set
    member val Current = SideState.NotOpened with get, set
    member val PreviousOutcome: ClassOutcome option = None with get, set
    member val CurrentOutcome: ClassOutcome option = None with get, set
    member val Retained = 0L with get, set
    member val SpoolWaitYield = false with get, set
    member val Failure: TextDiffSourceFailure option = None with get, set
    member val Session: TextDiffSession option = None with get, set
    member val Infos: (DiffSourceInfo * DiffSourceInfo) option = None with get, set
    member val Validated = 0L with get, set
    member val Total = 0L with get, set
    /// Where the session keeps its data. The first Open chooses it and its continuations keep the choice.
    member val Storage: StorageChoice option = None with get, set
    /// The bytes of committed blobs that a memory session holds. The memory budget counts them.
    member val MemoryBlobBytes = 0.0 with get, set
    /// The journal index store of the engine session. The write checks read its length.
    member val IndexStore: ITempStore option = None with get, set
    /// True while a background ReadPage runs.
    member val Background = false with get, set
    /// The free space of the temp drive and the time it was read.
    member val FreeSpace: (float option * float) option = None with get, set

    /// The bytes that the growing spools of the open sides have yet to receive.
    member this.SpoolRemaining() : float =
        [| this.Previous; this.Current |]
        |> Array.sumBy (fun state ->
            match state with
            | SideState.SideOpen side -> side.SpoolRemaining()
            | _ -> 0.0)

    member this.RecordFailure(value: TextDiffSourceFailure) =
        if this.Failure.IsNone then this.Failure <- Some value

    member this.StateOf(side: DiffSide) =
        match side with
        | DiffSide.Previous -> this.Previous
        | DiffSide.Current -> this.Current

    member this.SetState(side: DiffSide, state: SideState) =
        match side with
        | DiffSide.Previous -> this.Previous <- state
        | DiffSide.Current -> this.Current <- state

    member this.OutcomeOf(side: DiffSide) =
        match side with
        | DiffSide.Previous -> this.PreviousOutcome
        | DiffSide.Current -> this.CurrentOutcome

    member this.SetOutcome(side: DiffSide, outcome: ClassOutcome) =
        match side with
        | DiffSide.Previous -> this.PreviousOutcome <- Some outcome
        | DiffSide.Current -> this.CurrentOutcome <- Some outcome

type private Worker = {
    RunShort: WorkerHost -> TextDiffOwner -> string[] -> int option -> JS.Promise<GitShort>
    Tokens: PreparationTokenStore
    BlobLedger: Ledger
    Slots: Dictionary<int, Slot>
    /// The Open that runs now. The dispatcher runs one request at a time.
    mutable ActiveOpen: WorkerHost option
    Clock: IClock
    IdleMs: float
    mutable Sweeping: bool
}

let private noRelease () : JS.Promise<unit> = Promise.lift ()

let private noCheck () : Async<unit> = async.Return()

let private noRemaining () : float = 0.0

let private releaseSide (state: SideState) : Async<unit> = async {
    match state with
    | SideState.SideOpen side ->
        try
            do! side.Release() |> Async.AwaitPromise
        with _ -> ()
    | _ -> ()
}

/// Closes the session and the sources of a slot and gives back the retained blob reservation. It is safe to call twice.
let private releaseResources (worker: Worker) (slot: Slot) : Async<unit> = async {
    match slot.Session with
    | Some session ->
        slot.Session <- None

        try
            do! session.Close()
        with _ -> ()
    | None -> ()

    let previous = slot.Previous
    let current = slot.Current
    slot.Previous <- SideState.NotOpened
    slot.Current <- SideState.NotOpened
    slot.MemoryBlobBytes <- 0.0
    slot.IndexStore <- None
    do! releaseSide previous
    do! releaseSide current

    if slot.Retained > 0L then
        worker.BlobLedger.Release(AllocationCategory.RetainedBlobs, slot.Retained)
        slot.Retained <- 0L
}

let private disposeSlot (worker: Worker) (slot: Slot) : Async<unit> = async {
    do! releaseResources worker slot
    slot.Phase <- SlotPhase.Ended

    match worker.Slots.TryGetValue slot.Generation with
    | true, current when obj.ReferenceEquals(current, slot) -> worker.Slots.Remove slot.Generation |> ignore
    | _ -> ()
}

/// Releases the resources and makes every later call except Close fail with the given failure.
let private invalidate (worker: Worker) (slot: Slot) (reason: OperationFailure) : Async<unit> = async {
    match slot.Phase with
    | SlotPhase.Invalid _
    | SlotPhase.Ended -> ()
    | _ ->
        slot.Phase <- SlotPhase.Invalid reason
        do! releaseResources worker slot
        slot.Host.ReleaseSession()
}

let private progressOf (slot: Slot) () : int64 * int64 =
    match slot.Session with
    | Some session ->
        let progress = session.Progress
        progress.ValidatedBytes, progress.TotalBytes
    | None -> slot.Validated, slot.Total

let private startTicker (host: WorkerHost) (progress: unit -> int64 * int64) : unit -> unit =
    let timer =
        startInterval
            (fun () ->
                let validated, total = progress ()
                host.ReportProgress(validated, total))
            ProgressIntervalMs

    fun () -> stopInterval timer

type private OpenStep =
    | Proceed
    | Finish of Result<Resumable<OpenDiffResult>, OperationFailure>
    | Pause

let private canceledStep = Finish(Error(canceledFailure ()))

let private utf8Class = { Enc = TextEncoding.Utf8; BomLen = 0; Chosen = false }

let private sizeOf (side: ResolvedSide) =
    match side with
    | ResolvedSide.Absent -> 0L
    | ResolvedSide.GitBlob(_, size) -> size
    | ResolvedSide.WorkingFile(_, identity) -> identity.Size
    | ResolvedSide.LfsObject(_, size, _, _) -> size

let private resolvedSideOf (sources: ResolvedSources) (side: DiffSide) =
    match side with
    | DiffSide.Previous -> sources.Previous
    | DiffSide.Current -> sources.Current

let private isAbsent (side: ResolvedSide) =
    match side with
    | ResolvedSide.Absent -> true
    | _ -> false

let private parseChoice (name: string option) : Result<TextEncoding option, OperationFailure> =
    match name with
    | None -> Ok None
    | Some value ->
        match Decoders.tryParseName value with
        | Some encoding -> Ok(Some encoding)
        | None -> Error(OperationFailure.create Validation TextDiffFailureCodes.UnsupportedEncoding "The chosen encoding is not supported.")

let private resolveStep (worker: Worker) (host: WorkerHost) (request: OpenDiffRequest) (owner: TextDiffOwner) (slot: Slot) : Async<OpenStep> = async {
    match slot.Resolved with
    | Some _ -> return Proceed
    | None ->
        let path = RepositoryPath.value request.Path
        let previousPath = request.PreviousPath |> Option.map RepositoryPath.value

        if request.Preparation.IsNone then
            worker.Tokens.ReleaseForPath(owner.WindowOwner, path)

        let input: ResolverInput = {
            RepositoryRoot = owner.WorkspaceRoot
            LfsMediaDirectory = owner.LfsMediaDirectory
            Path = path
            PreviousPath = previousPath
        }

        let runGit arguments =
            worker.RunShort host owner arguments None |> Async.AwaitPromise

        let! outcome = resolve (localFileHostWithCancellation runGit host.IsCanceled) input

        if host.IsCanceled() then
            return canceledStep
        else
            match outcome with
            | ResolveOutcome.Canceled -> return canceledStep
            | ResolveOutcome.Blocked blocker -> return Finish(Ok(Resumable.Ready(OpenDiffResult.NotDiffable blocker)))
            | ResolveOutcome.ReadError message -> return Finish(Error(OperationFailure.create ProviderError TextDiffFailureCodes.ReadFailed message))
            | ResolveOutcome.Resolved sources ->
                let binding = bindingOf path previousPath sources

                let validation =
                    match request.Preparation with
                    | Some token -> worker.Tokens.Validate(token, binding, owner.WindowOwner)
                    | None -> Ok()

                match validation with
                | Error problem -> return Finish(Error problem)
                | Ok() ->
                    slot.Resolved <- Some sources
                    slot.Binding <- Some binding
                    slot.Total <- sizeOf sources.Previous + sizeOf sources.Current

                    match request.Preparation with
                    | Some token ->
                        for kept in worker.Tokens.Kept token do
                            let chosen =
                                match kept.Side with
                                | DiffSide.Previous -> request.PreviousEncoding
                                | DiffSide.Current -> request.CurrentEncoding

                            match chosen, Decoders.tryParseName kept.Encoding with
                            | None, Some encoding ->
                                slot.SetOutcome(kept.Side, Settled { Enc = encoding; BomLen = kept.BomLength; Chosen = kept.WasChosen })
                            | _ -> ()
                    | None -> ()

                    return Proceed
}

/// The free space of the temp drive. A write check reuses an answer that is at most a second old.
let private freeSpace (worker: Worker) (slot: Slot) (directory: string) : Async<float option> = async {
    let now = worker.Clock.NowMs()

    match slot.FreeSpace with
    | Some(value, readAt) when now - readAt <= FreeSpaceCacheMs -> return value
    | _ ->
        let! value = NodePositionalFile.tryFreeBytes directory |> Async.AwaitPromise
        slot.FreeSpace <- Some(value, worker.Clock.NowMs())
        return value
}

/// Chooses disk or memory at the first Open, once the resolver knows the committed blobs and their sizes. A
/// continuation finds the choice in the slot and does not read the free space again. A memory session cannot take
/// a committed blob above the memory blob limit, so that Open ends with a blocker before any blob is read.
let private storageStep (worker: Worker) (host: WorkerHost) (slot: Slot) : Async<OpenStep> = async {
    match slot.Storage, slot.Resolved with
    | None, Some sources ->
        let blobs =
            [| DiffSide.Previous, sources.Previous; DiffSide.Current, sources.Current |]
            |> Array.choose (fun (side, resolved) ->
                match resolved with
                | ResolvedSide.GitBlob(_, size) -> Some(side, size)
                | _ -> None)

        let committedBytes =
            if blobs.Length = 0 then None else Some(blobs |> Array.sumBy (fun (_, size) -> float size))

        let policy = slot.Request.Storage

        let! free =
            match policy with
            | DiffStoragePolicy.PreferDisk _ -> freeSpace worker slot host.TempDirectory
            | DiffStoragePolicy.MemoryOnly _ -> async.Return None

        let choice = chooseStorage policy free committedBytes
        slot.Storage <- Some choice

        match choice with
        | StorageChoice.Memory budget ->
            let limit = memoryBlobLimit budget

            match blobs |> Array.tryFind (fun (_, size) -> float size > limit) with
            | Some(side, size) ->
                return Finish(Ok(Resumable.Ready(OpenDiffResult.NotDiffable(DiffBlocker.BlobTooLargeForMemory(side, size, int64 limit)))))
            | None -> return Proceed
        | StorageChoice.Disk -> return Proceed
    | _ -> return Proceed
}

/// Starts the blob child of a side and opens its spool once the supervisor has created the file.
let private openBlobSide
    (worker: Worker)
    (host: WorkerHost)
    (slot: Slot)
    (side: DiffSide)
    (resolved: ResolvedSide)
    (oid: string)
    (size: int64)
    (meter: Meter)
    : Async<OpenStep> =
    async {
        let start =
            match slot.StateOf side with
            | SideState.Starting existing -> existing
            | _ ->
                let retained =
                    if size <= RetainedBlobLimit && worker.BlobLedger.TryReserve(AllocationCategory.RetainedBlobs, size) then
                        slot.Retained <- slot.Retained + size
                        Some(Array.zeroCreate<byte> (int size))
                    else
                        None

                let spoolPath =
                    NodePath.join [| host.TempDirectory; $"spool-{host.Generation}-{NodeInterop.randomUuid()}.blob" |]

                let spawnFailure: string option ref = ref None
                let closed = host.SpawnBlob(slot.Owner.WorkspaceRoot, oid, spoolPath)

                NodeInterop.observePromise closed ignore (fun error -> spawnFailure.Value <- Some(NodeInterop.errorMessage error))

                let created = {
                    SpoolPath = spoolPath
                    Closed = closed
                    Retained = retained
                    BlobSize = size
                    SpawnFailure = spawnFailure
                }

                slot.SetState(side, SideState.Starting created)
                created

        let mutable spool: SpoolSource option = None
        let mutable stop: OpenStep option = None

        while spool.IsNone && stop.IsNone do
            if host.IsCanceled() then
                stop <- Some canceledStep
            elif start.SpawnFailure.Value.IsSome then
                stop <- Some(Finish(Error(OperationFailure.create ProviderError TextDiffFailureCodes.ReadFailed start.SpawnFailure.Value.Value)))
            elif Meter.overBudget meter then
                stop <- Some Pause
            else
                try
                    let! opened =
                        SpoolSource.Open(
                            start.SpoolPath,
                            start.BlobSize,
                            start.Retained,
                            start.Closed,
                            onFailure = (fun value -> slot.RecordFailure value),
                            onWait = (fun () -> slot.SpoolWaitYield <- true)
                        )
                        |> Async.AwaitPromise

                    spool <- Some opened
                with error when errorCode (box error) = "ENOENT" ->
                    do! sleep SpoolPollMs

        match spool, stop with
        | Some opened, _ ->
            slot.SetState(
                side,
                SideState.SideOpen {
                    Resolved = resolved
                    SideLength = size
                    Src = Some(opened :> IByteSource)
                    IsSpool = true
                    Check = noCheck
                    Release = fun () -> opened.Dispose()
                    SpoolRemaining = fun () -> opened.RemainingBytes
                }
            )

            return Proceed
        | None, Some step -> return step
        | None, None -> return Pause
    }

/// Reads a committed blob whole into memory. The supervisor raises the output limit of this one command, and the
/// length of the answer has to match the size that the resolver learned from cat-file -s.
let private openMemoryBlobSide
    (worker: Worker)
    (host: WorkerHost)
    (slot: Slot)
    (side: DiffSide)
    (resolved: ResolvedSide)
    (oid: string)
    (size: int64)
    (meter: Meter)
    : Async<OpenStep> =
    async {
        let! result = worker.RunShort host slot.Owner [| "cat-file"; "blob"; oid |] (Some(int size)) |> Async.AwaitPromise

        if host.IsCanceled() then
            return canceledStep
        else
            match result.ExitCode, result.Error with
            | Some 0, None when int64 result.Stdout.Length = size ->
                Meter.chargeBytes meter result.Stdout.Length
                slot.MemoryBlobBytes <- slot.MemoryBlobBytes + float size

                slot.SetState(
                    side,
                    SideState.SideOpen {
                        Resolved = resolved
                        SideLength = size
                        Src = Some(MemoryBlobSource(result.Stdout) :> IByteSource)
                        IsSpool = true
                        Check = noCheck
                        Release = noRelease
                        SpoolRemaining = noRemaining
                    }
                )

                return Proceed
            | _ ->
                let detail =
                    match result.Error with
                    | Some message -> message
                    | None when result.ExitCode = Some 0 -> $"Git returned {result.Stdout.Length} bytes, expected {size}."
                    | None -> result.Stderr.Trim()

                return Finish(Error(OperationFailure.create ProviderError TextDiffFailureCodes.ReadFailed $"The Git blob for {RepositoryPath.value slot.Request.Path} could not be read into memory. {detail}"))
    }

let private openSide
    (worker: Worker)
    (host: WorkerHost)
    (request: OpenDiffRequest)
    (slot: Slot)
    (side: DiffSide)
    (meter: Meter)
    : Async<OpenStep> =
    async {
        match slot.StateOf side, slot.Resolved with
        | SideState.SideOpen _, _ -> return Proceed
        | _, None -> return Proceed
        | _, Some sources ->
            let resolved = resolvedSideOf sources side
            let onFailure = fun (value: TextDiffSourceFailure) -> slot.RecordFailure value

            match resolved with
            | ResolvedSide.Absent ->
                slot.SetState(
                    side,
                    SideState.SideOpen {
                        Resolved = resolved
                        SideLength = 0L
                        Src = None
                        IsSpool = false
                        Check = noCheck
                        Release = noRelease
                        SpoolRemaining = noRemaining
                    }
                )

                return Proceed
            | ResolvedSide.WorkingFile(path, identity) ->
                let! source = FileSource.OpenWorkingFile(path, identity, onFailure = onFailure) |> Async.AwaitPromise

                slot.SetState(
                    side,
                    SideState.SideOpen {
                        Resolved = resolved
                        SideLength = identity.Size
                        Src = Some(source :> IByteSource)
                        IsSpool = false
                        Check = fun () -> source.CheckIdentity()
                        Release = fun () -> source.Dispose()
                        SpoolRemaining = noRemaining
                    }
                )

                return Proceed
            | ResolvedSide.LfsObject(objectPath, size, objectIdentity, pointerIdentity) ->
                let pointerPath =
                    pointerIdentity
                    |> Option.map (fun _ -> NodePath.join [| slot.Owner.WorkspaceRoot; RepositoryPath.value request.Path |])

                let! source =
                    FileSource.OpenLfsObject(objectPath, size, objectIdentity, pointerPath, pointerIdentity, onFailure = onFailure)
                    |> Async.AwaitPromise

                slot.SetState(
                    side,
                    SideState.SideOpen {
                        Resolved = resolved
                        SideLength = size
                        Src = Some(source :> IByteSource)
                        IsSpool = false
                        Check = fun () -> source.CheckIdentity()
                        Release = fun () -> source.Dispose()
                        SpoolRemaining = noRemaining
                    }
                )

                return Proceed
            | ResolvedSide.GitBlob(oid, size) ->
                match slot.Storage with
                | Some(StorageChoice.Memory _) -> return! openMemoryBlobSide worker host slot side resolved oid size meter
                | _ -> return! openBlobSide worker host slot side resolved oid size meter
    }

let private openSidesStep (worker: Worker) (host: WorkerHost) (request: OpenDiffRequest) (slot: Slot) (meter: Meter) : Async<OpenStep> = async {
    let! previous = openSide worker host request slot DiffSide.Previous meter

    match previous with
    | Proceed -> return! openSide worker host request slot DiffSide.Current meter
    | other -> return other
}

/// Reads a range of a source. It answers none when the range is not available yet and the request is out of
/// budget or canceled.
let private readRange (source: IByteSource) (meter: Meter) (isCanceled: unit -> bool) (position: int64) (count: int) : Async<byte[] option> = async {
    let buffer: byte[] = Array.zeroCreate count
    let mutable total = 0
    let mutable ended = false
    let mutable interrupted = false

    while not ended && not interrupted && total < count do
        let! outcome = source.ReadAt (position + int64 total) buffer total (count - total)

        match outcome with
        | ReadOutcome.Bytes read when read > 0 ->
            total <- total + read
            Meter.chargeBytes meter read
        | ReadOutcome.Bytes _
        | ReadOutcome.EndOfSource -> ended <- true
        | ReadOutcome.NotYetAvailable ->
            if isCanceled () || Meter.overBudget meter then
                interrupted <- true
            else
                do! sleep SpoolPollMs

    if interrupted then return None else return Some(Array.sub buffer 0 total)
}

/// The byte ranges to classify as bufferOffset, read count, sample offset and sample length.
let private samplePlan (isSpool: bool) (length: int64) : (int64 * int * int * int)[] =
    if isSpool then
        let count = int (min length (int64 SampleLength))
        [| (0L, count, 0, count) |]
    elif length <= WholeSampleLimit then
        let count = int length
        [| (0L, count, 0, count) |]
    else
        let sample = int64 SampleLength
        let middle = max sample (min (length - 2L * sample) ((length / 2L - sample / 2L) / 4L * 4L))

        [| 0L; middle; length - sample |]
        |> Array.map (fun start ->
            let padStart = min SamplePadding start
            let padEnd = min SamplePadding (length - (start + sample))
            (start - padStart, int (padStart + sample + padEnd), int padStart, SampleLength))

let private readSamples (source: IByteSource) (isSpool: bool) (length: int64) (meter: Meter) (isCanceled: unit -> bool) : Async<ClassificationSample[] option> = async {
    let plan = samplePlan isSpool length
    let samples = ResizeArray<ClassificationSample>()
    let mutable index = 0
    let mutable interrupted = false

    while not interrupted && index < plan.Length do
        let bufferOffset, count, sampleOffset, sampleLength = plan[index]
        let! bytes = readRange source meter isCanceled bufferOffset count

        match bytes with
        | Some read ->
            samples.Add {
                BufferOffset = bufferOffset
                Bytes = read
                SampleOffset = sampleOffset
                SampleLength = max 0 (min sampleLength (read.Length - sampleOffset))
            }

            index <- index + 1
        | None -> interrupted <- true

    if interrupted then return None else return Some(samples.ToArray())
}

/// Classifies one side once. It answers false when the request ran out of budget or was canceled first.
let private classifySide (slot: Slot) (host: WorkerHost) (meter: Meter) (side: DiffSide) (chosen: TextEncoding option) : Async<bool> = async {
    if (slot.OutcomeOf side).IsSome then
        return true
    else
        match slot.StateOf side with
        | SideState.SideOpen opened ->
            match opened.Src with
            | None ->
                slot.SetOutcome(side, Settled utf8Class)
                return true
            | Some _ when opened.SideLength = 0L ->
                slot.SetOutcome(side, Settled { Enc = defaultArg chosen TextEncoding.Utf8; BomLen = 0; Chosen = chosen.IsSome })
                return true
            | Some source ->
                let! samples = readSamples source opened.IsSpool opened.SideLength meter host.IsCanceled

                match samples with
                | None -> return false
                | Some read ->
                    slot.Validated <- slot.Validated + (read |> Array.sumBy (fun sample -> int64 sample.SampleLength))

                    let result =
                        match chosen with
                        | Some encoding -> Classification.classifyWithChoice opened.SideLength read encoding
                        | None -> Classification.classify opened.SideLength read

                    match result with
                    | Classified(encoding, _, bomLength) ->
                        slot.SetOutcome(side, Settled { Enc = encoding; BomLen = bomLength; Chosen = chosen.IsSome })
                    | BinaryEvidence evidence -> slot.SetOutcome(side, Evidence evidence)

                    return true
        | _ -> return true
}

/// Answers an Open with the encoding choice for one side. The token keeps the settled encoding of the other side.
let private requireEncoding
    (worker: Worker)
    (request: OpenDiffRequest)
    (owner: TextDiffOwner)
    (slot: Slot)
    (side: DiffSide)
    (candidates: EncodingCandidate[])
    : OpenStep =
    let token = worker.Tokens.Issue(slot.Binding.Value, owner.WindowOwner)

    let other =
        match side with
        | DiffSide.Previous -> DiffSide.Current
        | DiffSide.Current -> DiffSide.Previous

    let kept =
        match slot.OutcomeOf other, slot.Resolved with
        | Some(Settled settled), Some sources when not (isAbsent (resolvedSideOf sources other)) ->
            [|
                {
                    Side = other
                    Encoding = Decoders.name settled.Enc
                    BomLength = settled.BomLen
                    WasChosen = settled.Chosen
                }
            |]
        | _ -> [||]

    worker.Tokens.Keep(token, kept)
    request.Preparation |> Option.iter worker.Tokens.Release
    Finish(Ok(Resumable.Ready(OpenDiffResult.NotDiffable(DiffBlocker.EncodingRequired(side, token, candidates)))))

/// Bytes on each side of an invalid sequence that the encoding previews show.
[<Literal>]
let private MismatchPreviewBytes = 512

[<JS.PojoAttribute>]
type private TextDecoderOptions(?fatal: bool, ?ignoreBOM: bool) =
    member val fatal: bool option = fatal with get, set
    member val ignoreBOM: bool option = ignoreBOM with get, set

[<Global>]
type private TextDecoder(label: string, options: TextDecoderOptions) =
    member _.decode(bytes: byte[]) : string = jsNative

/// Decodes UTF-8 and replaces each invalid sequence with U+FFFD. ignoreBOM keeps a leading U+FEFF in the text.
let private lenientUtf8Decoder = TextDecoder("utf-8", TextDecoderOptions(fatal = false, ignoreBOM = true))

let private windows1252Text (bytes: byte[]) (start: int) : string =
    let text = System.Text.StringBuilder()

    for index = start to bytes.Length - 1 do
        let scalar = Decoders.windows1252Scalar (int bytes[index])
        text.Append(if scalar < 0 then '\uFFFD' else char scalar) |> ignore

    text.ToString()

/// Decides whether a content failure is an invalid UTF-8 sequence on a side that classification read as UTF-8
/// without a byte order mark or a choice of the caller, and that Windows-1252 can decode. It answers the two
/// candidates with previews of the text around the sequence, or none for any other failure.
let private encodingMismatchCandidates (worker: Worker) (slot: Slot) (detail: DiffContentBlocked) : Async<EncodingCandidate[] option> = async {
    match detail.InvalidSequenceOffset, slot.OutcomeOf detail.Side, slot.StateOf detail.Side with
    | Some offset, Some(Settled settled), SideState.SideOpen opened when
        settled.Enc = TextEncoding.Utf8 && not settled.Chosen && settled.BomLen = 0 && offset >= 0L && offset < opened.SideLength
        ->
        match opened.Src with
        | None -> return None
        | Some source ->
            let start = max 0L (offset - int64 MismatchPreviewBytes)
            let count = int (min (offset + int64 MismatchPreviewBytes) opened.SideLength - start)
            let meter = Meter.create worker.Clock Limits.defaults
            let! read = readRange source meter slot.Host.IsCanceled start count

            match read with
            | Some bytes when bytes.Length > int (offset - start) ->
                let bad = int bytes[int (offset - start)]

                if bad < 0x80 || Decoders.windows1252Scalar bad < 0 then
                    return None
                else
                    // A window that starts inside a sequence would show a replacement character the file does not have.
                    let mutable first = 0

                    while start > 0L && first < 3 && first < int (offset - start) && (int bytes[first] &&& 0xC0) = 0x80 do
                        first <- first + 1

                    let trimmed = Array.sub bytes first (bytes.Length - first)

                    return
                        Some [|
                            { Encoding = Decoders.name TextEncoding.Utf8; Preview = lenientUtf8Decoder.decode trimmed }
                            { Encoding = Decoders.name TextEncoding.Windows1252; Preview = windows1252Text trimmed 0 }
                        |]
            | _ -> return None
    | _ -> return None
}

let private classifyStep
    (host: WorkerHost)
    (slot: Slot)
    (meter: Meter)
    (previousChoice: TextEncoding option)
    (currentChoice: TextEncoding option)
    : Async<OpenStep> =
    async {
        let! previousDone = classifySide slot host meter DiffSide.Previous previousChoice
        let! currentDone = if previousDone then classifySide slot host meter DiffSide.Current currentChoice else async.Return false

        if host.IsCanceled() then
            return canceledStep
        elif not (previousDone && currentDone) then
            return Pause
        else
            match slot.PreviousOutcome.Value, slot.CurrentOutcome.Value with
            | Evidence evidence, _ -> return Finish(Ok(Resumable.Ready(OpenDiffResult.NotDiffable(DiffBlocker.Binary(DiffSide.Previous, evidence)))))
            | _, Evidence evidence -> return Finish(Ok(Resumable.Ready(OpenDiffResult.NotDiffable(DiffBlocker.Binary(DiffSide.Current, evidence)))))
            | Settled _, Settled _ -> return Proceed
    }

/// The bytes of a page envelope that are not part sizes: the handle, both source infos, the longest identifiers,
/// the widest progress and the largest pending preview.
let private envelopeReserve (host: WorkerHost) (handle: DiffHandle) (previous: DiffSourceInfo) (current: DiffSourceInfo) : int =
    let longest = String.replicate 128 "x"

    let snippet: PendingSnippet = {
        Line = Int64.MaxValue
        OffsetUtf16 = Int64.MaxValue
        Text = String.replicate 2048 "\u0001"
        End = SnippetEnd.MoreTextPending
    }

    let pending: PendingPreview = {
        Previous = PendingSide.Snippet snippet
        Current = PendingSide.Snippet snippet
        Mismatch = Some { PreviousOffsetUtf16 = Int64.MaxValue; CurrentOffsetUtf16 = Int64.MaxValue }
    }

    let page: DiffPage = {
        PageId = longest
        NextCursor = Some longest
        Parts = [||]
        Progress = { ValidatedBytes = Int64.MaxValue; TotalBytes = Int64.MaxValue; ScanComplete = false }
        OutputComplete = false
        Pending = Some pending
    }

    let payload = ResultPayload.Open(Resumable.Ready(OpenDiffResult.Opened(handle, previous, current, Resumable.Ready page, DiffStorage.InMemory Int64.MaxValue)))

    envelopeByteLength (encode (TextDiffMessage.Result(host.RequestId, host.Generation, payload)))
    + EnvelopeSlack

let private describeSide (path: RepositoryPath) (commitId: string option) (opened: OpenedSide) (settled: SideClass) : DiffSourceInfo =
    let absent = opened.Src.IsNone

    let revision =
        match opened.Resolved, commitId with
        | ResolvedSide.GitBlob _, Some id ->
            match RevisionId.tryCreate id with
            | Ok value -> Some value
            | Error _ -> None
        | _ -> None

    {
        Path = path
        Revision = revision
        IsAbsent = absent
        ByteLength = opened.SideLength
        LineCount = None
        Encoding = if absent then None else Some(Decoders.name settled.Enc)
        EncodingWasChosen = settled.Chosen
        HasBom = settled.BomLen > 0
    }

let private sourceSpec (opened: OpenedSide) (settled: SideClass) : SourceSpec = {
    Source = opened.Src
    Encoding = Decoders.name settled.Enc
    BomLength = settled.BomLen
    ByteLength = opened.SideLength
}

/// Creates the engine session and runs its first page.
let private createStep (worker: Worker) (host: WorkerHost) (request: OpenDiffRequest) (owner: TextDiffOwner) (slot: Slot) : Async<OpenStep> = async {
    match slot.Previous, slot.Current, slot.PreviousOutcome, slot.CurrentOutcome, slot.Resolved with
    | SideState.SideOpen previous, SideState.SideOpen current, Some(Settled previousClass), Some(Settled currentClass), Some sources ->
        let handle: DiffHandle = { Id = string host.Generation; Version = slot.Version }
        let previousPath = defaultArg request.PreviousPath request.Path
        let previousInfo = describeSide previousPath sources.CommitId previous previousClass
        let currentInfo = describeSide request.Path sources.CommitId current currentClass
        slot.Infos <- Some(previousInfo, currentInfo)

        let reserve = envelopeReserve host handle previousInfo currentInfo
        let uuid = NodeInterop.randomUuid().Replace("-", "")
        let sessionId = $"s{host.Generation}-{uuid}"

        let config = {
            (SessionConfig.defaults sessionId) with
                PageMaxBytes = PageEnvelopeLimit - reserve
                ContextLines = request.ContextLines
        }

        let indexLength () =
            match slot.IndexStore with
            | Some store -> float (store.Length())
            | None -> 0.0

        let rememberIndex (name: string) (store: ITempStore) =
            if name.EndsWith(":journal-index", StringComparison.Ordinal) then
                slot.IndexStore <- Some store

            store

        let yieldToHost () =
            if slot.SpoolWaitYield then
                slot.SpoolWaitYield <- false
                sleep SpoolPollMs
            else
                host.Yield() |> Async.AwaitPromise

        let engine: EngineHost =
            match slot.Storage with
            | Some(StorageChoice.Memory budget) ->
                // The cap only detects bugs. The write check keeps the session well below it.
                let capBytes = int64 (min 1073741824.0 (2.0 * budget + 1048576.0))
                let group = MemoryStoreGroup(capBytes)

                {
                    Clock = worker.Clock
                    Yield = yieldToHost
                    CheckWrite =
                        fun () -> async {
                            return
                                memoryRefusal (float group.StoredBytes) (indexLength ()) slot.MemoryBlobBytes budget slot.Background
                                |> Option.map (fun message -> TextDiffFailureCodes.MemoryBudgetReached, message)
                        }
                    CreateTempStore = fun name -> async { return rememberIndex name (group.Create name) }
                }
            | _ ->
                let directory = host.TempDirectory

                let checkWrite =
                    match slot.Request.Storage with
                    | DiffStoragePolicy.PreferDisk(minimumFreeBytes, _) ->
                        fun () -> async {
                            let! free = freeSpace worker slot directory

                            return
                                diskRefusal free (indexLength ()) (slot.SpoolRemaining()) (float minimumFreeBytes)
                                |> Option.map (fun message -> TextDiffFailureCodes.TempSpaceLow, message)
                        }
                    | DiffStoragePolicy.MemoryOnly _ -> fun () -> async.Return None

                {
                    Clock = worker.Clock
                    Yield = yieldToHost
                    CheckWrite = checkWrite
                    CreateTempStore =
                        fun name -> async {
                            let! store =
                                NodeTempStore.Create(directory, $"{name}-{NodeInterop.randomUuid()}.tmp")
                                |> Async.AwaitPromise

                            return rememberIndex name (store :> ITempStore)
                        }
                }

        let! session =
            TextDiffSession.create
                engine
                worker.BlobLedger
                config
                (fun part -> partByteLength part + 1)
                (sourceSpec previous previousClass)
                (sourceSpec current currentClass)

        slot.Session <- Some session
        slot.SpoolWaitYield <- false
        let! first = session.FirstPage host.IsCanceled

        match first with
        | EngineResult.Ok page ->
            slot.Phase <- SlotPhase.Active
            slot.Continuation <- None
            request.Preparation |> Option.iter worker.Tokens.Release
            let storage =
                match slot.Storage with
                | Some(StorageChoice.Memory budget) -> DiffStorage.InMemory(int64 budget)
                | _ -> DiffStorage.OnDisk

            return Finish(Ok(Resumable.Ready(OpenDiffResult.Opened(handle, previousInfo, currentInfo, page, storage))))
        | EngineResult.Failed(code, _, Some detail) when code = TextDiffFailureCodes.ContentNotText ->
            let! candidates = encodingMismatchCandidates worker slot detail

            match candidates with
            | Some found -> return requireEncoding worker request owner slot detail.Side found
            | None -> return Finish(Ok(Resumable.Ready(OpenDiffResult.NotDiffable(DiffBlocker.Binary(detail.Side, detail.Evidence)))))
        | EngineResult.Failed(code, message, detail) ->
            let problem =
                match slot.Failure with
                | Some value -> sourceFailure value
                | None -> { (failure code message) with DiffDetail = detail }

            return Finish(Error problem)
        | EngineResult.Canceled -> return canceledStep
    | _ -> return Finish(Error(failure TextDiffFailureCodes.WorkerFailed "The diff sources are not ready."))
}

let private runOpen
    (worker: Worker)
    (host: WorkerHost)
    (request: OpenDiffRequest)
    (owner: TextDiffOwner)
    (slot: Slot)
    (meter: Meter)
    : Async<OpenStep> =
    async {
        match parseChoice request.PreviousEncoding, parseChoice request.CurrentEncoding with
        | Error problem, _
        | _, Error problem -> return Finish(Error problem)
        | Ok previousChoice, Ok currentChoice ->
            let! resolved = resolveStep worker host request owner slot

            match resolved with
            | Proceed ->
                let! stored = storageStep worker host slot

                match stored with
                | Proceed ->
                    let! opened = openSidesStep worker host request slot meter

                    match opened with
                    | Proceed ->
                        let! classified = classifyStep host slot meter previousChoice currentChoice

                        match classified with
                        | Proceed -> return! createStep worker host request owner slot
                        | other -> return other
                    | other -> return other
                | other -> return other
            | other -> return other
    }

let private isPreparing (slot: Slot) =
    match slot.Phase with
    | SlotPhase.Preparing -> true
    | _ -> false

let private sweep (worker: Worker) =
    let now = performanceNow ()

    for slot in worker.Slots.Values |> Seq.toArray do
        let ended =
            match slot.Phase with
            | SlotPhase.Ended -> true
            | _ -> false

        if not ended && not slot.Busy && now - slot.LastUsed >= worker.IdleMs then
            slot.Busy <- true
            let host = slot.Host

            async {
                try
                    do! disposeSlot worker slot
                    host.ReleaseSession()
                    host.ReportSessionExpired()
                with _ -> ()
            }
            |> Async.StartAsPromise
            |> ignore

let private ensureSweeper (worker: Worker) =
    if not worker.Sweeping then
        worker.Sweeping <- true
        startInterval (fun () -> sweep worker) (int (max 1.0 (min 30000.0 (worker.IdleMs / 3.0)))) |> ignore

/// Raises the first failure a mutable source reports, or when its file no longer matches the one that was opened.
let private checkSources (slot: Slot) : Async<unit> = async {
    for state in [| slot.Previous; slot.Current |] do
        match state with
        | SideState.SideOpen side -> do! side.Check()
        | _ -> ()

    match slot.Failure with
    | Some value -> raise (TextDiffSourceException(value.Code, value.Message))
    | None -> ()
}

let private openRequest
    (worker: Worker)
    (host: WorkerHost)
    (request: OpenDiffRequest)
    (owner: TextDiffOwner)
    : Async<Result<Resumable<OpenDiffResult>, OperationFailure>> =
    async {
        ensureSweeper worker
        let stored = { request with Continuation = None }

        let! found = async {
            match request.Continuation with
            | Some continuation ->
                match worker.Slots.TryGetValue host.Generation with
                | true, existing when
                    isPreparing existing
                    && existing.Continuation = Some continuation
                    // The slot keeps the storage policy of the first Open, so a setting that changed since does not
                    // turn the continuation into a mismatch.
                    && existing.Request = { stored with Storage = existing.Request.Storage }
                    && existing.Owner = owner
                    ->
                    return Ok existing
                | true, existing when
                    (match existing.Phase with
                     | SlotPhase.Ended -> false
                     | _ -> true)
                    ->
                    return Error(failure TextDiffFailureCodes.ContinuationMismatch "The continuation does not belong to this session.")
                | _ -> return Error(closedFailure ())
            | None ->
                match worker.Slots.TryGetValue host.Generation with
                | true, old -> do! disposeSlot worker old
                | _ -> ()

                let created = Slot(host.Generation, owner, stored, host)
                worker.Slots[host.Generation] <- created
                return Ok created
        }

        match found with
        | Error problem ->
            if problem.Code = TextDiffFailureCodes.ContinuationMismatch then
                match worker.Slots.TryGetValue host.Generation with
                | true, slot when isPreparing slot -> do! disposeSlot worker slot
                | _ -> ()

            return Error problem
        | Ok slot ->
            slot.Host <- host
            slot.Busy <- true
            slot.Continuation <- None
            worker.ActiveOpen <- Some host
            let stopTicker = startTicker host (progressOf slot)
            let meter = Meter.create worker.Clock Limits.defaults

            let! result = async {
                try
                    // A new slot has no sources yet. A resumed one holds the sources of the earlier step.
                    do! checkSources slot
                    let! step = runOpen worker host request owner slot meter

                    // Reads do not check identity, so an edit during the step surfaces here.
                    match step with
                    | Finish(Error _) -> ()
                    | _ -> do! checkSources slot

                    match step with
                    | Finish outcome -> return outcome
                    | Pause ->
                        let continuation = "prep-" + NodeInterop.randomUuid().Replace("-", "")
                        slot.Continuation <- Some continuation

                        let progress: ScanProgress = {
                            ValidatedBytes = slot.Validated
                            TotalBytes = slot.Total
                            ScanComplete = false
                        }

                        return Ok(Resumable.Scanning(progress, continuation, None))
                    | Proceed -> return Error(failure TextDiffFailureCodes.WorkerFailed "The diff open ended without a result.")
                with error ->
                    return Error(describeError slot.Failure error)
            }

            stopTicker ()
            worker.ActiveOpen <- None
            slot.Busy <- false
            slot.LastUsed <- performanceNow ()

            let keep =
                match result with
                | Ok(Resumable.Ready(OpenDiffResult.Opened _))
                | Ok(Resumable.Scanning _) -> true
                | _ -> false

            if not keep then
                do! disposeSlot worker slot

                if host.IsCanceled() then
                    host.ReleaseSession()

            return result
    }


let private findActive (worker: Worker) (host: WorkerHost) (handle: DiffHandle) : Result<Slot, OperationFailure> =
    match worker.Slots.TryGetValue host.Generation with
    | true, slot when slot.Version = handle.Version ->
        match slot.Phase with
        | SlotPhase.Active -> Ok slot
        | SlotPhase.Invalid problem -> Error problem
        | _ -> Error(closedFailure ())
    | _ -> Error(closedFailure ())

/// Maps an engine answer and invalidates the session when the answer says its content or sources are gone.
let private settle (worker: Worker) (slot: Slot) (result: EngineResult<'T>) : Async<Result<'T, OperationFailure>> = async {
    match result with
    | EngineResult.Ok value -> return Ok value
    | EngineResult.Canceled -> return Error(canceledFailure ())
    | EngineResult.Failed(code, message, detail) ->
        let! candidates = async {
            match detail with
            | Some blocked when code = TextDiffFailureCodes.ContentNotText && slot.Failure.IsNone ->
                return! encodingMismatchCandidates worker slot blocked
            | _ -> return None
        }

        let problem =
            match slot.Failure, detail, candidates with
            | Some value, _, _ -> sourceFailure value
            | None, Some blocked, Some _ ->
                let side = if blocked.Side = DiffSide.Previous then "previous" else "current"

                {
                    (failure TextDiffFailureCodes.EncodingMismatch $"The {side} source was read as UTF-8 and holds bytes that are invalid in UTF-8 but valid in Windows-1252 ({blocked.Evidence}). Open the diff again with an encoding chosen.") with
                        DiffDetail = detail
                }
            | None, _, _ -> { (failure code message) with DiffDetail = detail }

        if
            slot.Failure.IsSome
            || code = TextDiffFailureCodes.ContentNotText
            || code = TextDiffFailureCodes.SourceChanged
        then
            do! invalidate worker slot problem

        return Error problem
}

let private callOn
    (worker: Worker)
    (host: WorkerHost)
    (handle: DiffHandle)
    (work: Slot -> Async<Result<'T, OperationFailure>>)
    : JS.Promise<Result<'T, OperationFailure>> =
    match findActive worker host handle with
    | Error problem -> Promise.lift (Error problem)
    | Ok slot ->
        async {
            slot.Busy <- true
            slot.Host <- host
            let stop = startTicker host (progressOf slot)

            try
                try
                    return! work slot
                with error ->
                    let problem = describeError slot.Failure error

                    if slot.Failure.IsSome || (error :? TextDiffSourceException) then
                        do! invalidate worker slot problem

                    return Error problem
            finally
                stop ()
                slot.Busy <- false
                slot.LastUsed <- performanceNow ()
        }
        |> Async.StartAsPromise

/// Runs an engine call that reads source bytes. The sources are checked before and after it.
let private readingCallOn
    (worker: Worker)
    (host: WorkerHost)
    (handle: DiffHandle)
    (call: Slot -> TextDiffSession -> Async<EngineResult<'T>>)
    : JS.Promise<Result<'T, OperationFailure>> =
    callOn worker host handle (fun slot -> async {
        match slot.Session with
        | None -> return Error(closedFailure ())
        | Some session ->
            do! checkSources slot
            slot.SpoolWaitYield <- false
            let! result = call slot session
            do! checkSources slot
            return! settle worker slot result
    })

let private readingCall
    (worker: Worker)
    (host: WorkerHost)
    (handle: DiffHandle)
    (call: TextDiffSession -> Async<EngineResult<'T>>)
    : JS.Promise<Result<'T, OperationFailure>> =
    readingCallOn worker host handle (fun _ session -> call session)

let private createHandler
    (runShort: WorkerHost -> TextDiffOwner -> string[] -> int option -> JS.Promise<GitShort>)
    (idleMs: float)
    : ITextDiffRequestHandler =
    let worker = {
        RunShort = runShort
        Tokens = PreparationTokenStore()
        BlobLedger = Ledger()
        Slots = Dictionary<int, Slot>()
        ActiveOpen = None
        Clock = { new IClock with member _.NowMs() = performanceNow () }
        IdleMs = idleMs
        Sweeping = false
    }

    { new ITextDiffRequestHandler with
        member _.Open(host, request, owner) =
            openRequest worker host request owner |> Async.StartAsPromise

        member _.ReadPage(host, request) =
            // Only a page read can run in the background. The flag lets a memory session stop it earlier.
            readingCallOn worker host request.Handle (fun slot session -> async {
                slot.Background <- request.Background

                try
                    return! session.ReadPage request.Cursor host.IsCanceled
                finally
                    slot.Background <- false
            })

        member _.ReplayPage(host, request) =
            callOn worker host request.Handle (fun slot -> async {
                match slot.Session with
                | None -> return Error(closedFailure ())
                | Some session ->
                    let! result = session.ReplayPage request.PageId
                    return! settle worker slot result
            })

        member _.Expand(host, request) =
            readingCall worker host request.Handle (fun session ->
                session.Expand(
                    request.GapId,
                    request.FromStart,
                    request.Count,
                    request.Continuation,
                    host.IsCanceled
                ))

        member _.ReadLine(host, request) =
            readingCall worker host request.Handle (fun session ->
                session.ReadLine(
                    request.Side,
                    request.Line,
                    request.OffsetUtf16,
                    request.MaxUtf16,
                    request.Continuation,
                    host.IsCanceled
                ))

        member _.GetSourceInfo(host, request) =
            callOn worker host request.Handle (fun slot -> async {
                match slot.Infos, slot.Session with
                | Some(previousInfo, currentInfo), Some session ->
                    let previousState, currentState = session.SourceInfo

                    return
                        Ok(
                            { previousInfo with LineCount = previousState.LineCount },
                            { currentInfo with LineCount = currentState.LineCount }
                        )
                | _ -> return Error(closedFailure ())
            })

        member _.Close(host, handle) =
            match worker.Slots.TryGetValue host.Generation with
            | true, slot when handle |> Option.forall (fun requested -> slot.Version = requested.Version) ->
                async {
                    do! disposeSlot worker slot
                    host.ReleaseSession()
                }
                |> Async.StartAsPromise
            | _ -> Promise.lift ()

        member _.Cancel requestId =
            match worker.ActiveOpen with
            | Some host when host.RequestId = requestId -> host.ReleaseRequest()
            | _ -> ()
    }

/// Creates the handler that opens Git diff sessions. Open resolves both sources, classifies their text and
/// answers the first page or a blocker. Calls on a handle go to the engine session of that handle. The handler
/// keeps the preparation tokens and the sessions of its worker. The runner takes no output limit, so a handler
/// built with it cannot read a blob into memory. Only createDefaultHandler can serve a memory session.
let createDefaultHandlerWithRunner
    (runShort: WorkerHost -> TextDiffOwner -> string[] -> JS.Promise<GitShort>)
    : ITextDiffRequestHandler =
    createHandler (fun host owner arguments _ -> runShort host owner arguments) DefaultIdleMs

let createDefaultHandler () : ITextDiffRequestHandler =
    createHandler
        (fun host owner arguments outputLimit -> host.SpawnShort(owner.WorkspaceRoot, arguments, ?outputLimit = outputLimit))
        DefaultIdleMs

/// Serves text diff requests on the given port with the given handler. It switches the async trampoline of the
/// whole thread to setImmediate, so it runs only on a worker thread.
let bootstrapWith (port: NodeWorkerThreads.MessagePort) (handler: ITextDiffRequestHandler) : unit =
    AsyncTrampoline.switchToSetImmediate ()
    let receive = attachDispatcher ((fun message -> port.postMessage message), (fun () -> port.close ()), handler)
    port.onMessage receive

/// Serves text diff requests on the given port with a new default handler.
let bootstrap (port: NodeWorkerThreads.MessagePort) : unit = bootstrapWith port (createDefaultHandler ())
