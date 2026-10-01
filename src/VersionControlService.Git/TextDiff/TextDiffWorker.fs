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

module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodePositionalFile = VersionControlService.Runtime.Node.PositionalFile
module NodeWorkerThreads = VersionControlService.Runtime.Node.WorkerThreads

/// Progress messages go out about four times per second.
[<Literal>]
let private ProgressIntervalMs = 225

/// The largest response envelope of a page, an Open or an expansion.
[<Literal>]
let private PageEnvelopeLimit = 524288

/// The largest response envelope of a line slice.
[<Literal>]
let private LineEnvelopeLimit = 65536

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
    | Ambiguous of EncodingCandidate[]
    | Evidence of string

/// The Git child that fills the spool of one blob side.
type private SpoolStart = {
    SpoolPath: string
    Closed: JS.Promise<NodeProcess.ChildExit>
    Retained: byte[] option
    BlobSize: int64
    SpawnFailure: string option ref
}

/// One source of a session, with what the worker needs to check and release it.
type private OpenedSide = {
    Resolved: ResolvedSide
    SideLength: int64
    Src: IByteSource option
    IsSpool: bool
    Check: unit -> Async<unit>
    Release: unit -> JS.Promise<unit>
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
    RunShort: WorkerHost -> TextDiffOwner -> string[] -> JS.Promise<GitShort>
    Tokens: PreparationTokenStore
    BlobLedger: Ledger
    Scratch: WorkerScratch
    Slots: Dictionary<int, Slot>
    ActiveOpens: Dictionary<string, WorkerHost>
    Clock: IClock
    IdleMs: float
    mutable Sweeping: bool
}

let private noRelease () : JS.Promise<unit> = Promise.lift ()

let private noCheck () : Async<unit> = async.Return()

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

let private guardEnvelope (host: WorkerHost) (limit: int) (payload: ResultPayload) (value: 'T) : Result<'T, OperationFailure> =
    let length =
        envelopeByteLength (encode (TextDiffMessage.Result(host.RequestId, host.Generation, payload)))

    if length > limit then
        Error(failure TextDiffFailureCodes.WorkerFailed "The response exceeds the size limit of the protocol.")
    else
        Ok value


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
        | None -> Error(OperationFailure.create Validation "unsupported_encoding" "The chosen encoding is not supported.")

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
            worker.RunShort host owner arguments |> Async.AwaitPromise

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
                }
            )

            return Proceed
        | None, Some step -> return step
        | None, None -> return Pause
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
                    }
                )

                return Proceed
            | ResolvedSide.WorkingFile(path, identity) ->
                let! source = WorkingFileSource.Open(path, identity, onFailure = onFailure) |> Async.AwaitPromise

                slot.SetState(
                    side,
                    SideState.SideOpen {
                        Resolved = resolved
                        SideLength = identity.Size
                        Src = Some(source :> IByteSource)
                        IsSpool = false
                        Check = fun () -> source.CheckIdentity()
                        Release = fun () -> source.Dispose()
                    }
                )

                return Proceed
            | ResolvedSide.LfsObject(objectPath, size, objectIdentity, pointerIdentity) ->
                let pointerPath =
                    pointerIdentity
                    |> Option.map (fun _ -> NodePath.join [| slot.Owner.WorkspaceRoot; RepositoryPath.value request.Path |])

                let! source =
                    LfsObjectSource.Open(objectPath, size, objectIdentity, pointerPath, pointerIdentity, onFailure = onFailure)
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
                    }
                )

                return Proceed
            | ResolvedSide.GitBlob(oid, size) -> return! openBlobSide worker host slot side resolved oid size meter
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
                    | Candidates candidates -> slot.SetOutcome(side, Ambiguous candidates)
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

/// The byte offset of an invalid UTF-8 sequence that the engine reports as "invalid utf-8 sequence: ... at byte N".
let private invalidUtf8Offset (evidence: string) : int64 option =
    let marker = " at byte "
    let at = evidence.LastIndexOf(marker, StringComparison.Ordinal)

    if at < 0 || not (evidence.StartsWith("invalid " + Decoders.name TextEncoding.Utf8 + " sequence", StringComparison.Ordinal)) then
        None
    else
        match Int64.TryParse(evidence.Substring(at + marker.Length)) with
        | true, offset -> Some offset
        | _ -> None

/// Bytes on each side of an invalid sequence that the encoding previews show.
[<Literal>]
let private MismatchPreviewBytes = 512

/// Decodes UTF-8 and replaces each byte that does not start a valid sequence with U+FFFD.
let private lenientUtf8 (bytes: byte[]) (start: int) : string =
    let text = System.Text.StringBuilder()
    let mutable index = start

    while index < bytes.Length do
        let lead = int bytes[index]

        let width =
            if lead < 0x80 then 1
            elif lead >= 0xC2 && lead < 0xE0 then 2
            elif lead >= 0xE0 && lead < 0xF0 then 3
            elif lead >= 0xF0 && lead < 0xF5 then 4
            else 0

        let mutable valid = width > 0 && index + width <= bytes.Length

        let mutable scalar =
            if width = 1 then lead
            elif width = 2 then lead &&& 0x1F
            elif width = 3 then lead &&& 0x0F
            else lead &&& 0x07

        for follower = 1 to width - 1 do
            if valid then
                let next = int bytes[index + follower]

                if (next &&& 0xC0) = 0x80 then
                    scalar <- (scalar <<< 6) ||| (next &&& 0x3F)
                else
                    valid <- false

        valid <-
            valid
            && (width = 1
                || (width = 2 && scalar >= 0x80)
                || (width = 3 && scalar >= 0x800 && not (scalar >= 0xD800 && scalar <= 0xDFFF))
                || (width = 4 && scalar >= 0x10000 && scalar <= 0x10FFFF))

        if not valid then
            text.Append('\uFFFD') |> ignore
            index <- index + 1
        else
            if scalar < 0x10000 then
                text.Append(char scalar) |> ignore
            else
                text.Append(char (0xD800 + ((scalar - 0x10000) >>> 10))).Append(char (0xDC00 + ((scalar - 0x10000) &&& 0x3FF)))
                |> ignore

            index <- index + width

    text.ToString()

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
    match invalidUtf8Offset detail.Evidence, slot.OutcomeOf detail.Side, slot.StateOf detail.Side with
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
                            { Encoding = Decoders.name TextEncoding.Utf8; Preview = lenientUtf8 trimmed 0 }
                            { Encoding = Decoders.name TextEncoding.Windows1252; Preview = windows1252Text trimmed 0 }
                        |]
            | _ -> return None
    | _ -> return None
}

let private classifyStep
    (worker: Worker)
    (host: WorkerHost)
    (request: OpenDiffRequest)
    (owner: TextDiffOwner)
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
            | Ambiguous candidates, _ -> return requireEncoding worker request owner slot DiffSide.Previous candidates
            | _, Ambiguous candidates -> return requireEncoding worker request owner slot DiffSide.Current candidates
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

    let payload = ResultPayload.Open(Resumable.Ready(OpenDiffResult.Opened(handle, previous, current, Resumable.Ready page)))

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

        let engine: EngineHost = {
            Clock = worker.Clock
            Yield = fun () ->
                if slot.SpoolWaitYield then
                    slot.SpoolWaitYield <- false
                    sleep SpoolPollMs
                else
                    host.Yield() |> Async.AwaitPromise
            CreateTempStore =
                fun name -> async {
                    let! store =
                        NodeTempStore.Create(host.TempDirectory, $"{name}-{NodeInterop.randomUuid()}.tmp")
                        |> Async.AwaitPromise

                    return store :> ITempStore
                }
        }

        let! session =
            TextDiffSession.createWithScratch
                worker.Scratch
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
            let result = Resumable.Ready(OpenDiffResult.Opened(handle, previousInfo, currentInfo, page))

            match guardEnvelope host PageEnvelopeLimit (ResultPayload.Open result) result with
            | Ok opened ->
                slot.Phase <- SlotPhase.Active
                slot.Continuation <- None
                request.Preparation |> Option.iter worker.Tokens.Release
                return Finish(Ok opened)
            | Error problem -> return Finish(Error problem)
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
                let! opened = openSidesStep worker host request slot meter

                match opened with
                | Proceed ->
                    let! classified = classifyStep worker host request owner slot meter previousChoice currentChoice

                    match classified with
                    | Proceed -> return! createStep worker host request owner slot
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
                    && existing.Request = stored
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
            worker.ActiveOpens[host.RequestId] <- host
            let stopTicker = startTicker host (progressOf slot)
            let meter = Meter.create worker.Clock Limits.defaults

            let! result = async {
                try
                    let! step = runOpen worker host request owner slot meter

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
            worker.ActiveOpens.Remove host.RequestId |> ignore
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
let private readingCall
    (worker: Worker)
    (host: WorkerHost)
    (handle: DiffHandle)
    (limit: int)
    (payload: 'T -> ResultPayload)
    (call: TextDiffSession -> Async<EngineResult<'T>>)
    : JS.Promise<Result<'T, OperationFailure>> =
    callOn worker host handle (fun slot -> async {
        match slot.Session with
        | None -> return Error(closedFailure ())
        | Some session ->
            do! checkSources slot
            slot.SpoolWaitYield <- false
            let! result = call session
            do! checkSources slot
            let! settled = settle worker slot result

            match settled with
            | Ok value -> return guardEnvelope host limit (payload value) value
            | Error problem -> return Error problem
    })

let private createHandler
    (runShort: WorkerHost -> TextDiffOwner -> string[] -> JS.Promise<GitShort>)
    (idleMs: float)
    : ITextDiffRequestHandler =
    let worker = {
        RunShort = runShort
        Tokens = PreparationTokenStore()
        BlobLedger = Ledger()
        Scratch = WorkerScratch()
        Slots = Dictionary<int, Slot>()
        ActiveOpens = Dictionary<string, WorkerHost>()
        Clock = { new IClock with member _.NowMs() = performanceNow () }
        IdleMs = idleMs
        Sweeping = false
    }

    { new ITextDiffRequestHandler with
        member _.Open(host, request, owner) =
            openRequest worker host request owner |> Async.StartAsPromise

        member _.ReadPage(host, request) =
            readingCall worker host request.Handle PageEnvelopeLimit ResultPayload.ReadPage (fun session ->
                session.ReadPage request.Cursor host.IsCanceled)

        member _.ReplayPage(host, request) =
            callOn worker host request.Handle (fun slot -> async {
                match slot.Session with
                | None -> return Error(closedFailure ())
                | Some session ->
                    let! result = session.ReplayPage request.PageId
                    let! settled = settle worker slot result

                    match settled with
                    | Ok page -> return guardEnvelope host PageEnvelopeLimit (ResultPayload.ReplayPage page) page
                    | Error problem -> return Error problem
            })

        member _.Expand(host, request) =
            readingCall worker host request.Handle PageEnvelopeLimit ResultPayload.Expand (fun session ->
                session.Expand(
                    request.GapId,
                    request.FromStart,
                    request.Count,
                    request.Continuation,
                    host.IsCanceled
                ))

        member _.ReadLine(host, request) =
            readingCall worker host request.Handle LineEnvelopeLimit ResultPayload.ReadLine (fun session ->
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
            | true, slot when handle.Id = "" || slot.Version = handle.Version ->
                async {
                    do! disposeSlot worker slot
                    host.ReleaseSession()
                }
                |> Async.StartAsPromise
            | _ -> Promise.lift ()

        member _.Cancel requestId =
            match worker.ActiveOpens.TryGetValue requestId with
            | true, host -> host.ReleaseRequest()
            | _ -> ()
    }

/// Creates the handler that opens Git diff sessions. Open resolves both sources, classifies their text and
/// answers the first page or a blocker. Calls on a handle go to the engine session of that handle. The handler
/// keeps the preparation tokens and the sessions of its worker.
let createDefaultHandlerWithRunner
    (runShort: WorkerHost -> TextDiffOwner -> string[] -> JS.Promise<GitShort>)
    : ITextDiffRequestHandler =
    createHandler runShort DefaultIdleMs

let createDefaultHandler () : ITextDiffRequestHandler =
    createDefaultHandlerWithRunner (fun host owner arguments -> host.SpawnShort(owner.WorkspaceRoot, arguments))

/// Serves text diff requests on the given port with the given handler.
let bootstrapWith (port: NodeWorkerThreads.MessagePort) (handler: ITextDiffRequestHandler) : unit =
    let receive = attachDispatcher ((fun message -> port.postMessage message), (fun () -> port.close ()), handler)
    port.onMessage receive

/// A shared instance of the default handler.
let defaultHandler: ITextDiffRequestHandler = createDefaultHandler ()

/// Serves text diff requests on the given port with a new default handler.
let bootstrap (port: NodeWorkerThreads.MessagePort) : unit = bootstrapWith port (createDefaultHandler ())
