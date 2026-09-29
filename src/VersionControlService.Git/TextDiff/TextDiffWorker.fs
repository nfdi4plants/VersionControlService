/// Entry points for a host-owned worker file.
module VersionControlService.Git.TextDiff.TextDiffWorker

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff.TextDiffProtocol
open VersionControlService.Git.TextDiff.TextDiffWorkerDispatcher
open VersionControlService.Git.TextDiff.TextDiffSourceResolver
open VersionControlService.Git.TextDiff.TextDiffPreparation

module NodePositionalFile = VersionControlService.Runtime.Node.PositionalFile
module NodeWorkerThreads = VersionControlService.Runtime.Node.WorkerThreads

/// The failure code of an Open whose sources could not be read.
[<Literal>]
let ReadFailedCode = "diff_read_failed"

[<Emit("$0?.code ?? ''")>]
let private errorCode (_error: obj) : string = jsNative

let private sessionClosed () =
    OperationFailure.create Validation TextDiffFailureCodes.SessionClosed "The diff session is closed."
    |> Error
    |> Promise.lift

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
let localFileHost (runGit: string[] -> Async<GitShort>) : IResolverHost =
    { new IResolverHost with
        member _.RunGit arguments = runGit arguments
        member _.Lstat path = lstat path
        member _.ReadPrefix path count = readPrefix path count
    }

let private openSources (tokens: PreparationTokenStore) (host: WorkerHost) (request: OpenDiffRequest) (owner: TextDiffOwner) = promise {
    let path = RepositoryPath.value request.Path
    let previousPath = request.PreviousPath |> Option.map RepositoryPath.value

    if request.Preparation.IsNone then
        tokens.ReleaseForPath(owner.WindowOwner, path)

    let input = {
        RepositoryRoot = owner.WorkspaceRoot
        LfsMediaDirectory = owner.LfsMediaDirectory
        Path = path
        PreviousPath = previousPath
    }

    // Git runs through the supervisor in the main process. Files are read inside the worker.
    let runGit arguments =
        host.SpawnShort(owner.WorkspaceRoot, arguments) |> Async.AwaitPromise

    let! outcome = resolve (localFileHost runGit) input |> Async.StartAsPromise

    if host.IsCanceled() then
        return Error(canceledFailure ())
    else
        match outcome with
        | ResolveOutcome.Blocked blocker -> return Ok(Resumable.Ready(OpenDiffResult.NotDiffable blocker))
        | ResolveOutcome.ReadError message -> return Error(OperationFailure.create ProviderError ReadFailedCode message)
        | ResolveOutcome.Resolved sources ->
            let binding = bindingOf path previousPath sources

            let validation =
                match request.Preparation with
                | Some token -> tokens.Validate(token, binding, owner.WindowOwner)
                | None -> Ok()

            match validation with
            | Error failure -> return Error failure
            | Ok() ->
                // The diff engine replaces this answer. Until it is connected, a diffable pair reports that the
                // provider cannot diff it.
                let result = Resumable.Ready(OpenDiffResult.NotDiffable DiffBlocker.ProviderUnsupported)

                match result, request.Preparation with
                | Resumable.Ready(OpenDiffResult.Opened _), Some token -> tokens.Release token
                | _ -> ()

                return Ok result
}

/// Creates the handler used until a diff engine is plugged in. Open resolves both sources and answers
/// ProviderUnsupported for a diffable pair. A blocked source answers its blocker, and an unreadable one fails
/// with diff_read_failed. Calls on a handle report a closed session and Close succeeds. The handler keeps the
/// preparation tokens of its worker.
let createDefaultHandler () : ITextDiffRequestHandler =
    let tokens = PreparationTokenStore()

    { new ITextDiffRequestHandler with
        member _.Open(host, request, owner) = openSources tokens host request owner

        member _.ReadPage(_, _) = sessionClosed ()
        member _.ReplayPage(_, _) = sessionClosed ()
        member _.Expand(_, _) = sessionClosed ()
        member _.ReadLine(_, _) = sessionClosed ()
        member _.GetSourceInfo(_, _) = sessionClosed ()
        member _.Close(_, _) = Promise.lift ()
        member _.Cancel _ = ()
    }

/// Serves text diff requests on the given port with the given handler.
let bootstrapWith (port: NodeWorkerThreads.MessagePort) (handler: ITextDiffRequestHandler) : unit =
    let receive = attachDispatcher ((fun message -> port.postMessage message), (fun () -> port.close ()), handler)
    port.onMessage receive

/// A shared instance of the default handler.
let defaultHandler: ITextDiffRequestHandler = createDefaultHandler ()

/// Serves text diff requests on the given port with a new default handler.
let bootstrap (port: NodeWorkerThreads.MessagePort) : unit = bootstrapWith port (createDefaultHandler ())
