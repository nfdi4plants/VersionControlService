/// The text diff service of a Git session. Every call goes to the shared worker pool under an owner made of
/// the workspace, its LFS media directory and the window that made the call.
module VersionControlService.Git.TextDiff.GitTextDiffService

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff.TextDiffProtocol

module TextDiffPool = VersionControlService.Git.TextDiff.TextDiffPool

type GitTextDiffOptions = {
    /// The session calls this on a diff Open until it returns a pool. None fails that Open with
    /// diff_worker_failed, and the next Open asks again. The session keeps the first pool it gets, so the host
    /// must return the same pool on every call. A host with a ready pool passes `fun () -> Promise.lift (Some pool)`.
    Pool: unit -> JS.Promise<TextDiffPool.TextDiffPool option>
    /// The window that started the call. The host maps its operation id to that window.
    WindowOwnerOf: OperationContext -> string
}

let private sessionClosed () =
    Failed(OperationFailure.create Validation TextDiffFailureCodes.SessionClosed "The diff session is closed.")

/// Forwards to the pool that the first successful Open acquires. Handles stay bound to the window owner that
/// opened them, so a call from another window finds no session and a Close from another window changes
/// nothing. Until an Open acquires a pool, handle calls report a closed session. The second value closes the
/// pool's diffs of the workspace and does nothing without an acquired pool.
let create
    (options: GitTextDiffOptions)
    (workspaceRoot: string)
    (resolveMediaDirectory: OperationContext -> Async<Result<string, OperationFailure>>)
    : TextDiffService * (unit -> Async<unit>) =
    let acquired: TextDiffPool.TextDiffPool option ref = ref None

    let acquire () = async {
        if acquired.Value.IsNone then
            let! pool =
                async {
                    try
                        return! options.Pool() |> Async.AwaitPromise
                    with _ ->
                        return None
                }

            if acquired.Value.IsNone then
                acquired.Value <- pool

        return acquired.Value
    }

    let withService
        (pool: TextDiffPool.TextDiffPool)
        (context: OperationContext)
        (onMediaFailure: OperationFailure -> OperationResult<'T>)
        (call: TextDiffService -> Async<OperationResult<'T>>)
        : Async<OperationResult<'T>> =
        async {
            let! mediaDirectory = resolveMediaDirectory context

            match mediaDirectory with
            | Error failure -> return onMediaFailure failure
            | Ok directory ->
                let owner = {
                    WorkspaceRoot = workspaceRoot
                    LfsMediaDirectory = directory
                    WindowOwner = options.WindowOwnerOf context
                }

                return! call (pool.Service owner)
        }

    let withAcquired
        (context: OperationContext)
        (onMediaFailure: OperationFailure -> OperationResult<'T>)
        (onNoPool: OperationResult<'T>)
        (call: TextDiffService -> Async<OperationResult<'T>>)
        =
        match acquired.Value with
        | Some pool -> withService pool context onMediaFailure call
        | None -> async.Return onNoPool

    let service: TextDiffService = {
        Open =
            fun request context -> async {
                let! pool = acquire ()

                match pool with
                | Some pool -> return! withService pool context Failed (fun service -> service.Open request context)
                | None ->
                    return
                        Failed(
                            OperationFailure.create
                                ProviderError
                                TextDiffFailureCodes.WorkerFailed
                                "The text diff worker pool could not be set up."
                        )
            }
        ReadPage =
            fun request context ->
                withAcquired context Failed (sessionClosed ()) (fun service -> service.ReadPage request context)
        ReplayPage =
            fun request context ->
                withAcquired context Failed (sessionClosed ()) (fun service -> service.ReplayPage request context)
        Expand =
            fun request context ->
                withAcquired context Failed (sessionClosed ()) (fun service -> service.Expand request context)
        ReadLine =
            fun request context ->
                withAcquired context Failed (sessionClosed ()) (fun service -> service.ReadLine request context)
        GetSourceInfo =
            fun request context ->
                withAcquired context Failed (sessionClosed ()) (fun service -> service.GetSourceInfo request context)
        Close =
            fun handle context ->
                withAcquired
                    context
                    (fun _ -> OperationResult.succeeded ())
                    (OperationResult.succeeded ())
                    (fun service -> service.Close handle context)
    }

    let closeWorkspace () =
        match acquired.Value with
        | Some pool -> pool.CloseWorkspace workspaceRoot |> Async.AwaitPromise
        | None -> async.Return()

    service, closeWorkspace

/// The service of a session created without a worker pool. The provider has no in-process diff, so Open
/// fails with diff_worker_failed. No handle can exist, so handle calls report a closed session.
let unavailable: TextDiffService = {
    Open =
        fun _ _ -> async {
            return
                Failed(
                    OperationFailure.create
                        ProviderError
                        TextDiffFailureCodes.WorkerFailed
                        "The Git session was created without a text diff worker pool."
                )
        }
    ReadPage = fun _ _ -> async { return sessionClosed () }
    ReplayPage = fun _ _ -> async { return sessionClosed () }
    Expand = fun _ _ -> async { return sessionClosed () }
    ReadLine = fun _ _ -> async { return sessionClosed () }
    GetSourceInfo = fun _ _ -> async { return sessionClosed () }
    Close = fun _ _ -> async { return OperationResult.succeeded () }
}
