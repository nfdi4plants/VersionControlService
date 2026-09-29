/// The text diff service of a Git session. Every call goes to the shared worker pool under an owner made of
/// the workspace, its LFS media directory and the window that made the call.
module VersionControlService.Git.TextDiff.GitTextDiffService

open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff.TextDiffProtocol

module TextDiffPool = VersionControlService.Git.TextDiff.TextDiffPool

type GitTextDiffOptions = {
    Pool: TextDiffPool.TextDiffPool
    /// The window that started the call. The host maps its operation id to that window.
    WindowOwnerOf: OperationContext -> string
}

let private sessionClosed () =
    Failed(OperationFailure.create Validation TextDiffFailureCodes.SessionClosed "The diff session is closed.")

/// Forwards to the pool. Handles stay bound to the window owner that opened them, so a call from another
/// window finds no session and a Close from another window changes nothing.
let create
    (options: GitTextDiffOptions)
    (workspaceRoot: string)
    (resolveMediaDirectory: OperationContext -> Async<Result<string, OperationFailure>>)
    : TextDiffService =
    let withService (context: OperationContext) (onMediaFailure: OperationFailure -> OperationResult<'T>) (call: TextDiffService -> Async<OperationResult<'T>>) = async {
        let! mediaDirectory = resolveMediaDirectory context

        match mediaDirectory with
        | Error failure -> return onMediaFailure failure
        | Ok directory ->
            let owner = {
                WorkspaceRoot = workspaceRoot
                LfsMediaDirectory = directory
                WindowOwner = options.WindowOwnerOf context
            }

            return! call (options.Pool.Service owner)
    }

    {
        Open = fun request context -> withService context Failed (fun service -> service.Open request context)
        ReadPage = fun request context -> withService context Failed (fun service -> service.ReadPage request context)
        ReplayPage = fun request context -> withService context Failed (fun service -> service.ReplayPage request context)
        Expand = fun request context -> withService context Failed (fun service -> service.Expand request context)
        ReadLine = fun request context -> withService context Failed (fun service -> service.ReadLine request context)
        GetSourceInfo = fun request context -> withService context Failed (fun service -> service.GetSourceInfo request context)
        Close =
            fun handle context ->
                withService context (fun _ -> OperationResult.succeeded ()) (fun service -> service.Close handle context)
    }

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
