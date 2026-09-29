/// Entry points for a host-owned worker file.
module VersionControlService.Git.TextDiff.TextDiffWorker

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff.TextDiffWorkerDispatcher

module NodeWorkerThreads = VersionControlService.Runtime.Node.WorkerThreads

let private sessionClosed () =
    OperationFailure.create Validation TextDiffFailureCodes.SessionClosed "The diff session is closed."
    |> Error
    |> Promise.lift

/// The handler used until a diff engine is plugged in. Open reports that no diff is available,
/// calls on a handle report a closed session and Close succeeds.
let defaultHandler: ITextDiffRequestHandler =
    { new ITextDiffRequestHandler with
        member _.Open(_, _, _) =
            Promise.lift (Ok(Resumable.Ready(OpenDiffResult.NotDiffable DiffBlocker.ProviderUnsupported)))

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

/// Serves text diff requests on the given port with the default handler.
let bootstrap (port: NodeWorkerThreads.MessagePort) : unit = bootstrapWith port defaultHandler
