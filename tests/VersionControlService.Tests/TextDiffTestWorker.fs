/// Worker entry for the worker_threads tests. With VCS_TEXT_DIFF_TEST_WORKER_MODE=default it serves the
/// default handler. Otherwise Open streams the requested blob of HEAD into a spool file and waits until canceled.
module VersionControlService.Tests.TextDiffTestWorker

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff
open VersionControlService.Git.TextDiff.TextDiffWorkerDispatcher

module NodePath = VersionControlService.Runtime.Node.Path
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodeWorkerThreads = VersionControlService.Runtime.Node.WorkerThreads

[<Emit("process.env.VCS_TEXT_DIFF_TEST_WORKER_MODE ?? ''")>]
let private workerMode () : string = jsNative

[<Emit("Buffer.from($0).toString('utf8')")>]
let private bytesToUtf8 (_bytes: byte[]) : string = jsNative

[<Import("setTimeout", "node:timers/promises")>]
let private delay (milliseconds: int) : JS.Promise<unit> = jsNative

let private spoolingHandler: ITextDiffRequestHandler =
    { new ITextDiffRequestHandler with
        member _.Open(host, request, owner) = promise {
            let! resolved = host.SpawnShort(owner.WorkspaceRoot, [| "rev-parse"; $"HEAD:{RepositoryPath.value request.Path}" |])
            let oid = (bytesToUtf8 resolved.Stdout).Trim()
            let spoolPath = NodePath.join [| host.TempDirectory; $"{host.RequestId}.spool" |]
            // The blob result arrives only after the child ends, which in these tests happens after cancellation.
            NodeInterop.observePromise (host.SpawnBlob(owner.WorkspaceRoot, oid, spoolPath)) ignore ignore

            while not (host.IsCanceled()) do
                do! delay 10

            return Error(canceledFailure ())
          }

        member _.ReadPage(host, request) = TextDiffWorker.defaultHandler.ReadPage(host, request)
        member _.ReplayPage(host, request) = TextDiffWorker.defaultHandler.ReplayPage(host, request)
        member _.Expand(host, request) = TextDiffWorker.defaultHandler.Expand(host, request)
        member _.ReadLine(host, request) = TextDiffWorker.defaultHandler.ReadLine(host, request)
        member _.GetSourceInfo(host, request) = TextDiffWorker.defaultHandler.GetSourceInfo(host, request)
        member _.Close(host, handle) = TextDiffWorker.defaultHandler.Close(host, handle)
        member _.Cancel _ = ()
    }

do
    match NodeWorkerThreads.parentPort with
    | Some port when workerMode () = "default" -> TextDiffWorker.bootstrap port
    | Some port -> TextDiffWorker.bootstrapWith port spoolingHandler
    | None -> ()
