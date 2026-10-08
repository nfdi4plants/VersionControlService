/// Worker entry for the worker_threads tests. With VCS_TEXT_DIFF_TEST_WORKER_MODE=default it serves the
/// default handler. With free-space:<file> it serves the default handler and takes the free space of the temp
/// drive from the number in that file. Otherwise Open streams the requested blob of HEAD into a spool file and
/// waits until canceled.
module VersionControlService.Tests.TextDiffTestWorker

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff
open VersionControlService.Git.TextDiff.TextDiffWorkerDispatcher

module NodePath = VersionControlService.Runtime.Node.Path
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodeWorkerThreads = VersionControlService.Runtime.Node.WorkerThreads

[<Emit("process.env.VCS_TEXT_DIFF_TEST_WORKER_MODE ?? ''")>]
let private workerMode () : string = jsNative

[<Emit("Buffer.from($0).toString('utf8')")>]
let private bytesToUtf8 (_bytes: byte[]) : string = jsNative

[<Import("setTimeout", "node:timers/promises")>]
let private delay (milliseconds: int) : JS.Promise<unit> = jsNative

[<Literal>]
let private FreeSpaceModePrefix = "free-space:"

/// The free space of the temp drive is the number in the file, which a test rewrites between requests.
let private freeBytesFromFile (file: string) (_directory: string) : JS.Promise<float option> = promise {
    let! text = NodeFileSystem.readFileAsync file NodeFileSystem.TextEncoding.Utf8
    return Some(float (text.Trim()))
}

let private defaultHandler = TextDiffWorker.createDefaultHandler ()

let private spoolingHandler: ITextDiffRequestHandler =
    { new ITextDiffRequestHandler with
        member _.Open(host, request, owner) = promise {
            // Two calls because the supervisor only allows a rev-parse that verifies a commit, not
            // one that resolves a path straight to a blob oid.
            let! commit = host.SpawnShort(owner.WorkspaceRoot, [| "rev-parse"; "--verify"; "--quiet"; "HEAD^{commit}" |])
            let commitOid = (bytesToUtf8 commit.Stdout).Trim()
            let! listing =
                host.SpawnShort(
                    owner.WorkspaceRoot,
                    [| "ls-tree"; "-z"; "--full-tree"; commitOid; "--"; RepositoryPath.value request.Path |]
                )
            let oid = (bytesToUtf8 listing.Stdout).Split('\t').[0].Split(' ').[2]
            let spoolPath = NodePath.join [| host.TempDirectory; $"{host.RequestId}.spool" |]
            // The blob result arrives only after the child ends, which in these tests happens after cancellation.
            NodeInterop.observePromise (host.SpawnBlob(owner.WorkspaceRoot, oid, spoolPath)) ignore ignore

            while not (host.IsCanceled()) do
                do! delay 10

            return Error(canceledFailure ())
          }

        member _.ReadPage(host, request) = defaultHandler.ReadPage(host, request)
        member _.ReplayPage(host, request) = defaultHandler.ReplayPage(host, request)
        member _.Expand(host, request) = defaultHandler.Expand(host, request)
        member _.ReadLine(host, request) = defaultHandler.ReadLine(host, request)
        member _.GetSourceInfo(host, request) = defaultHandler.GetSourceInfo(host, request)
        member _.Close(host, handle) = defaultHandler.Close(host, handle)
        member _.Cancel _ = ()
    }

let private slowResolverHandler =
    TextDiffWorker.createDefaultHandlerWithRunner(fun host owner arguments -> promise {
        let! result = host.SpawnShort(owner.WorkspaceRoot, arguments)

        match arguments with
        | [| "ls-tree"; "-z"; "--full-tree"; _; "--"; "large.txt" |] ->
            let header = (bytesToUtf8 result.Stdout).Split('\t').[0].Split(' ')
            let spoolPath = NodePath.join [| host.TempDirectory; $"{host.RequestId}.spool" |]
            let! _ = host.SpawnBlob(owner.WorkspaceRoot, header.[2], spoolPath)
            return result
        | _ -> return result
    })

let private slowPreparationHandler =
    let mutable delayed = false

    TextDiffWorker.createDefaultHandlerWithRunner(fun host owner arguments -> promise {
        let! result = host.SpawnShort(owner.WorkspaceRoot, arguments)

        match arguments with
        | [| "cat-file"; "-s"; _ |] when not delayed ->
            delayed <- true
            do! delay 500
            return result
        | _ -> return result
    })

do
    match NodeWorkerThreads.parentPort with
    | Some port when workerMode () = "default" -> TextDiffWorker.bootstrap port
    | Some port when workerMode().StartsWith FreeSpaceModePrefix ->
        let file = workerMode().Substring FreeSpaceModePrefix.Length
        TextDiffWorker.bootstrapWith port (TextDiffWorker.createDefaultHandlerWithFreeBytes (freeBytesFromFile file))
    | Some port when workerMode () = "slow-resolver" -> TextDiffWorker.bootstrapWith port slowResolverHandler
    | Some port when workerMode () = "slow-preparation" -> TextDiffWorker.bootstrapWith port slowPreparationHandler
    | Some port -> TextDiffWorker.bootstrapWith port spoolingHandler
    | None -> ()
