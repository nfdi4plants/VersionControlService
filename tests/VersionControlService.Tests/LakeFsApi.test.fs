module VersionControlService.Tests.LakeFsApiTests

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.LakeFs.LakeFsTypes
open VersionControlService.Tests.NodePath
open Vitest

module LakeFsApi = VersionControlService.LakeFs.LakeFsApi
module LakeFsRequestOutput = VersionControlService.LakeFs.LakeFsRequestOutput
module LakeFsCredentials = VersionControlService.LakeFs.LakeFsCredentials
module LakeFsProviderFactory = VersionControlService.LakeFs.LakeFsProviderFactory
module NodeInterop = VersionControlService.Runtime.Node.Interop

let private httpModule: obj = importAll "http"
let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

[<Emit("(() => { const b = Buffer.alloc($0, 0xa5); b[0] = 0; b[1] = 255; b[2] = 254; b[3] = 253; return b; })()")>]
let private invalidUtf8Payload (_length: int) : obj = jsNative

[<Emit("Buffer.concat($0)")>]
let private concatBuffers (_buffers: obj[]) : obj = jsNative

[<Emit("Buffer.compare($0, $1) === 0")>]
let private buffersEqual (_left: obj) (_right: obj) : bool = jsNative

let private ctx (name: string) = OperationContext.detached name

[<Emit("globalThis.fetch = $0")>]
let private setFetch (_fetch: obj) : unit = jsNative

/// A fetch that rejects the way undici does: a TypeError whose cause carries the error code.
[<Emit("(async () => { throw Object.assign(new TypeError($0), { cause: Object.assign(new Error($1), { code: $2 }) }); })")>]
let private fetchRejectingWith (_outerMessage: string) (_causeMessage: string) (_causeCode: string) : obj = jsNative

/// The same, with the AggregateError that Node raises for a connect to several addresses.
[<Emit("(async () => { throw Object.assign(new TypeError('fetch failed'), { cause: Object.assign(new AggregateError([], $0), { code: $1 }) }); })")>]
let private fetchRejectingWithAggregate (_causeMessage: string) (_causeCode: string) : obj = jsNative

/// A fetch that answers with a status line and then fails the body the way undici does.
/// Both reading the text and streaming the body reject with the timeout.
[<Emit("(() => { const timeout = () => Object.assign(new TypeError('terminated'), { cause: Object.assign(new Error($0), { code: 'UND_ERR_BODY_TIMEOUT' }) }); return async () => ({ status: 200, headers: { get: () => null }, text: async () => { throw timeout(); }, body: new ReadableStream({ start(controller) { controller.error(timeout()); } }) }); })()")>]
let private fetchWithTimedOutBody (_causeMessage: string) : obj = jsNative

/// A fetch that is canceled by the caller and then fails with a timeout error.
[<Emit("(async () => { $0(); throw Object.assign(new TypeError('fetch failed'), { cause: Object.assign(new Error('Headers Timeout Error'), { code: 'UND_ERR_HEADERS_TIMEOUT' }) }); })")>]
let private fetchCancelingThenTimingOut (_cancel: unit -> unit) : obj = jsNative

let private withFetch (replacement: obj) (run: unit -> JS.Promise<unit>) : JS.Promise<unit> = promise {
    let originalFetch: obj = emitJsExpr () "globalThis.fetch"
    setFetch replacement

    try
        do! run ()
    finally
        setFetch originalFetch
}

let private withTempDirectory (run: string -> JS.Promise<unit>) : JS.Promise<unit> = promise {
    let! root =
        fsPromisesDynamic?mkdtemp (
            join [|
                osDynamic?tmpdir () |> unbox<string>
                "vcs-lakefs-timeout-"
            |]
        )
        |> unbox<JS.Promise<string>>

    try
        do! run root
    finally
        fsPromisesDynamic?rm (root, createObj [ "recursive" ==> true; "force" ==> true ])
        |> ignore
}

/// A recorded request the fake server observed.
type private RecordedRequest = {
    Method: string
    Url: string
    Authorization: string
}

/// Fake lakeFS server: a route table of (method, url-prefix) -> (status, body).
/// "hang" routes never respond (cancellation tests).
type private FakeServer = {
    Port: int
    Requests: ResizeArray<RecordedRequest>
    Close: unit -> unit
}

let private startFakeServer (routes: (string * string * int * string) list) : JS.Promise<FakeServer> =
    promise {
        let requests = ResizeArray<RecordedRequest>()

        let handler =
            fun (request: obj) (response: obj) ->
                let method: string = unbox request?method
                let url: string = unbox request?url

                let authorization: string =
                    request?headers?authorization
                    |> Option.ofObj
                    |> Option.map unbox<string>
                    |> Option.defaultValue ""

                requests.Add {
                    Method = method
                    Url = url
                    Authorization = authorization
                }

                let matched =
                    routes
                    |> List.tryFind (fun (routeMethod, routePrefix, _, _) ->
                        routeMethod = method && url.StartsWith routePrefix)

                match matched with
                | Some(_, _, -1, _) ->
                    // Hang: never respond, for cancellation coverage.
                    ()
                | Some(_, _, status, body) ->
                    response?writeHead (status, createObj [ "Content-Type" ==> "application/json" ])
                    |> ignore

                    response?``end`` (body) |> ignore
                | None ->
                    response?writeHead (404, createObj [ "Content-Type" ==> "application/json" ])
                    |> ignore

                    response?``end`` ("""{"message":"route not found"}""") |> ignore

        let server = httpModule?createServer (handler)

        let! port =
            Fable.Core.JS.Constructors.Promise.Create(fun resolve _ ->
                server?listen (
                    0,
                    fun () ->
                        let address = server?address ()
                        resolve (unbox<int> address?port)
                )
                |> ignore)

        return {
            Port = port
            Requests = requests
            Close = fun () -> server?close () |> ignore
        }
    }

let private connectionFor (server: FakeServer) : LakeFsConnection = {
    Endpoint = $"http://127.0.0.1:{server.Port}"
    AccessKeyId = "AKIA-test"
    SecretAccessKey = "wJalrXUtnFEMI-supersecret"
}

/// A fake server whose handler answers every request itself.
let private startHandlerServer (handler: obj -> obj -> unit) : JS.Promise<FakeServer> = promise {
    let server = httpModule?createServer (handler)

    let! port =
        Fable.Core.JS.Constructors.Promise.Create(fun resolve _ ->
            server?listen (0, fun () -> resolve (unbox<int> (server?address ())?port))
            |> ignore
        )

    return {
        Port = port
        Requests = ResizeArray<RecordedRequest>()
        Close = fun () -> server?close () |> ignore
    }
}

[<Emit("Buffer.from($0, 'utf8').toString('base64')")>]
let private toBase64 (_value: string) : string = jsNative

[<Emit("encodeURIComponent($0)")>]
let private encodeUriComponent (_value: string) : string = jsNative

/// A context that collects every delivered batch.
let private collectOutput (cancellation: OperationCancellation) =
    let batches = ResizeArray<OperationOutput[]>()

    let context =
        OperationContext.createWithOutput "lakefs-output" cancellation ignore batches.Add

    batches, context

let private flattenBatches (batches: ResizeArray<OperationOutput[]>) : OperationOutput[] =
    batches |> Seq.collect id |> Seq.toArray

/// The credential values of a connection, as written and as the Basic payload of the Authorization header.
let private credentialValues (connection: LakeFsConnection) : string[] = [|
    connection.AccessKeyId
    connection.SecretAccessKey
    toBase64 $"{connection.AccessKeyId}:{connection.SecretAccessKey}"
|]

let private expectNoCredentials (connection: LakeFsConnection) (texts: string[]) =
    for text in texts do
        for value in credentialValues connection do
            Vitest.expect(text.Contains value).toBe false

Vitest.describe (
    "lakeFS streams binary objects",
    fun () ->
        Vitest.test (
            "round-trips invalid UTF-8 bytes reports chunks and cleans canceled downloads",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let payload = invalidUtf8Payload (4 * 1024 * 1024)
                let mutable uploaded: obj option = None
                let mutable slowTimer: int option = None

                let handler =
                    fun (request: obj) (response: obj) ->
                        let method: string = unbox request?method
                        let url: string = unbox request?url

                        if method = "GET" && url.Contains "path=binary.dat" then
                            response?writeHead (200, createObj [ "Content-Type" ==> "application/octet-stream" ])
                            |> ignore
                            response?``end`` (payload) |> ignore
                        elif method = "POST" && url.Contains "path=binary.dat" then
                            let chunks = ResizeArray<obj>()
                            request?on ("data", fun (chunk: obj) -> chunks.Add chunk) |> ignore
                            request?on (
                                "end",
                                fun () ->
                                    uploaded <- Some(concatBuffers (chunks.ToArray()))
                                    response?writeHead (201, createObj [ "Content-Type" ==> "application/json" ])
                                    |> ignore
                                    response?``end`` ("{}") |> ignore
                            )
                            |> ignore
                        elif method = "GET" && url.Contains "path=slow.dat" then
                            response?writeHead (200, createObj [ "Content-Type" ==> "application/octet-stream" ])
                            |> ignore

                            let timer =
                                JS.setInterval (fun () -> response?write (invalidUtf8Payload (256 * 1024)) |> ignore) 10

                            slowTimer <- Some timer
                            response?on ("close", fun () -> JS.clearInterval timer) |> ignore
                        else
                            response?writeHead 404 |> ignore
                            response?``end`` () |> ignore

                let server = httpModule?createServer (handler)

                let! port =
                    Fable.Core.JS.Constructors.Promise.Create(fun resolve _ ->
                        server?listen (
                            0,
                            fun () -> resolve (unbox<int> (server?address ())?port)
                        )
                        |> ignore)

                let connection = {
                    Endpoint = $"http://127.0.0.1:{port}"
                    AccessKeyId = "binary-access"
                    SecretAccessKey = "binary-secret"
                }

                let! root =
                    fsPromisesDynamic?mkdtemp (join [| osDynamic?tmpdir () |> unbox<string>; "vcs-lakefs-binary-" |])
                    |> unbox<JS.Promise<string>>

                try
                    let downloadedPath = join [| root; "downloaded.bin" |]
                    let progress = ResizeArray<float>()
                    let downloadContext =
                        OperationContext.create
                            "binary-download"
                            OperationCancellation.none
                            (fun report -> report.Completed |> Option.iter progress.Add)

                    let! downloaded =
                        LakeFsApi.downloadObjectToFile
                            connection
                            "repo"
                            "main"
                            "binary.dat"
                            downloadedPath
                            downloadContext
                        |> Async.StartAsPromise

                    match downloaded with
                    | Ok result ->
                        Vitest.expect(result.BytesCopied).toBe (float (4 * 1024 * 1024))
                        Vitest.expect(progress.Count > 1).toBe true
                    | Error failure -> failwith $"Binary download failed: {failure.Code}"

                    let! downloadedBytes = fsPromisesDynamic?readFile downloadedPath |> unbox<JS.Promise<obj>>
                    Vitest.expect(buffersEqual downloadedBytes payload).toBe true

                    let! upload =
                        LakeFsApi.uploadObjectFromFile
                            connection
                            "repo"
                            "main"
                            "binary.dat"
                            downloadedPath
                            (ctx "binary-upload")
                        |> Async.StartAsPromise

                    match upload with
                    | Ok result -> Vitest.expect(result.BytesCopied).toBe (float (4 * 1024 * 1024))
                    | Error failure -> failwith $"Binary upload failed: {failure.Code}"

                    Vitest.expect(uploaded |> Option.exists (fun bytes -> buffersEqual bytes payload)).toBe true

                    let canceledPath = join [| root; "canceled.bin" |]
                    let source = OperationCancellation.Source()
                    let cancelContext =
                        OperationContext.create
                            "binary-cancel"
                            source.Cancellation
                            (fun report ->
                                if report.Completed |> Option.defaultValue 0.0 > 0.0 then
                                    source.Cancel())

                    let! canceled =
                        LakeFsApi.downloadObjectToFile
                            connection
                            "repo"
                            "main"
                            "slow.dat"
                            canceledPath
                            cancelContext
                        |> Async.StartAsPromise

                    match canceled with
                    | Error failure -> Vitest.expect(failure.Category).toEqual Canceled
                    | Ok _ -> failwith "Expected the streaming download to be canceled."

                    let! canceledExists =
                        fsPromisesDynamic?access canceledPath
                        |> unbox<JS.Promise<obj>>
                        |> Promise.map (fun _ -> true)
                        |> Promise.catch (fun _ -> false)

                    Vitest.expect(canceledExists).toBe false
                finally
                    slowTimer |> Option.iter JS.clearInterval
                    server?close () |> ignore
                    fsPromisesDynamic?rm (root, createObj [ "recursive" ==> true; "force" ==> true ])
                    |> ignore
            }
        )

        Vitest.test (
            "keeps upload bytes intact when fetch attaches after the first progress report",
            TestOptions(timeout = 30000),
            fun () -> promise {
                let payload = invalidUtf8Payload (1024 * 1024 + 17)
                let expectedHash =
                    let hash = NodeInterop.createSha256Hash ()
                    NodeInterop.updateHash hash payload
                    NodeInterop.digestHashHex hash

                let mutable uploaded: obj option = None
                let handler =
                    fun (request: obj) (response: obj) ->
                        let method: string = unbox request?method

                        if method = "POST" then
                            let chunks = ResizeArray<obj>()
                            request?on ("data", fun (chunk: obj) -> chunks.Add chunk) |> ignore
                            request?on (
                                "end",
                                fun () ->
                                    uploaded <- Some(concatBuffers (chunks.ToArray()))
                                    response?writeHead (201, createObj [ "Content-Type" ==> "application/json" ])
                                    |> ignore
                                    response?``end`` ("{}") |> ignore
                            )
                            |> ignore
                        else
                            response?writeHead 404 |> ignore
                            response?``end`` () |> ignore

                let originalFetch: obj = emitJsExpr () "globalThis.fetch"
                let mutable releaseFetch: (unit -> unit) option = None
                let mutable gateTimer: int option = None

                let mutable server: obj option = None
                let mutable root: string option = None

                try
                    let serverValue = httpModule?createServer (handler)
                    server <- Some serverValue

                    let! port =
                        Fable.Core.JS.Constructors.Promise.Create(fun resolve _ ->
                            serverValue?listen (
                                0,
                                fun () -> resolve (unbox<int> (serverValue?address ())?port)
                            )
                            |> ignore)

                    let! temporaryRoot =
                        fsPromisesDynamic?mkdtemp (join [| osDynamic?tmpdir () |> unbox<string>; "vcs-lakefs-upload-gate-" |])
                        |> unbox<JS.Promise<string>>

                    root <- Some temporaryRoot
                    let sourcePath = join [| temporaryRoot; "payload.bin" |]

                    do!
                        fsPromisesDynamic?writeFile (sourcePath, payload)
                        |> unbox<JS.Promise<obj>>
                        |> Promise.map ignore

                    let fetchGate: JS.Promise<unit> =
                        Fable.Core.JS.Constructors.Promise.Create(fun resolve reject ->
                            let timer =
                                JS.setTimeout
                                    (fun () ->
                                        gateTimer <- None
                                        reject (NodeInterop.createError "Timed out waiting for the first upload progress report."))
                                    5000

                            gateTimer <- Some timer
                            releaseFetch <-
                                Some(fun () ->
                                    gateTimer |> Option.iter JS.clearTimeout
                                    gateTimer <- None
                                    resolve ()))

                    let gatedFetch: obj =
                        emitJsExpr
                            (originalFetch, fetchGate)
                            "((original, gate) => (...args) => gate.then(() => original(...args)))($0, $1)"

                    // The gate resolves in the first progress callback. Native fetch then runs
                    // in a later microtask, after that source data event returns; the old data
                    // listener had already consumed the chunk before fetch attached.
                    emitJsStatement gatedFetch "globalThis.fetch = $0"

                    let connection: LakeFsConnection = {
                        Endpoint = $"http://127.0.0.1:{port}"
                        AccessKeyId = "binary-access"
                        SecretAccessKey = "binary-secret"
                    }

                    let mutable gateReleased = false
                    let uploadContext =
                        OperationContext.create
                            "binary-upload-late-fetch"
                            OperationCancellation.none
                            (fun report ->
                                if not gateReleased && (report.Completed |> Option.defaultValue 0.0) > 0.0 then
                                    gateReleased <- true
                                    releaseFetch |> Option.iter (fun release -> release ()))

                    let! upload =
                        LakeFsApi.uploadObjectFromFile
                            connection
                            "repo"
                            "main"
                            "binary-gated.dat"
                            sourcePath
                            uploadContext
                        |> Async.StartAsPromise

                    match upload with
                    | Ok result ->
                        Vitest.expect(result.BytesCopied).toBe (float (NodeInterop.bufferLength payload))
                        Vitest.expect(result.Sha256).toBe expectedHash
                    | Error failure -> failwith $"Binary upload failed: {failure.Code} ({failure.Message})"

                    match uploaded with
                    | Some bytes ->
                        Vitest.expect(NodeInterop.bufferLength bytes).toBe (NodeInterop.bufferLength payload)
                        Vitest.expect(buffersEqual bytes payload).toBe true

                        let receivedHash =
                            let hash = NodeInterop.createSha256Hash ()
                            NodeInterop.updateHash hash bytes
                            NodeInterop.digestHashHex hash

                        Vitest.expect(receivedHash).toBe expectedHash
                    | None -> failwith "The upload server did not receive a complete request body."
                finally
                    gateTimer |> Option.iter JS.clearTimeout
                    emitJsStatement originalFetch "globalThis.fetch = $0"
                    server |> Option.iter (fun value -> value?close () |> ignore)

                    root
                    |> Option.iter (fun path ->
                        fsPromisesDynamic?rm (path, createObj [ "recursive" ==> true; "force" ==> true ])
                        |> ignore)
            }
        )
)

Vitest.describe (
    "lakeFS API operational contract",
    fun () ->
        Vitest.test (
            "lakeFS API classifies errors paginates and redacts",
            TestOptions(timeout = 120000),
            fun () -> promise {
                // Pagination: two pages of objects.
                let pageOne =
                    """{"results":[{"path":"a.txt","checksum":"c1","size_bytes":1,"mtime":1},{"path":"b.txt","checksum":"c2","size_bytes":2,"mtime":2}],"pagination":{"has_more":true,"next_offset":"b.txt"}}"""

                let pageTwo =
                    """{"results":[{"path":"c.txt","checksum":"c3","size_bytes":3,"mtime":3}],"pagination":{"has_more":false,"next_offset":""}}"""

                let! paginationServer =
                    startFakeServer [
                        "GET", "/api/v1/repositories/repo/refs/main/objects/ls?amount=100&prefix=&after=b.txt", 200, pageTwo
                        "GET", "/api/v1/repositories/repo/refs/main/objects/ls", 200, pageOne
                    ]

                try
                    let connection = connectionFor paginationServer

                    let! listing =
                        Async.StartAsPromise(LakeFsApi.listObjects connection "repo" "main" "" (ctx "paginate"))

                    match listing with
                    | Ok objects ->
                        Vitest.expect(objects |> Array.map _.Path).toEqual ([| "a.txt"; "b.txt"; "c.txt" |])
                    | Error failure -> failwith $"Expected the paginated listing to succeed: {failure.Message}"

                    // Both pages were requested; every request carried Basic auth.
                    let listRequests =
                        paginationServer.Requests |> Seq.filter (fun r -> r.Url.Contains "objects/ls") |> Seq.toArray

                    Vitest.expect(listRequests.Length).toBe (2)

                    for request in listRequests do
                        Vitest.expect(request.Authorization.StartsWith "Basic ").toBe (true)
                finally
                    paginationServer.Close()

                // Error classification.
                let classificationCases = [
                    401, Authentication
                    403, Authorization
                    404, NotFound
                    409, Conflict
                ]

                for status, expectedCategory in classificationCases do
                    let! errorServer =
                        startFakeServer [
                            "GET", "/api/v1/repositories/repo/branches/main", status, """{"message":"denied"}"""
                        ]

                    try
                        let! result =
                            Async.StartAsPromise(
                                LakeFsApi.getBranch (connectionFor errorServer) "repo" "main" (ctx "classify")
                            )

                        match result with
                        | Error failure -> Vitest.expect(failure.Category).toEqual (expectedCategory)
                        | Ok _ -> failwith $"Expected status {status} to fail."
                    finally
                        errorServer.Close()

                // Redaction: a failure echoing credential material never leaks it.
                let! leakServer =
                    startFakeServer [
                        "GET",
                        "/api/v1/repositories/repo/branches/main",
                        401,
                        """{"message":"rejected Authorization: Basic QUtJQS10ZXN0OndK-echoed"}"""
                    ]

                try
                    let! result =
                        Async.StartAsPromise(
                            LakeFsApi.getBranch (connectionFor leakServer) "repo" "main" (ctx "redact")
                        )

                    match result with
                    | Error failure ->
                        Vitest.expect(failure.Message.Contains "QUtJQS10ZXN0").toBe (false)
                        Vitest.expect(failure.Message.Contains "[REDACTED]").toBe (true)
                    | Ok _ -> failwith "Expected the leaking response to fail."
                finally
                    leakServer.Close()

                // Cancellation stops a hanging transfer with a structured result.
                let! hangingServer =
                    startFakeServer [ "GET", "/api/v1/repositories/repo/branches/main", -1, "" ]

                try
                    let source = OperationCancellation.Source()

                    let cancelContext =
                        OperationContext.create "lakefs-cancel" source.Cancellation ignore

                    JS.setTimeout (fun () -> source.Cancel()) 100 |> ignore

                    let! result =
                        Async.StartAsPromise(
                            LakeFsApi.getBranch (connectionFor hangingServer) "repo" "main" cancelContext
                        )

                    match result with
                    | Error failure -> Vitest.expect(failure.Category).toEqual (Canceled)
                    | Ok _ -> failwith "Expected the canceled request to fail."
                finally
                    hangingServer.Close()

                // Network failures are retryable and classified.
                let unreachable: LakeFsConnection = {
                    Endpoint = "http://127.0.0.1:1"
                    AccessKeyId = "AKIA-test"
                    SecretAccessKey = "irrelevant"
                }

                let! networkResult =
                    Async.StartAsPromise(LakeFsApi.getBranch unreachable "repo" "main" (ctx "network"))

                match networkResult with
                | Error failure ->
                    Vitest.expect(failure.Category).toEqual (Network)
                    Vitest.expect(failure.Retryable).toBe (true)
                | Ok _ -> failwith "Expected the unreachable endpoint to fail."
            }
        )
)

Vitest.describe (
    "lakeFS factory provisioning contract",
    fun () ->
        Vitest.test (
            "lakeFS factory treats markers as hints and verifies access intents",
            TestOptions(timeout = 120000),
            fun () -> promise {
                // Probe: the lakectl marker is a detection hint, never a binding.
                let! markerRoot =
                    fsPromisesDynamic?mkdtemp (join [| osDynamic?tmpdir () |> unbox<string>; "vcs-lakefs-probe-" |])
                    |> unbox<JS.Promise<string>>

                try
                    do!
                        fsPromisesDynamic?writeFile (join [| markerRoot; ".lakefs_ref.yaml" |], "ref: main\n", "utf8")
                        |> unbox<JS.Promise<obj>>
                        |> Promise.map ignore

                    let! probeResult = Async.StartAsPromise(LakeFsProviderFactory.probe markerRoot)

                    match probeResult with
                    | Detected(root, _, bindingHint) ->
                        Vitest.expect(root).toBe (markerRoot)
                        Vitest.expect(bindingHint.IsSome).toBe (true)
                    | _ -> failwith "Expected the marker directory to be detected."

                    let! emptyProbe = Async.StartAsPromise(LakeFsProviderFactory.probe (join [| markerRoot; "sub" |]))

                    match emptyProbe with
                    | NotDetected -> ()
                    | _ -> failwith "Expected no detection without the marker."
                finally
                    fsPromisesDynamic?rm (markerRoot, createObj [ "recursive" ==> true; "force" ==> true ])
                    |> ignore

                // Access intents: reads granted, writes denied by the server.
                let stateRoot =
                    join [|
                        osDynamic?tmpdir () |> unbox<string>
                        $"vcs-lakefs-api-state-{DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()}"
                    |]

                let! server =
                    startFakeServer [
                        "GET", "/api/v1/repositories/repo", 200, """{"id":"repo","default_branch":"main"}"""
                        "POST", "/api/v1/repositories/repo/branches", 403, """{"message":"read only"}"""
                    ]

                try
                    let connection = connectionFor server

                    let mutable observedProfiles: string option list = []

                    let strategy: LakeFsCredentials.LakeFsCredentialStrategy = {
                        ResolveConnection =
                            fun profileId ->
                                async {
                                    observedProfiles <- profileId :: observedProfiles
                                    return Ok connection
                                }
                    }

                    let factory =
                        LakeFsProviderFactory.createFactory {
                            StateRoot = stateRoot
                            PathCaseSensitivity = CaseInsensitive
                        } strategy

                    let location: RepositoryLocation = {
                        ProviderId = LakeFsProviderFactory.lakeFsProviderId
                        DisplayName = None
                        ProviderLocation = "lakefs://repo/main/data/vault-a"
                        ConnectionProfileId = Some "lake-profile"
                    }

                    let! verifyResult =
                        Async.StartAsPromise(
                            factory.VerifyLocation
                                {
                                    Location = location
                                    Intents = [|
                                        ReadIntent
                                        WriteObjectIntent
                                        CreateRevisionIntent
                                        PublishIntent
                                    |]
                                }
                                (ctx "verify-intents")
                        )

                    match verifyResult with
                    | Succeeded outcome ->
                        Vitest.expect(outcome.Value.GrantedIntents).toEqual ([| ReadIntent |])
                        Vitest.expect(outcome.Value.DeniedIntents.Length).toBe (3)
                    | PartiallySucceeded _
                    | Failed _ -> failwith "Expected the intent verification to report granted and denied intents."

                    // The strategy saw the location's connection profile.
                    Vitest.expect(observedProfiles |> List.contains (Some "lake-profile")).toBe (true)

                    // Bindings come from the request alone and never carry secrets.
                    let! bindResult =
                        Async.StartAsPromise(
                            factory.Bind
                                {
                                    WorkspaceRoot = "/lake-workspace"
                                    Location = location
                                }
                                (ctx "bind")
                        )

                    match bindResult with
                    | Succeeded outcome ->
                        let serialized: string = emitJsExpr outcome.Value "JSON.stringify($0)"
                        Vitest.expect(serialized.Contains "supersecret").toBe (false)
                        Vitest.expect(serialized.Contains "AKIA-test").toBe (false)
                        Vitest.expect(outcome.Value.Location.ProviderLocation).toBe ("lakefs://repo/main/data/vault-a")
                    | PartiallySucceeded _
                    | Failed _ -> failwith "Expected the bind to succeed."
                finally
                    server.Close()
                    fsPromisesDynamic?rm (stateRoot, createObj [ "recursive" ==> true; "force" ==> true ])
                    |> ignore
            }
        )
)

Vitest.describe (
    "lakeFS API failure classification",
    fun () ->
        Vitest.test (
            "a transport error that mentions abort is a network failure when nothing was canceled",
            TestOptions(timeout = 30000),
            fun () -> promise {
                // The client only classifies a request as canceled when its own context asked
                // for it. Proxies and sockets report aborts of their own, and those are outages.
                let originalFetch: obj = emitJsExpr () "globalThis.fetch"

                emitJsStatement
                    ()
                    "globalThis.fetch = async () => { throw new Error('socket hang up: request aborted by upstream proxy'); }"

                try
                    let connection: LakeFsConnection = {
                        Endpoint = "http://127.0.0.1:1"
                        AccessKeyId = "AKIA-test"
                        SecretAccessKey = "irrelevant"
                    }

                    let! result =
                        Async.StartAsPromise(LakeFsApi.getBranch connection "repo" "main" (ctx "abort-text"))

                    match result with
                    | Error failure ->
                        Vitest.expect(failure.Category).toEqual (Network)
                        Vitest.expect(failure.Code).toBe ("network_failure")
                        Vitest.expect(failure.Retryable).toBe (true)
                    | Ok _ -> failwith "Expected the failing transport to report an error."
                finally
                    emitJsStatement originalFetch "globalThis.fetch = $0"
            }
        )
)

Vitest.describe (
    "lakeFS timeout classification",
    fun () ->
        let connection: LakeFsConnection = {
            Endpoint = "http://127.0.0.1:1"
            AccessKeyId = "AKIA-test"
            SecretAccessKey = "irrelevant"
        }

        let expectTimeout (detail: string) (failure: OperationFailure) =
            Vitest.expect(failure.Category).toEqual (Timeout)
            Vitest.expect(failure.Code).toBe ("timeout")
            Vitest.expect(failure.Retryable).toBe (true)
            Vitest.expect(failure.Message.Contains detail).toBe (true)

        let timeoutCases = [
            "UND_ERR_CONNECT_TIMEOUT", "Connect Timeout Error (attempted address: 10.0.0.1:80, timeout: 10000ms)"
            "UND_ERR_HEADERS_TIMEOUT", "Headers Timeout Error"
            "UND_ERR_BODY_TIMEOUT", "Body Timeout Error"
            "ETIMEDOUT", "read ETIMEDOUT"
        ]

        for code, detail in timeoutCases do
            Vitest.test (
                $"an API request that fails with {code} is a retryable timeout",
                TestOptions(timeout = 30000),
                fun () ->
                    withFetch
                        (fetchRejectingWith "fetch failed" detail code)
                        (fun () -> promise {
                            let! result =
                                Async.StartAsPromise(LakeFsApi.getBranch connection "repo" "main" (ctx "timeout"))

                            match result with
                            | Error failure -> expectTimeout detail failure
                            | Ok _ -> failwith "Expected the timed out transport to report an error."
                        })
            )

        for code in [ "UND_ERR_SOCKET"; "ECONNREFUSED"; "ECONNRESET" ] do
            Vitest.test (
                $"an API request that fails with {code} stays a network failure",
                TestOptions(timeout = 30000),
                fun () ->
                    withFetch
                        (fetchRejectingWith "fetch failed" "transport error" code)
                        (fun () -> promise {
                            let! result =
                                Async.StartAsPromise(LakeFsApi.getBranch connection "repo" "main" (ctx "network"))

                            match result with
                            | Error failure ->
                                Vitest.expect(failure.Category).toEqual (Network)
                                Vitest.expect(failure.Code).toBe ("network_failure")
                                Vitest.expect(failure.Retryable).toBe (true)
                            | Ok _ -> failwith "Expected the failing transport to report an error."
                        })
            )

        Vitest.test (
            "a timeout code on the AggregateError of a multi-address connect is a timeout",
            TestOptions(timeout = 30000),
            fun () ->
                withFetch
                    (fetchRejectingWithAggregate "connect ETIMEDOUT" "ETIMEDOUT")
                    (fun () -> promise {
                        let! result =
                            Async.StartAsPromise(LakeFsApi.getBranch connection "repo" "main" (ctx "aggregate"))

                        match result with
                        | Error failure -> expectTimeout "connect ETIMEDOUT" failure
                        | Ok _ -> failwith "Expected the timed out transport to report an error."
                    })
        )

        Vitest.test (
            "an API response whose body times out while it is read is a timeout",
            TestOptions(timeout = 30000),
            fun () ->
                withFetch
                    (fetchWithTimedOutBody "Body Timeout Error")
                    (fun () -> promise {
                        let! result = Async.StartAsPromise(LakeFsApi.getBranch connection "repo" "main" (ctx "body"))

                        match result with
                        | Error failure -> expectTimeout "Body Timeout Error" failure
                        | Ok _ -> failwith "Expected the timed out body to report an error."
                    })
        )

        Vitest.test (
            "a timeout error after the caller canceled stays canceled",
            TestOptions(timeout = 30000),
            fun () ->
                let source = OperationCancellation.Source()
                let context = OperationContext.create "canceled-timeout" source.Cancellation ignore

                withFetch
                    (fetchCancelingThenTimingOut (fun () -> source.Cancel()))
                    (fun () -> promise {
                        let! result = Async.StartAsPromise(LakeFsApi.getBranch connection "repo" "main" context)

                        match result with
                        | Error failure ->
                            Vitest.expect(failure.Category).toEqual (Canceled)
                            Vitest.expect(failure.Code).toBe ("operation_canceled")
                        | Ok _ -> failwith "Expected the canceled request to report an error."
                    })
        )

        Vitest.test (
            "a download whose request times out is a retryable timeout",
            TestOptions(timeout = 30000),
            fun () ->
                withTempDirectory (fun root ->
                    withFetch
                        (fetchRejectingWith "fetch failed" "Headers Timeout Error" "UND_ERR_HEADERS_TIMEOUT")
                        (fun () -> promise {
                            let! result =
                                LakeFsApi.downloadObjectToFile
                                    connection
                                    "repo"
                                    "main"
                                    "slow.dat"
                                    (join [| root; "slow.dat" |])
                                    (ctx "download-timeout")
                                |> Async.StartAsPromise

                            match result with
                            | Error failure -> expectTimeout "Headers Timeout Error" failure
                            | Ok _ -> failwith "Expected the timed out download to report an error."
                        })
                )
        )

        Vitest.test (
            "a download whose body times out while it streams is a timeout and leaves no file",
            TestOptions(timeout = 30000),
            fun () ->
                withTempDirectory (fun root ->
                    withFetch
                        (fetchWithTimedOutBody "Body Timeout Error")
                        (fun () -> promise {
                            let target = join [| root; "partial.dat" |]

                            let! result =
                                LakeFsApi.downloadObjectToFile
                                    connection
                                    "repo"
                                    "main"
                                    "slow.dat"
                                    target
                                    (ctx "download-body")
                                |> Async.StartAsPromise

                            match result with
                            | Error failure -> expectTimeout "Body Timeout Error" failure
                            | Ok _ -> failwith "Expected the timed out download to report an error."

                            let! exists =
                                fsPromisesDynamic?access target
                                |> unbox<JS.Promise<obj>>
                                |> Promise.map (fun _ -> true)
                                |> Promise.catch (fun _ -> false)

                            Vitest.expect(exists).toBe false
                        })
                )
        )

        Vitest.test (
            "an upload whose request times out is a retryable timeout",
            TestOptions(timeout = 30000),
            fun () ->
                withTempDirectory (fun root ->
                    withFetch
                        (fetchRejectingWith "fetch failed" "Headers Timeout Error" "UND_ERR_HEADERS_TIMEOUT")
                        (fun () -> promise {
                            let source = join [| root; "upload.dat" |]
                            do! fsPromisesDynamic?writeFile (source, "payload") |> unbox<JS.Promise<unit>>

                            let! result =
                                LakeFsApi.uploadObjectFromFile
                                    connection
                                    "repo"
                                    "main"
                                    "upload.dat"
                                    source
                                    (ctx "upload-timeout")
                                |> Async.StartAsPromise

                            match result with
                            | Error failure -> expectTimeout "Headers Timeout Error" failure
                            | Ok _ -> failwith "Expected the timed out upload to report an error."
                        })
                )
        )

        Vitest.test (
            "an upload that fails with a socket error stays a network failure",
            TestOptions(timeout = 30000),
            fun () ->
                withTempDirectory (fun root ->
                    withFetch
                        (fetchRejectingWith "fetch failed" "other side closed" "UND_ERR_SOCKET")
                        (fun () -> promise {
                            let source = join [| root; "upload.dat" |]
                            do! fsPromisesDynamic?writeFile (source, "payload") |> unbox<JS.Promise<unit>>

                            let! result =
                                LakeFsApi.uploadObjectFromFile
                                    connection
                                    "repo"
                                    "main"
                                    "upload.dat"
                                    source
                                    (ctx "upload-socket")
                                |> Async.StartAsPromise

                            match result with
                            | Error failure ->
                                Vitest.expect(failure.Category).toEqual (Network)
                                Vitest.expect(failure.Code).toBe ("network_failure")
                            | Ok _ -> failwith "Expected the failing upload to report an error."
                        })
                )
        )
)

Vitest.describe (
    "lakeFS output channel",
    fun () ->
        let isKind (kind: OperationOutputKind) (entry: OperationOutput) = entry.Kind = kind

        let texts (entries: OperationOutput[]) = entries |> Array.map _.Text

        let hasNoticeWith (text: string) (entries: OperationOutput[]) =
            entries
            |> Array.exists (fun entry -> isKind OperationOutputKind.Notice entry && entry.Text.Contains text)

        let commandIds (entries: OperationOutput[]) =
            entries |> Array.map _.CommandId |> Array.distinct

        let unreachable: LakeFsConnection = {
            Endpoint = "http://127.0.0.1:1"
            AccessKeyId = "AKIA-test"
            SecretAccessKey = "wJalrXUtnFEMI-supersecret"
        }

        Vitest.test (
            "a JSON request reports one command with its header and status and no credential",
            TestOptions(timeout = 30000),
            fun () -> promise {
                let! server =
                    startFakeServer [
                        "GET", "/api/v1/repositories/repo/branches/main", 200, """{"id":"main","commit_id":"c1"}"""
                    ]

                try
                    let connection = connectionFor server
                    let batches, context = collectOutput OperationCancellation.none
                    let! result = Async.StartAsPromise(LakeFsApi.getBranch connection "repo" "main" context)

                    match result with
                    | Ok branch -> Vitest.expect(branch.CommitId).toBe "c1"
                    | Error failure -> failwith $"Expected the branch lookup to succeed: {failure.Message}"

                    let entries = flattenBatches batches
                    let commands = entries |> Array.filter (isKind OperationOutputKind.Command)
                    Vitest.expect(commands.Length).toBe 1
                    Vitest.expect(commands[0].Text.Contains "GET").toBe true
                    Vitest.expect(commands[0].Text.Contains "/repositories/repo/branches/main").toBe true
                    Vitest.expect(hasNoticeWith "200" entries).toBe true
                    Vitest.expect((commandIds entries).Length).toBe 1
                    Vitest.expect(commands[0].CommandId = "").toBe false
                    expectNoCredentials connection (texts entries)
                    Vitest.expect(server.Requests[0].Authorization.StartsWith "Basic ").toBe true
                finally
                    server.Close()
            }
        )

        Vitest.test (
            "an error response reports its body as stderr and neither the output nor the failure keeps a secret",
            TestOptions(timeout = 30000),
            fun () -> promise {
                let secret = "s3cr3t/with+chars="
                let accessKey = "AKIAEXAMPLEKEY"
                let payload = toBase64 $"{accessKey}:{secret}"

                let body =
                    "{\"message\":\"rejected "
                    + secret
                    + " and "
                    + encodeUriComponent secret
                    + " and "
                    + payload
                    + " for "
                    + accessKey
                    + "\"}"

                let! server =
                    startFakeServer [
                        "GET", "/api/v1/repositories/repo/branches/main", 500, body
                    ]

                try
                    let connection = {
                        connectionFor server with
                            AccessKeyId = accessKey
                            SecretAccessKey = secret
                    }

                    let batches, context = collectOutput OperationCancellation.none
                    let! result = Async.StartAsPromise(LakeFsApi.getBranch connection "repo" "main" context)

                    let failure =
                        match result with
                        | Error failure -> failure
                        | Ok _ -> failwith "Expected the lookup to fail."

                    Vitest.expect(failure.Category).toEqual (ProviderError)

                    let entries = flattenBatches batches

                    Vitest.expect(hasNoticeWith "500" entries).toBe true

                    let redactedBody =
                        entries
                        |> Array.exists (fun entry ->
                            isKind OperationOutputKind.StdErr entry && entry.Text.Contains "[REDACTED]"
                        )

                    Vitest.expect(redactedBody).toBe true

                    let surfaces = Array.append (texts entries) [| failure.Message |]
                    expectNoCredentials connection surfaces

                    for text in surfaces do
                        Vitest.expect(text.Contains(encodeUriComponent secret)).toBe false

                    Vitest.expect(failure.Message.Contains "[REDACTED]").toBe true
                finally
                    server.Close()
            }
        )

        Vitest.test (
            "an object download reports its header and status and no body lines",
            TestOptions(timeout = 30000),
            fun () -> promise {
                let payload = invalidUtf8Payload (1024 * 1024)

                let! server =
                    startHandlerServer (fun _ response ->
                        response?writeHead (200, createObj [ "Content-Type" ==> "application/octet-stream" ])
                        |> ignore

                        response?``end`` (payload) |> ignore
                    )

                try
                    let connection = connectionFor server
                    let batches, context = collectOutput OperationCancellation.none

                    do!
                        withTempDirectory (fun root -> promise {
                            let! result =
                                LakeFsApi.downloadObjectToFile
                                    connection
                                    "repo"
                                    "main"
                                    "data/binary.dat"
                                    (join [| root; "downloaded.bin" |])
                                    context
                                |> Async.StartAsPromise

                            match result with
                            | Ok copied -> Vitest.expect(copied.BytesCopied).toBe (float (1024 * 1024))
                            | Error failure -> failwith $"Expected the download to succeed: {failure.Message}"
                        })

                    let entries = flattenBatches batches
                    let commands = entries |> Array.filter (isKind OperationOutputKind.Command)
                    Vitest.expect(commands.Length).toBe 1
                    Vitest.expect(commands[0].Text.Contains "GET").toBe true
                    Vitest.expect(commands[0].Text.Contains "/repositories/repo/refs/main/objects").toBe true
                    Vitest.expect(commands[0].Text.Contains "path=data%2Fbinary.dat").toBe true
                    Vitest.expect(hasNoticeWith "200" entries).toBe true
                    Vitest.expect(hasNoticeWith (string (1024 * 1024)) entries).toBe true
                    Vitest.expect(entries |> Array.exists (isKind OperationOutputKind.StdOut)).toBe false
                    Vitest.expect(entries |> Array.exists (isKind OperationOutputKind.StdErr)).toBe false
                    Vitest.expect((commandIds entries).Length).toBe 1
                    expectNoCredentials connection (texts entries)
                finally
                    server.Close()
            }
        )

        Vitest.test (
            "an upload reports its header and status and its command ends after the body is verified",
            TestOptions(timeout = 30000),
            fun () -> promise {
                let content = String.replicate 12345 "x"

                let! server =
                    startHandlerServer (fun request response ->
                        request?on ("data", fun (_: obj) -> ()) |> ignore

                        request?on (
                            "end",
                            fun () ->
                                response?writeHead (201, createObj [ "Content-Type" ==> "application/json" ])
                                |> ignore

                                response?``end`` ("{}") |> ignore
                        )
                        |> ignore
                    )

                try
                    let connection = connectionFor server
                    let batches, context = collectOutput OperationCancellation.none

                    do!
                        withTempDirectory (fun root -> promise {
                            let source = join [| root; "upload.dat" |]
                            do! fsPromisesDynamic?writeFile (source, content) |> unbox<JS.Promise<unit>>

                            let! result =
                                LakeFsApi.uploadObjectFromFile
                                    connection
                                    "repo"
                                    "workspace"
                                    "data/upload.dat"
                                    source
                                    context
                                |> Async.StartAsPromise

                            match result with
                            | Ok copied -> Vitest.expect(copied.BytesCopied).toBe (float content.Length)
                            | Error failure -> failwith $"Expected the upload to succeed: {failure.Message}"
                        })

                    let entries = flattenBatches batches
                    let commands = entries |> Array.filter (isKind OperationOutputKind.Command)
                    Vitest.expect(commands.Length).toBe 1
                    Vitest.expect(commands[0].Text.Contains "POST").toBe true
                    Vitest.expect(commands[0].Text.Contains "/repositories/repo/branches/workspace/objects").toBe true
                    Vitest.expect(hasNoticeWith "201" entries).toBe true
                    Vitest.expect(hasNoticeWith (string content.Length) entries).toBe true
                    Vitest.expect(entries |> Array.exists (isKind OperationOutputKind.StdOut)).toBe false
                    Vitest.expect((commandIds entries).Length).toBe 1
                    expectNoCredentials connection (texts entries)
                finally
                    server.Close()
            }
        )

        Vitest.test (
            "an upload that fails the local check sends no request and reports no command",
            TestOptions(timeout = 30000),
            fun () -> promise {
                let batches, context = collectOutput OperationCancellation.none

                do!
                    withTempDirectory (fun root -> promise {
                        let source = join [| root; "changed.dat" |]
                        do! fsPromisesDynamic?writeFile (source, "payload") |> unbox<JS.Promise<unit>>

                        let! result =
                            LakeFsApi.uploadObjectFromFileChecked
                                unreachable
                                "repo"
                                "workspace"
                                "data/changed.dat"
                                source
                                (fun _ ->
                                    Error(OperationFailure.create Validation "source_changed" "The file changed.")
                                )
                                context
                            |> Async.StartAsPromise

                        match result with
                        | Error failure -> Vitest.expect(failure.Code).toBe "source_changed"
                        | Ok _ -> failwith "Expected the upload of a changed file to fail."
                    })

                Vitest.expect(batches.Count).toBe 0
            }
        )

        Vitest.test (
            "a request without an output callback behaves as before and reports nothing",
            TestOptions(timeout = 30000),
            fun () -> promise {
                let! server =
                    startFakeServer [
                        "GET", "/api/v1/repositories/repo/branches/main", 200, """{"id":"main","commit_id":"c1"}"""
                    ]

                try
                    let observer, _ = collectOutput OperationCancellation.none
                    let context = ctx "no-output"
                    Vitest.expect(context.ReportOutput.IsNone).toBe true

                    let! result = Async.StartAsPromise(LakeFsApi.getBranch (connectionFor server) "repo" "main" context)

                    match result with
                    | Ok branch -> Vitest.expect(branch.CommitId).toBe "c1"
                    | Error failure -> failwith $"Expected the branch lookup to succeed: {failure.Message}"

                    Vitest.expect(server.Requests.Count).toBe 1
                    Vitest.expect(observer.Count).toBe 0
                finally
                    server.Close()
            }
        )

        Vitest.test (
            "a canceled request finishes its command and one canceled before it starts creates none",
            TestOptions(timeout = 30000),
            fun () -> promise {
                let! server = startFakeServer [ "GET", "/api/v1/repositories/repo/branches/main", -1, "" ]

                try
                    let source = OperationCancellation.Source()
                    let batches, context = collectOutput source.Cancellation
                    JS.setTimeout (fun () -> source.Cancel()) 100 |> ignore

                    let! result = Async.StartAsPromise(LakeFsApi.getBranch (connectionFor server) "repo" "main" context)

                    match result with
                    | Error failure -> Vitest.expect(failure.Category).toEqual (Canceled)
                    | Ok _ -> failwith "Expected the hanging request to be canceled."

                    // The command ended before the call returned, so the host already has every entry.
                    let entries = flattenBatches batches
                    Vitest.expect((entries |> Array.filter (isKind OperationOutputKind.Command)).Length).toBe 1
                    Vitest.expect(entries |> Array.exists (isKind OperationOutputKind.Notice)).toBe true
                    Vitest.expect(entries |> Array.exists (isKind OperationOutputKind.StdErr)).toBe false
                    Vitest.expect((commandIds entries).Length).toBe 1

                    let canceledSource = OperationCancellation.Source()
                    canceledSource.Cancel()
                    let canceledBatches, canceledContext = collectOutput canceledSource.Cancellation

                    let! early =
                        Async.StartAsPromise(LakeFsApi.getBranch (connectionFor server) "repo" "main" canceledContext)

                    match early with
                    | Error failure -> Vitest.expect(failure.Category).toEqual (Canceled)
                    | Ok _ -> failwith "Expected the request to be canceled before it started."

                    Vitest.expect(canceledBatches.Count).toBe 0
                finally
                    server.Close()
            }
        )

        Vitest.test (
            "follow-up work after a cancellation keeps the output callback and drops the cancellation",
            TestOptions(timeout = 30000),
            fun () -> promise {
                let! server =
                    startFakeServer [
                        "GET", "/api/v1/repositories/repo/branches/main", 200, """{"id":"main","commit_id":"c1"}"""
                    ]

                try
                    let source = OperationCancellation.Source()
                    let batches, context = collectOutput source.Cancellation
                    source.Cancel()

                    let followUp = LakeFsRequestOutput.withoutCancellation context "recovery"
                    Vitest.expect(followUp.Cancellation.IsCancellationRequested()).toBe false

                    let! result =
                        Async.StartAsPromise(LakeFsApi.getBranch (connectionFor server) "repo" "main" followUp)

                    match result with
                    | Ok branch -> Vitest.expect(branch.CommitId).toBe "c1"
                    | Error failure -> failwith $"Expected the follow-up request to succeed: {failure.Message}"

                    let entries = flattenBatches batches
                    Vitest.expect(entries |> Array.exists (isKind OperationOutputKind.Command)).toBe true
                    Vitest.expect(hasNoticeWith "200" entries).toBe true
                finally
                    server.Close()
            }
        )

        Vitest.test (
            "a burst of paginated requests keeps the entries of each command together within the batch size",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let pageCount = 25

                let! server =
                    startHandlerServer (fun request response ->
                        let url: string = unbox request?url
                        let marker = "after=o"
                        let markerIndex = url.IndexOf marker

                        let index =
                            if markerIndex < 0 then
                                0
                            else
                                int (url.Substring(markerIndex + marker.Length)) + 1

                        let hasMore = if index < pageCount - 1 then "true" else "false"

                        let body =
                            "{\"results\":[{\"id\":\"b"
                            + string index
                            + "\",\"commit_id\":\"c"
                            + string index
                            + "\"}],\"pagination\":{\"has_more\":"
                            + hasMore
                            + ",\"next_offset\":\"o"
                            + string index
                            + "\"}}"

                        response?writeHead (200, createObj [ "Content-Type" ==> "application/json" ])
                        |> ignore

                        response?``end`` (body) |> ignore
                    )

                try
                    let batches, context = collectOutput OperationCancellation.none

                    let! result = Async.StartAsPromise(LakeFsApi.listBranches (connectionFor server) "repo" context)

                    match result with
                    | Ok branches -> Vitest.expect(branches.Length).toBe pageCount
                    | Error failure -> failwith $"Expected the listing to succeed: {failure.Message}"

                    let entries = flattenBatches batches
                    let ids = entries |> Array.map _.CommandId

                    // Runs of equal ids: one per command when no command is split by another.
                    let runs =
                        ids
                        |> Array.fold
                            (fun (runs: string list) id ->
                                match runs with
                                | head :: _ when head = id -> runs
                                | _ -> id :: runs
                            )
                            []

                    Vitest.expect(runs.Length).toBe pageCount
                    Vitest.expect((List.distinct runs).Length).toBe pageCount
                    Vitest.expect(ids |> Array.exists (fun id -> id = "")).toBe false
                    Vitest.expect(batches |> Seq.forall (fun batch -> batch.Length <= 20)).toBe true

                    for id in commandIds entries do
                        let own = entries |> Array.filter (fun entry -> entry.CommandId = id)
                        Vitest.expect(isKind OperationOutputKind.Command (Array.head own)).toBe true
                finally
                    server.Close()
            }
        )

        Vitest.test (
            "a query value that looks like a credential is replaced in the header and in error text",
            TestOptions(timeout = 30000),
            fun () -> promise {
                let! server = startFakeServer []

                try
                    let connection = connectionFor server
                    let batches, context = collectOutput OperationCancellation.none

                    let! _ =
                        LakeFsApi.requestChecked
                            connection
                            "GET"
                            "/repositories/repo/branches?amount=5&signature=abc123token"
                            None
                            context
                        |> Async.StartAsPromise

                    let entries = flattenBatches batches
                    let header = (entries |> Array.find (isKind OperationOutputKind.Command)).Text
                    Vitest.expect(header.Contains "amount=5").toBe true
                    Vitest.expect(header.Contains "[REDACTED]").toBe true

                    for text in texts entries do
                        Vitest.expect(text.Contains "abc123token").toBe false

                    let redacted =
                        LakeFsRequestOutput.redactQueryValues
                            "see https://storage.example/b?X-Amz-Signature=zzz9&path=a%2Fb&after=o3"

                    Vitest.expect(redacted.Contains "zzz9").toBe false
                    Vitest.expect(redacted.Contains "path=a%2Fb").toBe true
                    Vitest.expect(redacted.Contains "after=o3").toBe true
                finally
                    server.Close()
            }
        )
)
