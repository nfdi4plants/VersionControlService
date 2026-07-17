module VersionControlService.Tests.LakeFsApiTests

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.LakeFs.LakeFsTypes
open VersionControlService.Tests.NodePath
open Vitest

module LakeFsApi = VersionControlService.LakeFs.LakeFsApi
module LakeFsCredentials = VersionControlService.LakeFs.LakeFsCredentials
module LakeFsProviderFactory = VersionControlService.LakeFs.LakeFsProviderFactory

let private httpModule: obj = importAll "http"
let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

let private ctx (name: string) = OperationContext.detached name

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

Vitest.describe (
    "lakeFS API operational contract",
    fun () ->
        Vitest.test (
            "lakeFS v1.83 API classifies errors paginates and redacts",
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
                        LakeFsProviderFactory.createFactory { StateRoot = stateRoot } strategy

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
