/// Pinned lakeFS API client: only the endpoints this provider needs,
/// with structured error classification, exhaustive pagination, injected Basic
/// credentials, cancellation through the portable contract, and redaction.
module VersionControlService.LakeFs.LakeFsApi

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.LakeFs.LakeFsTypes

module NodeCancellation = VersionControlService.Runtime.Node.Cancellation
module LakeFsBinaryTransfer = VersionControlService.LakeFs.LakeFsBinaryTransfer
module LakeFsTransportError = VersionControlService.LakeFs.LakeFsTransportError
module LakeFsRequestOutput = VersionControlService.LakeFs.LakeFsRequestOutput

[<Emit("Buffer.from($0, 'utf8').toString('base64')")>]
let private toBase64 (_value: string) : string = jsNative

[<Emit("encodeURIComponent($0)")>]
let private encodeUriComponent (_value: string) : string = jsNative

[<Emit("fetch($0, $1)")>]
let private fetchJs (_url: string, _options: obj) : JS.Promise<obj> = jsNative

[<Emit("JSON.stringify($0)")>]
let private jsonStringify (_value: obj) : string = jsNative

[<Emit("JSON.parse($0)")>]
let private jsonParse (_text: string) : obj = jsNative

let private classifyStatus (connection: LakeFsConnection) (status: int) (body: string) : OperationFailure =
    let category, code =
        match status with
        | 401 -> Authentication, "unauthorized"
        | 403 -> Authorization, "forbidden"
        | 404 -> NotFound, "not_found"
        | 409 -> Conflict, "conflict"
        | 412 -> Concurrency, "precondition_failed"
        | status when status >= 500 -> ProviderError, "server_error"
        | _ -> ProviderError, "request_failed"

    // A server can echo the credentials it received, so the body is cleaned of this connection's secrets too.
    {
        OperationFailure.createRedacted
            category
            code
            (LakeFsRequestOutput.redactText connection $"lakeFS request failed ({status}): {body}") with
            Retryable = status >= 500
    }

type LakeFsResponse = {
    Status: int
    BodyText: string
}

/// One HTTP request with injected Basic credentials, cancellation, and redaction.
/// Never logs or returns the credential; failure text passes the redaction guard.
let request
    (connection: LakeFsConnection)
    (method: string)
    (path: string)
    (body: (string * string) option)
    (context: OperationContext)
    : Async<Result<LakeFsResponse, OperationFailure>> =
    async {
        if context.Cancellation.IsCancellationRequested() then
            return
                Error(OperationFailure.create Canceled "operation_canceled" "The request was canceled before it started.")
        else
            let url = $"{connection.Endpoint.TrimEnd '/'}/api/v1{path}"
            let credentials = toBase64 $"{connection.AccessKeyId}:{connection.SecretAccessKey}"

            let headers =
                createObj [
                    "Authorization" ==> $"Basic {credentials}"
                    match body with
                    | Some(contentType, _) -> "Content-Type" ==> contentType
                    | None -> ()
                ]

            let options =
                createObj [
                    "method" ==> method
                    "headers" ==> headers
                    "signal" ==> NodeCancellation.toAbortSignal context.Cancellation
                    match body with
                    | Some(_, content) -> "body" ==> content
                    | None -> ()
                ]

            let output = LakeFsRequestOutput.tryBegin connection method path context

            try
                try
                    let! response = Async.AwaitPromise(fetchJs (url, options))
                    let status = unbox<int> response?status
                    LakeFsRequestOutput.reportStatus output status
                    let! bodyText = Async.AwaitPromise(response?text () |> unbox<JS.Promise<string>>)

                    if status < 200 || status >= 300 then
                        LakeFsRequestOutput.reportErrorBody output bodyText

                    return Ok { Status = status; BodyText = bodyText }
                with error ->
                    // Only a requested cancellation counts as canceled. The abort signal handed to fetch
                    // comes from this context, so an aborted request always has the flag set, and
                    // transport errors that merely mention "abort" stay network failures.
                    let failure =
                        if context.Cancellation.IsCancellationRequested() then
                            OperationFailure.create Canceled "operation_canceled" "The lakeFS request was canceled."
                        else
                            LakeFsTransportError.toFailure
                                "The lakeFS endpoint is unreachable"
                                "The lakeFS request timed out"
                                (box error)

                    LakeFsRequestOutput.reportFailed output failure
                    return Error failure
            finally
                LakeFsRequestOutput.finish output
    }

/// Request that fails on non-2xx statuses with classification.
let requestChecked connection method path body context =
    async {
        let! result = request connection method path body context

        match result with
        | Error failure -> return Error failure
        | Ok response when response.Status >= 200 && response.Status < 300 -> return Ok response
        | Ok response -> return Error(classifyStatus connection response.Status response.BodyText)
    }

let private parsePagination (parsed: obj) : LakeFsPagination =
    let pagination = parsed?pagination

    if isNull pagination then
        {
            HasMore = false
            NextOffset = ""
        }
    else
        {
            HasMore = unbox<bool> pagination?has_more
            NextOffset =
                pagination?next_offset
                |> Option.ofObj
                |> Option.map unbox<string>
                |> Option.defaultValue ""
        }

/// Exhausts every page of a paginated listing (`after` cursor protocol).
let private listAllPages
    (fetchPage: string -> Async<Result<obj[] * LakeFsPagination, OperationFailure>>)
    : Async<Result<obj[], OperationFailure>> =
    async {
        let mutable results = ResizeArray<obj>()
        let mutable after = ""
        let mutable finished = false
        let mutable failure: OperationFailure option = None

        while not finished && failure.IsNone do
            let! page = fetchPage after

            match page with
            | Error pageFailure -> failure <- Some pageFailure
            | Ok(items, pagination) ->
                results.AddRange items

                if pagination.HasMore && pagination.NextOffset <> "" then
                    after <- pagination.NextOffset
                else
                    finished <- true

        match failure with
        | Some value -> return Error value
        | None -> return Ok(results.ToArray())
    }

let getRepository (connection: LakeFsConnection) (repository: string) (context: OperationContext) =
    async {
        let! result =
            requestChecked connection "GET" $"/repositories/{encodeUriComponent repository}" None context

        match result with
        | Error failure -> return Error failure
        | Ok response -> return Ok(jsonParse response.BodyText)
    }

let getBranch (connection: LakeFsConnection) (repository: string) (branch: string) (context: OperationContext) =
    async {
        let! result =
            requestChecked
                connection
                "GET"
                $"/repositories/{encodeUriComponent repository}/branches/{encodeUriComponent branch}"
                None
                context

        match result with
        | Error failure -> return Error failure
        | Ok response ->
            let parsed = jsonParse response.BodyText

            return
                Ok {
                    Id = unbox<string> parsed?id
                    CommitId = unbox<string> parsed?commit_id
                }
    }

let createBranch
    (connection: LakeFsConnection)
    (repository: string)
    (name: string)
    (source: string)
    (context: OperationContext)
    : Async<Result<unit, OperationFailure>> =
    async {
        let payload = jsonStringify (createObj [ "name" ==> name; "source" ==> source ])

        let! result =
            requestChecked
                connection
                "POST"
                $"/repositories/{encodeUriComponent repository}/branches"
                (Some("application/json", payload))
                context

        match result with
        | Error failure -> return Error failure
        | Ok _ -> return Ok()
    }

let deleteBranch (connection: LakeFsConnection) (repository: string) (branch: string) (context: OperationContext) =
    async {
        let! result =
            requestChecked
                connection
                "DELETE"
                $"/repositories/{encodeUriComponent repository}/branches/{encodeUriComponent branch}"
                None
                context

        match result with
        | Error failure -> return Error failure
        | Ok _ -> return Ok()
    }

let listBranches
    (connection: LakeFsConnection)
    (repository: string)
    (context: OperationContext)
    : Async<Result<LakeFsBranch[], OperationFailure>> =
    async {
        let fetchPage (after: string) =
            async {
                let afterQuery = if after = "" then "" else $"&after={encodeUriComponent after}"

                let! result =
                    requestChecked
                        connection
                        "GET"
                        $"/repositories/{encodeUriComponent repository}/branches?amount=100{afterQuery}"
                        None
                        context

                match result with
                | Error failure -> return Error failure
                | Ok response ->
                    let parsed = jsonParse response.BodyText
                    return Ok(unbox<obj[]> parsed?results, parsePagination parsed)
            }

        let! pages = listAllPages fetchPage

        match pages with
        | Error failure -> return Error failure
        | Ok items ->
            return
                Ok(
                    items
                    |> Array.map (fun item -> {
                        Id = unbox<string> item?id
                        CommitId = unbox<string> item?commit_id
                    })
                )
    }

let listObjects
    (connection: LakeFsConnection)
    (repository: string)
    (reference: string)
    (prefix: string)
    (context: OperationContext)
    : Async<Result<LakeFsObjectStats[], OperationFailure>> =
    async {
        let fetchPage (after: string) =
            async {
                let afterQuery = if after = "" then "" else $"&after={encodeUriComponent after}"

                let! result =
                    requestChecked
                        connection
                        "GET"
                        ($"/repositories/{encodeUriComponent repository}/refs/{encodeUriComponent reference}/objects/ls"
                         + $"?amount=100&prefix={encodeUriComponent prefix}{afterQuery}")
                        None
                        context

                match result with
                | Error failure -> return Error failure
                | Ok response ->
                    let parsed = jsonParse response.BodyText
                    return Ok(unbox<obj[]> parsed?results, parsePagination parsed)
            }

        let! pages = listAllPages fetchPage

        match pages with
        | Error failure -> return Error failure
        | Ok items ->
            return
                Ok(
                    items
                    |> Array.map (fun item -> {
                        Path = unbox<string> item?path
                        Checksum = unbox<string> item?checksum
                        SizeBytes = unbox<float> item?size_bytes
                        Mtime = unbox<float> item?mtime
                    })
                )
    }

/// Announces a step before it runs, in the shape the Git provider uses: the event has no numbers and names the
/// step in its message. The byte meters of a transfer keep their own codes and their own message.
let reportStepStart (phaseCode: string) (message: string) (context: OperationContext) =
    context.ReportProgress {
        PhaseCode = phaseCode
        Item = None
        Completed = None
        Total = None
        DisplayMessage = Some message
    }

let downloadObjectToFile
    (connection: LakeFsConnection)
    (repository: string)
    (reference: string)
    (path: string)
    (temporaryPath: string)
    (context: OperationContext)
    =
    LakeFsBinaryTransfer.downloadObjectToFile
        connection
        repository
        reference
        path
        temporaryPath
        context

let uploadObjectFromFile
    (connection: LakeFsConnection)
    (repository: string)
    (branch: string)
    (path: string)
    (sourcePath: string)
    (context: OperationContext)
    =
    LakeFsBinaryTransfer.uploadObjectFromFile
        connection
        repository
        branch
        path
        sourcePath
        context

let uploadObjectFromFileChecked
    (connection: LakeFsConnection)
    (repository: string)
    (branch: string)
    (path: string)
    (sourcePath: string)
    (validateSource: VersionControlService.Runtime.Node.FileSystem.Stats -> Result<unit, OperationFailure>)
    (context: OperationContext)
    =
    LakeFsBinaryTransfer.uploadObjectFromFileChecked
        connection
        repository
        branch
        path
        sourcePath
        validateSource
        context

let deleteObject
    (connection: LakeFsConnection)
    (repository: string)
    (branch: string)
    (path: string)
    (context: OperationContext)
    : Async<Result<unit, OperationFailure>> =
    async {
        let! result =
            requestChecked
                connection
                "DELETE"
                ($"/repositories/{encodeUriComponent repository}/branches/{encodeUriComponent branch}/objects"
                 + $"?path={encodeUriComponent path}")
                None
                context

        match result with
        | Error failure -> return Error failure
        | Ok _ -> return Ok()
    }

let diffRefs
    (connection: LakeFsConnection)
    (repository: string)
    (leftRef: string)
    (rightRef: string)
    (context: OperationContext)
    : Async<Result<LakeFsDiffEntry[], OperationFailure>> =
    async {
        let fetchPage (after: string) =
            async {
                let afterQuery = if after = "" then "" else $"&after={encodeUriComponent after}"

                let! result =
                    requestChecked
                        connection
                        "GET"
                        ($"/repositories/{encodeUriComponent repository}/refs/{encodeUriComponent leftRef}"
                         + $"/diff/{encodeUriComponent rightRef}?amount=100{afterQuery}")
                        None
                        context

                match result with
                | Error failure -> return Error failure
                | Ok response ->
                    let parsed = jsonParse response.BodyText
                    return Ok(unbox<obj[]> parsed?results, parsePagination parsed)
            }

        let! pages = listAllPages fetchPage

        match pages with
        | Error failure -> return Error failure
        | Ok items ->
            return
                Ok(
                    items
                    |> Array.map (fun item -> {
                        Path = unbox<string> item?path
                        DiffType = unbox<string> item?``type``
                    })
                )
    }

/// The uncommitted changes of a branch, so a caller can skip a commit lakeFS would refuse as empty.
let diffBranch
    (connection: LakeFsConnection)
    (repository: string)
    (branch: string)
    (context: OperationContext)
    : Async<Result<LakeFsDiffEntry[], OperationFailure>> =
    async {
        let fetchPage (after: string) =
            async {
                let afterQuery = if after = "" then "" else $"&after={encodeUriComponent after}"

                let! result =
                    requestChecked
                        connection
                        "GET"
                        ($"/repositories/{encodeUriComponent repository}/branches/{encodeUriComponent branch}"
                         + $"/diff?amount=100{afterQuery}")
                        None
                        context

                match result with
                | Error failure -> return Error failure
                | Ok response ->
                    let parsed = jsonParse response.BodyText
                    return Ok(unbox<obj[]> parsed?results, parsePagination parsed)
            }

        let! pages = listAllPages fetchPage

        match pages with
        | Error failure -> return Error failure
        | Ok items ->
            return
                Ok(
                    items
                    |> Array.map (fun item -> {
                        Path = unbox<string> item?path
                        DiffType = unbox<string> item?``type``
                    })
                )
    }

let commit
    (connection: LakeFsConnection)
    (repository: string)
    (branch: string)
    (message: string)
    (context: OperationContext)
    : Async<Result<LakeFsCommit, OperationFailure>> =
    async {
        let payload = jsonStringify (createObj [ "message" ==> message ])

        let! result =
            requestChecked
                connection
                "POST"
                $"/repositories/{encodeUriComponent repository}/branches/{encodeUriComponent branch}/commits"
                (Some("application/json", payload))
                context

        match result with
        | Error failure -> return Error failure
        | Ok response ->
            let parsed = jsonParse response.BodyText

            return
                Ok {
                    Id = unbox<string> parsed?id
                    Parents = parsed?parents |> Option.ofObj |> Option.map unbox<string[]> |> Option.defaultValue [||]
                    Message = unbox<string> parsed?message
                }
    }

let getCommit
    (connection: LakeFsConnection)
    (repository: string)
    (commitId: string)
    (context: OperationContext)
    : Async<Result<LakeFsCommit, OperationFailure>> =
    async {
        let! result =
            requestChecked
                connection
                "GET"
                $"/repositories/{encodeUriComponent repository}/commits/{encodeUriComponent commitId}"
                None
                context

        match result with
        | Error failure -> return Error failure
        | Ok response ->
            let parsed = jsonParse response.BodyText

            return
                Ok {
                    Id = unbox<string> parsed?id
                    Parents = parsed?parents |> Option.ofObj |> Option.map unbox<string[]> |> Option.defaultValue [||]
                    Message = unbox<string> parsed?message
                }
    }

let mergeWithStrategy
    (connection: LakeFsConnection)
    (repository: string)
    (sourceRef: string)
    (destinationBranch: string)
    (message: string)
    (strategy: string option)
    (context: OperationContext)
    : Async<Result<LakeFsMergeResult, OperationFailure>> =
    async {
        let payload =
            jsonStringify (
                createObj [
                    "message" ==> message
                    yield! strategy |> Option.map (fun value -> "strategy" ==> value) |> Option.toList
                ]
            )

        let! result =
            requestChecked
                connection
                "POST"
                ($"/repositories/{encodeUriComponent repository}/refs/{encodeUriComponent sourceRef}"
                 + $"/merge/{encodeUriComponent destinationBranch}")
                (Some("application/json", payload))
                context

        match result with
        | Error failure -> return Error failure
        | Ok response ->
            let parsed = jsonParse response.BodyText

            return
                Ok {
                    Reference = unbox<string> parsed?reference
                }
    }

let merge
    (connection: LakeFsConnection)
    (repository: string)
    (sourceRef: string)
    (destinationBranch: string)
    (message: string)
    (context: OperationContext)
    : Async<Result<LakeFsMergeResult, OperationFailure>> =
    mergeWithStrategy connection repository sourceRef destinationBranch message None context
