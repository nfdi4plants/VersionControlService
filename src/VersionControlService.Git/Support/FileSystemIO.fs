module internal VersionControlService.Support.FileSystemIO

open Fable.Core

type private RetryStrategy = {
    DelaysMs: int[]
    IsTransientErrorCode: string option -> bool
}

let private removeRetryStrategy = {
    DelaysMs = [| 0; 75; 200; 500 |]
    IsTransientErrorCode =
        function
        | Some "ENOTEMPTY"
        | Some "EPERM"
        | Some "EACCES"
        | Some "EBUSY" -> true
        | _ -> false
}

[<Emit("$0?.code")>]
let private getNodeErrorCode (_error: exn) : string = jsNative

let private tryGetNodeErrorCode (error: exn) : string option =
    getNodeErrorCode error |> Option.ofObj

let removePathWithRetriesAsync
    (removePathAsync: string -> JS.Promise<unit>)
    (absolutePath: string)
    : JS.Promise<Result<unit, exn>> =
    let rec attempt (attemptIndex: int) = promise {
        if attemptIndex > 0 then
            do! Promise.sleep removeRetryStrategy.DelaysMs.[attemptIndex]

        try
            do! removePathAsync absolutePath
            return Ok()
        with removeError ->
            let errorCode = tryGetNodeErrorCode removeError

            if
                attemptIndex < removeRetryStrategy.DelaysMs.Length - 1
                && removeRetryStrategy.IsTransientErrorCode errorCode
            then
                return! attempt (attemptIndex + 1)
            else
                return Error removeError
    }

    attempt 0
