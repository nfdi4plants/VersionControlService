/// Helpers shared by the text diff tests that open a diff through a service.
module VersionControlService.Tests.TextDiffTestSupport

open Fable.Core
open VersionControlService.Abstractions

/// A busy machine can answer Open with Scanning several times before the sources are validated.
let private maxOpenAttempts = 10000

/// Calls Open and passes each Scanning continuation back with the next Open until the answer is ready.
let openUntilReady
    (service: TextDiffService)
    (initial: OpenDiffRequest)
    (context: OperationContext)
    : JS.Promise<OpenDiffResult> =
    promise {
        let mutable request = initial
        let mutable attempts = 0
        let mutable value = None

        while value.IsNone do
            attempts <- attempts + 1

            if attempts > maxOpenAttempts then
                failwith $"Open still answered Scanning after {maxOpenAttempts} attempts."

            let! result = service.Open request context |> Async.StartAsPromise

            match result with
            | Succeeded outcome ->
                match outcome.Value with
                | Resumable.Ready opened -> value <- Some opened
                | Resumable.Scanning(_, continuation, _) -> request <- { request with Continuation = Some continuation }
            | PartiallySucceeded(outcome, failure) ->
                failwith $"Open returned partial success: {failure.Code} {outcome.Value}"
            | Failed failure -> failwith $"Open failed: {failure.Code} {failure.Message}"

        return value.Value
    }
