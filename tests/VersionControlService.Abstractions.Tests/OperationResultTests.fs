module VersionControlService.Abstractions.Tests.OperationResultTests

open Expecto
open VersionControlService.Abstractions

let private expectRevisionId value =
    match RevisionId.tryCreate value with
    | Ok revisionId -> revisionId
    | Error message -> failtest message

[<Tests>]
let operationResultTests =
    testList "OperationResults" [
        testCase "succeeded wraps a performed outcome"
        <| fun () ->
            match OperationResult.succeeded 42 with
            | Succeeded outcome ->
                Expect.equal outcome.Value 42 "Value preserved."
                Expect.equal outcome.Effect Performed "Effect is Performed."
            | PartiallySucceeded _
            | Failed _ -> failtest "Expected Succeeded."

        testCase "a throwing progress callback does not escape the context"
        <| fun () ->
            let mutable calls = 0

            let context =
                OperationContext.create "progress-1" OperationCancellation.none (fun _ ->
                    calls <- calls + 1
                    failwith "Object has been destroyed")

            context.ReportProgress {
                PhaseCode = "transfer"
                Item = None
                Completed = None
                Total = None
                DisplayMessage = None
            }

            Expect.equal calls 1 "The callback ran once and its exception was dropped."

        testCase "noOp keeps its reason"
        <| fun () ->
            match OperationResult.noOp (Some "already up to date") "value" with
            | Succeeded outcome -> Expect.equal outcome.Effect (NoOp(Some "already up to date")) "NoOp reason kept."
            | PartiallySucceeded _
            | Failed _ -> failtest "Expected Succeeded NoOp."

        testCase "failure carries stable category, extensible code, retry guidance, state, and recovery"
        <| fun () ->
            let failure = {
                OperationFailure.create Concurrency "precondition_failed" "The workspace version is stale." with
                    Retryable = true
                    AffectedPaths = [| "a.txt" |]
                    RecoveryAction =
                        Some {
                            Code = "refresh_status"
                            Instructions = Some "Reload workspace status and retry with the fresh version."
                        }
            }

            match OperationResult.failed failure with
            | Failed observed ->
                Expect.equal observed.Category Concurrency "Stable category."
                Expect.equal observed.Code "precondition_failed" "Stable code."
                Expect.isTrue observed.Retryable "Retry guidance."
                Expect.isFalse observed.StateChanged "No state change reported."
                Expect.equal observed.AffectedPaths [| "a.txt" |] "Affected paths."
                Expect.equal (observed.RecoveryAction |> Option.map _.Code) (Some "refresh_status") "Recovery action."
            | Succeeded _
            | PartiallySucceeded _ -> failtest "Expected Failed."

        testCase "unknown provider failure codes round-trip without failure"
        <| fun () ->
            let failure =
                OperationFailure.create ProviderError "vendor.custom_condition_9" "Vendor-specific condition."

            match OperationResult.failed failure with
            | Failed observed -> Expect.equal observed.Code "vendor.custom_condition_9" "Unknown code unchanged."
            | Succeeded _
            | PartiallySucceeded _ -> failtest "Expected Failed."

        testCase "partial success is forced to describe changed state and recovery"
        <| fun () ->
            let outcome = {
                OperationOutcome.performed "created-revision" with
                    AffectedPaths = [| "b.txt" |]
                    ResultingRevision = Some(expectRevisionId "abc123")
            }

            let failure =
                OperationFailure.create ProviderError "hydration_failed" "Large-object hydration failed."

            let recovery = {
                Code = "retry_materialization"
                Instructions = Some "Retry downloading large objects."
            }

            match OperationResult.partiallySucceeded outcome failure recovery with
            | PartiallySucceeded(observedOutcome, observedFailure) ->
                Expect.isTrue observedFailure.StateChanged "Partial success always reports changed state."

                Expect.equal observedFailure.RecoveryAction (Some recovery) "Recovery action is mandatory."

                Expect.equal
                    (observedOutcome.ResultingRevision |> Option.map RevisionId.value)
                    (Some "abc123")
                    "The partial outcome keeps its resulting revision."
            | Succeeded _
            | Failed _ -> failtest "Expected PartiallySucceeded."

        testCase "outcome reports warnings, affected paths, revision, workspace version, and publication state"
        <| fun () ->
            let outcome = {
                OperationOutcome.performed () with
                    Warnings = [|
                        {
                            Code = "lfs_pointer_kept"
                            Message = "Objects stay as pointers."
                        }
                    |]
                    AffectedPaths = [| "x.txt"; "y.txt" |]
                    ResultingRevision = Some(expectRevisionId "def456")
                    ResultingWorkspaceVersion = Some "workspace-token-1"
                    Publication = LocalOnly
            }

            Expect.equal outcome.Warnings.Length 1 "Structured warning kept."
            Expect.equal outcome.AffectedPaths.Length 2 "Affected paths kept."
            Expect.equal outcome.ResultingWorkspaceVersion (Some "workspace-token-1") "Workspace version kept."
            Expect.equal outcome.Publication LocalOnly "Publication state kept."

        testCase "redaction strips credential material from failure details"
        <| fun () ->
            let details = [|
                "fatal: unable to access 'https://oauth2:supersecret@host.example/repo.git/'"
                "Authorization: Bearer abc123token"
                "PRIVATE-TOKEN: xyz789"
                "plain diagnostic line"
            |]

            let failure =
                OperationFailure.create Authentication "auth_failed" "Authentication failed."
                |> OperationFailure.withDetails details

            let allDetails = String.concat "\n" failure.Details

            Expect.isFalse (allDetails.Contains "supersecret") "URL credentials removed."
            Expect.isFalse (allDetails.Contains "abc123token") "Bearer token removed."
            Expect.isFalse (allDetails.Contains "xyz789") "Private token removed."
            Expect.isTrue (allDetails.Contains "[REDACTED]") "Redaction marker present."
            Expect.isTrue (allDetails.Contains "plain diagnostic line") "Clean lines are preserved."

        testCase "redaction also guards failure messages"
        <| fun () ->
            let failure =
                OperationFailure.createRedacted
                    Network
                    "transfer_failed"
                    "could not reach https://user:password1@host.example/repo.git"

            Expect.isFalse (failure.Message.Contains "password1") "Message credentials removed."
            Expect.isTrue (failure.Message.Contains "[REDACTED]") "Redaction marker present."
    ]
