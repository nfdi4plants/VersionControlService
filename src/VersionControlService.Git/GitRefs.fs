/// Authoritative Git ref validation: basic argument safety, then Git's own
/// `check-ref-format --branch` as the single source of truth.
module VersionControlService.Git.GitRefs

open System
open VersionControlService.Abstractions

module NodeProcess = VersionControlService.Runtime.Node.Process

type GitRunner = string[] -> string option -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>

let private invalidName (name: string) =
    OperationFailure.createRedacted Validation "invalid_ref_name" $"'{name}' is not a valid branch name."

/// Validates a local branch-like name. Argument safety first (empty, leading
/// dash, NUL), then Git's authoritative validator decides everything else.
let validateBranchName (runGit: GitRunner) (name: string) : Async<Result<string, OperationFailure>> =
    async {
        let trimmed = (name |> Option.ofObj |> Option.defaultValue "").Trim()

        if String.IsNullOrWhiteSpace trimmed then
            return Error(invalidName "<empty>")
        elif trimmed.StartsWith "-" then
            return Error(invalidName trimmed)
        elif trimmed.Contains "\000" then
            return Error(invalidName trimmed)
        else
            let! output = runGit [| "check-ref-format"; "--branch"; trimmed |] None

            match output with
            | Ok result when result.ExitCode = 0 -> return Ok trimmed
            | Ok _ -> return Error(invalidName trimmed)
            | Error failure -> return Error failure
    }
