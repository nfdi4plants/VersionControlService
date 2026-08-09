/// Literal path encoding and NUL/stdin transport for Git commands.
/// Selected paths are exact repository-relative file names, never pathspecs;
/// path lists travel over stdin so selection size is not bounded by platform
/// command-line length limits.
module internal VersionControlService.Git.GitPathTransport

open VersionControlService.Abstractions

/// Encodes one literal repository path as a Git pathspec that disables all
/// wildcard/attribute magic.
let literalPathspec (path: RepositoryPath) : string =
    $":(literal){RepositoryPath.value path}"

/// NUL-delimited payload for --pathspec-from-file=- --pathspec-file-nul.
/// Literal magic keeps bracket/wildcard characters as file-name bytes.
let nulDelimitedLiteralPathspecs (paths: RepositoryPath[]) : string =
    paths |> Array.map literalPathspec |> String.concat "\000"

/// NUL-delimited plain path payload for commands taking -z path lists on stdin
/// (e.g. git update-index --stdin, git check-attr --stdin -z).
let nulDelimitedPaths (paths: RepositoryPath[]) : string =
    paths |> Array.map RepositoryPath.value |> String.concat "\000"

/// Arguments enabling stdin pathspec transport for add/reset/checkout-style commands.
let pathspecFromStdinArguments: string[] = [|
    "--pathspec-from-file=-"
    "--pathspec-file-nul"
|]
