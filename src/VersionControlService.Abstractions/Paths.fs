namespace VersionControlService.Abstractions

open System

/// Exact repository-relative object key. Byte-exact: no Unicode normalization,
/// no case folding, no wildcard or pathspec interpretation, forward slash separator.
type RepositoryPath =
    private
    | RepositoryPath of string

module RepositoryPath =

    /// Validates an exact repository-relative path. Rules:
    /// - nonempty, not absolute, not drive-rooted;
    /// - forward slash is the only separator; backslash is rejected, never converted;
    /// - no empty, "." or ".." segment; no trailing separator; no NUL;
    /// - every other character (spaces, Unicode, wildcards, newlines) is a literal
    ///   file-name byte and round-trips unchanged.
    let tryCreate (value: string) : Result<RepositoryPath, string> =
        if isNull value || value.Length = 0 then
            Error "Repository path must not be empty."
        elif value.Contains "\000" then
            Error "Repository path must not contain NUL characters."
        elif value.Contains "\\" then
            Error "Repository path must use forward slashes; backslash is not a separator."
        elif value.StartsWith "/" then
            Error "Repository path must be relative, not absolute."
        elif value.Length >= 2 && Char.IsLetter value[0] && value[1] = ':' then
            Error "Repository path must not be drive-rooted."
        else
            let segments = value.Split '/'

            if segments |> Array.exists (fun segment -> segment.Length = 0) then
                Error "Repository path must not contain empty segments or trailing separators."
            elif segments |> Array.exists (fun segment -> segment = "." || segment = "..") then
                Error "Repository path must not contain '.' or '..' segments."
            else
                Ok(RepositoryPath value)

    /// The exact repository-relative key, unchanged from creation.
    let value (RepositoryPath path) = path
