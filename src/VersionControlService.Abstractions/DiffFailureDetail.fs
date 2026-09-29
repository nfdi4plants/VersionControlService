namespace VersionControlService.Abstractions

/// The side of a diff source that could not be read as text.
[<RequireQualifiedAccess>]
type DiffSide =
    | Previous
    | Current

/// Structured detail of a diff_content_not_text failure, identifying the side and evidence found.
type DiffContentBlocked = {
    Side: DiffSide
    Evidence: string
}
