namespace VersionControlService.Abstractions

/// The side of a diff source that could not be read as text.
[<RequireQualifiedAccess>]
type DiffSide =
    | Previous
    | Current

/// Structured detail of a diff_content_not_text or diff_encoding_mismatch failure. It names the side and the evidence.
type DiffContentBlocked = {
    Side: DiffSide
    Evidence: string
}
