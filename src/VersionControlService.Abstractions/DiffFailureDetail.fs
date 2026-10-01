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
    /// The byte offset of the first sequence that is invalid in the encoding the side was read with.
    /// None when the evidence is something else, such as a NUL character, a control ratio or a signature.
    InvalidSequenceOffset: int64 option
}
