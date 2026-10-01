namespace VersionControlService.TextDiff

open System

type IClock =
    abstract NowMs: unit -> float

[<RequireQualifiedAccess>]
type ReadOutcome =
    | Bytes of count: int
    | NotYetAvailable
    | EndOfSource

type IByteSource =
    abstract KnownLength: int64 option
    abstract AvailableLength: unit -> int64
    abstract IsComplete: unit -> bool
    abstract ReadAt: int64 -> byte[] -> int -> int -> Async<ReadOutcome>

type ITempStore =
    abstract Append: byte[] -> int -> int -> Async<int64>
    abstract WriteAt: int64 -> byte[] -> int -> int -> Async<unit>
    abstract ReadAt: int64 -> byte[] -> int -> int -> Async<int>
    abstract Length: unit -> int64
    abstract Dispose: unit -> Async<unit>

type EngineHost = {
    Clock: IClock
    Yield: unit -> Async<unit>
    CreateTempStore: string -> Async<ITempStore>
}
