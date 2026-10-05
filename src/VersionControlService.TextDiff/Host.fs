namespace VersionControlService.TextDiff

open System

#if FABLE_COMPILER
open Fable.Core
#endif

type IClock =
    abstract NowMs: unit -> float

[<RequireQualifiedAccess>]
type ReadOutcome =
    | Bytes of count: int
    | NotYetAvailable
    | EndOfSource

type IByteSource =
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

/// Fable's async trampoline breaks long bind chains with a setTimeout(0) hop, and on Windows each hop waits for
/// a timer tick of 11 to 15 ms. A diff takes thousands of hops. The .NET build has no trampoline.
module AsyncTrampoline =
#if FABLE_COMPILER
    // The trampoline is reached through the context of a running async. If a Fable version shapes it differently,
    // nothing changes.
    [<Emit("(ctx) => { const proto = ctx.trampoline ? Object.getPrototypeOf(ctx.trampoline) : null; if (proto && typeof proto.hijack === 'function') { proto.hijack = function (f) { this.callCount = 0; setImmediate(f); }; } ctx.onSuccess(); }")>]
    let private hopViaSetImmediate: Async<unit> = jsNative

    /// Makes the async trampoline hop through setImmediate. The change holds for every async on the thread, so
    /// only a thread that runs nothing but the engine calls it, such as a diff worker thread or a test process.
    /// Calling it again changes nothing.
    let switchToSetImmediate () : unit = Async.StartImmediate hopViaSetImmediate
#else
    let switchToSetImmediate () : unit = ()
#endif
