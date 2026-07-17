module VersionControlService.Runtime.Node.Interop

open Fable.Core
open Fable.Core.JsInterop

let childProcessDynamic: obj = importAll "node:child_process"

[<Emit("process.platform")>]
let processPlatform () : string = jsNative

[<Emit("$0.length")>]
let bufferLength (buffer: obj) : int = jsNative

[<Emit("$0[$1]")>]
let bufferByteAt (buffer: obj) (index: int) : int = jsNative

[<Emit("Buffer.concat($0)")>]
let bufferConcat (buffers: obj[]) : obj = jsNative

[<Emit("$0.subarray($1, $2)")>]
let bufferSubarray (buffer: obj) (startIndex: int) (endIndex: int) : obj = jsNative

[<Emit("Buffer.alloc($0)")>]
let bufferAlloc (length: int) : obj = jsNative

[<Emit("new Error($0)")>]
let createError (_message: string) : obj = jsNative

[<Emit("$0?.message ?? String($0)")>]
let errorMessage (_error: obj) : string = jsNative

[<Emit("require('node:crypto').randomUUID()")>]
let randomUuid () : string = jsNative

[<Emit("$0.then($1, $2)")>]
let observePromise (_promise: JS.Promise<'T>) (_onSucceeded: 'T -> unit) (_onFailed: obj -> unit) : unit = jsNative

[<Emit("$0.toString('utf8')")>]
let bufferToUtf8String (buffer: obj) : string = jsNative

[<Emit("(() => { try { new TextDecoder('utf-8', { fatal: true }).decode($0); return true; } catch (_) { return false; } })()")>]
let bufferIsValidUtf8 (buffer: obj) : bool = jsNative

[<Emit("$0.indexOf(0) >= 0")>]
let bufferContainsNul (buffer: obj) : bool = jsNative

[<Emit("require('node:crypto').createHash('sha256')")>]
let createSha256Hash () : obj = jsNative

[<Emit("$0.update($1)")>]
let updateHash (hash: obj) (buffer: obj) : unit = jsNative

[<Emit("$0.digest('hex')")>]
let digestHashHex (hash: obj) : string = jsNative

[<Emit("require('node:crypto').createHash('sha256').update($0, 'utf8').digest('hex')")>]
let sha256Utf8 (content: string) : string = jsNative

[<Emit("new (require('node:string_decoder').StringDecoder)('utf8')")>]
let createUtf8StringDecoder () : obj = jsNative

[<Emit("$0.write($1)")>]
let decodeUtf8Chunk (decoder: obj) (buffer: obj) : string = jsNative

[<Emit("$0.end()")>]
let finishUtf8Decoding (decoder: obj) : string = jsNative
