namespace VersionControlService.TextDiff.FableTests

open Fable.Core

module Vitest =
    [<ImportMember("vitest")>]
    let inline describe(name: string, fn: unit -> unit) : unit = jsNative

    [<ImportMember("vitest")>]
    let inline it(name: string, fn: unit -> JS.Promise<unit>) : unit = jsNative

    [<ImportMember("vitest")>]
    let inline expect(value: 'a) : obj = jsNative

    [<Emit("$0.toBe($1)")>]
    let inline toBe(assertion: obj, expected: 'a) : unit = jsNative

    [<Emit("console.log($0)")>]
    let inline log(message: string) : unit = jsNative

module BrowserClock =
    [<Emit("Date.now()")>]
    let nowMs() : float = jsNative
