namespace VersionControlService.Tests

module VitestBindings =

    open Fable.Core

    [<ImportMember("vitest")>]
    let inline describe(name: string, fn: unit -> unit) : unit = jsNative

    [<ImportMember("vitest")>]
    let inline test(name: string, fn: unit -> unit) : unit = jsNative

    [<ImportMember("vitest")>]
    let inline expect(value: 'a) : obj = jsNative

    [<Emit("$0.toEqual($1)")>]
    let inline toEqual (assertion: obj, expected: 'a) : unit = jsNative

    [<Emit("$0.toBe($1)")>]
    let inline toBe (assertion: obj, expected: 'a) : unit = jsNative

    [<Emit("$0.toContain($1)")>]
    let inline toContain (assertion: obj, expected: string) : unit = jsNative

module NodePath =

    open Fable.Core

    [<Import("join", "path")>]
    let join ([<ParamSeq>] paths: string[]) : string = jsNative

    [<Import("dirname", "path")>]
    let dirname (path: string) : string = jsNative
