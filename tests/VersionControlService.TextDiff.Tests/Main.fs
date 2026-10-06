module VersionControlService.TextDiff.Tests.Main

open Expecto
open VersionControlService.TextDiff.Tests

[<Tests>]
let textDiffTests =
    let tests =
        (TextDiffEngineCases.cases @ TextDiffSessionCases.cases @ TextDiffStreamingCases.cases @ TextDiffResyncCases.cases @ RowLinesCases.cases @ MemoryStoreCases.cases)
        |> List.map (fun (name, run) -> testCaseAsync name (run ()))

    testList "TextDiffEngine" (FnvReferenceTests.randomReferenceTests :: tests)
