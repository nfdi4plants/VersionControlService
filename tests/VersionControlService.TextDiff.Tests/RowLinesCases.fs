namespace VersionControlService.TextDiff.Tests

open VersionControlService.Abstractions
open VersionControlService.TextDiff

module RowLinesCases =
    let private lineRef number = {
        Number = float number
        Start = float number * 10.0
        Finish = float number * 10.0 + 9.0
        Length = 8.0
        Ending = 1
    }

    let private row kind previous current = { Kind = kind; Previous = lineRef previous; Current = lineRef current }

    let cases: (string * (unit -> Async<unit>)) list = [
        "row line arrays hold exactly the lines each side reads", fun () -> async {
            let rows = [|
                row DiffRowKind.Context 0 0
                row DiffRowKind.EndingChanged 1 1
                row DiffRowKind.Removed 2 0
                row DiffRowKind.Added 0 2
                row DiffRowKind.Replaced 3 3
                row DiffRowKind.Context 4 4
            |]
            let previousRefs, currentRefs = RowLines.collect rows 0 rows.Length
            Check.equal 5 previousRefs.Length "Every row except the added row reads a previous line."
            Check.equal 2 currentRefs.Length "Only the added and replaced rows read a current line."
            Check.sequence [| 0.0; 1.0; 2.0; 3.0; 4.0 |] (previousRefs |> Array.map (fun line -> line.Number)) "The previous lines keep row order."
            Check.sequence [| 2.0; 3.0 |] (currentRefs |> Array.map (fun line -> line.Number)) "The current lines keep row order."
            let middlePrevious, middleCurrent = RowLines.collect rows 2 3
            Check.equal 2 middlePrevious.Length "A window counts its own rows on the previous side."
            Check.equal 2 middleCurrent.Length "A window counts its own rows on the current side."
            Check.equal 3 (RowLines.slotBefore rows 0 4 true) "The previous slot of the replaced row follows three rows that read a previous line."
            Check.equal 1 (RowLines.slotBefore rows 0 4 false) "The current slot of the replaced row follows the added row."
        }
    ]
