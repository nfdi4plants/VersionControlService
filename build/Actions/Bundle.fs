[<RequireQualifiedAccessAttribute>]
module Bundle

open ProjectInfo

let Library () =
    run
        "dotnet"
        [
            "fable"
            "-o"
            "output"
            "-s"
            "-e"
            "fs.js"
        ]
        ProjectPaths.srcPath
