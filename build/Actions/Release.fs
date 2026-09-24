[<RequireQualifiedAccessAttribute>]
module Release

open System
open System.IO
open ProjectInfo

let nuget (key: string option) (isDryRun: bool) =
    let apiKey =
        if isDryRun then
            None
        else
            match key with
            | Some value when not (String.IsNullOrWhiteSpace value) -> Some value
            | _ -> failwith "NUGET_KEY is required unless --dry-run is used."

    let release = Changelog.getLatestVersion ()
    let version = release.Version.ToString()
    let feed = Path.Combine(ProjectPaths.repositoryRoot, "nupkgs")

    Pack.Local version feed (Some release.Body) |> ignore
    Verify.PackageGraph feed version

    match apiKey with
    | Some key ->
        printGreenfn "Pushing NuGet packages for version %s" version

        run
            "dotnet"
            [
                "nuget"
                "push"
                "nupkgs/*.nupkg"
                "--api-key"
                key
                "--skip-duplicate"
                "--source"
                "https://api.nuget.org/v3/index.json"
            ]
            ProjectPaths.repositoryRoot
    | None -> printGreenfn "Dry run complete. Packages were packed and verified without publishing."
