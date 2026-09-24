[<RequireQualifiedAccessAttribute>]
module Pack

open System
open System.IO
open ProjectInfo

/// Packing empties the output directory first, so refuse a target that would take the
/// repository, a whole volume, or anything reached through a junction with it.
let private prepareOutput (output: string) =
    let outputPath = Path.GetFullPath output
    let separator = string Path.DirectorySeparatorChar
    let comparison = StringComparison.OrdinalIgnoreCase
    let volumeRoot = Path.GetPathRoot outputPath
    let repositoryPrefix = ProjectPaths.repositoryRoot.TrimEnd(Path.DirectorySeparatorChar) + separator
    let outputPrefix = outputPath.TrimEnd(Path.DirectorySeparatorChar) + separator
    let releaseOutputPath = Path.Combine(ProjectPaths.repositoryRoot, "nupkgs")
    let isReleaseOutput = outputPath.Equals(releaseOutputPath, comparison)

    if
        outputPath.Equals(volumeRoot, comparison)
        || outputPath.Equals(ProjectPaths.repositoryRoot, comparison)
        || (outputPath.StartsWith(repositoryPrefix, comparison) && not isReleaseOutput)
        || ProjectPaths.repositoryRoot.StartsWith(outputPrefix, comparison)
    then
        failwithf "Refusing to clear unsafe package output directory '%s'." outputPath

    if File.Exists outputPath then
        failwithf "Package output path is not a directory: '%s'." outputPath

    let mutable segment = DirectoryInfo outputPath

    while not (isNull segment) do
        if segment.Exists && segment.Attributes.HasFlag FileAttributes.ReparsePoint then
            failwithf "Refusing package output beneath reparse point '%s'." segment.FullName

        segment <- segment.Parent

    if Directory.Exists outputPath then
        for file in Directory.EnumerateFiles outputPath do
            File.Delete file

        for directory in Directory.EnumerateDirectories outputPath do
            Directory.Delete(directory, true)
    else
        Directory.CreateDirectory outputPath |> ignore

    outputPath

/// Packs the five coordinated packages at one version into a local feed. The projects are
/// already restored by the time a target gets here, which is why packing skips restore.
let Local (version: string) (output: string) (releaseNotes: string option) =
    let outputPath = prepareOutput output

    for packageId, projectPath in Packages.projects do
        printGreenfn "Packing %s %s" packageId version

        let args =
            [
                "pack"
                projectPath
                "-c"
                "Release"
                "-o"
                outputPath
                $"-p:Version={version}"
                "--no-restore"
            ]

        let args =
            match releaseNotes with
            | Some notes ->
                // MSBuild treats semicolons and commas as property separators. Percent
                // encoding keeps the full changelog body inside PackageReleaseNotes.
                let escapedNotes =
                    notes
                        .Replace("%", "%25")
                        .Replace(";", "%3B")
                        .Replace(",", "%2C")

                args @ [ $"-p:PackageReleaseNotes={escapedNotes}" ]
            | None -> args

        run "dotnet" args ProjectPaths.repositoryRoot

    printGreenfn "Packed local %s graph into %s" project outputPath
    outputPath
