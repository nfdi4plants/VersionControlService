[<RequireQualifiedAccessAttribute>]
module Verify

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open ProjectInfo

/// The umbrella package carries metadata only, so any of these entries is a leak.
let private implementationAsset =
    Regex(@"^(lib|ref|runtimes|contentFiles|fable)/|\.(dll|pdb|fs|fsproj)$", RegexOptions.IgnoreCase)

let private list (values: string seq) = String.Join(", ", values)

/// Checks that a local feed holds exactly the coordinated packages at one version, with
/// the internal dependencies pinned exactly and the umbrella free of implementation
/// assets. Every problem is reported, not just the first one.
let PackageGraph (feed: string) (version: string) =
    let feedPath = Path.GetFullPath feed

    if not (Directory.Exists feedPath) then
        failwithf "Package feed does not exist: %s" feedPath

    let errors = ResizeArray<string>()
    let packagesById = Dictionary<string, PackageIO.PackageMetadata>()

    let packageFiles = [
        for file in Directory.EnumerateFiles feedPath do
            if file.EndsWith(".nupkg", StringComparison.OrdinalIgnoreCase) then
                file
    ]

    if packageFiles.Length <> Packages.all.Length then
        errors.Add $"Expected exactly five .nupkg files, found {packageFiles.Length}."

    for file in packageFiles do
        try
            let package = PackageIO.read file

            if packagesById.ContainsKey package.Id then
                errors.Add $"Duplicate package ID '{package.Id}'."
            else
                packagesById[package.Id] <- package
        with ex ->
            errors.Add ex.Message

    for expectedId in Packages.all do
        if not (packagesById.ContainsKey expectedId) then
            errors.Add $"Missing package '{expectedId}'."

    for actualId in packagesById.Keys do
        if not (List.contains actualId Packages.all) then
            errors.Add $"Unexpected package '{actualId}'."

    for packageId in Packages.all do
        match packagesById.TryGetValue packageId with
        | false, _ -> ()
        | true, package ->
            if package.Version <> version then
                errors.Add $"{packageId} has version '{package.Version}', expected '{version}'."

            if package.Authors <> packageAuthors then
                errors.Add $"{packageId} must declare author '{packageAuthors}'."

            if String.IsNullOrWhiteSpace package.Description then
                errors.Add $"{packageId} is missing a description."

            if String.IsNullOrWhiteSpace package.Tags then
                errors.Add $"{packageId} is missing package tags."

            if package.LicenseType <> "expression" || package.License <> packageLicense then
                errors.Add $"{packageId} must declare the {packageLicense} license expression."

            if package.RepositoryUrl <> projectRepo then
                errors.Add $"{packageId} must declare the {project} repository URL."

            let internalDependencies =
                package.Dependencies |> List.filter (fun dependency -> List.contains dependency.Id Packages.all)

            let actualIds = internalDependencies |> List.map _.Id |> List.distinct |> List.sort
            let expectedIds = Packages.internalDependencies[packageId] |> List.distinct |> List.sort

            if actualIds <> expectedIds then
                errors.Add
                    $"{packageId} internal dependencies are [{list actualIds}], expected [{list expectedIds}]."

            for dependency in internalDependencies do
                if dependency.Version <> $"[{version}]" then
                    errors.Add
                        $"{packageId} dependency '{dependency.Id}' uses '{dependency.Version}'; expected exact version '[{version}]'."

            if packageId = Packages.umbrella then
                for dependency in package.Dependencies do
                    if not (List.contains dependency.Id Packages.all) then
                        errors.Add $"Umbrella package has forbidden direct dependency '{dependency.Id}'."

                let assets = package.Entries |> List.filter implementationAsset.IsMatch

                if not assets.IsEmpty then
                    errors.Add $"Umbrella package contains implementation assets: {list assets}"

    for packageId in Packages.implementations do
        let symbolPackage = $"{packageId}.{version}.snupkg"

        if not (File.Exists(Path.Combine(feedPath, symbolPackage))) then
            errors.Add $"Missing symbol package '{symbolPackage}'."
        else
            match PackageIO.inspectSymbols sourceLinkPrefix (Path.Combine(feedPath, symbolPackage)) with
            | PackageIO.SourceLinked -> ()
            | PackageIO.NoPortablePdb ->
                errors.Add $"Symbol package '{symbolPackage}' contains no portable PDB source information."
            | PackageIO.NoSourceLink ->
                errors.Add $"Symbol package '{symbolPackage}' contains no repository Source Link record."

    if errors.Count > 0 then
        let report = String.Join(Environment.NewLine + " - ", errors)
        failwith $"Package graph verification failed:{Environment.NewLine} - {report}"

    printGreenfn "Verified exact five-package graph at version %s." version
