module PackageIO

open System
open System.IO
open System.IO.Compression
open System.Xml.Linq

type PackageDependency = { Id: string; Version: string }

type PackageMetadata = {
    Path: string
    Id: string
    Version: string
    Authors: string
    Description: string
    Tags: string
    LicenseType: string
    License: string
    RepositoryUrl: string
    Dependencies: PackageDependency list
    Entries: string list
}

type SymbolCheck =
    | SourceLinked
    | NoPortablePdb
    | NoSourceLink

let private childValue (ns: XNamespace) (name: string) (element: XElement) =
    let child = element.Element(ns + name)
    if isNull child then "" else child.Value

let private attributeValue (name: string) (element: XElement) =
    if isNull element then
        ""
    else
        let attribute = element.Attribute(XName.Get name)
        if isNull attribute then "" else attribute.Value

/// Reads the .nuspec out of a .nupkg together with the file list, which is everything the
/// graph verification asks of a package.
let read (path: string) =
    use archive = ZipFile.OpenRead path

    let nuspec =
        archive.Entries
        |> Seq.tryFind (fun entry -> entry.FullName.EndsWith(".nuspec", StringComparison.OrdinalIgnoreCase))

    match nuspec with
    | None -> failwithf "No .nuspec was found in '%s'." path
    | Some entry ->
        use stream = entry.Open()
        let document = XDocument.Load stream
        let ns = document.Root.Name.Namespace
        let metadata = document.Root.Element(ns + "metadata")

        if isNull metadata then
            failwithf "No package metadata was found in '%s'." path

        let license = metadata.Element(ns + "license")

        {
            Path = path
            Id = metadata |> childValue ns "id"
            Version = metadata |> childValue ns "version"
            Authors = metadata |> childValue ns "authors"
            Description = metadata |> childValue ns "description"
            Tags = metadata |> childValue ns "tags"
            LicenseType = license |> attributeValue "type"
            License = if isNull license then "" else license.Value
            RepositoryUrl = metadata.Element(ns + "repository") |> attributeValue "url"
            Dependencies = [
                for dependency in metadata.Descendants(ns + "dependency") ->
                    {
                        Id = dependency |> attributeValue "id"
                        Version = dependency |> attributeValue "version"
                    }
            ]
            Entries = [ for entry in archive.Entries -> entry.FullName ]
        }

let private carriesSourceLink (sourceLinkPrefix: string) (entry: ZipArchiveEntry) =
    try
        use stream = entry.Open()
        use buffer = new MemoryStream()
        stream.CopyTo buffer
        let portablePdbText = Text.Encoding.UTF8.GetString(buffer.ToArray())

        portablePdbText.Contains "\"documents\""
        && portablePdbText.Contains(sourceLinkPrefix, StringComparison.OrdinalIgnoreCase)
    with _ ->
        false

/// A symbol package passes when at least one of its portable PDBs carries the document
/// table and a Source Link record pointing back at this repository.
let inspectSymbols (sourceLinkPrefix: string) (symbolPackage: string) =
    use archive = ZipFile.OpenRead symbolPackage

    let portablePdbs = [
        for entry in archive.Entries do
            if entry.FullName.EndsWith(".pdb", StringComparison.OrdinalIgnoreCase) && entry.Length > 0L then
                entry
    ]

    if portablePdbs.IsEmpty then NoPortablePdb
    elif portablePdbs |> List.exists (carriesSourceLink sourceLinkPrefix) then SourceLinked
    else NoSourceLink
