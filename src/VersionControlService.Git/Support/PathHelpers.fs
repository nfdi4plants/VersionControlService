namespace VersionControlService.Support

open System

[<RequireQualifiedAccess>]
module PathHelpers =

    let formatContractErrors (errors: string[]) =
        errors |> Array.map string |> String.concat "\n"

    /// normalizes the path by replacing backslashes with forward slashes, trimming whitespace, and removing trailing slashes
    let normalizeSeparators (path: string) = path.Replace("\\", "/")

    /// normalizes the path by replacing backslashes with forward slashes, trimming whitespace, and removing trailing slashes
    let normalizePath (path: string) =
        normalizeSeparators path |> fun normalized -> normalized.Trim().TrimEnd('/')

    let normalizeRelativePath (path: string) =
        normalizeSeparators path |> fun normalized -> normalized.Trim('/').Trim()

    let normalizeCanonicalRelativePath (path: string) =
        path |> normalizeRelativePath |> normalizePath

    let normalizeForComparison (path: string) =
        normalizeSeparators path
        |> fun normalized -> normalized.Trim().TrimEnd('/').ToLowerInvariant()

    let normalizePathForFsComparison (path: string) =
        path |> normalizePath |> normalizeForComparison

    let isSameOrDescendantPath (path: string) (ancestorPath: string) =
        let normalizedPath = normalizePath path
        let normalizedAncestorPath = normalizePath ancestorPath

        String.IsNullOrWhiteSpace normalizedAncestorPath
        || normalizedPath = normalizedAncestorPath
        || normalizedPath.StartsWith(normalizedAncestorPath + "/", StringComparison.OrdinalIgnoreCase)

    let isSameOrDescendantPathForFsComparison (path: string) (ancestorPath: string) =
        let normalizedPath = normalizePathForFsComparison path
        let normalizedAncestorPath = normalizePathForFsComparison ancestorPath

        not (String.IsNullOrWhiteSpace normalizedPath)
        && not (String.IsNullOrWhiteSpace normalizedAncestorPath)
        && (normalizedPath = normalizedAncestorPath
            || normalizedPath.StartsWith(normalizedAncestorPath + "/"))

    let containsPathTraversalSegments (path: string) =
        normalizeSeparators path
        |> fun normalized -> normalized.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.exists (fun segment -> segment = "." || segment = "..")

    let isSafePathSegment (value: string) =
        not (String.IsNullOrWhiteSpace value)
        && not (value.Contains "/")
        && not (value.Contains "\\")
        && not (value.Contains "..")

    let pathsEqual (left: string) (right: string) =
        normalizeForComparison left = normalizeForComparison right

    let pathMatchesAny (candidates: string seq) (path: string) =
        candidates |> Seq.exists (fun candidate -> pathsEqual candidate path)

    let getNameFromPath (path: string) =
        normalizePath path |> (fun normalized -> normalized.Split('/')) |> Array.last

    let tryGetParentPath (path: string) =
        let normalizedPath = normalizePath path
        let separatorIndex = normalizedPath.LastIndexOf('/')

        if separatorIndex < 0 then
            None
        else
            Some(normalizedPath.Substring(0, separatorIndex))

    /// normalizes the path and splits it into parts
    let getPathParts (path: string) =
        normalizePath path |> fun p -> p.Split('/')

    let getFileName (path: string) = path |> getPathParts |> Array.last

    let isProtectedDeleteTarget protectedDeleteTargetNames (normalizedRelativePath: string) =
        normalizedRelativePath
        |> getFileName
        |> pathMatchesAny protectedDeleteTargetNames

    let pathExistsInSnapshot (existingPaths: string seq) (targetPath: string) =
        existingPaths
        |> Seq.exists (fun existingPath -> pathsEqual existingPath targetPath)
