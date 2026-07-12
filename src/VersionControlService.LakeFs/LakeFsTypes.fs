/// DTOs for the pinned lakeFS v1.83.0 API surface. Parsed dynamically from JSON;
/// only the fields this provider consumes are modeled.
module VersionControlService.LakeFs.LakeFsTypes

/// Nonsecret connection data. The secret access key arrives separately through
/// the injected credential resolver and never lives in bindings or requests.
type LakeFsConnection = {
    /// e.g. http://localhost:8000
    Endpoint: string
    AccessKeyId: string
    SecretAccessKey: string
}

type LakeFsPagination = {
    HasMore: bool
    NextOffset: string
}

type LakeFsCommit = {
    Id: string
    Parents: string[]
    Message: string
}

type LakeFsBranch = {
    Id: string
    CommitId: string
}

type LakeFsObjectStats = {
    Path: string
    Checksum: string
    SizeBytes: float
    Mtime: float
}

type LakeFsDiffEntry = {
    Path: string
    /// added | removed | changed | conflict | prefix_changed
    DiffType: string
}

type LakeFsMergeResult = {
    Reference: string
}

/// Parsed lakeFS repository location: lakefs://repository/ref/prefix.
type LakeFsLocation = {
    Repository: string
    TargetRef: string
    Prefix: string
}

module LakeFsLocation =

    /// Parses "lakefs://repo/ref" or "lakefs://repo/ref/prefix/..." locations.
    let tryParse (providerLocation: string) : Result<LakeFsLocation, string> =
        if not (providerLocation.StartsWith "lakefs://") then
            Error "A lakeFS location must start with lakefs://."
        else
            let remainder = providerLocation.Substring "lakefs://".Length
            let segments = remainder.Split '/'

            if segments.Length < 2 || segments[0] = "" || segments[1] = "" then
                Error "A lakeFS location needs at least lakefs://repository/ref."
            else
                Ok {
                    Repository = segments[0]
                    TargetRef = segments[1]
                    Prefix =
                        if segments.Length > 2 then
                            (segments |> Array.skip 2 |> String.concat "/").TrimEnd '/'
                        else
                            ""
                }
