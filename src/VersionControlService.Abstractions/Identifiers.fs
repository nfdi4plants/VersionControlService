namespace VersionControlService.Abstractions

open System

/// Validated opaque provider identity such as "git", "lakefs", or "vendor.product".
/// Not a closed union: external providers register their own identifiers.
type ProviderId =
    private
    | ProviderId of string

/// Opaque provider revision identity (for Git a commit hash, for lakeFS a commit id).
type RevisionId =
    private
    | RevisionId of string

/// Opaque provider ref carried unchanged between provider and consumer.
type ProviderRef =
    private
    | ProviderRef of string

module ProviderId =

    let private isValidCharacter (character: char) =
        Char.IsLetterOrDigit character
        || character = '.'
        || character = '-'
        || character = '_'

    /// Accepts lowercase alphanumeric identifiers with '.', '-', '_' separators, e.g. "git" or "vendor.product".
    let tryCreate (value: string) : Result<ProviderId, string> =
        let normalized =
            value |> Option.ofObj |> Option.map _.Trim() |> Option.defaultValue String.Empty

        if String.IsNullOrWhiteSpace normalized then
            Error "Provider ID must not be empty."
        elif normalized |> Seq.exists (isValidCharacter >> not) then
            Error $"Provider ID '{normalized}' contains characters outside letters, digits, '.', '-', and '_'."
        elif not (Char.IsLetterOrDigit normalized[0]) then
            Error $"Provider ID '{normalized}' must start with a letter or digit."
        else
            Ok(ProviderId normalized)

    let value (ProviderId providerId) = providerId

/// Well-known provider identifiers. External identifiers are equally valid.
module WellKnownProviderIds =

    [<Literal>]
    let Git = "git"

    [<Literal>]
    let LakeFs = "lakefs"

module RevisionId =

    let tryCreate (value: string) : Result<RevisionId, string> =
        let normalized =
            value |> Option.ofObj |> Option.map _.Trim() |> Option.defaultValue String.Empty

        if String.IsNullOrWhiteSpace normalized then
            Error "Revision ID must not be empty."
        else
            Ok(RevisionId normalized)

    let value (RevisionId revisionId) = revisionId

module ProviderRef =

    let tryCreate (value: string) : Result<ProviderRef, string> =
        if isNull value || String.IsNullOrWhiteSpace value then
            Error "Provider ref must not be empty."
        else
            Ok(ProviderRef value)

    let value (ProviderRef providerRef) = providerRef

/// Nonsecret provider repository location. Credential material is resolved separately
/// from ConnectionProfileId by an injected credential service; it never lives here.
type RepositoryLocation = {
    ProviderId: ProviderId
    DisplayName: string option
    /// Nonsecret provider-owned location data (URL, lakefs://repo/ref/prefix, ...).
    ProviderLocation: string
    ConnectionProfileId: string option
}

module RepositoryLocation =

    /// Removes HTTP(S) userinfo and passwords from SSH userinfo. Providers use this when creating bindings so credentials do not reach a stored binding.
    let withoutUserInfo (location: string) : string =
        if isNull location then
            location
        else
            let schemeSeparatorIndex = location.IndexOf("://", StringComparison.Ordinal)

            if schemeSeparatorIndex < 0 then
                location
            else
                let scheme = location.Substring(0, schemeSeparatorIndex)
                let authorityStart = schemeSeparatorIndex + 3
                let mutable authorityEnd = location.Length
                let mutable index = authorityStart

                while index < location.Length && authorityEnd = location.Length do
                    match location[index] with
                    | '/'
                    | '?'
                    | '#' -> authorityEnd <- index
                    | _ -> ()

                    index <- index + 1

                let authority = location.Substring(authorityStart, authorityEnd - authorityStart)
                let userInfoEnd = authority.LastIndexOf('@')

                if
                    String.Equals(scheme, "http", StringComparison.OrdinalIgnoreCase)
                    || String.Equals(scheme, "https", StringComparison.OrdinalIgnoreCase)
                then
                    if userInfoEnd < 0 then
                        location
                    else
                        location.Substring(0, authorityStart)
                        + authority.Substring(userInfoEnd + 1)
                        + location.Substring(authorityEnd)
                elif String.Equals(scheme, "ssh", StringComparison.OrdinalIgnoreCase) && userInfoEnd >= 0 then
                    let userInfo = authority.Substring(0, userInfoEnd)
                    let passwordSeparator = userInfo.IndexOf(':')

                    if passwordSeparator < 0 then
                        location
                    else
                        location.Substring(0, authorityStart)
                        + userInfo.Substring(0, passwordSeparator)
                        + "@"
                        + authority.Substring(userInfoEnd + 1)
                        + location.Substring(authorityEnd)
                else
                    location
