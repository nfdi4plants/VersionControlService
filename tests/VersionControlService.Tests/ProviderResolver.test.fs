module VersionControlService.Tests.ProviderResolverTests

open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.Tests.NodePath
open Vitest

module ProviderResolver = VersionControlService.Abstractions.ProviderResolver

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

let private createTempDirectoryAsync () : JS.Promise<string> =
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-resolver-tests-" |]
    fsPromisesDynamic?mkdtemp (prefix) |> unbox<JS.Promise<string>>

let private removeDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (path, createObj [ "recursive" ==> true; "force" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private ensureDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?mkdir (path, createObj [ "recursive" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private writeUtf8FileAsync (path: string) (content: string) : JS.Promise<unit> = promise {
    let! _ = fsPromisesDynamic?writeFile (path, content, "utf8") |> unbox<JS.Promise<obj>>
    return ()
}

let private pathExistsAsync (path: string) : JS.Promise<bool> = promise {
    try
        let! _ = fsPromisesDynamic?stat (path) |> unbox<JS.Promise<obj>>
        return true
    with _ ->
        return false
}

let private providerId (name: string) =
    match ProviderId.tryCreate name with
    | Ok id -> id
    | Error message -> failwith message

let private notExercised (name: string) : OperationResult<'T> =
    OperationResult.failed (OperationFailure.create Unsupported "not_exercised" $"{name} is not exercised.")

/// Minimal factory whose session reflects its binding's connection profile, so tests
/// can prove per-session configuration flows through Open.
let private createProbeFactory (idName: string) (probe: string -> Async<ProbeResult>) : ProviderFactory =
    let id = providerId idName

    {
        Id = id
        Probe = probe
        VerifyLocation = fun _ _ -> async { return notExercised "VerifyLocation" }
        Initialize = fun _ _ -> async { return notExercised "Initialize" }
        Clone = fun _ _ -> async { return notExercised "Clone" }
        Adopt =
            fun _ _ ->
                async {
                    return
                        OperationResult.failed (
                            OperationFailure.create
                                Unsupported
                                "operation_not_supported"
                                "Adoption is not supported by this provider."
                        )
                }
        Bind = fun _ _ -> async { return notExercised "Bind" }
        Open =
            fun binding _ -> async {
                let descriptor = {
                    ProviderId = binding.ProviderId
                    WorkspaceRoot = binding.WorkspaceRoot
                    Location = Some binding.Location
                }

                let profileToken =
                    binding.ConnectionProfileId |> Option.defaultValue "anonymous"

                let core: CoreVersionControl = {
                    GetStatus =
                        fun _ -> async {
                            return
                                OperationResult.succeeded {
                                    CurrentRef = None
                                    WorkspaceVersion = $"profile:{profileToken}"
                                    Changes = [||]
                                    ActiveConflictSession = None
                                    Synchronization = None
                                }
                        }
                    ListRefs = fun _ -> async { return OperationResult.succeeded [||] }
                    CreateRef = fun _ _ -> async { return notExercised "CreateRef" }
                    PreflightSwitchRef = fun _ _ -> async { return notExercised "PreflightSwitchRef" }
                    SwitchRef = fun _ _ -> async { return notExercised "SwitchRef" }
                    CreateRevision = fun _ _ -> async { return notExercised "CreateRevision" }
                    RestorePaths = fun _ _ -> async { return notExercised "RestorePaths" }
                    GetDiffSummary = fun _ -> async { return notExercised "GetDiffSummary" }
                }

                return OperationResult.succeeded (WorkspaceSession.createCoreOnly descriptor core)
            }
        CheckDependencies = fun _ -> async { return OperationResult.succeeded [||] }
        InstallDependency =
            fun _ _ ->
                async {
                    return
                        OperationResult.failed (
                            OperationFailure.create
                                Unsupported
                                "operation_not_supported"
                                "Dependency installation is not supported by this provider."
                        )
                }
    }

let private neverDetect (_path: string) : Async<ProbeResult> = async { return NotDetected }

let private detectUnder (root: string) (path: string) : Async<ProbeResult> =
    async {
        let normalizedRoot = ProviderResolver.normalizePath root
        let normalizedPath = ProviderResolver.normalizePath path

        if
            normalizedPath = normalizedRoot
            || normalizedPath.ToLowerInvariant().StartsWith(normalizedRoot.ToLowerInvariant() + "/")
        then
            return Detected(normalizedRoot, 100, None)
        else
            return NotDetected
    }

let private createCatalog (factoryList: ProviderFactory list) =
    match ProviderResolver.tryCreateCatalog factoryList with
    | Ok catalog -> catalog
    | Error message -> failwith message

let private location (id: ProviderId) (profile: string option) : RepositoryLocation = {
    ProviderId = id
    DisplayName = None
    ProviderLocation = "fake://repository"
    ConnectionProfileId = profile
}

let private binding (id: ProviderId) (root: string) (profile: string option) : WorkspaceBinding = {
    SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
    ProviderId = id
    WorkspaceRoot = root
    ProviderStateRef = None
    Location = location id profile
    ConnectionProfileId = profile
}

let private request path sensitivity binding : ProviderResolver.ResolutionRequest = {
    WorkspacePath = path
    ExplicitBinding = binding
    PathCaseSensitivity = sensitivity
}

let private resolveAsync catalog request =
    Async.StartAsPromise(ProviderResolver.resolve catalog request)

let private expectSucceeded (operationName: string) (result: OperationResult<'T>) : 'T =
    match result with
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded _ -> failwith $"{operationName} unexpectedly returned partial success."
    | Failed failure -> failwith $"{operationName} failed ({failure.Category}/{failure.Code}): {failure.Message}"

Vitest.describe (
    "provider resolver",
    fun () ->
        Vitest.test (
            "an explicit binding wins over probes",
            fun () -> promise {
                let mutable probeConsulted = false

                let gitFactory = createProbeFactory "git" neverDetect

                let lakeFactory =
                    createProbeFactory "lakefs" (fun path ->
                        probeConsulted <- true
                        detectUnder "/workspace" path)

                let catalog = createCatalog [ gitFactory; lakeFactory ]
                let explicitBinding = binding gitFactory.Id "/workspace" (Some "profile-a")

                let! report =
                    resolveAsync catalog (request "/workspace" CaseSensitive (Some explicitBinding))

                match report.Resolution with
                | ProviderResolver.Bound(boundBinding, factory) ->
                    Vitest.expect(ProviderId.value factory.Id).toBe ("git")
                    Vitest.expect(boundBinding.ConnectionProfileId).toEqual (Some "profile-a")
                | _ -> failwith "Expected the explicit binding to win."

                Vitest.expect(probeConsulted).toBe (false)
            }
        )

        Vitest.test (
            "provider resolver honors explicit path case semantics",
            fun () -> promise {
                let catalog =
                    createCatalog [ createProbeFactory "git" (detectUnder "/Repo") ]

                let! sensitive =
                    resolveAsync catalog (request "/repo/file" CaseSensitive None)

                let! insensitive =
                    resolveAsync catalog (request "/repo/file" CaseInsensitive None)

                match sensitive.Resolution with
                | ProviderResolver.UnmanagedWorkspace -> ()
                | _ -> failwith "Expected case-sensitive resolution to leave the workspace unmanaged."

                match insensitive.Resolution with
                | ProviderResolver.ProbedWorkspace(factory, _) ->
                    Vitest.expect(ProviderId.value factory.Id).toBe ("git")
                | _ -> failwith "Expected case-insensitive resolution to select Git."
            }
        )

        Vitest.test (
            "no probe match returns Unmanaged",
            fun () -> promise {
                let catalog =
                    createCatalog [
                        createProbeFactory "git" neverDetect
                        createProbeFactory "lakefs" neverDetect
                    ]

                let! report =
                    resolveAsync catalog (request "/plain/directory" CaseSensitive None)

                match report.Resolution with
                | ProviderResolver.UnmanagedWorkspace -> Vitest.expect(report.Diagnostics.Length).toBe (0)
                | _ -> failwith "Expected an unmanaged workspace."
            }
        )

        Vitest.test (
            "two exact matches return Ambiguous instead of registration order",
            fun () -> promise {
                let catalog =
                    createCatalog [
                        createProbeFactory "git" (detectUnder "/workspace")
                        createProbeFactory "lakefs" (detectUnder "/workspace")
                    ]

                let! report =
                    resolveAsync catalog (request "/workspace/sub" CaseSensitive None)

                match report.Resolution with
                | ProviderResolver.AmbiguousWorkspace candidates ->
                    Vitest.expect(candidates.Length).toBe (2)

                    let candidateIds =
                        candidates |> Array.map (fun c -> ProviderId.value c.ProviderId) |> Array.sort

                    Vitest.expect(candidateIds).toEqual ([| "git"; "lakefs" |])
                | _ -> failwith "Expected an ambiguous workspace."
            }
        )

        Vitest.test (
            "a throwing detector becomes structured probe diagnostics",
            fun () -> promise {
                let throwingFactory =
                    createProbeFactory "vendor.broken" (fun _ -> async { return failwith "detector exploded" })

                let catalog =
                    createCatalog [ throwingFactory; createProbeFactory "git" neverDetect ]

                let! report =
                    resolveAsync catalog (request "/workspace" CaseSensitive None)

                match report.Resolution with
                | ProviderResolver.UnmanagedWorkspace ->
                    Vitest.expect(report.Diagnostics.Length).toBe (1)
                    Vitest.expect(report.Diagnostics[0].Code).toBe ("probe_exception")
                    Vitest.expect(report.Diagnostics[0].Message.Contains "vendor.broken").toBe (true)
                | _ -> failwith "Expected an unmanaged workspace with diagnostics."
            }
        )

        Vitest.test (
            "the nearest owning root is selected for nested paths",
            fun () -> promise {
                let catalog =
                    createCatalog [
                        createProbeFactory "git" (detectUnder "/repo")
                        createProbeFactory "vendor.inner" (detectUnder "/repo/inner")
                    ]

                let! report =
                    resolveAsync catalog (request "/repo/inner/deep/file-parent" CaseSensitive None)

                match report.Resolution with
                | ProviderResolver.ProbedWorkspace(factory, candidate) ->
                    Vitest.expect(ProviderId.value factory.Id).toBe ("vendor.inner")
                    Vitest.expect(candidate.Root).toBe ("/repo/inner")
                | _ -> failwith "Expected the nearest owning root to win."
            }
        )

        Vitest.test (
            "a .git directory plus a .lakefs_ref.yaml marker at one root is ambiguous without a binding",
            fun () -> promise {
                let! rootPath = createTempDirectoryAsync ()

                try
                    do! ensureDirectoryAsync (join [| rootPath; ".git" |])
                    do! writeUtf8FileAsync (join [| rootPath; ".lakefs_ref.yaml" |]) "ref: main\n"

                    let markerProbe (marker: string) (path: string) : Async<ProbeResult> =
                        async {
                            let! markerExists =
                                Async.AwaitPromise(pathExistsAsync (join [| path; marker |]))

                            if markerExists then
                                return Detected(ProviderResolver.normalizePath path, 100, None)
                            else
                                return NotDetected
                        }

                    let catalog =
                        createCatalog [
                            createProbeFactory "git" (markerProbe ".git")
                            createProbeFactory "lakefs" (markerProbe ".lakefs_ref.yaml")
                        ]

                    let! report =
                        resolveAsync catalog (request rootPath CaseSensitive None)

                    match report.Resolution with
                    | ProviderResolver.AmbiguousWorkspace candidates -> Vitest.expect(candidates.Length).toBe (2)
                    | _ -> failwith "Expected ambiguity for a lakeFS checkout inside a Git repository."

                    // An explicit binding resolves the same directory deterministically.
                    let lakeId = providerId "lakefs"

                    let! boundReport =
                        resolveAsync catalog (request rootPath CaseSensitive (Some(binding lakeId rootPath (Some "lake-profile"))))

                    match boundReport.Resolution with
                    | ProviderResolver.Bound(_, factory) -> Vitest.expect(ProviderId.value factory.Id).toBe ("lakefs")
                    | _ -> failwith "Expected the explicit binding to disambiguate."

                    do! removeDirectoryAsync rootPath
                with error ->
                    do! removeDirectoryAsync rootPath
                    return raise error
            }
        )

        Vitest.test (
            "two sessions of the same provider ID use different connection profiles",
            fun () -> promise {
                let gitFactory = createProbeFactory "git" neverDetect
                let catalog = createCatalog [ gitFactory ]

                let openSession root profile = promise {
                    let! report =
                        resolveAsync catalog (request root CaseSensitive (Some(binding gitFactory.Id root (Some profile))))

                    match report.Resolution with
                    | ProviderResolver.Bound(boundBinding, factory) ->
                        let context = OperationContext.detached "resolver-open"
                        let! openResult = Async.StartAsPromise(factory.Open boundBinding context)
                        return expectSucceeded "open" openResult
                    | _ -> return failwith "Expected a bound workspace."
                }

                let! firstSession = openSession "/vault-one" "profile-one"
                let! secondSession = openSession "/vault-two" "profile-two"

                let context = OperationContext.detached "resolver-status"
                let! firstStatus = Async.StartAsPromise(firstSession.Core.GetStatus context)
                let! secondStatus = Async.StartAsPromise(secondSession.Core.GetStatus context)

                Vitest.expect((expectSucceeded "first status" firstStatus).WorkspaceVersion).toBe ("profile:profile-one")
                Vitest.expect((expectSucceeded "second status" secondStatus).WorkspaceVersion).toBe ("profile:profile-two")
            }
        )

        Vitest.test (
            "registering an external provider ID requires no library edit",
            fun () -> promise {
                let externalFactory =
                    createProbeFactory "vendor.external-dvcs" (detectUnder "/external")

                let catalog =
                    createCatalog [ createProbeFactory "git" neverDetect; externalFactory ]

                let! report =
                    resolveAsync catalog (request "/external/project" CaseSensitive None)

                match report.Resolution with
                | ProviderResolver.ProbedWorkspace(factory, _) ->
                    Vitest.expect(ProviderId.value factory.Id).toBe ("vendor.external-dvcs")
                | _ -> failwith "Expected the external provider to be detected."
            }
        )

        Vitest.test (
            "duplicate provider registrations are rejected at catalog construction",
            fun () ->
                match
                    ProviderResolver.tryCreateCatalog [
                        createProbeFactory "git" neverDetect
                        createProbeFactory "git" neverDetect
                    ]
                with
                | Ok _ -> failwith "Expected duplicate registration to be rejected."
                | Error message -> Vitest.expect(message.Contains "git").toBe (true)
        )
)
