/// The raw output channel of an operation. A runner opens one command per subprocess, feeds it the decoded
/// text of each stream and finishes it when the process ends. The channel redacts every line, collapses
/// carriage-return rewrites, bounds line length and queue size, and hands the host batches. A runner
/// asks for the channel through the operation context, so nothing runs for a host that set no callback.
module VersionControlService.Runtime.Node.OutputReporter

open System
open System.Text
open System.Text.RegularExpressions
open Fable.Core
open VersionControlService.Abstractions

/// Longest line the channel keeps, in UTF-16 units. A longer line is dropped and a notice takes its place.
[<Literal>]
let MaxLineLength = 4096

/// Most entries in one delivered batch.
[<Literal>]
let MaxBatchEntries = 20

/// Most entries waiting for delivery. A full queue drops its oldest line.
[<Literal>]
let MaxQueuedEntries = 128

/// The shortest time between two batches, which allows ten batches per second.
let BatchIntervalMilliseconds = 100.0

/// The shortest time between two snapshots of a line that is being rewritten.
let SnapshotIntervalMilliseconds = 250.0

/// Most arguments a command header shows after the leading configuration pairs.
[<Literal>]
let MaxHeaderArguments = 12

/// Longest argument a command header shows. The cut keeps a header inside the line limit.
[<Literal>]
let MaxHeaderArgumentLength = 200

/// A secret shorter than this is not replaced, because it would also hit unrelated text.
[<Literal>]
let MinSecretLength = 4

/// Time source of a channel. Tests substitute a clock they control.
type OutputClock = {
    Now: unit -> float
    /// Runs the callback after the delay in milliseconds and returns a function that cancels it.
    Schedule: float -> (unit -> unit) -> (unit -> unit)
}

[<Emit("Date.now()")>]
let private dateNow () : float = jsNative

let systemClock: OutputClock = {
    Now = dateNow
    Schedule =
        fun delay callback ->
            let handle = JS.setTimeout callback (int (Math.Ceiling delay))
            fun () -> JS.clearTimeout handle
}

[<Emit("(() => { try { return Buffer.from($0, 'base64').toString('utf8'); } catch (e) { return ''; } })()")>]
let private decodeBase64 (_value: string) : string = jsNative

[<Emit("(() => { try { return decodeURIComponent($0); } catch (e) { return $0; } })()")>]
let private decodeUriComponent (_value: string) : string = jsNative

[<Emit("encodeURIComponent($0)")>]
let private encodeUriComponent (_value: string) : string = jsNative

/// The string entries of an environment object, for the secret scan.
[<Emit("Object.entries($0 ?? {}).filter(entry => typeof entry[1] === 'string')")>]
let environmentEntries (_environment: obj) : (string * string)[] = jsNative

let private escapeSequencePattern =
    Regex(@"\u001b\[[0-?]*[ -/]*[@-~]|\u001b\][^\u0007\u001b]*(?:\u0007|\u001b\\)|\u001b[@-Z\\-_]")

// Tab stays. Line terminators never reach the pattern, because the framer splits on them.
let private controlCharacterPattern = Regex(@"[\u0000-\u0008\u000b-\u001f\u007f-\u009f]")

// A user name can hold an "@" (an e-mail address), so the userinfo ends at the last "@" before the first "/".
let private urlUserInfoPattern = Regex(@"://(?<userinfo>[^/\s]+)@")

let private headerValuePattern =
    Regex(
        @"(?:authorization\s*:\s*(?:(?<scheme>bearer|basic|token)\s+)?|private-token\s*:\s*|x-access-token\s*:\s*)(?<value>[^\s'""]+)",
        RegexOptions.IgnoreCase
    )

let private credentialNamePattern =
    Regex(@"token|secret|passw|passphrase|credential|apikey|api_key|access_key", RegexOptions.IgnoreCase)

let private addSecret (secrets: ResizeArray<string>) (value: string) =
    if not (isNull value) && value.Length >= MinSecretLength then
        secrets.Add value

// A value as written, decoded and encoded, because a URL carries the percent-encoded form of a secret.
let private addEncodedForms (secrets: ResizeArray<string>) (value: string) =
    if not (String.IsNullOrEmpty value) then
        let decoded = decodeUriComponent value
        addSecret secrets value
        addSecret secrets decoded
        addSecret secrets (encodeUriComponent decoded)

let private addUserInfo (secrets: ResizeArray<string>) (userInfo: string) =
    addEncodedForms secrets userInfo
    let colon = userInfo.IndexOf ':'

    if colon >= 0 then
        addEncodedForms secrets (userInfo.Substring(0, colon))
        addEncodedForms secrets (userInfo.Substring(colon + 1))

// The payload of a Basic header is base64 text of "user:secret", and the parts are secrets of their own.
let private addHeaderValue (secrets: ResizeArray<string>) (scheme: string) (value: string) =
    addSecret secrets value

    if String.Equals(scheme, "basic", StringComparison.OrdinalIgnoreCase) then
        let decoded = decodeBase64 value
        let colon = decoded.IndexOf ':'

        if colon > 0 && decoded.IndexOf '�' < 0 then
            addSecret secrets decoded
            addSecret secrets (decoded.Substring(0, colon))
            addSecret secrets (decoded.Substring(colon + 1))

let private collectFromText (secrets: ResizeArray<string>) (text: string) =
    if not (String.IsNullOrEmpty text) then
        for matched in urlUserInfoPattern.Matches text do
            addUserInfo secrets matched.Groups["userinfo"].Value

        for matched in headerValuePattern.Matches text do
            addHeaderValue secrets matched.Groups["scheme"].Value matched.Groups["value"].Value

/// The secrets a command carries, longest first so that "user:secret" is replaced before "secret". They come
/// from the authentication in the arguments (config values, URL userinfo, Authorization headers and the
/// decoded Basic payload) and from credential-looking environment entries. A value shorter than four
/// characters is left out.
let collectSecrets (arguments: string[]) (environment: (string * string)[]) : string[] =
    let secrets = ResizeArray<string>()
    let mutable index = 0

    while index < arguments.Length do
        if arguments[index] = "-c" && index + 1 < arguments.Length then
            let setting = arguments[index + 1]
            collectFromText secrets setting
            let equals = setting.IndexOf '='

            if equals > 0 && credentialNamePattern.IsMatch(setting.Substring(0, equals)) then
                addSecret secrets (setting.Substring(equals + 1))

            index <- index + 2
        else
            collectFromText secrets arguments[index]
            index <- index + 1

    for name, value in environment do
        collectFromText secrets value

        if credentialNamePattern.IsMatch name then
            addSecret secrets value

    secrets
    |> Seq.distinct
    |> Seq.sortByDescending (fun secret -> secret.Length)
    |> Seq.toArray

/// Replaces each known secret and then applies the structural redaction.
let redactText (secrets: string[]) (text: string) : string =
    let mutable result = text

    for secret in secrets do
        if result.Contains secret then
            result <- result.Replace(secret, "[REDACTED]")

    Redaction.redact result

/// The arguments after the leading `-c key=value` pairs, which can hold credentials.
let withoutLeadingConfig (arguments: string[]) : string[] =
    let mutable first = 0

    while first + 1 < arguments.Length && arguments[first] = "-c" do
        first <- first + 2

    arguments |> Array.skip first

/// The header line of a command: the program and its arguments without the leading configuration pairs.
/// It shows at most twelve arguments and then "...". The header redacts each argument with the secrets
/// before it cuts the argument to 200 characters, because a cut could leave a part of a long secret that
/// no longer matches.
let commandHeader (secrets: string[]) (command: string) (arguments: string[]) : string =
    let shown = withoutLeadingConfig arguments

    let clip (argument: string) =
        let redacted = redactText secrets argument

        if redacted.Length > MaxHeaderArgumentLength then
            redacted.Substring(0, MaxHeaderArgumentLength) + "..."
        else
            redacted

    let words =
        Array.append [| redactText secrets command |] (shown |> Array.truncate MaxHeaderArguments |> Array.map clip)

    let header = String.Join(" ", words)

    if shown.Length > MaxHeaderArguments then header + " ..." else header

/// The batching queue of one operation. Entries wait here until the rate limit lets a batch go. A full
/// queue drops its oldest line and keeps the command headers, unless it holds nothing but headers. Flush
/// bypasses the rate limit, so the end of each subprocess delivers its pending entries at once. Within a
/// subprocess the limit holds.
[<AllowNullLiteral>]
type OutputChannel(clock: OutputClock, deliver: OperationOutput[] -> unit) =
    let queue = ResizeArray<OperationOutput>()
    let mutable omitted = 0
    let mutable lastDelivery = -1.0e12
    let mutable cancelTimer: (unit -> unit) option = None
    let mutable commandCount = 0

    let hasPending () = queue.Count > 0 || omitted > 0

    let takeBatch () =
        let batch = ResizeArray<OperationOutput>()

        if omitted > 0 then
            batch.Add {
                CommandId = ""
                Kind = OperationOutputKind.Notice
                Text = if omitted = 1 then "1 output line was omitted." else $"{omitted} output lines were omitted."
                IsPartial = false
            }

            omitted <- 0

        let count = min queue.Count (MaxBatchEntries - batch.Count)
        batch.AddRange(queue.GetRange(0, count))
        queue.RemoveRange(0, count)
        batch.ToArray()

    let deliverBatch () =
        let batch = takeBatch ()
        lastDelivery <- clock.Now()

        if batch.Length > 0 then
            try
                deliver batch
            with _ ->
                ()

    let rec pump () =
        if hasPending () then
            let elapsed = clock.Now() - lastDelivery

            if elapsed >= BatchIntervalMilliseconds then
                deliverBatch ()

                if hasPending () && cancelTimer.IsNone then
                    arm BatchIntervalMilliseconds
            elif cancelTimer.IsNone then
                arm (BatchIntervalMilliseconds - elapsed)

    and arm (delay: float) =
        cancelTimer <-
            Some(
                clock.Schedule delay (fun () ->
                    cancelTimer <- None
                    pump ())
            )

    member _.Clock = clock

    /// An identifier for the next command, unique within the channel.
    member _.NextCommandId() =
        commandCount <- commandCount + 1
        string commandCount

    /// Queues an entry. A partial entry that is still waiting for its replacement is replaced in place.
    member _.Enqueue(entry: OperationOutput) =
        let mutable index = queue.Count - 1

        while index >= 0
              && not (queue[index].CommandId = entry.CommandId && queue[index].Kind = entry.Kind) do
            index <- index - 1

        if index >= 0 && queue[index].IsPartial then
            queue[index] <- entry
        else
            if queue.Count >= MaxQueuedEntries then
                // A header names the subprocess that the lines after it belong to, so a line goes first.
                let victim =
                    queue
                    |> Seq.tryFindIndex (fun waiting -> waiting.Kind <> OperationOutputKind.Command)
                    |> Option.defaultValue 0

                queue.RemoveAt victim
                omitted <- omitted + 1

            queue.Add entry

        pump ()

    /// Delivers everything that waits, in batches of at most twenty, and ignores the rate limit. A runner
    /// calls this when a process ends, so the host has the output before the operation completes. The
    /// pending entries of a subprocess therefore arrive at once, whatever the rate was before.
    member _.Flush() =
        cancelTimer |> Option.iter (fun cancel -> cancel ())
        cancelTimer <- None

        while hasPending () do
            deliverBatch ()

/// Turns the text of one stream into lines. It treats CRLF as one newline even across chunks, keeps only the
/// latest segment of a line that is rewritten with carriage returns, drops terminal escape sequences and
/// control characters, and replaces a line over the length limit with a notice.
type private LineFramer
    (
        kind: OperationOutputKind,
        commandId: string,
        secrets: string[],
        clock: OutputClock,
        enqueue: OperationOutput -> unit
    ) =
    let current = StringBuilder()
    // The last segment that a lone carriage return ended. A later segment of the same line replaces it.
    let mutable held: string = null
    let mutable afterCarriageReturn = false
    let mutable overlong = false
    // Text of the partial entry sent for the line in progress, null when none was sent.
    let mutable lastPartial: string = null
    let mutable partialAt = -1.0e12
    let mutable finished = false
    let mutable cancelSnapshotTimer: (unit -> unit) option = None

    let stopSnapshotTimer () =
        cancelSnapshotTimer |> Option.iter (fun cancel -> cancel ())
        cancelSnapshotTimer <- None

    let clean (raw: string) =
        if isNull raw then
            ""
        else
            raw
            |> fun text -> escapeSequencePattern.Replace(text, "")
            |> fun text -> controlCharacterPattern.Replace(text, "")
            |> redactText secrets
            |> fun text -> text.TrimEnd()

    let emit (text: string) (isPartial: bool) =
        enqueue {
            CommandId = commandId
            Kind = kind
            Text = text
            IsPartial = isPartial
        }

    let omitLongLine () =
        enqueue {
            CommandId = commandId
            Kind = OperationOutputKind.Notice
            Text = $"A line longer than {MaxLineLength} characters was omitted."
            IsPartial = false
        }

        overlong <- false
        current.Clear() |> ignore

    // The host keeps a partial entry until a complete one with the same command and kind replaces it.
    let closePartial () =
        if not (isNull lastPartial) then
            emit lastPartial false
            lastPartial <- null

    // The latest segment is the line. A blank line is dropped, except that it closes a snapshot already sent.
    let completeLine () =
        stopSnapshotTimer ()
        let latest = clean (current.ToString())
        let text = if String.IsNullOrWhiteSpace latest then clean held else latest

        if not (String.IsNullOrWhiteSpace text) then
            emit text false
        elif not (isNull lastPartial) then
            emit lastPartial false

        current.Clear() |> ignore
        held <- null
        lastPartial <- null
        afterCarriageReturn <- false

    // The next character decides whether this was half of a CRLF or a rewrite.
    let endSegment () =
        if overlong then
            omitLongLine ()
            // A line feed right after this segment must not complete the text of an earlier segment.
            held <- null
        elif current.Length > 0 then
            held <- current.ToString()
            current.Clear() |> ignore

        afterCarriageReturn <- true

    let endLine () =
        if overlong then
            stopSnapshotTimer ()
            closePartial ()
            omitLongLine ()
            held <- null
        else
            completeLine ()

        afterCarriageReturn <- false

    // A snapshot that the interval holds back goes out when the interval ends, even if no more text comes.
    let rec publishSnapshot () =
        if not finished && not (isNull held) then
            let now = clock.Now()
            let waited = now - partialAt

            if waited >= SnapshotIntervalMilliseconds then
                stopSnapshotTimer ()
                let text = clean held

                if not (String.IsNullOrWhiteSpace text) && text <> lastPartial then
                    emit text true
                    lastPartial <- text
                    partialAt <- now
            elif cancelSnapshotTimer.IsNone then
                cancelSnapshotTimer <-
                    Some(
                        clock.Schedule (SnapshotIntervalMilliseconds - waited) (fun () ->
                            cancelSnapshotTimer <- None
                            publishSnapshot ())
                    )

    member _.Push(text: string) =
        if not finished then
            for character in text do
                if afterCarriageReturn && character = '\n' then
                    completeLine ()
                else
                    afterCarriageReturn <- false

                    if character = '\r' then endSegment ()
                    elif character = '\n' then endLine ()
                    elif overlong then ()
                    elif current.Length >= MaxLineLength then
                        overlong <- true
                        current.Clear() |> ignore
                    else
                        current.Append character |> ignore

            publishSnapshot ()

    /// Sends the final state of the line in progress, which is the unterminated text at the end of a stream.
    member _.Finish() =
        if not finished then
            finished <- true
            stopSnapshotTimer ()

            if overlong then
                closePartial ()
                omitLongLine ()
                held <- null
            else
                completeLine ()

/// The output of one subprocess. The runner passes the decoded text of each stream to it and calls Finish
/// once the process has ended, whatever the reason. Finish flushes the channel, so the end of every
/// subprocess delivers its pending entries at once and the batch rate holds only within a subprocess.
[<AllowNullLiteral>]
type CommandOutput(channel: OutputChannel, commandId: string, secrets: string[], reportStdout: bool) =
    let stdoutFramer =
        LineFramer(OperationOutputKind.StdOut, commandId, secrets, channel.Clock, channel.Enqueue)

    let stderrFramer =
        LineFramer(OperationOutputKind.StdErr, commandId, secrets, channel.Clock, channel.Enqueue)

    let mutable finished = false

    member _.ObserveStdout(text: string) =
        if reportStdout && not finished then
            stdoutFramer.Push text

    member _.ObserveStderr(text: string) =
        if not finished then
            stderrFramer.Push text

    /// Sends the last state of both streams and everything that still waits in the channel.
    member _.Finish() =
        if not finished then
            finished <- true
            stdoutFramer.Finish()
            stderrFramer.Finish()
            channel.Flush()

/// Where a spawned command reports its output. ReportStdout is set only by a call that declared its
/// stdout as diagnostic text.
type OutputTarget = {
    Channel: OutputChannel
    ReportStdout: bool
}

type private ChannelRegistry =
    abstract get: key: obj -> OutputChannel
    abstract set: key: obj * channel: OutputChannel -> unit

[<Emit("new WeakMap()")>]
let private createRegistry () : ChannelRegistry = jsNative

// The context records are copied freely, and the callback function is what all copies share.
let private registry = createRegistry ()

/// A channel with the given clock and delivery function. Tests use it to control time.
let createChannel (clock: OutputClock) (deliver: OperationOutput[] -> unit) : OutputChannel =
    OutputChannel(clock, deliver)

/// The channel of the operation that owns the context, or None when its host set no output callback.
/// All copies of one context share one channel, so the batching limits hold per operation.
let tryOf (context: OperationContext) : OutputChannel option =
    match context.ReportOutput with
    | None -> None
    | Some report ->
        let key = box report
        let existing = registry.get key

        if isNull existing then
            let channel = OutputChannel(systemClock, report)
            registry.set (key, channel)
            Some channel
        else
            Some existing

module OutputTarget =

    /// A target that reports stderr only, or None when the context has no output callback.
    let ofContext (context: OperationContext) : OutputTarget option =
        tryOf context
        |> Option.map (fun channel ->
            {
                Channel = channel
                ReportStdout = false
            })

    /// The same target for a call that declared its stdout as diagnostic text.
    let withStdout (target: OutputTarget option) : OutputTarget option =
        target |> Option.map (fun current -> { current with ReportStdout = true })

/// Starts a command: queues its header and returns the object that frames its streams. `headerArguments`
/// feed the header and `secretArguments` feed the secret scan. They differ when a tool applies part of its
/// configuration itself. The header leaves out the leading `-c` pairs.
let beginCommand
    (channel: OutputChannel)
    (command: string)
    (headerArguments: string[])
    (secretArguments: string[])
    (environment: (string * string)[])
    (reportStdout: bool)
    : CommandOutput =
    let secrets = collectSecrets secretArguments environment
    let commandId = channel.NextCommandId()

    channel.Enqueue {
        CommandId = commandId
        Kind = OperationOutputKind.Command
        Text = commandHeader secrets command headerArguments
        IsPartial = false
    }

    CommandOutput(channel, commandId, secrets, reportStdout)
