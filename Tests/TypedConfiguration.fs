﻿module Tests.TypedConfiguration

open Xunit

open FsConfigLoader
open ProcessAttachments.ImapKit.ImapService

type PortStructure = { Port: int }

let fromJsonText = fun s -> FromJson.loadFromText (Core.ConfigSource(s))
let fromJsonFile = fun filename -> FromJson.loadFromFile (Core.ConfigSource(None, filename))

[<Fact>]
let ``Generated configuration should match input`` () =
    { Hostname = "test"; Port = 123 }
    |> TypedConfiguration.generateConfiguration
    |> (fun jsonText -> FromJson.loadFromText (Core.ConfigSource(jsonText)))
    |> Result.map (
        fun config ->
            let r = Read.read config
            Assert.Equal("test", r "hostname" |> Option.get)
            Assert.Equal("123", r "port" |> Option.get)
        )
    |> ignore

[<Fact>]
let ``Given an integer-valued configuration parameter, when it is parsed, the result should be Some integer`` () =
    let portVal = 101
    { Port = portVal }
    |> TypedConfiguration.generateConfiguration
    |> fromJsonText
    |> Result.map (
        fun config ->
            let r p = Read.read config p |> Option.bind Parsers.tryParseInt |> Option.get
            Assert.True(r "Port" = portVal)
        )
    |> ignore

[<Fact>]
let ``IMAP endpoint configuration should be loaded from the specified configuration file`` () =
    let maybeConfig = fromJsonFile "Config2.json"

    match maybeConfig with
    | Result.Ok config ->
        let maybeServiceParams = TypedConfiguration.imapServiceParameters config
        maybeServiceParams
        |> Option.iter (
            fun serviceParameters ->
                Assert.Equal("imap.example.com", serviceParameters.Endpoint.Hostname)
                Assert.Equal(999, serviceParameters.Endpoint.Port)
                Assert.Equal(@"goodguy", serviceParameters.Credentials.UserName)
                Assert.Equal(@"doinggood", serviceParameters.Credentials.Password)
            )
        Assert.False(maybeServiceParams.IsNone)
    | Result.Error msg -> Assert.True(false, msg)

[<Fact>]
let ``Mailbox configuration should be loaded from the specified configuration file`` () =
    match fromJsonFile "Config3.json" with
    | Result.Ok config ->
        let maybeMailboxParams = TypedConfiguration.mailboxParameters config
        maybeMailboxParams
        |> Option.iter (
            fun mailboxParameters ->
                let (TypedConfiguration.SourceMailFolders sourceFolders) = mailboxParameters.SourceFolders
                let materialized = Array.ofSeq sourceFolders
                Assert.Equal(2, materialized.Length)
                Assert.Contains("AmonitoredFolder", materialized)
                Assert.Contains("AnotherFolder", materialized)
            )
        Assert.False(maybeMailboxParams.IsNone, $"Configuration parameters should not be empty [{maybeMailboxParams}]")
    | Result.Error msg -> Assert.True(false, msg)

[<Fact>]
let ``Attachment Storage configuration should be loaded from the specified configuration file`` () =
    match fromJsonFile "Config4.json" with
    | Result.Ok config ->
        let maybeExportParams = TypedConfiguration.exportParameters config
        maybeExportParams
        |> Option.iter (
            fun exportParameters ->
                Assert.Equal("/var/data/export/attachments", exportParameters.DestinationFolder)
            )
        Assert.False(maybeExportParams.IsNone, $"Configuration parameters should not be empty [{maybeExportParams}]")
    | Result.Error msg -> Assert.True(false, msg)

let ignoreIfContainsAnyOfThese = seq { "blue"; "red" }
let ignoreIfEndsInAnyOfThese = seq { "end"; "excluded" }
let ignoreBasedOnFilename =
    TypedConfiguration.ignoreBasedOnFilename ignoreIfContainsAnyOfThese ignoreIfEndsInAnyOfThese

[<Fact>]
let ``IgnoreBasedOnFilename: Not Matched cases`` () =
    seq {
        Some "endstart"
        Some "excludedornot"
        Some "green"
        None
    }
    |> Seq.map ignoreBasedOnFilename
    |> Seq.iter Assert.False

[<Fact>]
let ``IgnoreBasedOnFilename: Matched cases`` () =
    seq {
        // "somebluestuff"
        "send"
        // "redGreen"
        // "bored"
        // "End"
        // "BookEnd"
        // "Blue"
        // "LightBlue"
        // @"darkREDspot"
    }
    |> Seq.map Some
    |> Seq.map (fun name ->
        let shouldIgnore = ignoreBasedOnFilename name
        printfn $"name = {string name}"
        shouldIgnore)
    |> Seq.iter Assert.True
