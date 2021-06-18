module Tests.TypedConfiguration

open Xunit

open Configuration.Load
open ProcessAttachments.ImapKit.ImapService

type PortStructure = { Port: int }

[<Fact>]
let ``Generated configuration should match input`` () =
    { Hostname = "test"; Port = 123 }
    |> TypedConfiguration.generateConfiguration
    |> fromJsonText
    |> fun config ->
        let r = read config
        Assert.Equal("test", r "hostname" |> Option.get)
        Assert.Equal("123", r "port" |> Option.get)

[<Fact>]
let ``Given an integer-valued configuration parameter, when it is parsed, the result should be Some integer`` () =
    let portVal = 101
    { Port = portVal }
    |> TypedConfiguration.generateConfiguration
    |> fromJsonText
    |> fun config ->
        let r p = read config p |> Option.bind tryParseInt |> Option.get
        Assert.True(r "Port" = portVal)

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
                Assert.Equal("goodguy", serviceParameters.Credentials.UserName)
                Assert.Equal("doinggood", serviceParameters.Credentials.Password)
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
                let materialized = Array.ofSeq mailboxParameters.SourceFolders
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
