module Tests.TypedConfiguration

open Xunit

open Configuration.Load
open ImapAttachments.ImapService

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
                Assert.Equal("AmonitoredFolder", mailboxParameters.SourceFolder)
            )
        Assert.False(maybeMailboxParams.IsNone, "Configuration parameters should not be empty")
    | Result.Error msg -> Assert.True(false, msg)