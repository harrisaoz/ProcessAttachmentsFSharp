module Tests.Configuration

open Xunit

open FsConfigLoader

[<Fact>]
let ``Given a present configuration file, when it is loaded, the result should be Ok`` () =
    match FromJson.loadFromFile(Core.ConfigSource(None, "Config1.json")) with
    | Result.Ok _ -> Assert.True(true)
    | Result.Error msg -> Assert.True(false, msg)

[<Fact>]
let ``Given a non-integer value, when it is parsed as an integer, the result should be None`` () =
    [ Some "abc" ; None ; Some "19x" ; Some "..." ]
    |> List.iter (fun v ->
        match (v |> Option.bind Parsers.tryParseInt) with
        | Some i -> Assert.False(true, $"Result should be None, but was Some %i{i}")
        | None -> Assert.True(true))
