// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open Main

[<EntryPoint>]
let main argv =
    let configFile =
        match (List.ofArray argv) with
        | [] -> "ProcessAttachments.json"
        | filename :: _ -> filename

    Configuration.Load.fromJsonFile configFile
    |> Processor.downloadAttachments
    |> fun result ->
        match result with
        | Ok _ -> 0
        | Error _ -> 1
