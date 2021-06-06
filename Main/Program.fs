// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open ImapAttachments
open Main

[<EntryPoint>]
let main argv =
    let configFile =
        match (List.ofArray argv) with
        | [] -> "ProcessAttachments.json"
        | filename :: _ -> filename

    let handleResult =
        fun r ->
            match r with
            | Result.Ok _ ->
                printfn "All good"
                0
            | Result.Error msg ->
                printf $"Something went wrong [{msg}]"
                1

    let passThroughLogger =
        Result.mapError (
            fun msg ->
                eprintfn $"{msg}"
                msg
            )

    configFile
    |> Configuration.Load.fromJsonFile
    |> Result.bind (Processor.downloadAttachments ImapFolder.personalNamespace passThroughLogger)
    |> handleResult

