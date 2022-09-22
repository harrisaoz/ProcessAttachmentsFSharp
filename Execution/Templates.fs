module ProcessAttachments.Execution.Templates

open System
open Microsoft.Extensions.Configuration
open ProcessAttachments.DomainInterface

module CC = ContentCategory

let program b configuration =
    0

let handleResult (r: Result<_, _>) =
    match r with
    | Ok _ ->
        printfn $"All good"
        0
    | Error data ->
        printfn $"Something went wrong [{string data}]"
        1

let main (behaviour: Behaviour<_, _, _, _, _, _, _, _>) argv =

    let runFromConfigurationFile configFile =
        Configuration.Load.fromJsonFile configFile
        |> Result.bind behaviour.configuration
        |> Result.map (program behaviour)
        |> handleResult

    printfn "Running..."

    match (List.ofArray argv) with
    | [] -> behaviour.defaultConfigFilename
    | filename :: _ -> filename
    |> runFromConfigurationFile

type RunAppConfiguredFromFile<'r, 'p> =
    ('p -> Result<'r,string>) -> (IConfiguration -> Result<'p, string>) -> string -> string array
     -> int

let runAppConfiguredFromFile: RunAppConfiguredFromFile<_, _> =
    fun run parametersFromConfiguration defaultNameOfConfigurationFile argv ->
        match (List.ofArray argv) with
        | [] -> defaultNameOfConfigurationFile
        | filename :: _ -> filename
        |> Configuration.Load.fromJsonFile
        |> Result.bind parametersFromConfiguration
        |> Result.bind run
        |> handleResult