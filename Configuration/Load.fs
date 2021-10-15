module Configuration.Load

open System
open Microsoft.Extensions.Configuration

type ConfigurationSource =
    | ConfigurationFile of name: string
    | JsonText of json: string

type ProgramConfiguration =
    | ProgramConfiguration of config: IConfiguration

let tryParseInt (s: string): int option =
    try
        s |> int |> Some
    with :? FormatException -> None

let fromJsonText (json: string) =
    try
        let bytes = System.Text.Encoding.UTF8.GetBytes json

        ConfigurationBuilder().AddJsonStream(new IO.MemoryStream(bytes)).Build() :> IConfiguration
        |> Ok
    with
        ex -> Result.Error ex.Message

let fromJsonFile (filename: string) =
    try
        ConfigurationBuilder().AddJsonFile(filename).Build() :> IConfiguration
        |> Ok
    with
        ex -> Result.Error ex.Message

let fromSource source =
    match source with
    | ConfigurationFile filename -> fromJsonFile filename
    | JsonText json -> fromJsonText json

let section (config: IConfiguration) sectionName: IConfiguration option =
    try
        Some (config.GetSection sectionName :> IConfiguration)
    with
        ex -> None

let readMany (section: IConfiguration) parameter =
    section.AsEnumerable true
    |> Seq.filter (fun (KeyValue (k, v)) ->
        k.StartsWith($"{parameter}:") && (not << String.IsNullOrWhiteSpace) v)
    |> Seq.map (fun (KeyValue (_, v)) -> v)

let read (config: IConfiguration) parameter =
    config.Item parameter
    |> Option.ofObj
    |> Option.filter (not << String.IsNullOrWhiteSpace)
