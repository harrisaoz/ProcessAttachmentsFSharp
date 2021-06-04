module Configuration.Load

open System
open Microsoft.Extensions.Configuration

let tryParseInt (s: string): int option =
    try
        s |> int |> Some
    with :? FormatException -> None

let fromJsonText (json: string) =
    let bytes = System.Text.Encoding.UTF8.GetBytes json
    
    ConfigurationBuilder().AddJsonStream(new IO.MemoryStream(bytes)).Build()

let fromJsonFile (filename: string) =
    try
        Result.Ok (ConfigurationBuilder().AddJsonFile(filename).Build())
    with
        ex -> Result.Error ex.Message

let section (config: IConfiguration) sectionName: IConfiguration option =
    try
        Some (config.GetSection sectionName :> IConfiguration)
    with
        ex -> None

let subsection (section: IConfiguration) subSectionName: IConfiguration option =
    try
        Some (section.GetSection subSectionName :> IConfiguration)
    with
        ex -> None
    
let readListFromSection (section: IConfiguration) parameter =
    section.AsEnumerable true
    |> Seq.filter (fun (KeyValue (k, v)) ->
        k.StartsWith($"{parameter}:") && (not << String.IsNullOrWhiteSpace) v)
    |> Seq.map (fun (KeyValue (_, v)) -> v)

let read (config: IConfiguration) parameter =
    config.Item parameter
    |> Option.ofObj
    |> Option.filter (not << String.IsNullOrWhiteSpace)
    
let readFromSection (section: IConfiguration) parameter =
    section.Item parameter
    |> Option.ofObj
    |> Option.filter (not << String.IsNullOrWhiteSpace)
