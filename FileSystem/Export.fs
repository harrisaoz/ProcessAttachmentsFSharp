module ProcessAttachments.FileSystem.Export

open System
open System.IO
open FSharp.Core.Extensions

let illegalCharacters = Path.GetInvalidFileNameChars()

let clean (replacement: string) =
    String.split illegalCharacters
    >> String.join replacement

let flatten (replacement: string) =
    String.split (Array.singleton Path.PathSeparator)
    >> String.join replacement

let sanitise =
    flatten "__" >> clean "_"

let tryCreateFile absoluteFilename =
    try
        Some (File.Open(absoluteFilename, FileMode.CreateNew))
        |> Ok
    with
        | :? OutOfMemoryException ->
            reraise()
        | :? IOException ->
            Ok None
        | ex ->
            Error ex.Message

let tryWriteToFile (length: #IDisposable -> int64) (decode: 'a -> #IDisposable -> unit) (stream: #IDisposable) (content: 'a) =
    try
        decode content stream
        Ok (length stream)
    with
        | :? OutOfMemoryException ->
            reraise()
        | ex -> Error ex.Message

let writeToFile (streamLength: #IDisposable -> int64) (streamFromFilename: string -> Result<#IDisposable option, string>) (decode: 'a -> #IDisposable -> unit) absoluteFilename content =
    match streamFromFilename absoluteFilename with
    | Ok (Some stream) ->
        use fStream = stream
        tryWriteToFile streamLength decode fStream content
    | Ok None -> Ok 0L
    | Error data -> Error data

let stdStreamLength (stream: Stream) = stream.Length

let fsWriteToFile decode = writeToFile stdStreamLength tryCreateFile decode
