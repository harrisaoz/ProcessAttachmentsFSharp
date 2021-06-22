module ProcessAttachments.FileSystem

open System
open System.IO
open FSharp.Core.Extensions

let illegalCharacters = Path.GetInvalidFileNameChars()

let clean (replacement: string) =
    String.split illegalCharacters
    >> String.join replacement

let sanitise =
    String.flatten Path.PathSeparator "__" >> clean "_"

let assertFolder name =
    match Directory.Exists name with
    | true -> DirectoryInfo name
    | false -> Directory.CreateDirectory name

module Export =
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

    let tryStreamCopy streamCopy (destination: #Stream) source =
        try
            streamCopy source destination
            Ok destination.Length
        with
            | :? OutOfMemoryException ->
                reraise()
            | ex -> Error ex.Message

    let writeContentToStream createStream streamCopy destStreamName source =
        match createStream destStreamName with
        | Ok (Some stream) ->
            use fStream = stream
            tryStreamCopy streamCopy fStream source
        | Ok None -> Ok 0L
        | Error data -> Error data

    let fsWriteToFile streamCopy = writeContentToStream tryCreateFile streamCopy
