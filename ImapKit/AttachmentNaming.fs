module ProcessAttachments.ImapKit.AttachmentNaming

open System
open FSharp.Core.Extensions

let folderPart folderName folder =
    folderName folder

let datePart (dateFormat: string) fallbackLabel (messageDate: 'm -> DateTimeOffset option) messageId message =
    match messageDate message with
    | Some date -> date.ToString(dateFormat)
    | None -> $"{fallbackLabel}-{string (messageId message)}"

let standardDatePart messageDate messageId =
    datePart "yyyyMM" "nodate" messageDate messageId

let filenamePart mimePartName mimePartDigest mimePart =
    mimePartName mimePart |> Option.defaultValue (mimePartDigest mimePart)

let composeName parts =
    String.join "_" parts

// Functionality mimicry - delete
//let folderBasedName (dateFormat: string) datelessLabel folderName mimePartName mimePartDigest (messageDate: 'c -> DateTimeOffset option) messageId =
//    fun (folder: 'a) (mimePart: 'b) (message: 'c) ->
//        let folderPart = folderName folder |> flatten "__"
//        let datePart =
//            match messageDate message with
//            | Some date -> date.ToString(dateFormat)
//            | None -> $"{datelessLabel}-{string (messageId message)}"
//        let filenamePart = mimePart |> (mimePartName >> Option.defaultValue (mimePartDigest mimePart))
//        
//        $"{folderPart}_{datePart}_{filenamePart}"
