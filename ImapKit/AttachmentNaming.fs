module ProcessAttachments.ImapKit.AttachmentNaming

open System
open MailKit
open MimeKit

open Combinators

open Message
open ProcessAttachments.ImapKit.Attachment

let IllegalCharacters = System.IO.Path.GetInvalidFileNameChars ()

[<Literal>]
let FormatTimestampAsMonth = @"yyyyMM"
[<Literal>]
let FormatTimestampAsIsoDateTime = @"yyyy-MM-ddTHHmmss"
[<Literal>]
let MissingTimestampFallback = "nodate"

let messageTimestampWithFallback fallbackAttribute missingDateFallbackLabel (dateFormat: string) (messageDate: 'm -> DateTimeOffset option) message =
    match messageDate message with
    | Some date -> date.ToString(dateFormat)
    | None -> $"{missingDateFallbackLabel}-{string (fallbackAttribute message)}"

let messageTimestampWithIdFallback format =
    messageTimestampWithFallback messageUniqueId MissingTimestampFallback format

let monthPart messageDate =
    messageTimestampWithIdFallback FormatTimestampAsMonth messageDate

let isoDateTime messageDate =
    messageTimestampWithIdFallback FormatTimestampAsIsoDateTime messageDate

let digestFallbackLabel (attachment: MimePart) =
    attachment.ContentMd5
    |> Option.ofObj
    |> Option.defaultValue (attachment.ComputeContentMd5())
    |> (fun digest -> $"{digest.Substring(0, 8)}")

let composeName parts =
    String.join "_" parts

let cleanFilename = String.split IllegalCharacters >> composeName

let filenameLabel preferredName fallbackName mimePart =
    preferredName mimePart
    |> Option.map cleanFilename
    |> Option.defaultValue (fallbackName mimePart)

type ComputeAttachmentName = IMailFolder -> IMessageSummary -> MimePart -> string

module TimesheetNaming =
    let name: ComputeAttachmentName =
        fun _ message mimePart ->
            let sender =
                message.Envelope.From
                |> Seq.choose (
                    function
                    | :? MailboxAddress as from -> Some from.Address
                    | _ -> None)
                |> Seq.tryHead
                |> Option.defaultValue "unknown"
            let timestamp =
                isoDateTime internalDate message
            let filename =
                filenameLabel tryGetFilename digestFallbackLabel mimePart
            
            $"{sender}_{timestamp}__{filename}"
    
module InvoiceNaming =
    [<Literal>]
    let DefaultFileExtension = ".pdf"

    let name: ComputeAttachmentName =
        fun folder message mimePart ->
            let folderName =
                folder.FullName
                |> String.split [| folder.DirectorySeparator |]
                |> String.join "__"
            let timestamp =
                monthPart envelopeDate message
            let filename =
                let fallbackName (part: MimePart) =
                    $"{digestFallbackLabel part}{DefaultFileExtension}"
                filenameLabel tryGetFilename fallbackName mimePart

            $"{folderName}_{timestamp}_{filename}"

type NamingAlgorithm =
    | SingleFolder
    | MultiFolder

let chooseAlgorithmForFolderCount =
    function
    | 1 -> SingleFolder
    | _ -> MultiFolder

let computeAttachmentName =
    function
    | SingleFolder -> TimesheetNaming.name
    | MultiFolder -> InvoiceNaming.name
