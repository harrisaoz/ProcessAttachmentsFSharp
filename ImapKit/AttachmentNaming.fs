module ProcessAttachments.ImapKit.AttachmentNaming

open System
open FSharp.Core.Extensions
open MailKit
open MimeKit

let datePart (dateFormat: string) fallbackLabel (messageDate: 'm -> DateTimeOffset option) messageId message =
    match messageDate message with
    | Some date -> date.ToString(dateFormat)
    | None -> $"{fallbackLabel}-{string (messageId message)}"

let standardDatePart messageDate messageId =
    datePart "yyyyMM" "nodate" messageDate messageId

let filenamePart preferredName fallbackName mimePart =
    preferredName mimePart |> Option.defaultValue (fallbackName mimePart)

let composeName parts =
    String.join "_" parts

module InvoiceNaming =
    let name (folder: IMailFolder) (message: IMessageSummary) (mimePart: MimePart) =
        let folderName (folder: IMailFolder) = folder.FullName
        let preferredName (part: MimePart) = part.FileName |> Option.ofObj
        let fallbackName (part: MimePart) =
            part.ContentMd5 |> Option.ofObj |> Option.defaultValue (part.ComputeContentMd5())
            |> (fun digest -> $"{digest.Substring(0, 8)}.pdf")
        let messageDate (message: IMessageSummary) = message.Envelope.Date |> Option.ofNullable
        let messageUniqueId (message: IMessageSummary) = message.UniqueId

        [|
            folderName folder
            standardDatePart messageDate messageUniqueId message
            filenamePart preferredName fallbackName mimePart
        |]
        |> composeName
