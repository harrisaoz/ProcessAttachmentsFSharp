module ProcessAttachments.ImapKit.BodyParts

open System
open MailKit
open MimeKit

module Conv = ProcessAttachments.Collections.Conversions

let asBasic (part: BodyPart) =
    match part with
    | :? BodyPartBasic as basic -> Some basic
    | _ -> None

let asMultiPart (part: BodyPart) =
    match part with
    | :? BodyPartMultipart as body -> Some body
    | _ -> None

let typedChildren asType (multiPart: BodyPartMultipart) =
    multiPart.BodyParts
    |> Seq.choose asType

let multiPartChildren: BodyPartMultipart -> seq<BodyPartMultipart> =
    typedChildren asMultiPart

let basicChildren: BodyPartMultipart -> seq<BodyPartBasic> =
    typedChildren asBasic

let enumerateAttachments (top: BodyPart) =
    match top with
    | :? BodyPartMultipart as body ->
        Conv.simpleDfsPre multiPartChildren body
    | _ ->
        Seq.empty

let notMimePartMessage folder subject contentType =
    let text = "The MIME entity found was not a MimePart, and so will be excluded from export"
    $"![Folder = {string folder}; Message subject = {string subject}; Content Type = {string contentType}] {text}"

let tryGetMimePart (folder: IMailFolder) (message: IMessageSummary) attachment =
    try
        // If this is excluding valid attachments, consider instead using MimeEntity.WriteToAsync.
        match folder.GetBodyPart(message.UniqueId, attachment) with
        | :? MimePart as mimePart -> mimePart |> Ok
        | entity -> notMimePartMessage folder.FullName message.Envelope.Subject entity.ContentType |> Error
    with
        | :? OutOfMemoryException ->
            reraise()
        | ex -> Error ex.Message
