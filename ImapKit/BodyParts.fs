module ProcessAttachments.ImapKit.BodyParts

open System
open MailKit
open MailKit

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

let entity (folder: IMailFolder) (message: IMessageSummary) attachment =
    try
        folder.GetBodyPart(message.UniqueId, attachment)
        |> Ok
    with
        | :? OutOfMemoryException ->
            reraise()
        | ex -> Error ex.Message
