module ImapKit.BodyParts

open MailKit

module Conv = Collections.Conversions

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
