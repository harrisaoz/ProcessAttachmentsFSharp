module ProcessAttachments.ImapKit.ImapMessage

open MailKit
open MimeKit

module BP = BodyParts
open FSharp.Core.Extensions

let dfsPre (summary: IMessageSummary) =
    summary.Body
    |> BP.enumerateAttachments
    |> Seq.collect BP.basicChildren

let collect: IMessageSummary seq -> (IMessageSummary * BodyPartBasic) seq =
    Seq.collectOver dfsPre

let decode (folder: IMailFolder, message: IMessageSummary, attachment: BodyPartBasic) =
    match BodyParts.entity folder message attachment with
    | Ok (:? MimePart as mimePart) ->
        mimePart.Content.DecodeTo
    | _ -> fun _ ->
        failwith $"No MIME part found for message dated {string message.Envelope.Date} in folder {folder.FullName}"
