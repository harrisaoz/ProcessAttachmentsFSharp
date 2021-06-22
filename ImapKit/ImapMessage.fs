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

let tryCopyAttachmentToStream (mimePart: MimePart) =
    mimePart.Content.DecodeTo
