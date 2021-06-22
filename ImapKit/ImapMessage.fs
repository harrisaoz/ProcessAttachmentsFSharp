module ProcessAttachments.ImapKit.ImapMessage

open MailKit
open MimeKit

module BP = BodyParts
open FSharp.Core.Extensions

let dfsPre (folder: IMailFolder) (message: IMessageSummary) =
    message.Body
    |> BP.enumerateAttachments
    |> Seq.collect BP.basicChildren
    |> Seq.map (BP.tryGetMimePart folder message)

let tryCopyAttachmentToStream (mimePart: MimePart) =
    mimePart.Content.DecodeTo
