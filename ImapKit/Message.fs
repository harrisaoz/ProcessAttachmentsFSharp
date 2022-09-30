module ProcessAttachments.ImapKit.Message

open MailKit
open MimeKit

module BP = BodyParts

let messageUniqueId (message: IMessageSummary) = message.UniqueId

let envelopeDate (message: IMessageSummary) =
    message.Envelope.Date |> Option.ofNullable

let internalDate (message: IMessageSummary) =
    message.InternalDate |> Option.ofNullable

let dfsPre (folder: IMailFolder) (message: IMessageSummary): Result<MimePart,string> seq =
    message.Body
    |> BP.enumerateAttachments
    |> Seq.collect BP.basicChildren
    |> Seq.map (BP.tryGetMimePart folder message)
