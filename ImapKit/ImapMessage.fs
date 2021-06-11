module ImapKit.ImapMessage

open MailKit

module BP = BodyParts

let dfsPre (summary: IMessageSummary) =
    summary.Body
    |> BP.enumerateAttachments
    |> Seq.collect BP.basicChildren
