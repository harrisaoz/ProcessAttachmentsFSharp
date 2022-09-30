module ProcessAttachments.ImapKit.Attachment

open MailKit
open MimeKit

type MessageAttachment =
    | MessageAttachment of folder: IMailFolder * message: IMessageSummary * mimePart: MimePart

let tryGetFilename (attachment: MimePart): string option =
    attachment.FileName |> Option.ofObj

let tryCopyAttachmentToStream (mimePart: MimePart): System.IO.Stream -> unit =
    mimePart.Content.DecodeTo

