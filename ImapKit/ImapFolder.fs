module ProcessAttachments.ImapKit.ImapFolder

open MailKit
open MailKit.Net.Imap
open MimeKit

open MailKit.Search
open ProcessAttachments.ImapKit.FolderTry
module Conv = ProcessAttachments.Collections.Conversions
module BP = BodyParts

let dfsPre = Conv.dfsPre tryOpenFolder tryGetSubfolders

let listFoldersInNamespace (fNamespace: ImapClient -> FolderNamespace) (client: ImapClient) =
    client
        .GetFolder(fNamespace client)
        .GetSubfolders(false)

//let categorizeAttachment (behaviour: Behaviour) (mimePart: MimePart) =
//    behaviour.accept mimePart
//
//let saveMessageAttachment (folder: IMailFolder) (summary: IMessageSummary) (attachment: BodyPartBasic) =
//    match folder.GetBodyPart(summary.UniqueId, attachment) with
//    | :? MimePart as mimePart ->
//        match categorizeAttachment basicDownloader mimePart with
//        | Warn msg -> Error msg
//        | Ignore msg -> Ok msg
//        | Accept ->
//            Ok "todo"
//    | _ -> Error $"Non-MIME attachment found [{attachment.ContentType.Name}]"

let enumerateMessages searchQuery (folder: IMailFolder) =
    let messageFields =
        MessageSummaryItems.UniqueId
        ||| MessageSummaryItems.Envelope
        ||| MessageSummaryItems.GMailLabels
        ||| MessageSummaryItems.BodyStructure
        ||| MessageSummaryItems.Size
        ||| MessageSummaryItems.Headers
        ||| MessageSummaryItems.ModSeq
    let headers = seq {
        HeaderId.ContentMd5
    }

    let query = searchQuery |> Option.defaultValue SearchQuery.NotDeleted
    tryFetch query messageFields headers folder
    |> Result.map (fun l -> l :> IMessageSummary seq)
            

// This corresponds to AttachmentProcessor.VisitFolder
// Fetch messages from a given folder
// For each message, list attachments
// For each attachment, save it

let closeFolder elevatedFolder =
    elevatedFolder |> Result.bind tryCloseFolder
