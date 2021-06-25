module ProcessAttachments.ImapKit.ImapFolder

open MailKit
open MailKit.Net.Imap
open MimeKit

open MailKit.Search
open ProcessAttachments.ImapKit.FolderTry
module Conv = ProcessAttachments.Collections.Conversions
module BP = BodyParts

let dfs = Conv.dfsPost tryOpenFolder tryGetSubfolders

// * Important *
// This does not handle exceptions in any way.
// It is expected that the occurrence of any exceptions here should be
// treated as a complete program failure and thus cause the program to abort.
let listFoldersInNamespace (fNamespace: ImapClient -> FolderNamespace) (client: ImapClient) =
    client
        .GetFolder(fNamespace client)
        .GetSubfolders(false)

let selectFoldersInNamespace (ns: ImapClient -> FolderNamespace) filter =
    listFoldersInNamespace ns
    >> Seq.filter filter

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

let closeFolder elevatedFolder =
    elevatedFolder |> Result.bind tryCloseFolder
