module ProcessAttachments.ImapKit.ImapFolder

open MailKit
open MailKit.Net.Imap
open MimeKit

open MailKit.Search
open ProcessAttachments.ImapKit.FolderTry
module Conv = ProcessAttachments.Collections.Conversions
module BP = BodyParts

let dfs = Conv.dfsPost tryOpenFolder tryGetSubfolders

let clientDefaultPersonalNamespace =
    fun (client: ImapClient) ->
        client.PersonalNamespaces.Item(0)

let listFoldersInNamespace (fNamespace: ImapClient -> FolderNamespace) =
    tryGetFolderInNamespace fNamespace
    >> Result.bind tryGetSubfolders

let selectFoldersFromNamespace folderNamespace filter =
    listFoldersInNamespace folderNamespace
    >> Result.map (Seq.filter filter)

let selectFoldersInNamespace (ns: ImapClient -> FolderNamespace) filter =
    fun (client: ImapClient) ->
        client.GetFolder(ns client).GetSubfolders(false)
        |> Seq.filter filter

let enumerateMessages searchQuery (folder: IMailFolder): Result<IMessageSummary seq, string> =
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
    tryFetch messageFields headers query folder
    |> Result.map (fun messageSummaries -> upcast messageSummaries)

let closeFolder elevatedFolder =
    elevatedFolder |> Result.bind tryCloseFolder
