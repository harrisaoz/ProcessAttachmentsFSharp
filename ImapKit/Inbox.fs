﻿module ProcessAttachments.ImapKit.Inbox

open MailKit
open MailKit.Net.Imap
open ProcessAttachments.ImapKit.ImapFolder
open ProcessAttachments.ImapKit.FolderTry
open ProcessAttachments.ImapKit.ImapService

module RString = FsCombinators.StringExtensions
module Birds = FsCombinators.Core

type FolderFullName = string
type FolderName = string

type EnumerateInbox = ImapClient -> Result<IMailFolder seq, string>
type GetFolderByName = FolderFullName -> IMailFolder seq -> Option<IMailFolder>

module Gmail =
    // Output folder names are NOT prepended by "INBOX".
    let enumerateInbox: EnumerateInbox =
        listFoldersInNamespace clientDefaultPersonalNamespace

module MsExchange =
    // Output folder names are prepended by "INBOX" (e.g. INBOX/PAYROLL)
    let enumerateInbox: EnumerateInbox =
        fun client ->
            client.Inbox
            |> tryOpenFolder
            |> Result.map (fun inbox -> inbox.GetSubfolders(false))

let enumerateByProvider: Provider -> EnumerateInbox =
    function
    | Gmail -> Gmail.enumerateInbox
    | MsExchange -> MsExchange.enumerateInbox

let splitName (folder: IMailFolder) fullName =
    let folderNameAsLabels = RString.split

    folderNameAsLabels [|folder.DirectorySeparator|] fullName

let joinLabels (folder: IMailFolder) labels =
    String.concat $"{folder.DirectorySeparator}" labels

let openDescendantFolder (parent: IMailFolder): FolderFullName -> IMailFolder option =
    let nextChild =
        fun maybeFolder (childName: FolderName) ->
            maybeFolder |> Option.bind (tryOpenSubfolder childName)

    function
    | "" -> Some parent
    | nonEmptyName ->
        splitName parent nonEmptyName
        |> Seq.fold nextChild (Some parent)

let getFolderByName: GetFolderByName =
    fun fullName ->
        Seq.tryFind (
            fun folder ->
                splitName folder fullName
                |> Seq.head
                |> RString.iequal folder.Name)
        >> Option.bind (fun topMatch ->
                splitName topMatch fullName
                |> Array.tail
                |> joinLabels topMatch
                |> openDescendantFolder topMatch)

let selectNamedFolders inboxFolders =
    Seq.choose (Birds.C getFolderByName inboxFolders)

let selectFoldersFromInboxByName provider includeNames client =
    enumerateByProvider provider client
    |> Result.map (Birds.C selectNamedFolders includeNames)
