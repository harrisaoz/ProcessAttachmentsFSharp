module ProcessAttachments.ImapKit.TopLevelFolder

open MailKit
open MailKit.Net.Imap
open ProcessAttachments.ImapKit.ImapFolder
open ProcessAttachments.ImapKit.FolderTry

module RString = Combinators.String
module Birds = Combinators.Standard

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

let splitName (folder: IMailFolder) fullName =
    let folderNameAsLabels = RString.split

    folderNameAsLabels [|folder.DirectorySeparator|] fullName

let joinLabels (folder: IMailFolder) labels =
    RString.join $"{folder.DirectorySeparator}" labels

let openDescendantFolder (parent: IMailFolder): FolderFullName -> IMailFolder option =
    let nextChild =
        fun (maybeFolder: IMailFolder option) (childName: FolderName) ->
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
