module ImapKit.ImapFolder

open MailKit
open MimeKit
open MailKit.Net.Imap

module Conv = Collections.Conversions
module BP = BodyParts

open Defaults

let personalNamespace =
    fun (client: ImapClient) ->
        client.PersonalNamespaces.Item(0)

let listFoldersInNamespace (fNamespace: ImapClient -> FolderNamespace) (client: ImapClient) =
    client
        .GetFolder(fNamespace client)
        .GetSubfolders(false)

let openMailFolder (folder: IMailFolder) =
    try
        match folder.Open(FolderAccess.ReadOnly) with
        | FolderAccess.None ->
            Error $"ReadOnly access to the folder $s{folder.FullName} was refused"
        | _ ->
            Ok folder
    with
        ex -> Error ex.Message

let subfolders (folder: IMailFolder): Result<IMailFolder seq, string> =
    try
        Ok (folder.GetSubfolders(false) :> IMailFolder seq)
    with
        ex -> Error ex.Message

let dfsPre = Conv.dfsPre openMailFolder subfolders

let categorizeAttachment (behaviour: Behaviour) (mimePart: MimePart) =
    behaviour.accept mimePart

let saveMessageAttachment (folder: IMailFolder) (summary: IMessageSummary) (attachment: BodyPartBasic) =
    match folder.GetBodyPart(summary.UniqueId, attachment) with
    | :? MimePart as mimePart ->
        match categorizeAttachment basicDownloader mimePart with
        | Warn msg -> Error msg
        | Ignore msg -> Ok msg
        | Accept ->
            Ok "todo"
    | _ -> Error $"Non-MIME attachment found [{attachment.ContentType.Name}]"

let saveMessageAttachments folder (summary: IMessageSummary) =
    summary.Body
    |> BP.enumerateAttachments
    |> Seq.collect BP.basicChildren

let saveFolderAttachments (writer: IMailFolder -> Result<IMailFolder, string>) (r: Result<IMailFolder, string>) =
    match r with
    | Result.Ok folder ->
        printfn $"[{folder.FullName}] Processing folder"
        writer folder
    | Result.Error msg ->
        printfn $"{msg}"
        Result.Error msg

let enumerateMessages searchQuery =
    let messageHeaders =
        MessageSummaryItems.UniqueId
        ||| MessageSummaryItems.Envelope
        ||| MessageSummaryItems.GMailLabels
        ||| MessageSummaryItems.BodyStructure
        ||| MessageSummaryItems.Size
        ||| MessageSummaryItems.ModSeq

    fun (folder: IMailFolder) ->
        match searchQuery with
        | Some query ->
            folder.Search(query)
            |> fun messages -> folder.Fetch(messages, messageHeaders)
        | None ->
            folder.Fetch(0, -1, 0uL, messageHeaders)
        :> IMessageSummary seq

// This corresponds to AttachmentProcessor.VisitFolder
// Fetch messages from a given folder
// For each message, list attachments
// For each attachment, save it
let save searchQuery (destinationFolderName: string) =
    let messageHeaders =
        MessageSummaryItems.UniqueId
        ||| MessageSummaryItems.Envelope
        ||| MessageSummaryItems.GMailLabels
        ||| MessageSummaryItems.BodyStructure
        ||| MessageSummaryItems.Size
        ||| MessageSummaryItems.ModSeq

    let fetch (folder: IMailFolder) =
        match searchQuery with
        | Some query ->
            folder.Search(query)
            |> fun messages -> folder.Fetch(messages, messageHeaders)
        | None ->
            folder.Fetch(0, -1, 0uL, messageHeaders)

    fun (folder: IMailFolder) ->
        fetch(folder)
        |> Seq.map (
            fun summary ->
                saveMessageAttachments folder summary
        )
        printfn $"Saving attachments to {destinationFolderName}"
        Result.Ok folder

let tryCloseFolder (folder: IMailFolder) =
    try
        folder.Close()
        printfn $"[{folder.FullName}] Closed folder"
        Ok folder
    with
        ex -> Error (Seq.singleton $"[{folder.FullName}] {ex.Message}")

let closeFolder elevatedFolder =
    elevatedFolder |> Result.bind tryCloseFolder
