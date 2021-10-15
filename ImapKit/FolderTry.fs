module ProcessAttachments.ImapKit.FolderTry

open System
open MailKit
open MailKit.Net.Imap
open MimeKit

let inline tryWithAsResult (expression: unit -> 'a) =
    try
        expression() |> Ok
    with
        | :? OutOfMemoryException ->
            reraise()
        | ex -> Error ex.Message

let getClientFolder (selector: ImapClient -> FolderNamespace) (client: ImapClient) =
    tryWithAsResult (fun () -> client.GetFolder(selector client))

let tryGetFolderInNamespace = getClientFolder

let tryOpenFolderInMode mode (folder: IMailFolder) =
    try
        match folder.Open(mode) with
        | FolderAccess.None ->
            Error $"{string mode} access to the folder $s{folder.FullName} was refused"
        | _ ->
            Ok folder
    with
        | :? OutOfMemoryException ->
            reraise()
        | ex -> Error ex.Message

let tryOpenSubfolderWritable (parent: IMailFolder) (subfolderName: string) =
    try
        Ok (Some (parent.GetSubfolder(subfolderName)))
    with
        | :? FolderNotFoundException ->
            Ok None
        | ex ->
            Error <| ex.Message

let tryCreateSubfolder (parent: IMailFolder) (subfolderName: string) =
    let folderWillContainMessages = true
    try
        Ok <| parent.Create(subfolderName, folderWillContainMessages)
    with
        | :? SystemException ->
            reraise()
        | ex ->
            Error <| ex.Message

let tryCreateSubfolderIfNotExists: IMailFolder -> string -> Result<IMailFolder, string> =
    fun parent subfolderName ->
        match (tryOpenSubfolderWritable parent subfolderName) with
        | Ok (Some folder) ->
            Ok folder
        | Ok None ->
            tryCreateSubfolder parent subfolderName
        | Error msg ->
            Error msg

let tryOpenFolder: IMailFolder -> Result<IMailFolder, string> =
    tryOpenFolderInMode FolderAccess.ReadOnly

let tryOpenFolderWritable: IMailFolder -> Result<IMailFolder, string> =
    tryOpenFolderInMode FolderAccess.ReadWrite

let tryGetSubfolders (folder: IMailFolder): Result<IMailFolder seq, string> =
    tryWithAsResult (fun () ->
        folder.GetSubfolders(false) :> IMailFolder seq
    )

let tryGetSubfoldersWhere filter =
    tryGetSubfolders >> (Result.map (Seq.where filter))

let tryMoveMessageTo (toFolder: IMailFolder) (fromFolder: IMailFolder) (message: IMessageSummary): Result<UniqueId, string> =
    try
        match (fromFolder.MoveTo(message.UniqueId, toFolder) |> Option.ofNullable) with
        | None -> Error "Unexpected condition: A new UID was not assigned to the moved message"
        | Some uid -> Ok uid
    with
        | :? SystemException ->
            reraise()
        | ex ->
            Error <| ex.Message
        

let tryFetch summaryItems (headers: HeaderId seq) query (folder: IMailFolder) =
    tryWithAsResult (fun () ->
        folder.Search(query) |> fun messages -> folder.Fetch(messages, summaryItems, headers)
    )

let tryCloseFolder (folder: IMailFolder) =
    try
        folder.Close()
        Ok folder
    with
        ex -> Error $"[{folder.FullName}] {ex.Message}"
