module ProcessAttachments.ImapKit.FolderTry

open System
open MailKit
open MimeKit

let inline tryWithAsResult (block: unit -> 'a) =
    try
        block() |> Ok
    with
        | :? OutOfMemoryException ->
            reraise()
        | ex -> Error ex.Message

let tryOpenFolder (folder: IMailFolder) =
    try
        match folder.Open(FolderAccess.ReadOnly) with
        | FolderAccess.None ->
            Error $"ReadOnly access to the folder $s{folder.FullName} was refused"
        | _ ->
            Ok folder
    with
        | :? OutOfMemoryException ->
            reraise()
        | ex -> Error ex.Message

let tryGetSubfolders (folder: IMailFolder): Result<IMailFolder seq, string> =
    tryWithAsResult (fun () ->
        folder.GetSubfolders(false) :> IMailFolder seq
    )

let tryFetch query summaryItems (headers: HeaderId seq) (folder: IMailFolder) =
    tryWithAsResult (fun () ->
        folder.Search(query) |> fun messages -> folder.Fetch(messages, summaryItems, headers)
    )

let tryCloseFolder (folder: IMailFolder) =
    try
        folder.Close()
        printfn $"[{folder.FullName}] Closed folder"
        Ok folder
    with
        ex -> Error $"[{folder.FullName}] {ex.Message}"
