module ProcessAttachments.Execution.Core

open FSharpx.Collections
open MimeKit
open MailKit
open Combinators.Logging
open ProcessAttachments
open ProcessAttachments.ImapKit.ImapService
open TypedConfiguration

module L = LazyList

module RString = Combinators.String
module Birds = Combinators.Standard
module Inbox = ProcessAttachments.ImapKit.Inbox
module FolderTry = ProcessAttachments.ImapKit.FolderTry
module ImF = ProcessAttachments.ImapKit.ImapFolder
module Message = ProcessAttachments.ImapKit.Message
module Att = ProcessAttachments.ImapKit.Attachment

module Rtp = RuntimeParameters

type MoveToFolders =
    | MoveToFolders of ok: IMailFolder * error: IMailFolder
type MessagesWithAttachments = LazyList<IMessageSummary * LazyList<Result<MimePart, string>>>

let openSessionFromParameters: Trace -> SessionParameters -> Result<OpenImapSession, string> =
    fun (Trace trace) sessionParams ->
        trace 2 "openSessionFromParameters"
        let client = initialize
        openSessionViaClient sessionParams client

let selectFolders (Trace trace, Alert alert) (SourceMailFolders sourceFolderNames, imapProvider) session =
    let folderNames = RString.join ";" (Seq.toArray sourceFolderNames)
    trace 2 $"selectFolders [{folderNames}]"
    
    let openFolder folder =
        match (FolderTry.tryOpenFolder folder) with
        | Ok f -> Some f
        | _ ->
            alert "<" "Unable to open selected folder" [$"Folder name = {folder.FullName}"]
            None

    Inbox.selectFoldersFromInboxByName imapProvider sourceFolderNames session.Client
    |> Result.map (Seq.choose openFolder)

let assertProcessingFolders (Inform inform) (OutputMailFolders(okName, errorName)) folder =
    let inform0 prefix = inform prefix "Create processing folders"

    let okFolderResult = FolderTry.tryCreateSubfolderIfNotExists folder okName
    let errorFolderResult = FolderTry.tryCreateSubfolderIfNotExists folder errorName

    match (okFolderResult, errorFolderResult) with
    | Ok okFolder, Ok errorFolder ->
        inform0 "\u2705" []
        Ok <| MoveToFolders(okFolder, errorFolder)
    | Error msg, _ ->
        inform0 "\u26a0" [$"Processed folder = \"{okName}\""]
        Error msg
    | _, Error msg ->
        inform0 "\u26a0" [$"Attention folder = \"{errorName}\""]
        Error msg

let enumerateFolderMessages (Inform inform, Alert alert) folder =
    let desc = "[Message] <- Folder" in
        let inform0 = inform "\u2705" desc
        let alert0 = alert "!" desc

    match ImF.enumerateMessages None folder with
    | Ok m ->
        inform0 [$"Folder = {folder.FullName}"; $"Message count = {Seq.length m}"]
        m
    | Error msg ->
        alert0 [$"Folder = {folder.FullName}"; $"Folder empty reason = \"{msg}\""]
        Seq.empty
    |> L.ofSeq

let appendToTuple2 (a, b) c =
    (a, b, c)

module Cat = ProcessAttachments.ImapKit.Categorise
type TernaryResult<'a, 'b> = Combinators.TernaryResult<'a, 'b>

let enumerateMessageAttachments (Inform inform) sourceFolder message =
    let inform0 = inform "\u2705" "[Attachment] <- Message"
    let attachments = Message.dfsPre sourceFolder message
    inform0 [
        $"Subject = \"{message.NormalizedSubject}\""
        $"Attachment count = {Seq.length attachments}"
    ]
    L.ofSeq attachments
    
// Todo: pass a more general ignore function, instead of assuming particular rules.
// This also facilitates elimination of the dependency on TypedConfiguration.AttachmentCategorisationParameters.
let categoriseAttachments (Inform inform, Alert alert) ignoreAtt accept attachments =
    let opDesc = "Categorise attachment" in
        let onOk part = inform "\u2705" opDesc (Cat.partInfo part)
        let onIgnore part = inform "~" opDesc (Cat.partInfo part)
        let onError part = alert "!\u26a0" opDesc (Cat.partInfo part)

    Cat.categoriseAttachments
        (onOk, onIgnore, onError)
        ignoreAtt
        accept
        attachments

module FS = FileSystem
module TR = Combinators.TernaryResult

let writeAttachmentToFile (Report report) destDir name (folder, message) attachment =
    let report0 = report "+" "Export attachment"
    let localPart = name folder message attachment

    report0 [$"Filename = \"{localPart}\""]
    FS.fsWriteToFile Att.tryCopyAttachmentToStream (FS.absoluteName destDir localPart) attachment

let saveMessageAttachments report destinationFolder name (folder, message) =
    L.map (writeAttachmentToFile report destinationFolder name (folder, message))
    >> TR.groupResult
    >> TR.map (L.fold (+) 0L)

let usingWritableFolder (Alert alert) fromFolder action =
    match FolderTry.tryOpenFolderWritable fromFolder with
    | Ok writableFolder ->
        action writableFolder |> ignore
    | Error msg ->
        alert "\u26a0" "Using writable folder" [$"Failed to open folder as writable = \"{msg}\""]
    
let moveMessagesFromFolder (Report report) (ok, error) messages fromFolder =
    let desc = "Move message"
    let countUniqueIds (okSet, otherSet) (r: Result<UniqueId option * UniqueId option, string>) =
        match r with
        | Ok (Some uid, None) -> (Set.add uid.Id okSet, otherSet)
        | Ok (None, Some uid) -> (okSet, Set.add uid.Id otherSet)
        | _ -> (okSet, otherSet)

    messages |> L.map (
        function
        | message: IMessageSummary, TernaryResult.Ok bytesExported ->
            report ">>" desc [
                $"To folder = \"{ok}\""
                $"Attachment bytes exported = {string bytesExported}"
                $"Subject = \"{string message.NormalizedSubject}\""
            ]
            FolderTry.tryMoveMessageTo ok fromFolder message
            |> Result.map (fun uid -> (Some uid, None))
        | message, _ ->
            report ">!" desc [
                $"To folder = \"{error}\""
                $"Subject = \"{string message.NormalizedSubject}\""
            ]
            FolderTry.tryMoveMessageTo error fromFolder message
            |> Result.map (fun uid -> (None, Some uid))
            )
    |> L.fold countUniqueIds (Set.empty, Set.empty)

let summarizeFolderActions (Report report) (folder: IMailFolder) messageExportResults okSet otherSet =
    let exportResults = messageExportResults |> L.map snd
    let totalBytesExported =
        let sumBytes total =
            function
            | TernaryResult.Ok byteCount -> total + byteCount
            | _ -> total
        L.fold sumBytes 0L

    report "| " "Folder summary" [
        $"Folder = \"{folder.FullName}\""
        $"Messages successfully exported = {string (Set.count okSet)}"
        $"Messages needing attention = {string (Set.count otherSet)}"
        $"Total size (in bytes) of exported attachments = {string (totalBytesExported exportResults)}"
    ]

let handleResult (r: Result<_, _>) =
    match r with
    | Ok _ ->
        printfn "All good"
        0
    | Error data ->
        printfn $"Something went wrong [{string data}]"
        1
