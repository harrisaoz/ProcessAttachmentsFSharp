namespace ProcessAttachments.Execution

open FSharpx.Collections
open MimeKit
open MailKit

open FsCombinators.ExtraTypes

module L = LazyList
module Cat = ProcessAttachments.ImapKit.Categorise
module IOExp = FsSimpleFileIO.Export
module IOFacade = FsSimpleFileIO.Facade
module IOFsExt = FsSimpleFileIO.FileSystemExtensions

module RString = FsCombinators.StringExtensions
module Birds = FsCombinators.Core

open ProcessAttachments.ImapKit.ImapService
open TypedConfiguration
open ProcessAttachments.Execution.Logging

module Inbox = ProcessAttachments.ImapKit.Inbox
module FolderTry = ProcessAttachments.ImapKit.FolderTry
module ImF = ProcessAttachments.ImapKit.ImapFolder
module Message = ProcessAttachments.ImapKit.Message
module Att = ProcessAttachments.ImapKit.Attachment

module Rtp = RuntimeParameters
module IrExt = ProcessAttachments.CommonExtensions.IgnorableResultExtensions

type MoveToFolders =
    | MoveToFolders of ok: IMailFolder * error: IMailFolder
type MessagesWithAttachments = LazyList<IMessageSummary * LazyList<Result<MimePart, string>>>

module Core =
    let openSessionFromParameters: Trace -> SessionParameters -> Result<OpenImapSession, string> =
        fun (Trace trace) sessionParams ->
            trace 2 "openSessionFromParameters"
            let client = initialize
            openSessionViaClient sessionParams client

    let selectFolders (Trace trace, Alert alert) (SourceMailFolders sourceFolderNames, imapProvider) session =
        let folderNames = String.concat ";" (Seq.toArray sourceFolderNames)
        trace 2 $"selectFolders [{folderNames}]"
        
        let openFolder folder =
            match (FolderTry.tryOpenFolder folder) with
            | Result.Ok f -> Some f
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
        | Result.Ok okFolder, Result.Ok errorFolder ->
            inform0 "\u2705" []
            Result.Ok <| MoveToFolders(okFolder, errorFolder)
        | Result.Error msg, _ ->
            inform0 "\u26a0" [$"Processed folder = \"{okName}\""]
            Result.Error msg
        | _, Result.Error msg ->
            inform0 "\u26a0" [$"Attention folder = \"{errorName}\""]
            Result.Error msg

    let enumerateFolderMessages (Inform inform, Alert alert) folder =
        let desc = "[Message] <- Folder" in
            let inform0 = inform "\u2705" desc
            let alert0 = alert "!" desc

        match ImF.enumerateMessages None folder with
        | Result.Ok m ->
            inform0 [$"Folder = {folder.FullName}"; $"Message count = {Seq.length m}"]
            m
        | Result.Error msg ->
            alert0 [$"Folder = {folder.FullName}"; $"Folder empty reason = \"{msg}\""]
            Seq.empty
        |> L.ofSeq

    let appendToTuple2 (a, b) c =
        (a, b, c)

    let enumerateMessageAttachments (Inform inform) sourceFolder message =
        let inform0 = inform "\u2705" "[Attachment] <- Message"
        let attachments = Message.dfsPre sourceFolder message
        inform0 [
            $"Subject = \"{message.NormalizedSubject}\""
            $"Attachment count = {Seq.length attachments}"
        ]
        L.ofSeq attachments
        
    let categoriseAttachments (Inform inform, Alert alert) (ignoreAtt, accept) attachments =
        let opDesc = "Categorise attachment" in
            let onOk part = inform "\u2705" opDesc (Cat.partInfo part)
            let onIgnore part = inform "~" opDesc (Cat.partInfo part)
            let onError part = alert "!\u26a0" opDesc (Cat.partInfo part)

        Cat.categoriseAttachments
            (onOk, onIgnore, onError)
            ignoreAtt
            accept
            attachments

    type StreamCopy<'Src, 'Dst> when 'Dst :> System.IO.Stream = 'Src -> 'Dst -> unit

    let tryStreamCopy: StreamCopy<'Src, 'Dst> -> 'Dst -> 'Src -> IgnorableResult<int64, string> =
        fun streamCopy (destination: 'Dst) source ->
            fun () ->
                streamCopy source destination
                destination.Length
            |> FsCombinators.ResultExtensions.tryAsResult
            |> IgnorableResult.ofResult

    let writeAttachmentToFile (Report report) destDir name (folder, message) (attachment: MimePart) =
        let report0: string seq -> unit = report "+" "Export attachment"
        let localPart: string = name folder message attachment

        report0 [$"Filename = \"{localPart}\""]
        IOFsExt.tryCreateFileWithoutOverwrite (IOFsExt.absoluteName destDir localPart)
        |> IgnorableResult.bind (fun stream ->
            use fStream = stream
            tryStreamCopy Att.tryCopyAttachmentToStream fStream attachment)

    let saveMessageAttachments report (destinationFolder, name) (folder, message): LazyList<MimePart> -> IgnorableResult<int64, string> =
        L.map (writeAttachmentToFile report destinationFolder name (folder, message))
        >> IrExt.groupResult
        >> IgnorableResult.map (L.fold (+) 0L)

    let usingWritableFolder (Alert alert) fromFolder action =
        match FolderTry.tryOpenFolderWritable fromFolder with
        | Result.Ok writableFolder ->
            action writableFolder |> ignore
        | Result.Error msg ->
            alert "\u26a0" "Using writable folder" [$"Failed to open folder as writable = \"{msg}\""]
        
    let moveMessagesFromFolder (Report report) (MoveToFolders (ok, error)) messages fromFolder =
        let desc = "Move message"
        let countUniqueIds (okSet, otherSet) (r: Result<UniqueId option * UniqueId option, string>) =
            match r with
            | Result.Ok (Some uid, None) -> (Set.add uid.Id okSet, otherSet)
            | Result.Ok (None, Some uid) -> (okSet, Set.add uid.Id otherSet)
            | _ -> (okSet, otherSet)

        messages |> L.map (
            function
            | message: IMessageSummary, IgnorableResult.Ok bytesExported ->
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

    let summarizeFolderActions (Report report) (folder: IMailFolder) messageExportResults (okSet, otherSet) =
        let exportResults = messageExportResults |> L.map snd
        let totalBytesExported =
            let sumBytes total =
                function
                | IgnorableResult.Ok byteCount -> total + byteCount
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
        | Result.Ok _ ->
            printfn "All good"
            0
        | Result.Error data ->
            printfn $"Something went wrong [{string data}]"
            1
