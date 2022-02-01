open System
open FSharpx.Collections.Experimental
open MailKit
open MailKit.Net.Imap
open Microsoft.Extensions.Configuration
open MimeKit

open ProcessAttachments.ImapKit.ImapService
open TypedConfiguration
open Combinators
open Microsoft.FSharp.Core

open FSharpx.Collections
module TR = TernaryResult
module RTG = Collections.RoseTreeGermination.ResultBoxed
module L = LazyList
module Config = Configuration.Load
module TC = TypedConfiguration
module ET = ProcessAttachments.Execution.Templates
module ImF = ProcessAttachments.ImapKit.ImapFolder
module FTry = ProcessAttachments.ImapKit.FolderTry
module Message = ProcessAttachments.ImapKit.ImapMessage
module Parts = ProcessAttachments.ImapKit.BodyParts
module Naming = ProcessAttachments.ImapKit.AttachmentNaming.InvoiceNaming
module FS = ProcessAttachments.FileSystem
module Service = ProcessAttachments.ImapKit.ImapService
module CC = ProcessAttachments.ImapKit.Categorise

let personalNamespace =
    fun (client: ImapClient) ->
        client.PersonalNamespaces.Item(0)

type Log = string -> unit

type DestinationFolder = IO.DirectoryInfo
type SourceFolders = string seq
type PostProcessingFolderName = string

type PostProcessingFolders =
    {
        Processed: PostProcessingFolderName
        Attention: PostProcessingFolderName
    }

type GenericRuntimeParameters<'a, 'b> =
    {
        SessionParameters: SessionParameters
        CategorisationParameters: AttachmentCategorisationParameters
        DestinationFolder: DestinationFolder
        ExtraParameters: 'a
        PostProcessingFolders: PostProcessingFolders
        LoggingDestinations: 'b
    }

type ExtraParameters =
    {
        SourceFolders: SourceFolders
    }

type AbsoluteFilename =
    | AbsoluteFilename of name: string

type RelativeFilename =
    | RelativeFilename of name: string

type LoggingParameters =
    {
        LogDir: AbsoluteFilename
        Info: RelativeFilename
    }

type RuntimeParameters = GenericRuntimeParameters<ExtraParameters, LoggingParameters>
type ParametersFromConfiguration = IConfiguration -> RuntimeParameters

type ProcFolders = IMailFolder * IMailFolder

type ProcCounts = int * int

type MessagesWithAttachments = LazyList<IMessageSummary * LazyList<Result<MimePart, string>>>

type FolderName = string

type ExecutionContext =
    | ExecutionContext of programArgs: string[] * defaultConfigurationFilename: string

type FolderResultTree = RoseTree<Result<IMailFolder, string>>

let chooseConfigurationFile (ExecutionContext (args, defaultFilename)) =
    match (List.ofArray args) with
    | [] -> defaultFilename
    | overrideFilename :: _ -> overrideFilename
    |> Config.ConfigurationFile

let inline next f p = (p, f p)
let rNext g f (p, r) = (p, r |> (f >> g) p)
let rAddContext g f (p, r) = ((p, r |> (f >> g) p), r)
let resultOnly = snd

module RoseForestOfResult =
    let inline bind f = (Result.bind >> RoseTree.map >> Seq.map >> Result.map) f
    let inline map f = (Result.map >> RoseTree.map >> Seq.map >> Result.map) f

module RFoR = RoseForestOfResult

[<EntryPoint>]
let main argv =
    let parametersFromConfiguration (configuration: IConfiguration): Result<RuntimeParameters, string> =
        match (TC.imapServiceParameters configuration,
               TC.categorisationParameters configuration,
               TC.mailboxParameters configuration,
               TC.exportParameters configuration,
               TC.loggingParameters configuration) with
        | Some session, Some categorisation, Some mailbox, Some export, Some logging ->
            let parameters = {
                SessionParameters = session
                CategorisationParameters = categorisation
                DestinationFolder = FS.assertFolder export.DestinationFolder
                PostProcessingFolders =
                    {
                        Processed = mailbox.ProcessedSubfolder
                        Attention = mailbox.AttentionSubfolder
                    }
                ExtraParameters =
                    {
                        SourceFolders = mailbox.SourceFolders
                    }
                LoggingDestinations =
                    {
                        LogDir = AbsoluteFilename logging.LogDir
                        Info = RelativeFilename logging.InfoFilename
                    }
                }
            parameters |> Result.Ok
        | _ -> Result.Error "Failed to infer valid runtime parameters from the provided configuration"

    let openSessionFromParameters: SessionParameters -> Result<OpenImapSession, string> =
        fun sessionParams ->
            printfn "== [openSessionFromParameters] =="
            let client = Service.initialize
            Service.openSessionViaClient sessionParams client

    let rootFoldersViaSession: SourceFolders -> OpenImapSession -> Result<IMailFolder seq, string> =
        fun sourceFolders session ->
            printfn "== [rootFoldersViaSession] =="
            let folderFilter (folder: IMailFolder) =
                sourceFolders |> Seq.icontains folder.Name
            ImF.selectFoldersFromNamespace ImF.clientDefaultPersonalNamespace folderFilter session.Client

    let fetchFolderForest: PostProcessingFolders -> IMailFolder seq -> FolderResultTree seq =
        let filter (parameters: PostProcessingFolders) (folder: IMailFolder) =
            not (Seq.contains folder.Name (seq {
                parameters.Processed
                parameters.Attention
            }))

        filter >> FTry.tryGetSubfoldersWhere >> (RTG.grow FTry.tryOpenFolder) >> Seq.map

    let assertProcessingFolders: Log -> PostProcessingFolders -> IMailFolder -> Result<IMailFolder * ProcFolders, string> =
        fun inform parameters folder ->
            let processedSubfolderName = parameters.Processed
            let attentionSubfolderName = parameters.Attention
            
            let processedResult = FTry.tryCreateSubfolderIfNotExists folder processedSubfolderName
            let attentionResult = FTry.tryCreateSubfolderIfNotExists folder attentionSubfolderName
            
            match (processedResult, attentionResult) with
            | Ok processed, Ok attention ->
                inform "\u2713  [assertProcessingFolders]"
                Ok (folder, (processed, attention))
            | Error msg, _ ->
                inform $"\u274c  [assertProcessingFolders] {processedSubfolderName}"
                Error msg
            | _, Error msg ->
                inform $"\u274c  [assertProcessingFolders] {attentionSubfolderName} "
                Error msg

    let enumerateFolderMessages: Log -> IMailFolder * ProcFolders -> IMailFolder * ProcFolders * LazyList<IMessageSummary> =
        fun inform (folder, processingFolders) ->
            let messages = 
                match ImF.enumerateMessages None folder with
                | Ok m -> m
                | Error _ -> Seq.empty
            inform $"?  [enumerateFolderMessages] message count = {Seq.length messages}"
            (folder, processingFolders, L.ofSeq messages)

    let enumerateMessageAttachments: Log -> IMailFolder * ProcFolders * LazyList<IMessageSummary> -> IMailFolder * ProcFolders * MessagesWithAttachments =
        fun inform (folder, procFolders, messages) ->
            inform $"?  [enumerateMessageAttachments] message count = {Seq.length messages}"
            messages
            |> L.map (
                fun message ->
                    let attachments = Message.dfsPre folder message
                    inform $"?  [enumerateMessageAttachments] attachment count = {Seq.length attachments}"
                    (message, L.ofSeq attachments)
                )
            |> (fun messagesWithAttachments ->
                (folder, procFolders, messagesWithAttachments))

    let categoriseAttachment: Log -> AttachmentCategorisationParameters -> MimePart ->
        TernaryResult<MimePart, string> =
            fun inform parameters attachment ->
                let ignore1 = parameters.IgnoreBasedOnFilename
                let ignore2 = CC.isContentType parameters.IgnoredMimeTypes
                let accept = parameters.AcceptedMimeTypes
                
                let contentTypeConverter: ContentType -> string option -> ContentType =
                    fun mimeType fileExtension ->
                        match (mimeType.MediaType, mimeType.MediaSubtype, fileExtension) with
                        | "text", "html", Some ".pdf" -> ContentType("application", "pdf")
                        | "text", "html", Some ".csv" -> ContentType("text", "comma-separated-values")
                        | "application", "octet-stream", Some ".csv" -> ContentType("text", "comma-separated-values")
                        | _ -> mimeType

                let fileExtension = IO.Path.GetExtension(attachment.FileName) |> Option.ofObj
                let maybeContentType, maybeFilename =
                    let ct = attachment.ContentType |> Option.ofObj
                    let fn = attachment.FileName |> Option.ofObj
                    
                    match ct with
                    | Some mimeType -> (Some (contentTypeConverter mimeType fileExtension), fn)
                    | None -> (None, fn)

                match (ignore1 maybeFilename || ignore2 maybeContentType) with
                | true ->
                    inform $"~  [categoriseAttachment] [type = {string attachment.ContentType.MimeType}] [name = {attachment.FileName}]"
                    Ignore
                | false ->
                    match (CC.isContentType accept maybeContentType) with
                    | true ->
                        inform $"+  [categoriseAttachment] [Content Type = {string attachment.ContentType.MimeType}] [Filename = {string attachment.FileName}]"
                        TernaryResult.Ok attachment
                    | false ->
                        printfn $"!\u274c [categoriseAttachment] [Content Type = {string attachment.ContentType.MimeType}] [Filename = {string attachment.FileName}]"
                        TernaryResult.Error $"[{string maybeContentType} Missing or unsupported MIME type"

    let categoriseAttachmentResult: Log -> AttachmentCategorisationParameters -> Result<MimePart, string> ->
        TernaryResult<MimePart, string> =
            fun inform parameters attachmentResult ->
                match attachmentResult with
                | Ok attachment -> categoriseAttachment inform parameters attachment
                | Error msg -> TernaryResult.Error msg

    let categoriseMessageAttachments: Log -> AttachmentCategorisationParameters -> LazyList<Result<MimePart, string>> ->
            TernaryResult<LazyList<MimePart>, string> =
        fun inform parameters ->
            L.map (categoriseAttachmentResult inform parameters)
            >> TernaryResult.groupResult

    let categoriseFolderMessageAttachments: Log -> AttachmentCategorisationParameters ->
            IMailFolder * ProcFolders * MessagesWithAttachments ->
            IMailFolder * ProcFolders * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart>, string>> =

        fun inform parameters (folder, procFolders, messagesAttachmentResults) ->
            messagesAttachmentResults
            |> (
                L.map (fun (message, messageAttachmentResults) ->
                    let categorised = categoriseMessageAttachments inform parameters messageAttachmentResults
                    match categorised with
                    | TernaryResult.Ok xs ->
                        inform $"+  [categoriseFolderMessageAttachments] Saving {Seq.length xs} attachments"
                    | TernaryResult.Ignore ->
                        inform "~  [categoriseFolderMessageAttachments] Message ignored"
                    | TernaryResult.Error msg ->
                        printfn $"!\u274c  [categoriseFolderMessageAttachments] {msg}"

                    (message, categorised))
                >> fun xs -> (folder, procFolders, xs))

    let writeAttachmentToFile: DestinationFolder -> IMailFolder * IMessageSummary * MimePart -> TernaryResult<int64, string> =
        fun destinationFolder (folder, message, attachment) ->
            let exportDir = destinationFolder
            let localPart = Naming.name folder message attachment
            printfn $"+  [writeAttachmentToFile] {localPart}"
            FS.fsWriteToFile Message.tryCopyAttachmentToStream (FS.absoluteName exportDir localPart) attachment

    let saveAttachment: DestinationFolder -> IMailFolder -> IMessageSummary * MimePart -> TernaryResult<MimePart * int64, string> =
        fun toFolder folder ->
            fun (message, attachment) ->
                writeAttachmentToFile toFolder (folder, message, attachment)
                |> TernaryResult.map (fun countOfBytesWritten -> (attachment, countOfBytesWritten))

    let saveMessageAttachments: DestinationFolder -> IMailFolder -> IMessageSummary -> TernaryResult<LazyList<MimePart>, string> ->
        IMessageSummary * TernaryResult<LazyList<MimePart> * int64, string> =
            let partial toFolder folder message =
                TernaryResult.bind (
                    L.map (fun attachment ->
                        saveAttachment toFolder folder (message, attachment))
                    >> TernaryResult.groupResult)
                >> TernaryResult.map (Seq.fold (fun (parts, total) (part, size) ->
                    (L.append parts (Seq.singleton part |> L.ofSeq), total + size)) (L.empty, 0L))
                
            fun saveToFolder folder message attachmentInstructions ->
                (message, partial saveToFolder folder message attachmentInstructions)

    let saveFolderAttachments: DestinationFolder -> IMailFolder * ProcFolders * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart>, string>> ->
        IMailFolder * ProcFolders * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart> * int64, string>> =
            let partial saveToFolder folder =
                L.map (
                    fun (message, messageAttachmentInstructions) ->
                        saveMessageAttachments saveToFolder folder message messageAttachmentInstructions
                    )

            fun saveToFolder (folder, procFolders, messagesAttachmentResults) ->
                (folder, procFolders, partial saveToFolder folder messagesAttachmentResults)

    let moveFolderMessages: IMailFolder * ProcFolders * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart> * int64, string>> ->
            Result<IMailFolder * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart> * int64, string>> * ProcCounts, string> =
        fun (folder, (processed, attention), messagesAttachmentsResults) ->
            FTry.tryOpenFolderWritable folder
            |> Result.map (fun fromFolder ->
                messagesAttachmentsResults
                |> L.map (fun (message, attachmentResult) ->
                    match attachmentResult with
                    | TernaryResult.Ok (_, sizeInBytes) ->
                        printfn $">> [moveFolderMessages][{string sizeInBytes} bytes] {string message.NormalizedSubject}"
                        FTry.tryMoveMessageTo processed fromFolder message |> Result.map (fun uid -> (Some uid, None))
                    | _ ->
                        printfn $">\u274c [moveFolderMessages] {string message.NormalizedSubject}"
                        FTry.tryMoveMessageTo attention fromFolder message |> Result.map (fun uid -> (None, Some uid))
                    )
                |> L.fold (fun (processedSet, attentionSet) r ->
                    match r with
                    | Ok (Some uid, None) -> (Set.add uid.Id processedSet, attentionSet)
                    | Ok (None, Some uid) -> (processedSet, Set.add uid.Id attentionSet)
                    | _ -> (processedSet, attentionSet)) (Set.empty, Set.empty)
                |> (fun (processedSet, attentionSet) ->
                    (fromFolder, messagesAttachmentsResults, (Set.count processedSet, Set.count attentionSet)))
            )

    let summarizeFolderResults: Log -> IMailFolder * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart> * int64, string>> * ProcCounts ->
            TernaryResult<ProcCounts * int * int64, string> =
        let logSummary report (folder: IMailFolder) (result: TernaryResult<ProcCounts * int * int64, string>) =
            report $"| [summarizeFolderResults][{string result}] {folder.FullName}"
            result

        fun report (folder, messagesExportResults, procCounts) ->
            messagesExportResults
            |> L.map (fun (_, tr) ->
                tr |> TernaryResult.map (fun (attachmentsSaved, totalSize) ->
                    (Seq.length attachmentsSaved, totalSize))
                )
            |> TernaryResult.groupResult
            |> TernaryResult.map (
                Seq.fold (
                    fun (procTotals, totalCount, totalSize) (count, size) ->
                        (procTotals, totalCount + count, totalSize + size)
                    ) (procCounts, 0, 0L)
                )
            |> logSummary report folder

    let collectFolderSummaries: Result<RoseTree<TernaryResult<ProcCounts * int * int64, string>> seq, string> ->
        Result<ProcCounts * int * int64, string> =
            let countGroup: TernaryResult<ProcCounts * int * int64, string> seq -> TernaryResult<ProcCounts * int * int64, string> =
                printfn "== countGroup =="
                L.ofSeq
                >> TernaryResult.groupResult
                >> TernaryResult.map (
                    Seq.fold (
                        fun ((processedTotal, attentionTotal), iTot, lTot) ((nProcessed, nAttention), i, l) ->
                            ((processedTotal + nProcessed, attentionTotal + nAttention), iTot + i, lTot + l)
                        ) ((0, 0), 0, 0L)
                    )

            fun forestResult ->
                printfn $"== collectFolderSummaries =="
                forestResult
                |> Result.bind (
                    Seq.map (RoseTree.dfsPre >> countGroup)
                    >> countGroup
                    >> TernaryResult.toResult (Ok ((0, 0), 0, 0L))
                    )

    let log (AbsoluteFilename logDir) (RelativeFilename filename) =
        let infoDir = FS.assertFolder logDir
        let logFile = FS.absoluteName infoDir filename
        let writer = IO.File.AppendText logFile

        fun (message: string) ->
            writer.WriteLine(message)
            writer.Flush()

    // Report:
    // - number of messages rejected due to MIME type (or other categorisation rule)
    // - number of messages for which attachments were exported
    // - number of messages for which no attachments were exported because they had already been exported
    // - total number of attachments exported
    // - total number of attachments not exported because they had already been exported
    let prog0 (parameters: RuntimeParameters) =
        let inform = log parameters.LoggingDestinations.LogDir parameters.LoggingDestinations.Info
        let report (msg: string) = printfn $"{msg}"
        openSessionFromParameters parameters.SessionParameters
        |> Result.bind (rootFoldersViaSession parameters.ExtraParameters.SourceFolders)
        |> Result.map (fetchFolderForest parameters.PostProcessingFolders)
        |> RFoR.bind (assertProcessingFolders inform parameters.PostProcessingFolders)
        |> RFoR.map (enumerateFolderMessages inform)
        |> RFoR.map (enumerateMessageAttachments inform)
        |> RFoR.map (categoriseFolderMessageAttachments inform parameters.CategorisationParameters)
        |> RFoR.map (saveFolderAttachments parameters.DestinationFolder)
        |> RFoR.bind moveFolderMessages
        |> (RoseTree.map >> Seq.map >> Result.map) (TernaryResult.ofResult >> (TernaryResult.bind (summarizeFolderResults report)))
        |> collectFolderSummaries

    let run =
        chooseConfigurationFile
        >> Config.fromSource
        >> Result.bind parametersFromConfiguration
        >> Result.bind prog0

    run (ExecutionContext (argv, "ProcessAttachments.json"))
    |> ET.handleResult
