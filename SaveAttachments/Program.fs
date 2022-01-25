open System
open System.IO
open System.Net.Mime
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
module Export = ProcessAttachments.FileSystem.Export
module Service = ProcessAttachments.ImapKit.ImapService
module CC = ProcessAttachments.ImapKit.Categorise

let personalNamespace =
    fun (client: ImapClient) ->
        client.PersonalNamespaces.Item(0)

type DestinationFolder = DirectoryInfo
type SourceFolders = string seq
type GenericRuntimeParameters<'a> =
    {
        SessionParameters: SessionParameters
        CategorisationParameters: AttachmentCategorisationParameters
        DestinationFolder: DestinationFolder
        ExtraParameters: 'a
    }

type ExtraParameters =
    {
        SourceFolders: SourceFolders
        ProcessedSubfolder: string
        AttentionSubfolder: string
    }

type RuntimeParameters = GenericRuntimeParameters<ExtraParameters>
type ParametersFromConfiguration = IConfiguration -> RuntimeParameters

type ProcFolders = IMailFolder * IMailFolder

type ProcCounts = int * int

type MessagesWithAttachments = LazyList<IMessageSummary * LazyList<Result<MimePart, string>>>

let inline (>?>) x f = x >> (Result.bind f)

type ExecutionContext =
    | ExecutionContext of programArgs: string[] * defaultConfigurationFilename: string

type FolderResultTree = RoseTree<Result<IMailFolder, string>>

type AttachmentStructure = IMailFolder * Result<IMessageSummary, string>

type StepContext = RuntimeParameters

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
               TC.exportParameters configuration) with
        | Some sessionParameters, Some categorisationParameters, Some mailboxParameters, Some exportParameters ->
            let parameters = {
                SessionParameters = sessionParameters
                CategorisationParameters = categorisationParameters
                DestinationFolder = FS.assertFolder exportParameters.DestinationFolder
                ExtraParameters =
                    {
                        SourceFolders = mailboxParameters.SourceFolders
                        ProcessedSubfolder = mailboxParameters.ProcessedSubfolder
                        AttentionSubfolder = mailboxParameters.AttentionSubfolder
                    }
            }
            printfn $"Runtime Parameters: {parameters}"
            parameters |> Result.Ok
        | _ -> Result.Error "Failed to infer valid runtime parameters from the provided configuration"

    let openSessionFromParameters: RuntimeParameters -> Result<OpenImapSession, string> =
        fun rtArgs ->
            printfn "== [openSessionFromParameters] =="
            let client = Service.initialize
            Service.openSessionViaClient rtArgs.SessionParameters client

    let rootFoldersViaSession: RuntimeParameters -> OpenImapSession -> Result<IMailFolder seq, string> =
        fun parameters session ->
            printfn "== [rootFoldersViaSession] =="
            let folderFilter (folder: IMailFolder) =
                parameters.ExtraParameters.SourceFolders |> Seq.icontains folder.Name
            ImF.selectFoldersFromNamespace ImF.clientDefaultPersonalNamespace folderFilter session.Client

    let fetchFolderForest: RuntimeParameters -> IMailFolder seq -> FolderResultTree seq =
        let validate = FTry.tryOpenFolder
        let ls excludedFolderNames r =
            let filter (folder: IMailFolder) = not (Seq.contains folder.Name excludedFolderNames)

            match r with
            | Result.Ok folder ->
                match (FTry.tryGetSubfoldersWhere filter folder) with
                | Result.Ok children -> (Result.Ok folder, L.ofSeq children |> L.map validate)
                | Result.Error msg -> (Result.Ok folder, L.ofList [Result.Error msg])
            | Result.Error msg -> (Result.Error msg, L.ofList [Result.Error msg])

        fun parameters roots ->
            printfn $"== [fetchFolderForest] [root count = {Seq.length roots}] =="
            let excludedFolderNames = seq {
                parameters.ExtraParameters.ProcessedSubfolder
                parameters.ExtraParameters.AttentionSubfolder
            }
            roots |> Seq.map (
                fun topLevelFolder ->
                    RoseTree.unfold (ls excludedFolderNames) (validate topLevelFolder)
                )

    let assertProcessingFolders: RuntimeParameters -> IMailFolder -> Result<IMailFolder * ProcFolders, string> =
        fun parameters folder ->
            let processedSubfolderName = parameters.ExtraParameters.ProcessedSubfolder
            let attentionSubfolderName = parameters.ExtraParameters.AttentionSubfolder
            
            let processedResult = FTry.tryCreateSubfolderIfNotExists folder processedSubfolderName
            let attentionResult = FTry.tryCreateSubfolderIfNotExists folder attentionSubfolderName
            
            match (processedResult, attentionResult) with
            | Ok processed, Ok attention ->
                printfn "\u2713 [assertProcessingFolders]"
                Ok (folder, (processed, attention))
            | Error msg, _ ->
                printfn $"\u274c [assertProcessingFolders] {parameters.ExtraParameters.ProcessedSubfolder}"
                Error msg
            | _, Error msg ->
                printfn $"\u274c [assertProcessingFolders] {parameters.ExtraParameters.AttentionSubfolder} "
                Error msg

    let enumerateFolderMessages: StepContext -> IMailFolder * ProcFolders -> IMailFolder * ProcFolders * LazyList<IMessageSummary> =
        fun _ (folder, processingFolders) ->
            let messages = 
                match ImF.enumerateMessages None folder with
                | Ok m -> m
                | Error _ -> Seq.empty
            printfn $"? [enumerateFolderMessages] message count = {Seq.length messages}"
            (folder, processingFolders, L.ofSeq messages)

    let enumerateMessageAttachments: StepContext -> IMailFolder * ProcFolders * LazyList<IMessageSummary> -> IMailFolder * ProcFolders * MessagesWithAttachments =
        fun _ (folder, procFolders, messages) ->
            printfn $"? [enumerateMessageAttachments] message count = {Seq.length messages}"
            messages
            |> L.map (
                fun message ->
                    let attachments = Message.dfsPre folder message
                    printfn $"? [enumerateMessageAttachments] attachment count = {Seq.length attachments}"
                    (message, L.ofSeq attachments)
                )
            |> (fun messagesWithAttachments ->
                (folder, procFolders, messagesWithAttachments))

    let categoriseAttachment: RuntimeParameters -> MimePart ->
        TernaryResult<MimePart, string> =
            fun parameters attachment ->
                let ignore1 = parameters.CategorisationParameters.IgnoreBasedOnFilename
                let ignore2 = CC.isContentType parameters.CategorisationParameters.IgnoredMimeTypes
                let accept = parameters.CategorisationParameters.AcceptedMimeTypes
                
                let contentTypeConverter: ContentType -> string option -> ContentType =
                    fun mimeType fileExtension ->
                        match (mimeType.MediaType, mimeType.MediaSubtype, fileExtension) with
                        | "text", "html", Some ".pdf" -> ContentType("application", "pdf")
                        | "text", "html", Some ".csv" -> ContentType("text", "comma-separated-values")
                        | "application", "octet-stream", Some ".csv" -> ContentType("text", "comma-separated-values")
                        | _ -> mimeType

                let fileExtension = Path.GetExtension(attachment.FileName) |> Option.ofObj
                let maybeContentType, maybeFilename =
                    let ct = attachment.ContentType |> Option.ofObj
                    let fn = attachment.FileName |> Option.ofObj
                    
                    match ct with
                    | Some mimeType -> (Some (contentTypeConverter mimeType fileExtension), fn)
                    | None -> (None, fn)

                match (ignore1 maybeFilename || ignore2 maybeContentType) with
                | true ->
                    printfn $"~ [categoriseAttachment] [type = {string attachment.ContentType.MimeType}] [name = {attachment.FileName}]"
                    Ignore
                | false ->
                    match (CC.isContentType accept maybeContentType) with
                    | true ->
                        printfn $"+ [categoriseAttachment] [Content Type = {string attachment.ContentType.MimeType}] [Filename = {string attachment.FileName}]"
                        TernaryResult.Ok attachment
                    | false ->
                        printfn $"! [categoriseAttachment] [Content Type = {string attachment.ContentType.MimeType}] [Filename = {string attachment.FileName}]"
                        TernaryResult.Error $"[{string maybeContentType} Missing or unsupported MIME type"

    let categoriseAttachmentResult: RuntimeParameters -> Result<MimePart, string> ->
        TernaryResult<MimePart, string> =
            fun parameters attachmentResult ->
                match attachmentResult with
                | Ok attachment -> categoriseAttachment parameters attachment
                | Error msg -> TernaryResult.Error msg

    let categoriseMessageAttachments: RuntimeParameters -> LazyList<Result<MimePart, string>> ->
            TernaryResult<LazyList<MimePart>, string> =
        fun parameters ->
            L.map (categoriseAttachmentResult parameters)
            >> TernaryResult.groupResult

    let categoriseFolderMessageAttachments:
        StepContext -> IMailFolder * ProcFolders * MessagesWithAttachments ->
            IMailFolder * ProcFolders * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart>, string>> =

        fun parameters (folder, procFolders, messagesAttachmentResults) ->
            messagesAttachmentResults
            |> (
                L.map (fun (message, messageAttachmentResults) ->
                    let categorised = categoriseMessageAttachments parameters messageAttachmentResults
                    match categorised with
                    | TernaryResult.Ok xs ->
                        printfn $"+ [categoriseFolderMessageAttachments] Saving {Seq.length xs} attachments"
                    | TernaryResult.Ignore ->
                        printfn $"~ [categoriseFolderMessageAttachments] Message ignored"
                    | TernaryResult.Error msg ->
                        printfn $"! [categoriseFolderMessageAttachments] {msg}"

                    (message, categorised))
                >> fun xs -> (folder, procFolders, xs))

    let writeAttachmentToFile: RuntimeParameters -> IMailFolder * IMessageSummary * MimePart -> TernaryResult<int64, string> =
        fun parameters (folder, message, attachment) ->
            let exportDir = parameters.DestinationFolder
            let localPart = Naming.name folder message attachment
            printfn $"+ [writeAttachmentToFile] {localPart}"
            Export.fsWriteToFile Message.tryCopyAttachmentToStream (FS.absoluteName exportDir localPart) attachment

    let saveAttachment: RuntimeParameters -> IMailFolder -> IMessageSummary * MimePart -> TernaryResult<MimePart * int64, string> =
        fun parameters folder ->
            fun (message, attachment) ->
                writeAttachmentToFile parameters (folder, message, attachment)
                |> TernaryResult.map (fun countOfBytesWritten -> (attachment, countOfBytesWritten))

    let saveMessageAttachments: RuntimeParameters -> IMailFolder -> IMessageSummary -> TernaryResult<LazyList<MimePart>, string> ->
        IMessageSummary * TernaryResult<LazyList<MimePart> * int64, string> =
            let partial parameters folder message =
                TernaryResult.bind (
                    L.map (fun attachment ->
                        saveAttachment parameters folder (message, attachment))
                    >> TernaryResult.groupResult)
                >> TernaryResult.map (Seq.fold (fun (parts, total) (part, size) ->
                    (L.append parts (Seq.singleton part |> L.ofSeq), total + size)) (L.empty, 0L))
                
            fun parameters folder message attachmentInstructions ->
                (message, partial parameters folder message attachmentInstructions)

    let saveFolderAttachments: StepContext -> IMailFolder * ProcFolders * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart>, string>> ->
        IMailFolder * ProcFolders * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart> * int64, string>> =
            let partial parameters folder =
                L.map (
                    fun (message, messageAttachmentInstructions) ->
                        saveMessageAttachments parameters folder message messageAttachmentInstructions
                    )

            fun parameters (folder, procFolders, messagesAttachmentResults) ->
                (folder, procFolders, partial parameters folder messagesAttachmentResults)

    let moveFolderMessages: StepContext -> IMailFolder * ProcFolders * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart> * int64, string>> ->
            Result<IMailFolder * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart> * int64, string>> * ProcCounts, string> =
        fun _ (folder, (processed, attention), messagesAttachmentsResults) ->
            FTry.tryOpenFolderWritable folder
            |> Result.map (fun fromFolder ->
                messagesAttachmentsResults
                |> L.map (fun (message, attachmentResult) ->
                    match attachmentResult with
                    | TernaryResult.Ok (_, sizeInBytes) ->
                        printfn $">> [moveFolderMessages][{string sizeInBytes} bytes] {string message.NormalizedSubject}"
                        FTry.tryMoveMessageTo processed fromFolder message |> Result.map (fun uid -> (Some uid, None))
                    | _ ->
                        printfn $">! [moveFolderMessages] {string message.NormalizedSubject}"
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

    let summarizeFolderResults: StepContext ->
        Result<IMailFolder * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart> * int64, string>> * ProcCounts, string> ->
            TernaryResult<ProcCounts * int * int64, string> =
        let logSummary (folder: IMailFolder) (result: TernaryResult<ProcCounts * int * int64, string>) =
            printfn $"| [summarizeFolderResults][{string result}] {folder.FullName}"
            result

        let partial (_: RuntimeParameters): IMailFolder * LazyList<IMessageSummary * TernaryResult<LazyList<MimePart> * int64, string>> * ProcCounts -> TernaryResult<ProcCounts * int * int64, string> =
            fun (folder, messagesExportResults, procCounts) ->
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
                |> logSummary folder

        fun parameters ->
            TernaryResult.ofResult
            >> TernaryResult.bind (partial parameters)

    let collectFolderSummaries: StepContext * Result<RoseTree<TernaryResult<ProcCounts * int * int64, string>> seq, string> ->
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

            fun (_, forestResult) ->
                printfn $"== collectFolderSummaries =="
                forestResult
                |> Result.bind (
                    Seq.map (RoseTree.dfsPre >> countGroup)
                    >> countGroup
                    >> TernaryResult.toResult (Ok ((0, 0), 0, 0L))
                    )

    // Report:
    // - number of messages rejected due to MIME type (or other categorisation rule)
    // - number of messages for which attachments were exported
    // - number of messages for which no attachments were exported because they had already been exported
    // - total number of attachments exported
    // - total number of attachments not exported because they had already been exported
    let prog0 =
        next openSessionFromParameters
        >> rNext Result.bind rootFoldersViaSession
        >> rNext Result.map fetchFolderForest
        >> rNext RFoR.bind assertProcessingFolders
        >> rNext RFoR.map enumerateFolderMessages
        >> rNext RFoR.map enumerateMessageAttachments
        >> rNext RFoR.map categoriseFolderMessageAttachments
        >> rNext RFoR.map saveFolderAttachments
        >> rNext RFoR.bind moveFolderMessages
        >> rNext (RoseTree.map >> Seq.map >> Result.map) summarizeFolderResults
        >> collectFolderSummaries

    let run =
        chooseConfigurationFile
        >> Config.fromSource
        >> Result.bind parametersFromConfiguration
        >> Result.bind prog0

    run (ExecutionContext (argv, "ProcessAttachments.json"))
    |> ET.handleResult
