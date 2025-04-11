module TypedConfiguration

open FSharp.Json

open MimeKit
open ProcessAttachments.ImapKit
open Microsoft.Extensions.Configuration

open FsCombinators.Core
module Pair = FsCombinators.Tuple2
module SeqExt = FsCombinators.SeqExtensions
module StringExt = FsCombinators.StringExtensions

module CfgRead = FsConfigLoader.Read
module CfgParse = FsConfigLoader.Parsers

let imapServiceSectionName = "ImapService"

// let inline getConfig name binder container =
//     name |> (Load.section container >> Option.bind binder)

let provider: IConfiguration -> ImapService.Provider option =
    fun serviceSection ->
        CfgRead.read serviceSection "Provider"
        |> Option.bind ImapService.providerByName

let endpoint: IConfiguration -> ImapService.Endpoint option =
    fun serviceSection ->
        CfgRead.section serviceSection "Endpoint"
        |> Option.bind (fun s ->
            match (CfgRead.read s "Hostname", CfgRead.read s "Port") with
            | Some hostname, maybePort ->
                Some {
                    Hostname = hostname
                    Port = maybePort |> Option.bind CfgParse.tryParseInt |> Option.defaultValue 993
                }
            | _ -> None)

let credentials: IConfiguration -> System.Net.NetworkCredential option =
    fun serviceSection ->
        CfgRead.section serviceSection "Credentials"
        |> Option.bind (fun s ->
            match (CfgRead.read s "Username", CfgRead.read s "Password") with
            | Some username, Some password -> Some (System.Net.NetworkCredential(username, password))
            | _ -> None)

let imapServiceParameters: IConfiguration -> ImapService.SessionParameters option =
    fun topLevelConfig ->
        CfgRead.section topLevelConfig "ImapService"
        |> Option.bind (fun serviceSection ->
            match (provider serviceSection,
                   endpoint serviceSection,
                   credentials serviceSection) with
            | Some provider, Some endpoint, Some credentials ->
                let parameters: ImapService.SessionParameters option =
                    Some {
                        Provider = provider
                        Endpoint = endpoint
                        Credentials = credentials
                    }
                parameters
            | _ -> None)

type MailFolderName = string

type SourceMailFolders =
    | SourceMailFolders of names: MailFolderName seq

type OutputMailFolders =
    | OutputMailFolders of ok: MailFolderName * error: MailFolderName

type MailboxParameters =
    {
        SourceFolders: SourceMailFolders
        OutputMailFolders: OutputMailFolders
    }

let mailboxParameters: IConfiguration -> MailboxParameters option =
    fun topLevelConfig ->
        CfgRead.section topLevelConfig "Mailbox"
        |> Option.bind (fun mailboxSection ->
            let sourceFolders = CfgRead.readMany mailboxSection "SourceFolders"
            let okSubfolder = CfgRead.read mailboxSection "OkSubfolder"
            let errorSubfolder = CfgRead.read mailboxSection "ErrorSubfolder"

            match (sourceFolders |> List.ofSeq, okSubfolder, errorSubfolder) with
            | _ :: _, Some processed, Some attention ->
                Some {
                    SourceFolders = SourceMailFolders sourceFolders
                    OutputMailFolders = OutputMailFolders (processed, attention)
                }
            | _ -> None)

type LoggingParameters =
    {
        LogDir: string
        InfoFilename: string
        ErrorFilename: string
        TraceFilename: string option
        ReportFilename: string option
    }

/// Supply the given onBind function with configuration parameter readers specific to the named section.
/// The readers are provided in the context of an Option, as the named section may not exist in
/// the provided configuration object.
/// The onBind function should take a tuple of configuration reader functions:
/// - childSection (gets a named subsection as an IConfiguration object)
/// - read (get the value of a single configuration parameter in the section)
/// - readMany (get all values associated with a multi-valued configuration parameter in the section).
let sectionConfig: string -> ((string -> IConfiguration option) * (string -> string option) * (string -> string seq) -> 'a option) -> IConfiguration -> 'a option  =
    fun sectionName onBind config ->
        CfgRead.section config sectionName
        |> Option.map (fun childSection ->
            CfgRead.section childSection, CfgRead.read childSection, CfgRead.readMany childSection)
        |> Option.bind onBind

let loggingParameters: IConfiguration -> LoggingParameters option =
    sectionConfig "Logging"
    <| (fun (_, read, _) ->
            match (
                read "LogDir",
                read "InfoFilename",
                read "ErrorFilename",
                read "TraceFilename",
                read "ReportFilename") with
            | Some logDir, Some infoFile, Some errFile, traceFile, reportFile ->
                Some {
                    LogDir = logDir
                    InfoFilename = infoFile
                    ErrorFilename = errFile
                    TraceFilename = traceFile
                    ReportFilename = reportFile
                }
            | _ -> None)

type ExportParameters =
    {
        DestinationFolder: string
    }

let exportParameters: IConfiguration -> ExportParameters option =
    sectionConfig "Export"
    <| (fun (_, read, _) ->
        read "DestinationFolder"
        |> Option.map (fun dest -> { DestinationFolder = dest }))

let inline generateConfiguration configurationData =
    Json.serialize configurationData

type AttachmentCategorisationParameters =
    {
        AcceptedMimeTypes: ContentType seq
        IgnoredMimeTypes: ContentType seq
        IgnoreBasedOnFilename: string option -> bool
    }

let ignoreBasedOnFilename anywheres endings =
    // let maybeExistsAnywheres = SeqExt.maybeExistsWhere StringExt.containsCI anywheres
    // let maybeExistsEndings = SeqExt.maybeExistsWhere StringExt.endsWithCI endings
    //
    // fun maybeName ->
    //     let result1 = (SeqExt.maybeExistsWhere) (StringExt.containsCI) anywheres maybeName
    //     let result2 = (SeqExt.maybeExistsWhere) (StringExt.endsWithCI) endings maybeName
    //     let result = result1 || result2
    //     result

    C SeqExt.existsPredicate <| seq
        {
            SeqExt.maybeExistsWhere (C StringExt.containsCI) anywheres
            SeqExt.maybeExistsWhere (C StringExt.endsWithCI) endings
        }

let categorisationParameters: IConfiguration -> AttachmentCategorisationParameters option =
    let contentTypeFromString (fullName: string) =
        match fullName.Split("/") with
        | [|mimeType; subType|] -> Some <| ContentType(mimeType, subType)
        | _ -> None

    let cons x xs =
        seq {
            yield x
            for x0 in xs do
                yield x0
        }

    let readAsContentTypes =
        Seq.map contentTypeFromString
        >> Seq.fold (fun contentTypes contentTypeOption ->
            match contentTypeOption with
            | Some contentType -> cons contentType contentTypes
            | None -> contentTypes) Seq.empty

    sectionConfig "Categorisation"
    <| (fun (childSection, _, readMany) ->
            let acceptedMimeTypes, ignoredMimeTypes =
                Pair.map (readMany >> readAsContentTypes) ("AcceptedMimeTypes", "IgnoredMimeTypes")

            let ignoreContains, ignoreEndsWith =
                match (childSection "IgnoreFilename") with
                | Some filenameSection ->
                    Pair.map (CfgRead.readMany filenameSection) ("Contains", "EndsWith")
                | None -> Seq.empty, Seq.empty
            
            if (Seq.isEmpty acceptedMimeTypes && Seq.isEmpty ignoredMimeTypes) then
                None
            else
                Some {
                    AcceptedMimeTypes = acceptedMimeTypes
                    IgnoredMimeTypes = ignoredMimeTypes
                    IgnoreBasedOnFilename = ignoreBasedOnFilename ignoreContains ignoreEndsWith
                })
