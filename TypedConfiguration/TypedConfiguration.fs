module TypedConfiguration

open FSharp.Json

open Combinators.Standard
open Combinators
open Configuration
open MimeKit
open ProcessAttachments.ImapKit
open Microsoft.Extensions.Configuration

let imapServiceSectionName = "ImapService"

let inline getConfig name binder container =
    name |> (Load.section container >> Option.bind binder)

let provider: IConfiguration -> ImapService.Provider option =
    fun serviceSection ->
        Load.read serviceSection "Provider"
        |> Option.bind ImapService.providerByName

let endpoint: IConfiguration -> ImapService.Endpoint option =
    getConfig "Endpoint" <| fun endpointSection ->
        let read = Load.read endpointSection
        let endpoint: ImapService.Endpoint option =
            match (read "Hostname", read "Port") with
            | Some hostname, maybePort ->
                Some {
                    Hostname = hostname
                    Port = maybePort |> Option.bind Load.tryParseInt |> Option.defaultValue 993
                }
            | _ -> None
        endpoint

let credentials: IConfiguration -> System.Net.NetworkCredential option =
    getConfig "Credentials" <| fun credentialsSection ->
        let read = Load.read credentialsSection
        match (read "Username", read "Password") with
        | Some username, Some password -> Some (System.Net.NetworkCredential(username, password))
        | _ -> None

let imapServiceParameters: IConfiguration -> ImapService.SessionParameters option =
    getConfig "ImapService" <| fun serviceSection ->
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
        | _ -> None

type MailboxParameters =
    {
        SourceFolders: string seq
        ProcessedSubfolder: string
        AttentionSubfolder: string
    }

let mailboxParameters: IConfiguration -> MailboxParameters option =
    getConfig "Mailbox" <| fun mailboxSection ->
        let sourceFolders = Load.readMany mailboxSection "SourceFolders"
        let processedSubfolder = Load.read mailboxSection "ProcessedSubfolder"
        let attentionSubfolder = Load.read mailboxSection "AttentionSubfolder"

        match (sourceFolders |> List.ofSeq, processedSubfolder, attentionSubfolder) with
        | _ :: _, Some processed, Some attention ->
            Some {
                SourceFolders = sourceFolders
                ProcessedSubfolder = processed
                AttentionSubfolder = attention
            }
        | _ -> None

type LoggingParameters =
    {
        LogDir: string
        InfoFilename: string
        ErrorFilename: string
        TraceFilename: string option
        ReportFilename: string option
    }

let loggingParameters: IConfiguration -> LoggingParameters option =
    getConfig "Logging" <| fun exportSection ->
        match (
            Load.read exportSection "LogDir",
            Load.read exportSection "InfoFilename",
            Load.read exportSection "ErrorFilename",
            Load.read exportSection "TraceFilename",
            Load.read exportSection "ReportFilename"
            ) with
        | Some logDir, Some infoFile, Some errFile, traceFile, reportFile ->
            Some {
                LogDir = logDir
                InfoFilename = infoFile
                ErrorFilename = errFile
                TraceFilename = traceFile
                ReportFilename = reportFile
            }
        | _ -> None

type ExportParameters =
    {
        DestinationFolder: string
    }

let exportParameters: IConfiguration -> ExportParameters option =
    getConfig "Export" <| fun exportSection ->
        Load.read exportSection "DestinationFolder"
        |> Option.map (fun dest -> { DestinationFolder = dest })

let inline generateConfiguration configurationData =
    Json.serialize configurationData

type AttachmentCategorisationParameters =
    {
        AcceptedMimeTypes: ContentType seq
        IgnoredMimeTypes: ContentType seq
        IgnoreBasedOnFilename: string option -> bool
    }

let ignoreBasedOnFilename anywheres endings =
    C Seq.existsPredicate <| seq
        {
            Seq.maybeExistsWhere String.containsCI anywheres
            Seq.maybeExistsWhere String.endsWithCI endings
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

    let readAsContentTypes section subsection =
        Load.readMany section subsection
        |> Seq.map contentTypeFromString
        |> Seq.fold (fun contentTypes contentTypeOption ->
            match contentTypeOption with
            | Some contentType -> cons contentType contentTypes
            | None -> contentTypes) Seq.empty

    let ignoreFilenameSection section =
        Load.section section "IgnoreFilename"

    getConfig "Categorisation" <| fun section ->
        let acceptedMimeTypes, ignoredMimeTypes =
            Pair.map (readAsContentTypes section) "AcceptedMimeTypes" "IgnoredMimeTypes"
        let ignoreContains, ignoreEndsWith =
            match ignoreFilenameSection section with
            | Some filenameSection ->
                Pair.map (Load.readMany filenameSection) "Contains" "EndsWith"
            | None -> (Seq.empty, Seq.empty)

        if (Seq.isEmpty acceptedMimeTypes && Seq.isEmpty ignoredMimeTypes) then
            None
        else
            Some {
                AcceptedMimeTypes = acceptedMimeTypes
                IgnoredMimeTypes = ignoredMimeTypes
                IgnoreBasedOnFilename = ignoreBasedOnFilename ignoreContains ignoreEndsWith
            }
