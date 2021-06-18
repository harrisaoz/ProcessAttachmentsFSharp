﻿module TypedConfiguration

open FSharp.Json

open Configuration
open ProcessAttachments.ImapKit
open Microsoft.Extensions.Configuration

let imapServiceSectionName = "ImapService"

let inline getConfig name binder container =
    name |> (Load.section container >> Option.bind binder)

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
        match (endpoint serviceSection, credentials serviceSection) with
        | Some endpoint, Some credentials ->
            let parameters: ImapService.SessionParameters option =
                Some {
                    Endpoint = endpoint
                    Credentials = credentials
                }
            parameters
        | _ -> None

type MailboxParameters =
    {
        SourceFolders: string seq
    }

let mailboxParameters: IConfiguration -> MailboxParameters option =
    getConfig "Mailbox" <| fun mailboxSection ->
        let sourceFolders = Load.readMany mailboxSection "SourceFolders"

        if (Seq.isEmpty sourceFolders) then
            None
        else
            Some { SourceFolders = sourceFolders }

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
