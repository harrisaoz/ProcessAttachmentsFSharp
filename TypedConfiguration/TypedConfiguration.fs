module TypedConfiguration

open FSharp.Json

open Configuration
open ImapAttachments
open Microsoft.Extensions.Configuration

let imapServiceSectionName = "ImapService"

let inline getConfig load binder name container =
    name |> (load container >> Option.bind binder)

let endpoint: IConfiguration -> ImapService.Endpoint option =
    let binder endpointSection =
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

    getConfig Load.section binder "Endpoint"

let credentials: IConfiguration -> System.Net.NetworkCredential option =
    let binder credentialsSection =
        let read = Load.read credentialsSection
        match (read "Username", read "Password") with
        | Some username, Some password -> Some (System.Net.NetworkCredential(username, password))
        | _ -> None

    getConfig Load.section binder "Credentials"

let imapServiceParameters: IConfiguration -> ImapService.Parameters option =
    let binder serviceSection =
        match (endpoint serviceSection, credentials serviceSection) with
        | Some endpoint, Some credentials ->
            let parameters: ImapService.Parameters option =
                Some {
                    Endpoint = endpoint
                    Credentials = credentials
                }
            parameters
        | _ -> None

    getConfig Load.section binder "ImapService"

type MailboxParameters =
    {
        SourceFolders: string seq
    }

let mailboxParameters: IConfiguration -> MailboxParameters option =
    let sectionBinder mailboxSection =
        let sourceFolders = Load.readMany mailboxSection "SourceFolders"

        if (Seq.isEmpty sourceFolders) then
            None
        else
            Some { SourceFolders = sourceFolders }

    getConfig Load.section sectionBinder "Mailbox"

type ExportParameters =
    {
        DestinationFolder: string
    }

let exportParameters: IConfiguration -> ExportParameters option =
    let sectionBinder exportSection =
        Load.read exportSection "DestinationFolder"
        |> Option.map (fun dest -> { DestinationFolder = dest })

    getConfig Load.section sectionBinder "Export"

let inline generateConfiguration configurationData =
    Json.serialize configurationData
