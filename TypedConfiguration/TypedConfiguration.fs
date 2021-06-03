module TypedConfiguration

open FSharp.Json

open Configuration
open ImapAttachments

let imapServiceSectionName = "ImapService"

let endpoint serviceSection: ImapService.Endpoint option =
    Load.subsection serviceSection "Endpoint"
    |> Option.map (
        fun endpointSection ->
            let read = Load.readFromSection endpointSection
            {
                Hostname = read "Hostname" |> Option.get
                Port = read "Port" |> Option.bind Load.tryParseInt |> Option.defaultValue 993
            }
        )

let credentials serviceSection =
    Load.subsection serviceSection "Credentials"
    |> Option.bind (
        fun credentialsSection ->
            let read = Load.readFromSection credentialsSection
            match (read "Username", read "Password") with
            | Some username, Some password -> Some (System.Net.NetworkCredential(username, password))
            | _ -> None
        )
let imapServiceParameters config: ImapService.Parameters option =
    Load.section config imapServiceSectionName
    |> Option.bind (
        fun serviceSection ->
            match (endpoint serviceSection, credentials serviceSection) with
            | Some endpoint, Some credentials ->
                Some {
                    Endpoint = endpoint
                    Credentials = credentials
                }
            | _ -> None
        )

type MailboxParameters =
    {
        SourceFolders: string seq
    }

let mailboxParameters config: MailboxParameters option =
    Load.section config "Mailbox"
    |> Option.bind (
        fun mailboxSection ->
            let sourceFolders = Load.readListFromSection mailboxSection "SourceFolders"
            
            if (Seq.isEmpty sourceFolders) then
                None
            else
                Some { SourceFolders = sourceFolders }
        )

let inline generateConfiguration configurationData =
    Json.serialize configurationData
