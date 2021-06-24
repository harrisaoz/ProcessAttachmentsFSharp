module ProcessAttachments.ImapKit.ImapService

open System.Net
open MailKit.Net.Imap
open MailKit.Security

type Endpoint =
    {
        Hostname: string
        Port: int
    }

type SessionParameters =
    {
        Endpoint: Endpoint
        Credentials: NetworkCredential
    }

let initialize: ImapClient =
    let client = new ImapClient()
    client.ServerCertificateValidationCallback <- fun _ _ _ _ -> true
    
    client
    
let tryConnect endpoint (client: ImapClient): Result<ImapClient, string> =
    try
        client.Connect(endpoint.Hostname, endpoint.Port, SecureSocketOptions.SslOnConnect)
        Ok client
    with
        ex ->
            Error ex.Message

let tryAuthenticate (credentials: ICredentials) (client: ImapClient): Result<ImapClient, string> =
    try
        client.Authenticate credentials
        Ok client
    with
        ex ->
            Error ex.Message

let connect parameters (client: ImapClient) =
    Ok client |>
    (Result.bind (tryConnect parameters.Endpoint)
     >> Result.bind (tryAuthenticate parameters.Credentials))
 
let disconnect (client: ImapClient) =
    try
        client.Disconnect(true)
        client.Dispose()
    with
        e -> printfn $"Unexpected failure to disconnect the imap client. %s{e.Message}"
