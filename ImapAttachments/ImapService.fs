﻿namespace ImapAttachments

open System.Net
open MailKit.Net.Imap
open MailKit.Security

module ImapService =
    type Endpoint =
        {
            Hostname: string
            Port: int
        }
    
    type Parameters =
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
            Result.Ok client
        with
            ex ->
                Result.Error ex.Message

    let tryAuthenticate (credentials: ICredentials) (client: ImapClient): Result<ImapClient, string> =
        try
            client.Authenticate credentials
            Result.Ok client
        with
            ex ->
                Result.Error ex.Message

    let disconnect (client: ImapClient) =
        try
            client.Disconnect(true)
            client.Dispose()
        with
            e -> printfn $"Unexpected failure to disconnect the imap client. %s{e.Message}"
