namespace Main

open MailKit.Net.Imap

open ImapAttachments

module Processor =
    let downloadAttachments config: Result<ImapClient, string> =
        match (TypedConfiguration.imapServiceParameters config, TypedConfiguration.mailboxParameters config) with
        | Some serviceParameters, Some mailboxParameters ->
            ImapService.initialize
            |> ImapService.tryConnect serviceParameters.Endpoint
            |> Result.bind (ImapService.tryAuthenticate serviceParameters.Credentials)
        | _ ->
            Result.Error "Failed to load configuration"
