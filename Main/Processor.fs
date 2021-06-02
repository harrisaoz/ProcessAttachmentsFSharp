namespace Main

open MailKit.Net.Imap

open ImapAttachments

module Processor =
    let downloadAttachments config: Result<ImapClient, string> =
        match (TypedConfiguration.imapServiceParameters config, TypedConfiguration.mailboxParameters config) with
        | Some serviceParameters, Some mailboxParameters ->
            use session = new ImapService.ImapSession(serviceParameters)

            match session.Open with
            | Result.Ok client ->
                Result.Ok client
            | Result.Error msg ->
                Result.Error msg
        | _ ->
            Result.Error "Failed to load configuration"
