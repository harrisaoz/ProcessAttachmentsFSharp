namespace Main

open MailKit
open MailKit.Net.Imap

open ImapAttachments

module S = FSharp.Core.Seq
module TC = TypedConfiguration

module Processor =
    let reportError result =
        result
        |> Result.mapError (fun msg ->
            printfn $"{msg}"
            msg)

    let downloadAttachments (fNamespace: ImapClient -> FolderNamespace) config =
        match (TC.imapServiceParameters config, TC.mailboxParameters config) with
        | Some serviceParameters, Some mailboxParameters ->
            use session = new ImapService.ImapSession(serviceParameters)

            match session.Open with
            | Result.Ok client ->
                client |>
                ImapFolder.filter fNamespace (
                    fun folder -> mailboxParameters.SourceFolders |> S.icontains folder.Name
                    )
                |> Seq.collect ImapFolder.dfsPre
                |> Seq.map ImapFolder.saveFolderAttachments
                |> Seq.map ImapFolder.closeFolder
                |> List.ofSeq
                |> List.map reportError
                |> List.filter (fun r ->
                               match r with
                               | Result.Ok  _ -> true
                               | _ -> false)
                |> List.length
                |> printfn "Processed %d folders"

                Result.Ok client
            | Result.Error msg ->
                Result.Error msg
        | _ ->
            Result.Error "Failed to load configuration"
