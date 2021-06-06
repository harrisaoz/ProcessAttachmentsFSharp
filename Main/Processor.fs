namespace Main

open ImapAttachments
open MailKit
open MailKit.Net.Imap

module S = FSharp.Core.Seq
module TC = TypedConfiguration

type TopLevelFolder = ImapClient -> FolderNamespace
type ResultPassThrough<'a, 'b> = Result<'a, 'b> -> Result<'a, 'b>

module Processor =
    let processFlow getTopLevel dfsPre save close reportError onComplete =
        getTopLevel
        >> Seq.collect dfsPre
        >> Seq.map save
        >> Seq.map close
        >> List.ofSeq
        >> List.map reportError
        >> List.partition (
            fun r ->
                match r with
                | Result.Ok _ -> true
                | _ -> false
            )
        >> onComplete

    let downloadAttachments (fNamespace: TopLevelFolder) reportError config =
        match (TC.imapServiceParameters config,
               TC.mailboxParameters config,
               TC.exportParameters config) with
        | Some serviceParameters, Some mailboxParameters, Some exportParameters ->
            use session = new ImapService.ImapSession(serviceParameters)
            let save = ImapFolder.save exportParameters.DestinationFolder

            let getTopLevel =
                ImapFolder.filter fNamespace (
                    fun folder -> mailboxParameters.SourceFolders |> S.icontains folder.Name
                )
            let saveAttachments = ImapFolder.saveFolderAttachments save
            let onComplete (l1, l2) =
                printfn $"=== Summary ==="
                printfn $"+ %d{List.length l1} ok"
                printfn $"! %d{List.length l2} errors"
                printfn $"==============="

            session.Open
            |> Result.map (processFlow getTopLevel ImapFolder.dfsPre saveAttachments ImapFolder.closeFolder reportError onComplete)
        | _ ->
            Result.Error "Failed to load configuration"