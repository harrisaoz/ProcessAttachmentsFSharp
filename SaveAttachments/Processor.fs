module SaveAttachments.Processor

open MailKit
open MailKit
open MailKit
open MailKit
open MailKit.Net.Imap

open ImapKit
module S = FSharp.Core.Seq
module TC = TypedConfiguration
module FS = Flows.DfsSequence
module Def = Defaults

type TopLevelFolder = ImapClient -> FolderNamespace
type ResultPassThrough<'a, 'b> = Result<'a, 'b> -> Result<'a, 'b>

let warningHandler: Result<IMailFolder, string seq> -> Result<IMailFolder, string seq> =
    Result.mapError (
        fun msgs ->
            msgs |> Seq.iter (fun msg -> eprintfn $" {msg}")
            msgs
    )

let downloadAttachments (behaviour: Def.Behaviour) config =
    match (TC.imapServiceParameters config,
           TC.mailboxParameters config,
           TC.exportParameters config) with
    | Some serviceParameters, Some mailboxParameters, Some exportParameters ->
        use session = new ImapService.ImapSession(serviceParameters)
        let save = ImapFolder.save None exportParameters.DestinationFolder

        let isASourceFolder (folder: IMailFolder) =
            mailboxParameters.SourceFolders |> S.icontains folder.Name

        let rootFolders =
            ImapFolder.listFoldersInNamespace behaviour.folderNamespace
            >> Seq.filter isASourceFolder

//            let saveAttachments = ImapFolder.saveFolderAttachments save
        let onComplete (l1, l2) =
            printfn $"=== Summary ==="
            printfn $"+ %d{List.length l1} ok"
            printfn $"! %d{List.length l2} errors"
            printfn $"==============="

        let items = ImapFolder.enumerateMessages behaviour.query
        let attachments = ImapMessage.dfsPre

        let action0 (items: 'a -> 'b seq) (attachments: 'b -> 'c seq) =
            Result.map (
                fun folder ->
                    items
                    >> Seq.collect attachments
                    >> Seq.allPairs (Seq.singleton folder)
                    <| folder
            )

        let action1 =
            Result.map (Seq.head >> fst)

        let noop: Result<IMailFolder, string seq> -> Result<IMailFolder, string seq> =
            (action0 items attachments) >> action1

        session.Open
        |> Result.map (
            rootFolders
            >> Seq.collect ImapFolder.dfsPre
            >> Seq.map (action0 items attachments)
            >> Seq.map action1
            >> Seq.map ImapFolder.closeFolder
            >> List.ofSeq
            >> List.map warningHandler
            >> List.partition (
                fun r ->
                    match r with
                    | Result.Ok _ -> true
                    | _ -> false
            )
            >> onComplete
        )
//                FS.pipeline rootFolders ImapFolder.dfsPre saveAttachments ImapFolder.closeFolder reportError onComplete)
    | _ ->
        Result.Error "Failed to load configuration"
