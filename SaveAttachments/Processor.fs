module SaveAttachments.Processor

open MailKit
open MailKit.Net.Imap

open ImapKit
module S = FSharp.Core.Seq
module TC = TypedConfiguration
module FS = Flows.DfsSequence
module Def = Defaults

type FolderResult<'e> = Result<IMailFolder, 'e>
type FolderMessages<'e> = FolderResult<'e> * IMessageSummary seq
type SaveResult = Result<IMessageSummary * BodyPartBasic, string>
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

        let rootFolders: Result<ImapClient, string> -> Result<IMailFolder seq, string> =
            Result.map (
                    ImapFolder.listFoldersInNamespace behaviour.folderNamespace
                    >> Seq.filter isASourceFolder
                )

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

        let (|>>) f g = f >> (Result.bind g)

        let folderDfs (roots: IMailFolder seq) =
            Seq.collect ImapFolder.dfsPre

        let client = session.Open
        let roots: Result<IMailFolder seq, string> = client |> rootFolders
        let folders: Result<IMailFolder, string> seq = roots |> Result.bind folderDfs
        let msgs' (folder: IMailFolder): Result<IMailFolder * IMessageSummary, string> seq =
            items folder
            |> Seq.map Ok (fun msg -> (folder, msg))
        let msgs: Result<IMailFolder * IMessageSummary, string> seq =
            folders
            |> Seq.collect (
                fun r ->
                    match r with
                    | Ok folder -> msgs' folder
                    | Error msg -> Seq.singleton (Error msg)
                )
        let atts' ((folder, msg): IMailFolder * IMessageSummary): Result<IMailFolder * IMessageSummary * BodyPartBasic, string> seq =
            attachments msg
            |> Seq.map Ok (fun bp -> (folder, msg, bp))
        let atts: Result<IMailFolder * IMessageSummary * BodyPartBasic, string> seq =
            msgs
            |> Seq.collect (
                fun r ->
                    match r with
                    | Ok folderMessage -> atts' folderMessage
                    | Error msg -> Seq.singleton (Error msg)
                )
        
        let mapOverResults (x2ys: 'a -> 'c seq) (xs: Result<'a, 'b> seq): (Result<'a, 'b> * 'c seq) seq =
            xs
            |> Seq.map (
                fun result ->
                    match result with
                    | Ok x -> (result, x2ys x)
                    | Error _ -> (result, Seq.empty)
                )

        let messages: FolderResult<'e> seq -> FolderMessages<'e> seq =
            mapOverResults (ImapFolder.enumerateMessages behaviour.query)

//        let folderAttachments (messages: FolderMessages<'e>): (FolderResult<'e> * (IMessageSummary * BodyPartBasic) seq) seq =
        let folderAttachments (messages: FolderMessages<'e>) =
            messages
            |> Seq.map (
                fun (result, folderMessages) ->)

        Ok "test"
//        let messageAttachments: (FolderResult * (IMessageSummary * BodyPartBasic seq) seq) seq
//        let saveResults: (FolderResult * SaveResult seq) seq
//        let writeAttachmentToFile: SaveResult =
//            fun (folder: IMailFolder) (message: IMessageSummary) (part: BodyPartBasic) ->
//                Ok (message, part)
//
//        let usedFolders =
//            atts |> Result.bind (
//                    Seq.iter (
//                        fun (folder, message, bodyPart) ->
//                            writeAttachmentToFile folder message bodyPart))
            // idea: communicate information about whether each write succeeded
            // why:
            //  - log failure cases
            //  - move only messages with full success to processed folder
//            folders
//        let xs': Result<IMailFolder * Result<IMessageSummary * BodyPartBasic, string> seq, string> =
//            atts |> Seq.groupBy (
//                fun r ->
//                    match r with
//                    | Ok x -> fst x
//                    | Error msg -> msg)
        
//        session.Open
//        |> (rootFolders
//            |>> folderDfs
//            >> Seq.map (action0 items attachments)
//            >> Seq.map action1
//            >> Seq.map ImapFolder.closeFolder
//            >> List.ofSeq
//            >> List.map warningHandler
//            >> List.partition (
//                fun r ->
//                    match r with
//                    | Result.Ok _ -> true
//                    | _ -> false
//            )
//            >> onComplete
//                FS.pipeline rootFolders ImapFolder.dfsPre saveAttachments ImapFolder.closeFolder reportError onComplete)
//        )
    | _ ->
        Error "Failed to load configuration"
