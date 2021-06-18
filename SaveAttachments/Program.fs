// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.ComponentModel

module ET = ProcessAttachments.Execution.Templates

open MailKit
open MailKit.Net.Imap

open ProcessAttachments.DomainInterface
open ProcessAttachments.ImapKit.ImapService
open FSharp.Core.Extensions

module TC = TypedConfiguration
module Folder = ProcessAttachments.ImapKit.ImapFolder
module Message = ProcessAttachments.ImapKit.ImapMessage

let personalNamespace =
    fun (client: ImapClient) ->
        client.PersonalNamespaces.Item(0)

open System
open ProcessAttachments.Collections

type FakeSession(id: int) =
    member _.Open = id
    
    interface IDisposable with
        member this.Dispose() =
            ()

[<EntryPoint>]
let main argv =
    let behaviour = {
        defaultConfigFilename = "ProcessAttachments.json"
        configuration =
            fun config ->
               match (TC.imapServiceParameters config,
                      TC.mailboxParameters config,
                      TC.exportParameters config) with
               | Some sessionParams, Some mailboxParams, Some exportParams ->
                   Ok (sessionParams, mailboxParams, exportParams)
               | _ -> Error "Failed to load configuration"
        session = fun (sessionParams, _, _) -> new ImapSession(sessionParams)
        container = fun session -> session.Open
        roots =
            fun (_, mailboxParams, _) ->
               let filter (node: IMailFolder) =
                   mailboxParams.SourceFolders
                   |> Seq.icontains node.Name
               Result.map (
                   fun container ->
                       Folder.listFoldersInNamespace personalNamespace container
                       |> Seq.filter filter
                   )
        nodes = Folder.dfsPre
        closeNode = Folder.closeFolder
        leaves = Folder.enumerateMessages None
        contentItems = Message.dfsPre
        exportContent = fun folder message attachment ->
            eprintfn $"Export attachment: [{folder.FullName}] [{message.Envelope.Subject}] [{attachment.FileName}]"
            Ok 0L
        onCompletion = fun (ok, failed) ->
            List.length failed
        inspectNode = fun node ->
            eprintfn $"Inspecting folder: {node.FullName}"
            node
        inspectLeaf = fun leaf ->
            eprintfn $"Inspecting message: {leaf.Envelope.Subject}"
        identifyNode = fun node -> $"[folder {node.FullName}]"
        identifyLeaf = fun leaf -> $"[subject {leaf.Envelope.Subject}][date {leaf.Envelope.Date}]"
    }

    let behaviourTest = {
        defaultConfigFilename = "Config1.json"
        configuration = fun _ -> () |> Ok
        session = fun _ -> new FakeSession(17)
        container = fun _ ->
            ()|> Ok
        roots = fun _ _ -> seq {0} |> Ok
        nodes = fun root ->
            let validate = Ok
            let ls x =
                match x with
                | 0 -> seq {1;6;7}
                | 1 -> seq {2;3;4}
                | 4 -> seq {5}
                | 7 -> seq {8;9}
                | _ -> Seq.empty
                |> Ok
            Conversions.dfsPre validate ls root
        closeNode = Result.map (fun n ->
            eprintfn $"[{string n}] Closing node"
            eprintfn "---------------------"
            n)
        leaves = fun x ->
            match x with
            | 1 -> seq {100; 101}
            | 2 -> seq {200; 201; 202}
            | 5 -> seq {500}
            | 8 -> seq { 800 .. 804 }
            | _ -> Seq.empty
            |> Ok
        contentItems = fun x ->
            match x with
            | small when small < 200 -> seq { small * 10 }
            | big when big > 700 -> seq { big * 10 }
            | _ -> Seq.empty
        exportContent = fun n l c ->
            eprintfn $"[{string n}] Export content [{string l}, {string c}]"
            Ok (Convert.ToInt64 c)
        onCompletion = fun (ok, failed) ->
            Seq.length failed
        inspectNode = fun n ->
            eprintfn $"[{string n}] node"
            n
        inspectLeaf = fun l ->
            eprintfn $"[{string l}] leaf"
        identifyNode = string
        identifyLeaf = string
    }
    argv |> ET.main behaviourTest
