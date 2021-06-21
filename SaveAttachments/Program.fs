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
        categorise = fun message ->
            Ignore // to do
        exportContent = fun folder message attachment ->
            eprintfn $"Export attachment: [{folder.FullName}] [{message.Envelope.Subject}] [{attachment.FileName}]"
            Ok 0L
        onCompletion = fun (_, failed) ->
            List.length failed
        inspectNode = fun node ->
            eprintfn $"Inspecting folder: {node.FullName}"
            node
        inspectLeaf = fun leaf ->
            eprintfn $"Inspecting message: {leaf.Envelope.Subject}"
        identifyNode = fun node -> $"[folder {node.FullName}]"
        identifyLeaf = fun leaf -> $"[subject {leaf.Envelope.Subject}][date {leaf.Envelope.Date}]"
    }

    argv |> ET.main behaviour
