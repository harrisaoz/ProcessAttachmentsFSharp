open System.IO
open MailKit
open MailKit.Net.Imap

open FSharp.Core.Extensions

open ProcessAttachments.DomainInterface
open ProcessAttachments.ImapKit.ImapService

module TC = TypedConfiguration
module ET = ProcessAttachments.Execution.Templates
module Folder = ProcessAttachments.ImapKit.ImapFolder
module Message = ProcessAttachments.ImapKit.ImapMessage
module Parts = ProcessAttachments.ImapKit.BodyParts
module Naming = ProcessAttachments.ImapKit.AttachmentNaming.InvoiceNaming
module FS = ProcessAttachments.FileSystem

type FakeStream(name: string) =
    member _.Create: Result<Stream option, string> =
        eprintfn $"Creating stream: {name}"
        new MemoryStream() :> Stream
        |> Some
        |> Ok

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
        initialise = fun (_, _, exportConfig) -> FS.assertFolder exportConfig.DestinationFolder
        session = fun (sessionParams, _, _) -> new ImapSession(sessionParams)
        container = fun session -> session.Open
        roots =
            // There is too much logic here
            // 1. List all children of the given container
            // 2. Filter to only those children meeting a rule based on name
            fun (_, mailboxParams, _) ->
               let filter sourceFolders (node: IMailFolder) =
                   sourceFolders
                   |> Seq.icontains node.Name
               Result.map (
                   fun container ->
                       Folder.listFoldersInNamespace personalNamespace container
                       |> Seq.filter (filter mailboxParams.SourceFolders)
                   )
        nodes = fun root -> Folder.dfsPre root |> Seq.take 1
        closeNode = Folder.closeFolder
        leaves = fun folder -> Folder.enumerateMessages None folder |> Result.map (Seq.take 3)
        contentItems = Message.dfsPre
        categorise = fun message ->
            Accept message // to do
        contentName = fun (folder, message, attachment) ->
            // To do: use FileNamers.FolderBasedName as the basis for implementation
            attachment.FileName
        exportContent = fun parent folder message attachment ->
            eprintfn $"Export attachment: [{folder.FullName}] [{message.Envelope.Subject}] [{attachment.FileName}]"
            let name = Naming.name folder message attachment
            let streamCopy = Message.tryCopyAttachmentToStream
            let absName = Path.Combine(parent.FullName, FS.sanitise name)
            let createStream name = FakeStream(name).Create

            FS.Export.writeContentToStream createStream streamCopy absName attachment
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
