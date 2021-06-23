open System.IO
open MailKit.Net.Imap
open MimeKit

open FSharp.Core.Extensions

open ProcessAttachments.DomainInterface

module TC = TypedConfiguration
module ET = ProcessAttachments.Execution.Templates
module Folder = ProcessAttachments.ImapKit.ImapFolder
module Message = ProcessAttachments.ImapKit.ImapMessage
module Parts = ProcessAttachments.ImapKit.BodyParts
module Naming = ProcessAttachments.ImapKit.AttachmentNaming.InvoiceNaming
module FS = ProcessAttachments.FileSystem
module Export = ProcessAttachments.FileSystem.Export
module Service = ProcessAttachments.ImapKit.ImapService
module CC = ProcessAttachments.ImapKit.Categorise

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
        initialise =
            fun (sessionParams, mailboxParams, exportConfig) ->
                (
                    Service.initialize,
                    Service.connect sessionParams,
                    (
                        FS.assertFolder exportConfig.DestinationFolder,
                        mailboxParams.SourceFolders
                    )
                )
        roots =
            fun (_, sourceFolders) ->
               Folder.selectFoldersInNamespace personalNamespace (
                   fun f -> sourceFolders |> Seq.icontains f.Name
               )
        nodes = Folder.dfsPre
        closeNode = Folder.closeFolder
        leaves = Folder.enumerateMessages None
        contentItems = Message.dfsPre
        categorise =
            // To do: leave all of this to run-time configuration:
            // - accepted MIME types
            // - ignored MIME types
            // - ignored filenames
            let acceptedMimeTypes =
                seq {
                    ContentType("application", "pdf")
                    ContentType("application", "octet-stream")
                }
            let ignoredMimeTypes =
                seq {
                    ContentType("text", "html")
                    ContentType("text", "plain")
                    ContentType("image", "png")
                    ContentType("application", "pkcs7-signature")
                }

            let ignoredFilenames _ (filename: string) =
                filename.Contains("detalhe")
                || filename.EndsWith(".zip")
                || filename.Contains("NEWSLETTER")
                || filename.Contains("FUNDO GARANTIA")
            let ignoredContentTypes _ = CC.isContentType ignoredMimeTypes

            CC.categorise ignoredFilenames ignoredContentTypes acceptedMimeTypes
        contentName = fun (folder, message, attachment) -> Naming.name folder message attachment
        exportContent =
            let createStream = Export.tryCreateFile
            let streamCopy = Message.tryCopyAttachmentToStream
            fun (parent, _) name ->
                let absName = Path.Combine(parent.FullName, FS.sanitise name)
                FS.Export.writeContentToStream createStream streamCopy absName
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
