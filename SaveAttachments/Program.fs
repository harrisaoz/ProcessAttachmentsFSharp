open System.IO
open MailKit
open MailKit.Net.Imap
open MimeKit

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
module Export = ProcessAttachments.FileSystem.Export

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
        nodes =
            fun root ->
                Folder.dfsPre root
        closeNode = Folder.closeFolder
        leaves =
            fun folder ->
                Folder.enumerateMessages None folder
        contentItems = Message.dfsPre
        categorise =
            let allowedMimeTypes = seq {
                ContentType("application", "pdf")
                ContentType("application", "octet-stream")
            }
            let ignoredMimeTypes = seq {
                ContentType("text", "html")
                ContentType("text", "plain")
                ContentType("image", "png")
                ContentType("application", "pkcs7-signature")
            }
            let isXMimeType (mimeTypes: ContentType seq) (contentType: ContentType) =
                mimeTypes
                |> Seq.exists (
                    fun ct ->
                        contentType.IsMimeType(ct.MediaType, ct.MediaSubtype)
                    )

            let isAllowedMimeType = isXMimeType allowedMimeTypes
            let isIgnoredMimeType = isXMimeType ignoredMimeTypes

            let shouldIgnore (maybeFilename: string option) =
                match maybeFilename with
                | Some filename ->
                    filename.Contains("detalhe")
                    || filename.EndsWith(".zip")
                    || filename.Contains("NEWSLETTER")
                    || filename.Contains("FUNDO GARANTIA")
                | None -> false

            fun mimePart ->
                match (mimePart.ContentType |> Option.ofObj, mimePart.FileName) with
                | _, filename when (shouldIgnore <| Option.ofObj filename) ->
                    Ignore
                | Some contentType, _ when (isIgnoredMimeType contentType) ->
                    Ignore
                | Some contentType, _ when (isAllowedMimeType contentType) ->
                    Accept mimePart
                | _ ->
                    Reject $"[{mimePart.ContentType}] Missing or unsupported mime type"
        contentName = fun (folder, message, attachment) -> Naming.name folder message attachment
        exportContent =
            let createStream = Export.tryCreateFile
            let streamCopy = Message.tryCopyAttachmentToStream
            fun parent name ->
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
