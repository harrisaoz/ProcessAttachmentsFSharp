module ImapKit.Defaults

open MimeKit

open MailKit
open MailKit.Net.Imap
open MailKit.Search

type Action =
    Accept
    | Ignore of string
    | Warn of string

type Behaviour =
    {
        standardConfigFile: string
        folderNamespace: ImapClient -> FolderNamespace
        query: SearchQuery option
        accept: MimePart -> Action
    }

let basicDownloader: Behaviour =
    let contentTypes = seq {
        ContentType("application", "pdf")
        ContentType("application", "octet-stream")
    }
    {
        standardConfigFile = "ProcessAttachments.json"
        folderNamespace = fun client -> client.PersonalNamespaces.Item(0)
        query = None
        accept = fun mimePart ->
            if mimePart.FileName.Contains("detalhe") || mimePart.FileName.EndsWith(".zip") then
                Ignore $"Ignoring attachment {mimePart.FileName}"
            elif contentTypes |> Seq.contains mimePart.ContentType then
                Accept
            else
                Warn $"Attachment has unacceptable MIME type [{mimePart.ContentType.Name}]"
    }
