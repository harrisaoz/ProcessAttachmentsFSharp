module ProcessAttachments.ImapKit.Categorise

open FSharpx.Collections
open MimeKit
open FsCombinators.ExtraTypes
module RSeq = FsCombinators.SeqExtensions

[<Literal>]
let PDF_Extension = ".pdf"
[<Literal>]
let CSV_Extension = ".csv"

type FileExtension = string
// Hacks to deal with non-standards-compliant systems feeding junk into email attachments.
let standardiseContentType: FileExtension option -> ContentType -> ContentType =
    fun maybeFileExtension mimeType ->
        let mediaType, subType = (mimeType.MediaType, mimeType.MediaSubtype)

        match (maybeFileExtension, mediaType, subType) with
        | Some PDF_Extension, "text", "html" -> ContentType("application", "pdf")
        | Some CSV_Extension, "text", "html" -> ContentType("text", "comma-separated-values")
        | Some CSV_Extension, "application", "octet-stream" -> ContentType("text", "comma-separated-values")
        | _ -> mimeType

let contentTypesEqual: ContentType -> ContentType -> bool =
    fun a b -> a.IsMimeType(b.MediaType, b.MediaSubtype)

let isContentType: ContentType seq -> ContentType option -> bool =
    RSeq.maybeExistsWhere contentTypesEqual

let tryGetFilename (attachment: MimePart) =
    attachment.FileName
    |> Option.ofObj

let tryGetFileExtension (attachment: MimePart) =
    System.IO.Path.GetExtension(attachment.FileName)
    |> Option.ofObj

let tryGetContentType (attachment: MimePart) =
    attachment.ContentType
    |> Option.ofObj
    |> Option.map (standardiseContentType (tryGetFileExtension attachment))

let partInfo (part: MimePart) =
    [
        $"Type = {string part.ContentType.MimeType}"
        $"Filename = \"{string part.FileName}\""
    ]

let categoriseAttachment (onOk, onIgnore, onError) ignoreAtt typeAccept (attachment: MimePart) =
    let maybeFilename, maybeContentType = (
        tryGetFilename attachment, tryGetContentType attachment)
    match (ignoreAtt maybeFilename maybeContentType) with
    | true ->
        onIgnore attachment
        Ignore
    | false ->
        match (isContentType typeAccept maybeContentType) with
        | true ->
            onOk attachment
            Ok attachment
        | false ->
            onError attachment
            Error $"Missing or unsupported MIME type [Type = {string maybeContentType}]"

module L = LazyList
module IrExt = ProcessAttachments.CommonExtensions.IgnorableResultExtensions

let categoriseAttachments boundActions ignoreAtt accept =
    L.map (
        IgnorableResult.ofResult
        >> IgnorableResult.bind (categoriseAttachment boundActions ignoreAtt accept))
    >> IrExt.groupResult
