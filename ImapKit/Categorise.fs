module ProcessAttachments.ImapKit.Categorise

open MimeKit
open Combinators

let contentTypesEqual: ContentType -> ContentType -> bool =
    fun a b -> a.IsMimeType(b.MediaType, b.MediaSubtype)

let isContentType: ContentType seq -> ContentType option -> bool =
    Seq.maybeExistsWhere contentTypesEqual

let categorise filenameTest contentTypeTest acceptable (mimePart: MimePart) =
    let maybeContentType, maybeFilename =
        (mimePart.ContentType |> Option.ofObj, mimePart.FileName |> Option.ofObj)

    let shouldIgnore filenameTest contentTypeTest (maybeContentType: ContentType option) (maybeFilename: string option) =
        match maybeContentType, maybeFilename with
        | None, None -> false
        | None, Some filename -> filenameTest None filename
        | Some _, None -> contentTypeTest None maybeContentType
        | Some _, Some filename ->
            filenameTest maybeContentType filename
            || contentTypeTest maybeFilename maybeContentType

    match shouldIgnore filenameTest contentTypeTest maybeContentType maybeFilename with
    | true -> Ignore
    | false ->
        match isContentType acceptable maybeContentType with
        | true -> Ok mimePart
        | false -> Error $"[{string maybeContentType} Missing or unsupported MIME type"
