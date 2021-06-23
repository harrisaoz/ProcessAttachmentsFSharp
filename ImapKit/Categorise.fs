module ProcessAttachments.ImapKit.Categorise

open MimeKit
open ProcessAttachments.DomainInterface

let isA p xs maybeX =
    match maybeX with
    | Some x0 -> xs |> Seq.exists (p x0)
    | None -> false

let isContentType referenceTypes underTest =
    isA (
        fun (a: ContentType) (b: ContentType) -> a.IsMimeType(b.MediaType, b.MediaSubtype)
    ) referenceTypes underTest

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
        | true -> Accept mimePart
        | false -> Reject $"[{string maybeContentType} Missing or unsupported MIME type"
