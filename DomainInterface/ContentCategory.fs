namespace ProcessAttachments.DomainInterface

type Reject = string
type Ignore = string

type ContentCategory<'a, 'Error> =
    | Reject of 'Error
    | Ignore
    | Accept of 'a

module ContentCategory =
    let inline bind f c =
        match c with
        | Reject r -> Reject r
        | Ignore -> Ignore
        | Accept p -> f p

    let inline map f c = bind (fun p -> Accept (f p)) c

    let folder (acc: ContentCategory<'a seq, 'b>) (c: ContentCategory<'a, 'b>) =
        match c with
        | Reject r ->
            Reject r
        | Ignore ->
            acc
        | Accept p ->
            match acc with
            | Reject r0 ->
                Reject r0
            | Ignore ->
                Accept (Seq.singleton p)
            | Accept ps ->
                Accept (Seq.singleton p |> Seq.append ps)
