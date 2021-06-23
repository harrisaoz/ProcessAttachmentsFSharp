namespace ProcessAttachments.DomainInterface

type Reject = string
type Ignore = string

type ContentCategory<'a, 'Error> =
    | Accept of 'a
    | Ignore
    | Reject of 'Error

module ContentCategory =
    let inline bind f c =
        match c with
        | Accept p -> f p
        | Ignore -> Ignore
        | Reject r -> Reject r

    let inline map f c = bind (fun p -> Accept (f p)) c

//    let inline bind2 f v category =
//        match category with
//        | Accept x -> f x
//        | Ignore -> v
//        | Reject r -> Reject r
//
//    let folder acc category =
//        let f x =
//            let f' xs = Accept (Seq.singleton x |> Seq.append xs)
//
//            acc
//            |> bind2 f' (Seq.singleton x |> Accept)
//
//        bind2 f acc category

    let folder (acc: ContentCategory<'a seq, 'b>) (c: ContentCategory<'a, 'b>) =
        match c with
        | Accept p ->
            match acc with
            | Accept ps ->
                Accept (Seq.singleton p |> Seq.append ps)
            | Ignore ->
                Accept (Seq.singleton p)
            | Reject r0 ->
                Reject r0
        | Ignore ->
            acc
        | Reject r ->
            Reject r
