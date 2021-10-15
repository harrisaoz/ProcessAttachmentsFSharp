namespace ProcessAttachments.DomainInterface

open Combinators.Standard

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

    let inline map f = bind (f >> Accept)

    let inline bind2 v f c =
        match c with
        | Accept x -> f x
        | Ignore -> v
        | Reject r -> Reject r

//    let folder acc category =
//        let f x =
//            let f' xs = Accept (Seq.singleton x |> Seq.append xs)
//
//            acc
//            |> bind2 f' (Seq.singleton x |> Accept)
//
//        bind2 f acc category

    // Ignore if all are Ignore (including empty sequence)
    // Reject (whichever Reject is encountered first) if any are Reject
    // otherwise, Accept (all items which are Accept)
    let categoriseGroup (xs: ContentCategory<'a, 'b> seq): ContentCategory<'a seq, 'b> =
        let folder' (acc: ContentCategory<'a seq, 'b>) (c: ContentCategory<'a, 'b>) =
            match c with
            | Ignore ->
                acc
            | Reject cRejectMessage ->
                Reject cRejectMessage
            | Accept p ->
                match acc with
                | Ignore -> Accept (Seq.singleton p)
                | Reject accRejectMessage -> Reject accRejectMessage
                | Accept ps -> Accept (Seq.singleton p |> Seq.append ps)
        let folder acc =
            let accBind x =
                bind2
                    ((Seq.singleton >> Accept) x)
                    ((Seq.singleton >> C Seq.append >> C Q Accept) x)
            
//            bind2
//                acc
//                (fun x ->
//                   bind2
//                       ((Seq.singleton >> Accept) x)
//                       ((Seq.singleton >> C Seq.append >> C Q Accept) x)
//                       acc)
            S bind2 (C accBind) acc

        Seq.fold folder Ignore xs
