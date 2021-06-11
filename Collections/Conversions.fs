module Collections.Conversions

open FSharpx.Collections

module L = LazyList
module R = Experimental.RoseTree

let rec asRoseTree (ls: 'a -> seq<'a>) (item: 'a) =
    let children = ls item
    if (Seq.isEmpty children) then
        R.singleton item
    else
        children
        |> Seq.map (asRoseTree ls)
        |> L.ofSeq
        |> R.create item

let simpleDfsPre ls =
    asRoseTree ls >> R.dfsPre

let dfsPre (validate: 'a -> Result<'a, 'b>) (rawLs: 'a -> Result<'a seq, 'b>) =
    let ls item =
        match item with
        | Result.Ok item ->
            match rawLs item with
            | Result.Ok xs -> xs |> Seq.map validate
            | Result.Error _ -> Seq.empty
        | Result.Error _ -> Seq.empty

    validate >> asRoseTree ls >> R.dfsPre
