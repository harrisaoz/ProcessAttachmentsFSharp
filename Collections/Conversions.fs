module ProcessAttachments.Collections.Conversions

open FSharpx.Collections

module L = LazyList
module R = Experimental.RoseTree

let rec asRoseTree ls item =
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

let dfs orderedDfs validate rawLs =
    let ls item =
        match item with
        | Ok item ->
            match rawLs item with
            | Ok xs -> xs |> Seq.map validate
            | Error _ -> Seq.empty
        | Error _ -> Seq.empty

    validate >> asRoseTree ls >> orderedDfs

let dfsPre validate = dfs R.dfsPre validate
let dfsPost validate = dfs R.dfsPost validate

let rec lsAll' (ls: 'a -> 'a list) (xs: 'a list) =
    match xs with
    | [] -> []
    | [x] -> lsAll' ls (ls x) |> List.append [x]
    | x :: tail -> lsAll' ls tail |> List.append (lsAll' ls [x])

