module Collections.Conversions

open FSharpx.Collections

module L = LazyList
module R = Experimental.RoseTree

let asLazyLs (ls: Result<'a, string> -> Result<seq<'a>, string>) (validate: 'a -> Result<'a, string>) (parent: Result<'a, string>) =
    let children = ls parent
    match children with
    | Result.Ok xs -> xs |> Seq.map validate
    | Result.Error msg -> Result.Error msg |> Seq.singleton
    |> L.ofSeq
            

let rec asRoseTree =
    fun (ls1: Result<'a, 'b> -> LazyList<Result<'a, 'b>>) (a: Result<'a, 'b>) ->
        match a with
        | Result.Ok _ ->
            let children = ls1 a
            if (Seq.isEmpty children) then
                R.singleton a
            else
                let subTree = children |> L.map (asRoseTree ls1)
                R.create a subTree
        | Result.Error e ->
            Result.Error e |> R.singleton
