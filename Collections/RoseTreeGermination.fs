module Collections.RoseTreeGermination

open FSharpx.Collections.Experimental

module L = FSharpx.Collections.LazyList

module ResultBoxed =
    let listChildrenWithParent (validate: 'a -> Result<'a, string>) (ls: 'a -> Result<'a seq, string>) =
        fun (nodeResult: Result<'a, string>) ->
            let children =
                match nodeResult with
                | Result.Ok node ->
                    match (ls node) with
                    | Result.Ok children -> L.ofSeq children |> L.map validate
                    | Result.Error msg -> L.ofList [Result.Error msg]
                | Result.Error msg -> L.ofList [Result.Error msg]
            (nodeResult, children)

    let grow validate ls top =
        RoseTree.unfold (listChildrenWithParent validate ls) (validate top)