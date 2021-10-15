module ProcessAttachments.Execution.Program

open FSharpx.Collections
open FSharpx.Collections.Experimental

module T = RoseTree

let inline (||>) x f = x |> Result.bind f

let inline (|/>) x f = x |> Result.map f

let inline (|=|>) x f = x |> Result.map (T.map f)

let inline (>|>) f1 f2 = f1 >> T.map f2

let program initialise configuration =
    let parameters = initialise configuration
    let _, connect, init = parameters

    fun root ls (filter: 'a -> 'a * bool) openNode closeNode leaves content category nameContent tryExport onComplete ->
        let treeBuild n = (n, ls n |> LazyList.ofSeq)
        let maybeOpen (n, p) =
            let tryOpen, unopenedValue = openNode
            (
                n,
                if p then tryOpen n else unopenedValue
            )
        let listLeaves (n, state) =
            let tryList, isOpen = leaves
            (n, state, if isOpen state then tryList n else Seq.empty)
        let listContent (n, state, nLeaves) =
            if Seq.isEmpty nLeaves then
                (n, state, nLeaves, Seq.empty)
            else
                (n, state, nLeaves, nLeaves |> Seq.map (fun leaf -> (leaf, content n leaf)))

        // ugly
        use client =
            match parameters with
            | client0, _, _ -> client0

        connect client
        ||> (root init)
        |/> (
            T.unfold treeBuild
            >|> filter
            >|> maybeOpen
            >|> listLeaves)
//        |=|> filter
//        |=|> maybeOpen
//        |=|> listLeaves