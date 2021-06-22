module ProcessAttachments.Execution.Templates

open ProcessAttachments.Collections.Extensions
open ProcessAttachments.DomainInterface

let inspectRightSeq inspectValue (leftResult, rightResult) =
    match leftResult with
    | Ok _ ->
        rightResult |> Result.map (Seq.iter inspectValue)
    | _ -> () |> Ok
    |> ignore
    (leftResult, rightResult)

let exportLeafContent identifyNode identifyLeaf getContentItems categorise export =
    let foldResult acc r =
        match r with
        | Ok n -> acc |> Result.map (fun a -> a + n)
        | Error data -> Error data

    let foldCat (acc: ContentCategory<'a seq, 'b>) (c: ContentCategory<'a, 'b>) =
        match c with
        | Reject r ->
            Reject r
        | Ignore ->
            acc
        | Process p ->
            match acc with
            | Reject r0 ->
                Reject r0
            | Ignore ->
                Process (Seq.singleton p)
            | Process ps ->
                Process (Seq.singleton p |> Seq.append ps)
        
    let exportResults node leaf =
        let contentCategory =
            getContentItems leaf
            |> Seq.map (
                fun content ->
                    ContentCategory.map (export node leaf) (categorise content)
                )
            |> Seq.fold foldCat Ignore
        match contentCategory with
        | Reject r -> Error $"[{identifyNode node} | {identifyLeaf leaf}] {string r}"
        | Ignore -> Error $"[{identifyNode node} | {identifyLeaf leaf}] no content to export"
        | Process exportResults ->
            match (Seq.fold foldResult (Ok 0L) exportResults) with
            | Ok n -> Ok (node, leaf, n)
            | Error data -> Error $"[{identifyNode node} | {identifyLeaf leaf}] {string data}"

    fun (leftResult, rightResult) ->
        match leftResult with
        | Ok node ->
            let newRight =
                match rightResult with
                | Ok leaves ->
                    Seq.map (exportResults node) leaves
                | Error data -> Seq.singleton <| Error data
            (leftResult, newRight)
        | _ -> (leftResult, Seq.empty)
        |> (fun (l,r) -> (l, r |> List.ofSeq))

let partitionResults identifyNode identifyLeaf results =
    results
    |> List.fold (
        fun (ok, noop, err) r ->
            match r with
            | Ok (node, leaf, 0L) ->
                eprintfn $"= {identifyNode node} {identifyLeaf leaf} [size 0]"
                (ok, List.append noop [(node, leaf, 0L)], err)
            | Ok (node, leaf, n) ->
                eprintfn $"+ {identifyNode node} {identifyLeaf leaf} [size {n}]"
                (List.append ok [(node, leaf, n)], noop, err)
            | Error data ->
                eprintfn $"- {string data}"
                (ok, noop, List.append err [Error data])
        ) (List.empty, List.empty, List.empty)

let program b configuration =
    use session = b.session configuration
    let roots = b.roots configuration
    let export = b.exportContent configuration
    
    let exportContent =
        exportLeafContent b.identifyNode b.identifyLeaf b.contentItems b.categorise export

    b.container session
    |> roots
    |> RSeq.collectBoundTransform b.nodes
    |> Seq.map (Result.map b.inspectNode)
    |> Seq.map (RSeq.pairResultWithDirect (Error "empty") b.leaves)
    |> Seq.map (inspectRightSeq b.inspectLeaf)
    |> Seq.map exportContent
    |> Seq.map (fun (l,r) -> (b.closeNode l, r))
    |> Seq.map snd
    |> List.concat
    |> (partitionResults b.identifyNode b.identifyLeaf)
    |> (fun (ok, _, err) -> b.onCompletion (ok, err))
    |> Ok

let main (behaviour: Behaviour<_, _, _, _, _, _, _, _>) argv =
    let handleResult (r: Result<_, _>) =
        match r with
        | Result.Ok n ->
            printfn $"All good [{n}]"
            0
        | Result.Error data ->
            printfn $"Something went wrong [{string data}]"
            1

    let runFromConfigurationFile configFile =
        Configuration.Load.fromJsonFile configFile
        |> Result.bind behaviour.configuration
        |> Result.bind (program behaviour)
        |> handleResult

    printfn "Running..."

    match (List.ofArray argv) with
    | [] -> behaviour.defaultConfigFilename
    | filename :: _ -> filename
    |> runFromConfigurationFile
