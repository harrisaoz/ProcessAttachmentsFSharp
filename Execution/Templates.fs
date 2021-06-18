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

let exportAttachments identifyNode identifyLeaf getContentItems export =
    let folder acc r =
        match r with
        | Ok n -> acc |> Result.map (fun a -> a + n)
        | Error data -> Error data

    let exportResults node leaf =
        let result =
            getContentItems leaf
            |> Seq.map (
                fun content ->
                    export node leaf content)
            |> Seq.fold folder (Ok 0L)
        match result with
        | Ok n -> Ok (node, leaf, n)
        | Error data -> Error $"[{identifyNode node} | {identifyLeaf leaf}] {data}"

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

let partitionResults behaviour results =
    results
    |> List.fold (
        fun (ok, noop, err) r ->
            match r with
            | Ok (node, leaf, 0L) ->
                eprintfn $"= {behaviour.identifyNode node} {behaviour.identifyLeaf leaf} [size 0]"
                (ok, List.append noop [(node, leaf, 0L)], err)
            | Ok (node, leaf, n) ->
                eprintfn $"+ {behaviour.identifyNode node} {behaviour.identifyLeaf leaf} [size {n}]"
                (List.append ok [(node, leaf, n)], noop, err)
            | Error data ->
                eprintfn $"- {string data}"
                (ok, noop, List.append err [Error data])
        ) (List.empty, List.empty, List.empty)

let program behaviour configuration =
    use session = behaviour.session configuration
    behaviour.container session
    |> behaviour.roots configuration
    |> RSeq.collectBoundTransform behaviour.nodes
    |> Seq.map (Result.map behaviour.inspectNode)
    |> Seq.map (RSeq.pairResultWithDirect (Error "empty") behaviour.leaves)
    |> Seq.map (inspectRightSeq behaviour.inspectLeaf)
    |> Seq.map (exportAttachments behaviour.identifyNode behaviour.identifyLeaf behaviour.contentItems behaviour.exportContent)
    |> Seq.map (fun (l,r) -> (behaviour.closeNode l, r))
    |> Seq.map snd
    |> List.concat
    |> partitionResults behaviour
    |> (fun (ok, _, err) -> behaviour.onCompletion (ok, err))
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
