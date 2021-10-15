module ProcessAttachments.Execution.Templates

open System
open Microsoft.Extensions.Configuration
open ProcessAttachments.DomainInterface

module CC = ContentCategory

let inspectRightSeq inspectValue (leftResult, rightResult) =
    match leftResult with
    | Ok _ ->
        rightResult |> Result.map (Seq.iter inspectValue)
    | _ -> () |> Ok
    |> ignore
    (leftResult, rightResult)

let exportLeafContent getContentItems categorise name export =
    let error data maybeNode maybeLeaf = Error (maybeNode, maybeLeaf, data)
    let foldResult acc r =
        match r with
        | Ok n -> acc |> Result.map (fun a -> a + n)
        | Error data -> Error data

    let exportResults node leaf =
        let contentCategory =
            getContentItems node leaf
            |> Seq.map (
                fun contentResult ->
                    match contentResult with
                    | Ok content ->
                        let item = (node, leaf, content)

                        categorise content
                        |> ContentCategory.map (name item |> export)
                    | Error data -> Reject data
                )
            |> CC.categoriseGroup
        match contentCategory with
        | Accept acceptedExportResults ->
            match (Seq.fold foldResult (Ok 0L) acceptedExportResults) with
            | Ok n -> Ok (node, leaf, n)
            | Error data -> error data (Some node) (Some leaf)
        | Ignore -> error "no content to export" (Some node) (Some leaf)
        | Reject r -> error (string r) (Some node) (Some leaf)

    fun (leftResult, rightResult) ->
        match leftResult with
        | Ok node ->
            let newRight =
                match rightResult with
                | Ok leaves ->
                    Seq.map (exportResults node) leaves
                | Error data -> Seq.singleton <| error data (Some node) None
            (leftResult, newRight)
        | _ -> (leftResult, Seq.empty)
        |> (fun (l,r) -> (l, r |> List.ofSeq))

let partitionResults results =
    results
    |> List.fold (
        fun (ok, noop, err) r ->
            match r with
            | Ok (node, leaf, 0L) ->
                (ok, List.append noop [(node, leaf, 0L)], err)
            | Ok (node, leaf, n) ->
                (List.append ok [(node, leaf, n)], noop, err)
            | Error (maybeNode, maybeLeaf, data) ->
                (ok, noop, List.append err [(maybeNode, maybeLeaf, data)])
        ) (List.empty, List.empty, List.empty)

let program b configuration =
    0
//    let parameters = b.initialise configuration
//    let _, connect, init = parameters
//    use client =
//        match parameters with
//        | client0, _, _ -> client0
//
//    let roots = b.roots init
//    let export = b.exportContent init
//    
//    let exportContent = exportLeafContent b.contentItems b.categorise b.contentName export
//
//    connect client
//    |> Result.map roots
//    |> RSeq.collectBoundTransform b.nodes
//    |> Seq.map (Result.map b.inspectNode)
//    |> Seq.map (RSeq.pairResultWithDirect (Error "empty") b.leaves)
//    |> Seq.map (inspectRightSeq b.inspectLeaf)
//    |> Seq.map exportContent
//    |> Seq.map (fun (l,r) -> (b.closeNode l, r))
//    |> Seq.map snd
//    |> List.concat
//    |> partitionResults
//    |> b.onCompletion

let handleResult (r: Result<_, _>) =
    match r with
    | Ok _ ->
        printfn $"All good"
        0
    | Error data ->
        printfn $"Something went wrong [{string data}]"
        1

let main (behaviour: Behaviour<_, _, _, _, _, _, _, _>) argv =

    let runFromConfigurationFile configFile =
        Configuration.Load.fromJsonFile configFile
        |> Result.bind behaviour.configuration
        |> Result.map (program behaviour)
        |> handleResult

    printfn "Running..."

    match (List.ofArray argv) with
    | [] -> behaviour.defaultConfigFilename
    | filename :: _ -> filename
    |> runFromConfigurationFile

type RunAppConfiguredFromFile<'r, 'p> =
    ('p -> Result<'r,string>) -> (IConfiguration -> Result<'p, string>) -> string -> string array
     -> int

let runAppConfiguredFromFile: RunAppConfiguredFromFile<_, _> =
    fun run parametersFromConfiguration defaultNameOfConfigurationFile argv ->
        match (List.ofArray argv) with
        | [] -> defaultNameOfConfigurationFile
        | filename :: _ -> filename
        |> Configuration.Load.fromJsonFile
        |> Result.bind parametersFromConfiguration
        |> Result.bind run
        |> handleResult