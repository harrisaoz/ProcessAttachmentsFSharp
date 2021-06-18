module ET = ProcessAttachments.Execution.Templates

open System

open FSharp.Core.Extensions

open ProcessAttachments.Collections
open ProcessAttachments.DomainInterface

type FakeSession(id: int) =
    member _.Open = id
    
    interface IDisposable with
        member this.Dispose() =
            ()

[<EntryPoint>]
let main argv =
    let behaviour = {
        defaultConfigFilename = "DemoApp.json"
        configuration = fun _ -> () |> Ok
        session = fun _ -> new FakeSession(17)
        container = fun _ ->
            ()|> Ok
        roots = fun _ _ -> seq {0} |> Ok
        nodes = fun root ->
            let validate = Ok
            let ls x =
                match x with
                | 0 -> seq {1;6;7}
                | 1 -> seq {2;3;4}
                | 4 -> seq {5}
                | 7 -> seq {8;9}
                | _ -> Seq.empty
                |> Ok
            Conversions.dfsPre validate ls root
        closeNode = Result.map (fun n ->
            eprintfn $"[{string n}] Closing node"
            eprintfn "---------------------"
            n)
        leaves = fun x ->
            match x with
            | 1 -> seq {100; 101}
            | 2 -> seq {200; 201; 202}
            | 5 -> seq {500}
            | 8 -> seq { 800 .. 804 }
            | _ -> Seq.empty
            |> Ok
        contentItems = fun x ->
            match x with
            | small when small < 200 -> seq { small * 10 }
            | big when big > 700 -> seq { big * 10 }
            | _ -> Seq.empty
        exportContent = fun n l c ->
            eprintfn $"[{string n}] Export content [{string l}, {string c}]"
            Ok (Convert.ToInt64 c)
        onCompletion = fun (_, failed) ->
            Seq.length failed
        inspectNode = fun n ->
            eprintfn $"[{string n}] node"
            n
        inspectLeaf = fun l ->
            eprintfn $"[{string l}] leaf"
        identifyNode = string
        identifyLeaf = string
    }
    argv |> ET.main behaviour
