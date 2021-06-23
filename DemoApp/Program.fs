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
        initialise = fun _ -> (new FakeSession(17), (fun (s: FakeSession) -> Ok s), ())
        roots = fun _ _ -> seq {0} |> Ok
        nodes = fun root ->
            let validate = fun x ->
                match x with
                | e0 when e0 = 2 -> Error (string e0)
                | x0 when 0 <= x0 && x0 <= 8 -> Ok x0
                | e -> Error (string e)
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
            | 1 -> seq {100; 101} |> Ok
            | 2 -> seq {200; 201; 202} |> Ok
            | 5 -> seq {500} |> Ok
            | 6 -> Error "broken listing"
            | 7 -> seq { 700 .. 701 } |> Ok
            | 8 -> seq { 800 .. 804 } |> Ok
            | 9 -> seq { 900 .. 902 } |> Ok
            | _ -> Seq.empty |> Ok
        contentItems = fun _ x ->
            match x with
            | small when small < 700 -> seq { small * 10; small * 10 + 1 }
            | big when big > 700 -> seq { big }
            | _ -> Seq.empty
            |> Seq.map Ok
        categorise = fun x ->
            match x with
            | even when even % 2 = 0 -> Accept x
            | multOf3 when multOf3 % 3 = 0 -> Ignore
            | _ -> Reject (string x)
        contentName = fun (n, l, c) -> $"[{string n}] Export content [{string l}, {string c}]"
        exportContent = fun _ name c ->
            eprintfn $"{name}"
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
