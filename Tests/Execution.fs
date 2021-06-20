module Tests.Execution

open System
open Xunit

open FSharpx.Collections

open ProcessAttachments.Collections
open ProcessAttachments.Execution.Templates
open ProcessAttachments.DomainInterface

type FakeSession(id: int) =
    member _.Open = id
    
    interface IDisposable with
        member this.Dispose() =
            ()

[<Fact>]
let ``Execution flow: example 1`` () =
    let mutable leafInspectionCount = 0
    let mutable sessionId = 0
    let mutable leafInspectionSequence = Seq.empty
    let mutable okExports = Seq.empty
    let mutable failedExports = Seq.empty

    let behaviour = {
        defaultConfigFilename = "Config1.json"
        configuration = fun _ -> () |> Ok
        session = fun _ -> new FakeSession(17)
        container = fun s ->
            sessionId <- s.Open
            ()|> Ok
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
                | 9 -> seq {10}
                | _ -> Seq.empty
                |> Ok
            Conversions.dfsPre validate ls root
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
        contentItems = fun x ->
            match x with
            | small when small < 200 -> seq { small * 10 }
            | big when big > 700 -> seq { big * 10 }
            | _ -> Seq.empty
        inspectNode = id
        inspectLeaf = fun l ->
            leafInspectionCount <- leafInspectionCount + 1
            leafInspectionSequence <- Seq.cons l leafInspectionSequence
        closeNode = id
        exportContent = fun n l c ->
            eprintfn $"[{string n}] Export content [{string l}, {string c}]"
            Ok (Convert.ToInt64 c)
        onCompletion = fun (ok, failed) ->
            okExports <- ok
            failedExports <- failed
            Seq.length failed
        identifyNode = string
        identifyLeaf = string
    }

    let n = main behaviour Array.empty
    Assert.Equal(17, sessionId)
    Assert.Equal(0, n)
    Assert.Equal(10, leafInspectionCount)
    Assert.Equal([100;101;500;700;701;800;801;802;803;804], Seq.rev leafInspectionSequence)
    let expectedExportResults =
        [
            (1,100,1000L);
            (1,101,1010L);
            (7,701,7010L);
            (8,800,8000L);
            (8,801,8010L);
            (8,802,8020L);
            (8,803,8030L);
            (8,804,8040L)
        ]
    Assert.Equal(expectedExportResults, okExports)
