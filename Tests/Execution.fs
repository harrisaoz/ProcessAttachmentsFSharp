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
    let mutable completedExports = Seq.empty
    let mutable incompleteExports = Seq.empty

    let behaviour = {
        defaultConfigFilename = "Config1.json"
        configuration = fun _ -> () |> Ok
        initialise =
            let connectSession (s: FakeSession) =
                sessionId <- s.Open
                Ok s
            fun _ ->
                (
                    new FakeSession(17),
                    connectSession,
                    ()
                )
        roots = fun _ _ -> seq {0}
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
        contentItems = fun _ x ->
            match x with
            | small when small < 700 -> seq { small * 10; small * 10 + 1 }
            | big when big > 700 -> seq { big }
            | _ -> Seq.empty
            |> Seq.map Ok
        categorise = fun x ->
            match x with
            | even when even % 2 = 0 -> Accept x
            | divisibleBy3 when divisibleBy3 % 3 = 0 -> Ignore
            | _ -> Reject (string x)
        inspectNode = id
        inspectLeaf = fun l ->
            leafInspectionCount <- leafInspectionCount + 1
            leafInspectionSequence <- Seq.cons l leafInspectionSequence
        closeNode = id
        contentName = fun (_,_,c) -> string c
        exportContent = fun _ _ c ->
            Ok (Convert.ToInt64 c)
        onCompletion = fun (ok, failed) ->
            completedExports <- ok
            incompleteExports <- failed
            List.length failed
        identifyNode = string
        identifyLeaf = string
    }

    let n = main behaviour Array.empty
    Assert.Equal(17, sessionId)
    Assert.Equal(0, n)
    Assert.Equal(10, leafInspectionCount)
    Assert.Equal([100;101;500;700;701;800;801;802;803;804], Seq.rev leafInspectionSequence)
    let expectedCompleteLeaves =
        [
            (1, 101, 1010L)
            (5, 500, 5000L)
            (8, 800, 800L)
            (8, 802, 802L)
            (8, 804, 804L)
        ]
    Assert.Equal(expectedCompleteLeaves, completedExports)
