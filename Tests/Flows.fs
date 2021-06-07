module Tests.Flows

open Xunit

module Conv = Collections.Conversions

let lsCore1 x =
    match x with
    | 0 -> seq { 1; 2; 3 }
    | _ -> Seq.empty

let lsCore2 x =
    match x with
    | 0 -> seq { 1; 6; 7 }
    | 1 -> seq { 2; 3 }
    | 3 -> seq { 4; 5 }
    | 7 -> seq { 8; 9 }
    | 9 -> seq { 10 }
    | 100 -> seq { 101; 107 }
    | 101 -> seq { 102; 106 }
    | 102 -> seq { 103; 104; 105 }
    | 107 -> seq { 108 }
    | 108 -> seq { 109; 110 }
    | _ -> Seq.empty

[<Fact>]
let ``Pipeline 1: single top-level item with only direct children`` () =
    let mutable pipelineValue = -1

    let client = 0
    let getTop client =
        match client with
        | 0 -> Seq.singleton 0
        | _ -> Seq.empty

    let enumerate = Conv.dfsPre Result.Ok (lsCore1 >> Result.Ok)
    let action =
        Result.map (
            fun x ->
                pipelineValue <- x
                Assert.True(0 <= x && x <= 3)
                x
        )
    let close =
        Result.map (
            fun x ->
                Assert.Equal(pipelineValue, x)
                x)
    let reportError = id
    let onComplete (l1, l2) =
        Assert.Equal(4, List.length l1)
        Assert.Equal(0, List.length l2)

    client
    |> Flows.DfsSequence.pipeline getTop enumerate action close reportError onComplete

[<Fact>]
let ``Pipeline 2: two top-level items each with multiple descendent generations`` () =
    let mutable pipelineValue = -1

    let client = 0
    let stubTop client =
        match client with
        | 0 -> seq {0; 100}
        | _ -> Seq.empty

    let validate x =
        match x with
        | x0 when 0 <= x0 && x0 <= 8 -> Result.Ok x0
        | x1 when 100 <= x1 && x1 <= 109 -> Result.Ok x1
        | xError -> Result.Error xError

    let enumerate = Conv.dfsPre validate (lsCore2 >> Result.Ok)
    let action =
        Result.map (
            fun x ->
                pipelineValue <- x
                x
        )
    let close =
        Result.map (
            fun x ->
                Assert.Equal(pipelineValue, x)
                x)
    let reportError = id
    let onComplete (l1, l2) =
        let s1 = Seq.ofList l1
        let s2 = Seq.ofList l2
        let s1values =
            s1
            |> Seq.map (
                fun r ->
                    match r with
                    | Result.Ok v -> v
                    | Result.Error _ -> -1
            )
        Assert.Equal([0; 1; 2; 3; 4; 5; 6; 7; 8; 100; 101; 102; 103; 104; 105; 106; 107; 108; 109], s1values)
        Assert.Contains(Result.Error 9, s2)
        Assert.Contains(Result.Error 110, s2)
        Assert.Equal(19, List.length l1)
        Assert.Equal(2, List.length l2)

    client
    |> Flows.DfsSequence.pipeline stubTop enumerate action close reportError onComplete
