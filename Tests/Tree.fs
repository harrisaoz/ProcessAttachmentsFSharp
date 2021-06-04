module Tests.Tree

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
    | _ -> Seq.empty

[<Fact>]
let ``dfsPre 1: error at root`` () =
    let errorMsg = "invalid root"
    let ls _ = Seq.empty |> Result.Ok
    let validate _ = Result.Error errorMsg

    let actual =
        0 |> Conv.dfsPre validate ls |> Seq.toList

    Assert.Equal(Result.Error errorMsg, List.head actual)
    Assert.Equal(1, List.length actual)

[<Fact>]
let ``dfsPre 2: childless root`` () =
    let ls _ = Seq.empty |> Result.Ok
    let validate = Result.Ok

    let actual =
        0 |> Conv.dfsPre validate ls |> Seq.toList

    Assert.Equal(Result.Ok 0, List.head actual)
    Assert.Equal(1, List.length actual)

let wrapLs ls item =
    match item with
    | Result.Ok x -> ls x
    | _ -> Seq.empty

[<Fact>]
let ``dfsPre 3: ok children`` () =
    let ls = lsCore1 >> Result.Ok
    let validate = Result.Ok

    let actual =
        0 |> Conv.dfsPre validate ls |> Seq.toList

    Assert.Equal(4, List.length actual)
    Assert.Equal(Result.Ok 0, actual.Item(0))
    Assert.Equal(Result.Ok 1, actual.Item(1))
    Assert.Equal(Result.Ok 2, actual.Item(2))
    Assert.Equal(Result.Ok 3, actual.Item(3))

[<Fact>]
let ``dfsPre 4: error children`` () =
    let errorMsg = "invalid child"
    let ls = lsCore1 >> Result.Ok

    let validate x =
        match x with
        | 0 -> Result.Ok 0
        | _ -> Result.Error errorMsg

    //  fixme! ls should use lsCore1, validate should fail on all but 0
    let actual =
        0 |> Conv.dfsPre validate ls |> Seq.toList

    Assert.Equal(4, List.length actual)
    Assert.Equal(Result.Ok 0, actual.Item(0))
    Assert.Equal(Result.Error errorMsg, actual.Item(1))
    Assert.Equal(Result.Error errorMsg, actual.Item(2))
    Assert.Equal(Result.Error errorMsg, actual.Item(3))

[<Fact>]
let ``dfsPre 5: ok tree`` () =
    let ls = lsCore2 >> Result.Ok
    let validate = Result.Ok

    let actual =
        0 |> Conv.dfsPre validate ls |> Seq.toList

    Assert.Equal(11, List.length actual)

[<Fact>]
let ``dfsPre 6: ok tree - non-root nodes should only be visited on demand`` () =
    let mutable visitCount = 0

    let ls item =
        match item with
        | 0 -> lsCore2 0
        | x ->
            visitCount <- visitCount + 1
            lsCore2 x
        |> Result.Ok

    let validate = Result.Ok

    let extractOk r =
        match r with
        | Result.Ok x -> x
        | Result.Error msg -> failwith $"unexpected Error [{msg}]"

    let actual =
        0 |> Conv.dfsPre validate ls |> Seq.map extractOk

    Assert.Equal(0, visitCount)
    Assert.Equal([ 0 .. 10 ], actual)
    Assert.Equal(10, visitCount)
    Assert.Equal([ 0 .. 10 ], actual)
    Assert.Equal(10, visitCount)
