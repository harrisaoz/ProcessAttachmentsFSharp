module Tests.RoseTreeGerminationSpec

open Xunit

open FSharpx.Collections
module L = LazyList

open Collections.RoseTreeGermination.ResultBoxed

[<Fact>]
let ``The first element of listChildrenWithParent should be the input Result`` () =
    let inputResults = [
        Result.Ok 1
        Result.Error "error"
    ]
    let ls _ = Result.Error "unused"
    let validate _ = Result.Error "unused"
    
    inputResults |> List.iter (fun inputResult ->
        Assert.Equal(inputResult, fst (listChildrenWithParent ls validate inputResult)))

[<Fact>]
let ``The second element of listChildrenWithParent of a valid parent should be the child nodes (valid children)`` () =
    let inputNode: Result<int, string> = Result.Ok 0
    let ls _ = Result.Ok (seq {1;2})
    let validate = Result.Ok
    
    let expected: LazyList<Result<int, string>> = L.ofSeq (seq {1;2}) |> L.map Result.Ok
    let actual = snd (listChildrenWithParent validate ls inputNode)
    
    Assert.Equal(L.toList expected, L.toSeq actual)

[<Fact>]
let ``The second element of listChildrenWithParent of a valid parent should be a list of Error results (invalid children)`` () =
    let inputNode: Result<int, string> = Result.Ok 0
    let s = Seq.init 7 id
    let ls _ = Result.Ok s
    let validate k = Result.Error $"invalid child {k}"
    
    let expected: LazyList<Result<int, string>> =
        L.ofSeq s |> L.map (fun v -> Result.Error $"invalid child {v}")
    let actual =
        snd (listChildrenWithParent validate ls inputNode)
    
    Assert.Equal(L.toList expected, actual)

[<Fact>]
let ``The second element of listChildrenWithParent should be a singleton list of Error Result when the child listing fails`` () =
    let nodeResult = Result.Ok 1
    let msg = "ls failed; corrupt structure"
    let ls _ = Result.Error msg
    let validate = Result.Ok
    
    let expected = L.ofList [Result.Error msg]
    let actual = snd (listChildrenWithParent validate ls nodeResult)
    
    Assert.Equal(L.toList expected, actual)
