module Tests.TernaryResultSpec

open Xunit
open FSharpx.Collections
open Combinators

module L = LazyList

[<Fact>]
let ``groupResult: rule 1 - all ok should be ok`` () =
    let okValues = [1; 2] |> L.ofList
    let ungrouped = okValues |> L.map TernaryResult.Ok

    let result = TernaryResult.groupResult ungrouped
    Assert.True(TernaryResult.isOk result)

[<Fact>]
let ``groupResult: rule 2 - all Ignore should be Ignored`` () =
    let ignoreValues = [0; 1] |> L.ofList
    let ungrouped = ignoreValues |> L.map (fun _ -> TernaryResult.Ignore)

    let result = TernaryResult.groupResult ungrouped
    Assert.True(TernaryResult.shouldIgnore result)

[<Fact>]
let ``groupResult: rule 3 - some Ok without Errors should be Ok`` () =
    let case1 = [TernaryResult.Ignore; TernaryResult.Ok 1] |> L.ofList
    let case2 = [TernaryResult.Ok 1; TernaryResult.Ignore] |> L.ofList

    Assert.True(TernaryResult.isOk (TernaryResult.groupResult case1))
    Assert.True(TernaryResult.isOk (TernaryResult.groupResult case2))
