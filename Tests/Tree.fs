module Tests.Tree

open Xunit

open FSharpx.Collections

open Collections.Conversions

module R = Experimental.RoseTree

let lsAlwaysOk (f: int -> seq<int>) =
    let ls a =
        match a with
        | Result.Ok x ->
            f x |> Result.Ok
        | Result.Error msg ->
            Result.Error msg
    ls

let lsAlwaysErrors msg _ =
    let ls =
        fun _ -> Result.Error msg
    ls

let validateAlwaysOk k: Result<'a, string> = Result.Ok k

let validateAlwaysErrors msg: Result<'a, string> =
    Result.Error msg

[<Fact>]
let ``Traversing an invalid root should list a single error Result`` () =
    let stubValidate _: Result<'a, string> = Result.Error "invalid root"
    let stubLs =
        fun x ->
            match x with
            | 0 -> seq {1;2;3}
            | _ -> Seq.empty
        |> lsAlwaysOk

    let tree = stubValidate 0 |> asRoseTree (asLazyLs stubLs stubValidate)
    let materialized = R.dfsPre tree |> Seq.toList

    Assert.Equal(Result.Error "invalid root", List.head materialized)
    Assert.Equal(1, List.length materialized)

[<Fact>]
let ``Given a root with an invalid list of children, when the root is traversed only ...`` () =
    let errorMsg = "failed to list children"
    let stubLs =
        fun x ->
            match x with
            | 0 -> seq {1;2;3}
            | _ -> Seq.empty
        |> lsAlwaysErrors errorMsg

    let tree = validateAlwaysOk 0 |> asRoseTree (asLazyLs stubLs validateAlwaysOk)
    let materialized = R.dfsPre tree |> Seq.toList

    Assert.Equal(Result.Ok 0, List.head materialized)
    Assert.Equal(Result.Error errorMsg, List.item 1 materialized)
    Assert.Equal(2, List.length materialized)

[<Fact>]
let ``dfsPre should traverse a RoseTree in pre-order`` () =
    let stubValidate = validateAlwaysOk
    let mutable enumerationCount = 0
    let mockLs =
        fun x ->
            match x with
            | 0 -> seq {1; 6; 7}
            | 1 ->
                enumerationCount <- enumerationCount + 1
                seq {2; 3}
            | 3 -> seq {4; 5}
            | 7 -> seq {8; 9}
            | 9 -> seq {10}
            | _ -> Seq.empty
        |> lsAlwaysOk

    let tree = Result.Ok 0 |> asRoseTree (asLazyLs mockLs stubValidate)
    let extract resultValue =
        match resultValue with
        | Result.Ok v -> v
        | _ -> -1
    
    let materialized = R.dfsPre tree |> Seq.map extract
    let ``expected sequence as List`` = [0..10]
    
    Assert.Equal(0, enumerationCount)
    Assert.Equal(``expected sequence as List``, materialized)
    Assert.Equal(1, enumerationCount)
    Assert.Equal(``expected sequence as List``, materialized)
    Assert.Equal(1, enumerationCount)
