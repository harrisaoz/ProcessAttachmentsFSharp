module Tests.Collections

open Xunit

open FSharp.Core.Extensions

let e1 f x =
    match x with
    | 0 -> seq {100;101}
    | 1 -> seq {102}
    | 2 -> seq {103;104;105}
    | _ -> Seq.empty
    |> Seq.map (fun ys ->
        f()
        ys)

let xs1 f = seq { 0 .. 5 } |> Seq.map (
                fun i ->
                    f()
                    i
                )

let noop () = ()

[<Fact>]
let ``collectOver: rule 1 - the collector function should be applied exactly once for each element in the input sequence`` () =
    let mutable callCounterMock: int = 0

    let f = e1 <| fun () -> callCounterMock <- callCounterMock + 1
    let xs = xs1 noop

    Seq.collectOver f xs
    |> List.ofSeq
    |> ignore

    Assert.StrictEqual(Seq.length xs, callCounterMock)

[<Fact>]
let ``collectOver: rule 2 - the input sequence should be evaluated exactly once`` () =
    let mutable callCounterMock: int = 0
    
    let f = e1 noop
    let xs' = xs1 noop
    let xs = xs1 <| fun () -> callCounterMock <- callCounterMock + 1 
    
    Seq.collectOver f xs
    |> List.ofSeq
    |> ignore

    Assert.StrictEqual(Seq.length xs', callCounterMock)

[<Fact>]
let ``collectOver: example 1`` () =
    let f = e1 noop
    let xs = xs1 noop
    
    Seq.collectOver f xs
    |> fun actual ->
        Assert.Equal([(0,100); (0, 101); (1,102); (2,103); (2,104); (2,105)], actual)
