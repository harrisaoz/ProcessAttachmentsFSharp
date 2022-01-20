namespace Combinators

open FSharpx.Collections

module L = LazyList

type TernaryResult<'a, 'Error> =
    | Ok of 'a
    | Ignore
    | Error of 'Error

module TernaryResult =
    open Standard

    let inline bind f tr =
        match tr with
        | Ok x -> f x
        | Ignore -> Ignore
        | Error r -> Error r

    let inline bindLogRejections log f tr =
        match tr with
        | Ok x -> f x
        | Ignore -> Ignore
        | Error r ->
            log r
            Error r

    let inline map f = bind (f >> Ok)

    let inline bind2 v f c =
        match c with
        | Ok x -> f x
        | Ignore -> v
        | Error r -> Error r

    let ofResult r =
        match r with
        | Result.Ok x -> Ok x
        | Result.Error y -> Error y

    let toResult resultOfIgnore tr =
        match tr with
        | Ok x -> Result.Ok x
        | Ignore -> resultOfIgnore
        | Error msg -> Result.Error msg

    let isOk tr =
        match tr with
        | Ok _ -> true
        | _ -> false

    let shouldIgnore tr =
        match tr with
        | Ignore _ -> true
        | _ -> false

    let groupResult (xs: LazyList<TernaryResult<'a, 'b>>): TernaryResult<LazyList<'a>, 'b> =
        let folder acc =
            let accBind x =
                bind2
                    ((Seq.singleton >> L.ofSeq >> Ok) x)
                    ((Seq.singleton >> L.ofSeq >> C L.append >> B Ok) x)

            S bind2 (C accBind) acc

        L.fold folder Ignore xs
