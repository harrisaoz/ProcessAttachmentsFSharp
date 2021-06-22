namespace FSharp.Core.Extensions

module String =
    let icompare s1 s2 =
        System.String.Compare(s1, s2, true)

    let iequal s1 s2 =
        icompare s1 s2 = 0

    let join (glue: string) (parts: string[]) =
        System.String.Join(glue, parts)

    let split (separators: char[]) (whole: string) =
        whole.Split(separators)

    let flatten labelSeparator (replacement: string) =
        split (Array.singleton labelSeparator)
        >> join replacement

module Seq =
    let icontains needle haystack =
        Seq.filter (String.iequal needle) haystack
        |> (not << Seq.isEmpty)

    let inline collectOver (enumerate: 'a -> 'b seq): 'a seq -> ('a * 'b) seq =
        Seq.collect (
            fun x ->
                enumerate x
                |> Seq.map (fun y -> (x, y)))

namespace ProcessAttachments.Collections.Extensions

module RSeq =
    let inline pairResultWith (empty: 'c) (f: 'a -> 'c) (t2x: 'y -> 't -> 'a) (y2r: 'y -> Result<'t, 'e>) (y: 'y) =
        let result = y2r y
        match result with
        | Ok r -> (result, f (t2x y r))
        | Error _ -> (result, empty)

    let inline pairResultWithDirect empty f result = pairResultWith empty f (fun _ -> id) id result

    let inline pairResultWithTransform empty f y = pairResultWith empty f (fun (_, s) _ -> s) fst y

    let inline collectBoundTransform f r =
        match r with
        | Ok xs -> Seq.collect f xs
        | Error data -> Seq.singleton (Error data)
