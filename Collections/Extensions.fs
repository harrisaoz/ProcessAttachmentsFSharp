namespace FSharp.Core

module String =
    let icompare s1 s2 =
        System.String.Compare(s1, s2, true)
    
    let iequal s1 s2 =
        icompare s1 s2 = 0

module Seq =
    let icontains needle haystack =
        Seq.filter (String.iequal needle) haystack
        |> (not << Seq.isEmpty)
