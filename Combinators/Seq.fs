module Combinators.Seq

open Combinators.Standard

let inline icontains needle haystack =
    Seq.filter (String.iequal needle) haystack
    |> (not << Seq.isEmpty)

/// There exists a predicate p <- ps such that p x is true.
let inline existsPredicate x predicates =
    Seq.exists (T x) predicates

let inline collectOver (enumerate: 'a -> 'b seq): 'a seq -> ('a * 'b) seq =
    Seq.collect (
        fun x ->
            enumerate x
            |> Seq.map (fun y -> (x, y)))

/// There exists an x <- xs such that, given y, p y x is true.
let existsGiven p y xs = (p >> Seq.exists) y xs

/// 1. The given option, m, has Some value, y; and
/// 2. there exists an x <- xs such that p y x is true.
let maybeExistsWhere p: 'b seq -> 'a option -> bool =
    C (existsGiven p) >> Option.exists
// Equivalent to:
//    fun p xs m ->
//        Option.exists (fun y -> existsGiven p y xs) m

/// 1. The given option, m, has Some value, x; and
/// 2. there exists a predicate, p <- ps, such that p x is true
let anyPredicate (ps: ('a -> bool) seq): 'a option -> bool =
    (C existsPredicate >> Option.exists) ps
// Equivalent to:
//    fun ps m ->
//        match m with
//        | Some x -> Seq.exists (fun p -> p x) ps
//        | None -> false
