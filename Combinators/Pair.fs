module Combinators.Pair

// Preferable to List.map + pattern matching, due to the need to match all possible patterns in assignment.
let map f a b = (f a, f b)
