module Combinators.Boolean

open Combinators.Standard

// eitherForG f g x = f x || g x
let eitherForG f = S (f >> (||))
