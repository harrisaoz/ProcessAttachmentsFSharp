module Combinators.Standard

// "reverse" composition. B = (<<)
let inline B f =
    fun g x -> f (g x)

// "Cardinal"
let inline C f =
    fun x y -> f y x

let inline K x =
    fun _ -> x

// "forward" composition. Q = (>>)
let inline Q f =
    fun g x -> g (f x)

let M f = Q f f

// "Starling"
let inline S f =
    fun g x -> f x (g x)

// "Thrush": this combinator is also known by (|>) and used infix as x |> f
// (|>) x f = f x
let inline T x f = f x

let inline W x y = x y y

let rec Y f x = f (Y f) x
