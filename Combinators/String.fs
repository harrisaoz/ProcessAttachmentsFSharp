module Combinators.String

let contains: string -> string -> bool =
    fun haystack -> haystack.Contains

let endsWith: string -> string -> bool =
    fun haystack -> haystack.EndsWith

let join (glue: string) (parts: string[]) =
    System.String.Join(glue, parts)

let split (separators: char[]) (whole: string) =
    whole.Split(separators)

let flatten labelSeparator (replacement: string) =
    split (Array.singleton labelSeparator)
    >> join replacement

let iequal (s1: string) (s2: string) =
    System.String.Equals (s1, s2, System.StringComparison.InvariantCultureIgnoreCase)
