namespace ProcessAttachments.Execution

module Logging =
    type Log = Log of (string -> unit)

    type Report = Report of (string -> string -> string seq -> unit)
    type Trace = Trace of (int -> string -> unit)
    type Inform = Inform of (string -> string -> string seq -> unit)
    type Alert = Alert of (string -> string -> string seq -> unit)

    let customTrace bracketText level (Log log) detail =
        let bracket = String.replicate level bracketText
        log $"{bracket} {detail} {bracket}"

    let trace level = customTrace "=" level

    let formatLogDetail data: string =
        data
        |> Seq.map (fun datum -> $"[{string datum}]")
        |> Array.ofSeq
        |> String.concat " "

    let logCustomData prefix (Log log) (description: string) data: unit =
        match (formatLogDetail data) with
        | "" -> log $"{prefix} {description}"
        | detail -> log $"{prefix} {description}: {detail}"

    let inform log = logCustomData "?" log
    let alert log = logCustomData "!\u26a0" log
    let report prefix = logCustomData prefix
