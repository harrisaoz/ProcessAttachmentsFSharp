module Execution.Templates

let main defaultConfigFile act argv =
    let handleResult =
        fun r ->
            match r with
            | Result.Ok _ ->
                printfn "All good"
                0
            | Result.Error msg ->
                printf $"Something went wrong [{msg}]"
                1

    match (List.ofArray argv) with
    | [] -> defaultConfigFile
    | filename :: _ -> filename
    |> (Configuration.Load.fromJsonFile >> Result.bind act >> handleResult)
