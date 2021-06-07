module Flows.DfsSequence

let pipeline getTop enumerate action close reportError onComplete =
    getTop
    >> Seq.collect enumerate
    >> Seq.map action
    >> Seq.map close
    >> List.ofSeq
    >> List.map reportError
    >> List.partition (
        fun r ->
            match r with
            | Result.Ok _ -> true
            | _ -> false
        )
    >> onComplete
