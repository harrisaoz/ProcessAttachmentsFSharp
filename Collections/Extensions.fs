namespace ProcessAttachments.Collections.Extensions

module RSeq =
    let inline collectBoundTransform (f: 'a -> Result<'b, 'c> seq) (r: Result<'a seq, 'c>): Result<'b, 'c> seq =
        match r with
        | Ok xs -> Seq.collect f xs
        | Error err -> Seq.singleton (Error err)
