namespace ProcessAttachments.CommonExtensions

module IgnorableResultExtensions =
    open FsCombinators.ExtraTypes
    module L = FSharpx.Collections.LazyList

    let groupResult xs =
        IgnorableResult.groupResultGeneric ((fun x -> L.cons x L.empty), L.append, L.fold) xs
