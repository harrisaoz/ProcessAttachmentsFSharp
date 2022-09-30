module Tests.Execution

open Xunit

[<Fact>]
let ``The current timestamp should have exactly 19 characters`` () =
    let result = ProcessAttachments.Execution.Program.currentTimestamp ()
    Assert.Equal(19, result.Length)
