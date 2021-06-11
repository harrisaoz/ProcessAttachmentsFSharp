// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open ImapKit
open SaveAttachments

module ET = Execution.Templates
module Def = Defaults
module P = Processor

[<EntryPoint>]
let main =
    let behaviour = Def.basicDownloader
    ET.main behaviour.standardConfigFile (P.downloadAttachments behaviour)
