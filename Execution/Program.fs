module ProcessAttachments.Execution.Program

open Combinators.Logging
open ProcessAttachments.Execution.RuntimeParameters
open TypedConfiguration

module L = FSharpx.Collections.LazyList
module Config = Configuration.Load
module FS = ProcessAttachments.FileSystem
module Rtp = RuntimeParameters
module Cat = ProcessAttachments.ImapKit.Categorise
module Att = ProcessAttachments.ImapKit.Attachment
module AttName = ProcessAttachments.ImapKit.AttachmentNaming
module TR = Combinators.TernaryResult

type ExecutionContext =
    | ExecutionContext of programArgs: string[] * defaultConfigurationFilename: string

let chooseConfigurationFile (ExecutionContext (args, defaultFilename)) =
    match (List.ofArray args) with
    | [] -> defaultFilename
    | overrideFilename :: _ -> overrideFilename
    |> Config.ConfigurationFile

let currentTimestamp () =
    System.DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm:ss")

let logToFile (Rtp.AbsoluteFilename logDir) (Rtp.RelativeFilename filename) =
    let infoDir = FS.assertFolder logDir
    let logFile = FS.absoluteName infoDir filename
    let writer = System.IO.File.AppendText logFile

    fun (message: string) ->
        writer.WriteLine($"[{currentTimestamp ()}] {message}")
        writer.Flush()

let runWithConfiguredParameters prog =
    chooseConfigurationFile
    >> Config.fromSource
    >> Result.bind Rtp.parametersFromConfiguration
    >> Result.bind prog

let algorithm functions parameters =
    let init, select, prep, ls, leaves, cat, export, feedbackInit, feedback, report = functions
    let initParams,
        selectParams,
        prepParams,
        catParams,
        exportParams = parameters
    
    init initParams
    |> Result.bind (select selectParams)
    |> Result.map (Seq.map (
        fun node ->
            prep prepParams node
            |> Result.map (
                fun prepResult ->
                    let exportResults =
                        ls node
                        |> L.map (
                            fun x ->
                                let exportDataResult =
                                    leaves node x
                                    |> (cat catParams)
                                    |> TR.bind (export exportParams (node, x))
                                (x, exportDataResult))
                    feedbackInit node (
                        feedback prepResult exportResults
                        >> report node exportResults))))

let exec =
    let program (parameters: Rtp.RuntimeParameters) =
        let logToFolder = logToFile parameters.LoggingDestinations.LogDir in
        let infoLogger = Log (logToFolder parameters.LoggingDestinations.Info)
        let errorLogger = Log (logToFolder parameters.LoggingDestinations.Errors)
        let traceLogger =
            match parameters.LoggingDestinations.Trace with
            | Some traceFilename -> logToFolder traceFilename
            | None -> fun _ -> ()
            |> Log
        let reportLogger =
            match parameters.LoggingDestinations.Report with
            | Some reportFilename ->
                logToFolder reportFilename
            | None ->
                fun msg -> printfn $"{string msg}"
            |> Log
        let assignStandardLogger logger = fun x -> logCustomData x logger
        let inform0 = assignStandardLogger infoLogger |> Inform
        let alert0 =
            fun prefix ->
                logCustomData prefix errorLogger
            |> Alert
        let trace0 =
            fun depth ->
                trace depth traceLogger
            |> Trace
        let report0 =
            fun prefix ->
                logCustomData prefix reportLogger
            |> Report
        let ignoreAtt =
            fun maybeFilename maybeContentType ->
                parameters.CategorisationParameters.IgnoreBasedOnFilename maybeFilename ||
                Cat.isContentType parameters.CategorisationParameters.IgnoredMimeTypes maybeContentType
        let computeAttachmentName =
            match parameters.SourceMailFolders with
            | SourceMailFolders names ->
                AttName.chooseAlgorithmForFolderCount (Seq.length names)
                |> AttName.computeAttachmentName

        algorithm (
           Core.openSessionFromParameters trace0,
           Core.selectFolders (trace0, alert0),
           Core.assertProcessingFolders inform0,
           Core.enumerateFolderMessages (inform0, alert0),
           Core.enumerateMessageAttachments inform0,
           Core.categoriseAttachments (inform0, alert0),
           Core.saveMessageAttachments report0,
           Core.usingWritableFolder alert0,
           Core.moveMessagesFromFolder report0,
           Core.summarizeFolderActions report0) (
             parameters.SessionParameters,
             (parameters.SourceMailFolders, parameters.SessionParameters.Provider),
             parameters.OutputMailFolders,
             (ignoreAtt, parameters.CategorisationParameters.AcceptedMimeTypes),
             (parameters.DestinationFolder, computeAttachmentName))
        |> (fun r ->
                printfn "Execution Report"
                match r with
                | Ok s ->
                    printfn $"Number of folders processed = {Seq.length s}"
                | Error msg ->
                    printfn $"Failed. Reason = {msg}"
                r)

    runWithConfiguredParameters program

let main argv =
    exec (ExecutionContext (argv, "export-attachments.json"))
    |> Core.handleResult
