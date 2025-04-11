module ProcessAttachments.Execution.Program

module L = FSharpx.Collections.LazyList

module Config = FsConfigLoader.Read
module IOFsExt = FsSimpleFileIO.FileSystemExtensions

module FCT = FsCombinators.ExtraTypes

open TypedConfiguration

module Rtp = ProcessAttachments.Execution.RuntimeParameters
module Cat = ProcessAttachments.ImapKit.Categorise
module Att = ProcessAttachments.ImapKit.Attachment
module AttName = ProcessAttachments.ImapKit.AttachmentNaming

type ExecutionContext =
    | ExecutionContext of programArgs: string[] * defaultConfigurationFilename: string

let chooseConfigurationFile (ExecutionContext (args, defaultFilename)) =
    match (List.ofArray args) with
    | [] -> defaultFilename
    | overrideFilename :: _ -> overrideFilename
    |> FsConfigLoader.Core.ConfigSource

let currentTimestamp () =
    System.DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm:ss")

let logToFile (Rtp.AbsoluteFilename logDir) (Rtp.RelativeFilename filename) =
    let infoDir = IOFsExt.assertFolder logDir
    let logFile = IOFsExt.absoluteName infoDir filename
    let writer = System.IO.File.AppendText logFile

    fun (message: string) ->
        writer.WriteLine($"[{currentTimestamp ()}] {message}")
        writer.Flush()

// let runWithConfiguredParameters prog =
//     chooseConfigurationFile
//     >> Config.fromSource
//     >> Result.bind Rtp.parametersFromConfiguration
//     >> Result.bind prog

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
                                    |> FCT.IgnorableResult.bind (export exportParams (node, x))
                                (x, exportDataResult))
                    feedbackInit node (
                        feedback prepResult exportResults
                        >> report node exportResults))))

module Logging = ProcessAttachments.Execution.Logging

let program (parameters: Rtp.RuntimeParameters) =
    let logToFolder = logToFile parameters.LoggingDestinations.LogDir in
    let infoLogger = Logging.Log (logToFolder parameters.LoggingDestinations.Info)
    let errorLogger = Logging.Log (logToFolder parameters.LoggingDestinations.Errors)
    let traceLogger =
        match parameters.LoggingDestinations.Trace with
        | Some traceFilename -> logToFolder traceFilename
        | None -> fun _ -> ()
        |> Logging.Log
    let reportLogger =
        match parameters.LoggingDestinations.Report with
        | Some reportFilename ->
            logToFolder reportFilename
        | None ->
            fun msg -> printfn $"{string msg}"
        |> Logging.Log
    let assignStandardLogger logger = fun x -> Logging.logCustomData x logger
    let inform0 = assignStandardLogger infoLogger |> Logging.Inform
    let alert0 =
        fun prefix ->
            Logging.logCustomData prefix errorLogger
        |> Logging.Alert
    let trace0 =
        fun depth ->
            Logging.trace depth traceLogger
        |> Logging.Trace
    let report0 =
        fun prefix ->
            Logging.logCustomData prefix reportLogger
        |> Logging.Report
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

let main argv =
    FsConfigLoader.Facade.SimpleConsoleApp.tryWithConfiguration
        Rtp.parametersFromConfiguration
        program
        argv
