module ProcessAttachments.Execution.RuntimeParameters

open Microsoft.Extensions.Configuration

module TC = TypedConfiguration
module FS = ProcessAttachments.FileSystem

type DestinationFolder = System.IO.DirectoryInfo
type GenericRuntimeParameters<'a> =
    {
        SessionParameters: ProcessAttachments.ImapKit.ImapService.SessionParameters
        CategorisationParameters: TypedConfiguration.AttachmentCategorisationParameters
        DestinationFolder: DestinationFolder
        SourceMailFolders: TC.SourceMailFolders
        OutputMailFolders: TC.OutputMailFolders
        LoggingDestinations: 'a
    }

type AbsoluteFilename =
    | AbsoluteFilename of name: string

type RelativeFilename =
    | RelativeFilename of name: string

type LoggingParameters =
    {
        LogDir: AbsoluteFilename
        Info: RelativeFilename
        Errors: RelativeFilename
        Trace: RelativeFilename option
        Report: RelativeFilename option
    }

type RuntimeParameters = GenericRuntimeParameters<LoggingParameters>
type ParametersFromConfiguration = IConfiguration -> RuntimeParameters

let parametersFromConfiguration (configuration: IConfiguration): Result<RuntimeParameters, string> =
    match (TC.imapServiceParameters configuration,
           TC.categorisationParameters configuration,
           TC.mailboxParameters configuration,
           TC.exportParameters configuration,
           TC.loggingParameters configuration) with
    | Some session, Some categorisation, Some mailbox, Some export, Some logging ->
        let parameters = {
            SessionParameters = session
            CategorisationParameters = categorisation
            DestinationFolder = FS.assertFolder export.DestinationFolder
            SourceMailFolders = mailbox.SourceFolders
            OutputMailFolders = mailbox.OutputMailFolders
            LoggingDestinations =
                {
                    LogDir = AbsoluteFilename logging.LogDir
                    Info = RelativeFilename logging.InfoFilename
                    Errors = RelativeFilename logging.ErrorFilename
                    Trace = Option.map RelativeFilename logging.TraceFilename
                    Report = Option.map RelativeFilename logging.ReportFilename
                }
            }
        parameters |> Result.Ok
    | _ -> Result.Error "Failed to infer valid runtime parameters from the provided configuration"
