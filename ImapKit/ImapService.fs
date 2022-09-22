module ProcessAttachments.ImapKit.ImapService

open System.Net
open MailKit.Net.Imap
open MailKit.Security

type Provider =
    | Gmail
    | MsExchange

[<Literal>]
let ProviderGmail = "gmail"
[<Literal>]
let ProviderMsExchange = "MsExchange"

let providerName: Provider -> string =
    function
    | Gmail -> ProviderGmail
    | MsExchange -> ProviderMsExchange

let providerByName: string -> Provider option =
    function
    | ProviderGmail -> Some Gmail
    | ProviderMsExchange -> Some MsExchange
    | _ -> None

type Endpoint =
    {
        Hostname: string
        Port: int
    }

type SessionParameters =
    {
        Provider: Provider
        Endpoint: Endpoint
        Credentials: NetworkCredential
    }

type ConnectedImapClient =
    {
        Endpoint: Endpoint
        Client: ImapClient
    }

type OpenImapSession =
    {
        Endpoint: Endpoint
        Credentials: ICredentials
        Client: ImapClient
    }

let initialize: ImapClient =
    let client = new ImapClient()
    client.ServerCertificateValidationCallback <- fun _ _ _ _ -> true
    
    client
    
type TryConnectClient = Endpoint -> ImapClient -> Result<ConnectedImapClient, string>
type TryOpenSession = ICredentials -> ConnectedImapClient -> Result<OpenImapSession, string>
type OpenSessionViaClient = SessionParameters -> ImapClient -> Result<OpenImapSession, string>

let tryConnect endpoint (client: ImapClient): Result<ImapClient, string> =
    try
        client.Connect(endpoint.Hostname, endpoint.Port, SecureSocketOptions.SslOnConnect)
        Ok client
    with
        ex ->
            Error ex.Message

let tryConnectClient: TryConnectClient =
    fun endpoint client ->
        try
            client.Connect(endpoint.Hostname, endpoint.Port, SecureSocketOptions.SslOnConnect)
            Ok { Endpoint = endpoint; Client = client }
        with
            ex ->
                Error ex.Message

let tryOpenSession: TryOpenSession =
    fun credentials connectedClient ->
        try
            connectedClient.Client.Authenticate credentials
            Ok {
                Endpoint = connectedClient.Endpoint
                Credentials = credentials
                Client = connectedClient.Client
            }
        with
            ex ->
                Error ex.Message

let openSessionViaClient: OpenSessionViaClient =
    fun sessionParameters ->
        tryConnectClient sessionParameters.Endpoint
        >> (Result.bind (tryOpenSession sessionParameters.Credentials))

let tryAuthenticate (credentials: ICredentials) (client: ImapClient): Result<ImapClient, string> =
    try
        client.Authenticate credentials
        Ok client
    with
        ex ->
            Error ex.Message

let connect (parameters: SessionParameters) (client: ImapClient) =
    client |> ((tryConnect parameters.Endpoint) >> (Result.bind (tryAuthenticate parameters.Credentials)))
