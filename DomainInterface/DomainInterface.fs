module ProcessAttachments.DomainInterface

open System
open Microsoft.Extensions.Configuration

type Behaviour<'ResolvedConfig, 'Error, 'Session, 'Container, 'Node, 'Leaf, 'Content, 'Export when 'Session :> IDisposable> =
    {
        defaultConfigFilename: string
        configuration: IConfiguration -> Result<'ResolvedConfig, 'Error>
        session: 'ResolvedConfig -> 'Session
        container: 'Session -> Result<'Container, 'Error>
        roots: 'ResolvedConfig -> Result<'Container, 'Error> -> Result<'Node seq, 'Error>
        nodes: 'Node -> Result<'Node, 'Error> seq
        closeNode: Result<'Node, 'Error> -> Result<'Node, 'Error>
        leaves: 'Node -> Result<'Leaf seq, 'Error>
        contentItems: 'Leaf -> 'Content seq
        exportContent: 'Node -> 'Leaf -> 'Content -> Result<'Export, 'Error>
        onCompletion: ('Node * 'Leaf * 'Export) list * Result<'Export, 'Error> list -> int
        inspectNode: 'Node -> 'Node
        inspectLeaf: 'Leaf -> unit
        identifyNode: 'Node -> string
        identifyLeaf: 'Leaf -> string
    }
