﻿namespace ProcessAttachments.DomainInterface

open System
open Microsoft.Extensions.Configuration

type ExportItemName = string

type Behaviour<'ResolvedConfig, 'Init, 'Error, 'Client, 'Node, 'Leaf, 'Content, 'Export
when 'Client :> IDisposable> =
    {
        defaultConfigFilename: string
        configuration: IConfiguration -> Result<'ResolvedConfig, 'Error>
        initialise: 'ResolvedConfig -> 'Client * ('Client -> Result<'Client, 'Error>) * 'Init
        roots: 'Init -> Result<'Client, 'Error> -> Result<'Node seq, 'Error>
        nodes: 'Node -> Result<'Node, 'Error> seq
        closeNode: Result<'Node, 'Error> -> Result<'Node, 'Error>
        leaves: 'Node -> Result<'Leaf seq, 'Error>
        contentItems: 'Node -> 'Leaf -> Result<'Content, 'Error> seq
        categorise: 'Content -> ContentCategory<'Content, 'Error>
        contentName: 'Node * 'Leaf * 'Content -> ExportItemName
        exportContent: 'Init -> ExportItemName -> 'Content -> Result<'Export, 'Error>
        onCompletion: ('Node * 'Leaf * 'Export) list * Result<'Export, 'Error> list -> int
        inspectNode: 'Node -> 'Node
        inspectLeaf: 'Leaf -> unit
        identifyNode: 'Node -> string
        identifyLeaf: 'Leaf -> string
    }
