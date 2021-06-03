module ImapAttachments.ImapFolder

open FSharpx.Collections.Experimental
open MailKit

open Collections.Conversions
open MailKit.Net.Imap

//let traverseFolder (folder: IMailFolder) =
//    let subfolders = folder.GetSubfolders(false)

let personalNamespace =
    fun (client: ImapClient) ->
        client.PersonalNamespaces.Item(0)

let filter (fNamespace: ImapClient -> FolderNamespace) condition (client: ImapClient) =
    client
        .GetFolder(fNamespace client)
        .GetSubfolders(false)
    |> Seq.filter condition
    
let openMailFolder (folder: IMailFolder) =
    try
        match folder.Open(FolderAccess.ReadOnly) with
        | FolderAccess.None ->
            Result.Error $"ReadOnly access to the folder $s{folder.FullName} was refused"
        | _ ->
            Result.Ok folder
    with
        ex -> Result.Error ex.Message

let subfolders (folder: IMailFolder): Result<seq<IMailFolder>, string> =
    try
        folder.GetSubfolders(false) :> seq<IMailFolder> |> Result.Ok
    with
        ex -> ex.Message |> Result.Error

let validate = openMailFolder

let ls (validatedFolder: Result<IMailFolder, string>) =
    match validatedFolder with
    | Result.Ok folder -> subfolders folder
    | Result.Error msg -> Result.Error msg

let dfsPre folder =
    let lazyLs = asLazyLs ls validate
    let roseTree = validate folder |> asRoseTree lazyLs
    RoseTree.dfsPre roseTree

let saveFolderAttachments (r: Result<IMailFolder, string>) =
    match r with
    | Result.Ok folder ->
        printfn $"[{folder.FullName}] Processed folder"
    | Result.Error msg ->
        printfn $"{msg}"
    r

let tryCloseFolder (folder: IMailFolder) =
    try
        folder.Close()
        printfn $"[{folder.FullName}] Closed folder"
        Result.Ok folder
    with
        ex -> $"[{folder.FullName}] {ex.Message}" |> Result.Error

let closeFolder (elevatedFolder: Result<IMailFolder, string>) =
    elevatedFolder |> Result.bind tryCloseFolder

let dfs (openNode: 'a -> Result<'a,string>) (listChildren: 'a -> Result<seq<'a>,string>) (errorKey: 'a -> string) =
    fun (topNode: 'a) ->
        let rec listDescendents (node': 'a): seq<Result<'a, string>> =
            let ls (node'': 'a): seq<Result<'a,string>> =
                match (listChildren node'') with
                | Result.Error msg -> Result.Error $"{errorKey node''} [{msg}]" |> Seq.singleton
                | Result.Ok childNodes -> Seq.map openNode childNodes

            let recursiveCall (lsResult: Result<'a,string>): seq<Result<'a,string>> =
                match lsResult with
                | Result.Error msg -> Result.Error msg |> Seq.singleton
                | Result.Ok folder -> listDescendents folder

            ls node' |> (Seq.map recursiveCall >> Seq.concat)

        match (openNode topNode) with
        | Result.Error msg -> Result.Error $"{errorKey topNode} [{msg}]" |> Seq.singleton
        | Result.Ok rootNode -> listDescendents rootNode
