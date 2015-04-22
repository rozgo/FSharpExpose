open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json
open Newtonsoft.Json.FSharp
open FSharp.Control
open FSharp.CompilerBinding

module Symbols =
  /// We always know the text of the identifier that resolved to symbol.
  /// Trim the range of the referring text to only include this identifier.
  /// This means references like A.B.C are trimmed to "C".  This allows renaming to just rename "C". 
  let trimSymbolRegion(symbolUse:FSharpSymbolUse) (lastIdentAtLoc:string) =
    let m = symbolUse.RangeAlternate 
    let ((beginLine, beginCol), (endLine, endCol)) = ((m.StartLine, m.StartColumn), (m.EndLine, m.EndColumn))
    let (beginLine, beginCol) =
        if endCol >=lastIdentAtLoc.Length && (beginLine <> endLine || (endCol-beginCol) >= lastIdentAtLoc.Length) then 
            (endLine,endCol-lastIdentAtLoc.Length)
        else
            (beginLine, beginCol)
    (beginLine, beginCol), (endLine, endCol)


module CmdRegex =

    let (|Project|_|) input =
       let m = Regex.Match (input, "^project\s\"(.*)\"$") 
       if (m.Success) then Some m.Groups.[1].Value else None 

    let (|Parse|_|) input =
       let m = Regex.Match (input, "^parse\s\"(.*)\"$") 
       if (m.Success) then Some m.Groups.[1].Value else None 

    let (|Tooltip|_|) input =
       let m = Regex.Match (input, "^tooltip\s\"(.*)\"\s([0-9]+)\s([0-9]+)$")
       if (m.Success) then Some (m.Groups.[1].Value, Int32.Parse(m.Groups.[2].Value), Int32.Parse(m.Groups.[3].Value)) else None

    let (|Completion|_|) input =
       let m = Regex.Match (input, "^completion\s\"(.*)\"\s\"(.*)\"\s([0-9]+)\s([0-9]+)$")
       if (m.Success) then Some (m.Groups.[1].Value, m.Groups.[2].Value, Int32.Parse(m.Groups.[3].Value), Int32.Parse(m.Groups.[4].Value)) else None

type CmdResultError = {
    Kind : string
    Data : FSharpErrorInfo []
}

type CmdDebug = {
    Kind : string
    Log : string
}

type CmdProjectOptions = {
    Kind : string
    Log : FSharpProjectOptions
}

type CmdResultTooltip = {
    Kind : string
    Data : string
}

type CmdResultCompletion = {
    Kind : string
    Data : (string * string * string) []
}


[<EntryPoint>]
let main argv = 
//    printfn "%A" argv

    let projectFile = "/Users/rozgo/Projects/fsharp-edit/Test1/Test1.fsproj"
    let sourceFile = "/Users/rozgo/Projects/fsharp-edit/Test1/Program.fs"

//    let projectFile = "/Users/rozgo/Projects/SingleAppDemo/SingleAppDemo/SingleAppDemo.fsproj"
//    let sourceFile = "/Users/rozgo/Projects/SingleAppDemo/SingleAppDemo/Main.fs"
//
    let sourceText = (new StreamReader(sourceFile)).ReadToEnd ()

    let checker = FSharpChecker.Create ()

    let projectOptions = {
        IsIncompleteTypeCheckEnvironment = false
        LoadTime = DateTime.Now
        OtherOptions =
            [|
                "--noframework"
//                "-r:/Library/Frameworks/Xamarin.iOS.framework/Versions/Current/lib/mono/Xamarin.iOS/FSharp.Core.dll"
//                "-r:/Library/Frameworks/Xamarin.iOS.framework/Versions/Current/lib/mono/Xamarin.iOS/mscorlib.dll"
//                "-r:/Library/Frameworks/Xamarin.iOS.framework/Versions/Current/lib/mono/Xamarin.iOS/System.dll"
//                "-r:/Library/Frameworks/Xamarin.iOS.framework/Versions/Current/lib/mono/Xamarin.iOS/System.Core.dll"
//                "-r:/Library/Frameworks/Xamarin.iOS.framework/Versions/Current/lib/mono/Xamarin.iOS/System.Xml.dll"
//                "-r:/Library/Frameworks/Xamarin.iOS.framework/Versions/Current/lib/mono/Xamarin.iOS/Xamarin.iOS.dll"
            |]
        ProjectFileName = projectFile
        ProjectFileNames =
            [|
                "/Users/rozgo/Projects/fsharp-edit/Test1/FileTwo.fs"
                "/Users/rozgo/Projects/fsharp-edit/Test1/Program.fs"
//                "/Users/rozgo/Projects/SingleAppDemo/SingleAppDemo/AppDelegate.fs"
//                "/Users/rozgo/Projects/SingleAppDemo/SingleAppDemo/ViewController.fs"
//                "/Users/rozgo/Projects/SingleAppDemo/SingleAppDemo/Main.fs"
            |]
        ReferencedProjects = [||]
        UnresolvedReferences = None
        UseScriptResolutionRules = false
    }

//    let projectResults = 
//        checker.ParseAndCheckProject (projectOptions)
//        |> Async.RunSynchronously

//    projectResults.Errors
//    |> Array.iter (fun e -> printfn "%A" e.Message)

    let identToken = Parser.tagOfToken (Parser.token.IDENT ("")) 


    let getToolTip (results:FSharpCheckFileResults) row col line = async {
//        printfn "getToolTip for line %s" line
        match Parsing.findLongIdents(col, line) with 
        | None -> return None
        | Some (col, identIsland) ->
//          printfn "identIsland %A" identIsland
          let! res = results.GetToolTipTextAlternate (row, col, line, identIsland, identToken)
          let! sym = results.GetSymbolUseAtLocation (row, col, line, identIsland)
//          printfn "Result: Got something, returning %A" sym
          return sym |> Option.bind (fun sym -> let (_, startCol), (_, endCol) = Symbols.trimSymbolRegion sym (Seq.last identIsland)
                                                Some (res, (startCol, endCol)))
    }

    let getDeclarationLocation (results:FSharpCheckFileResults) row col line = async {
        match Parsing.findLongIdents(col, line) with 
        | None -> return FSharpFindDeclResult.DeclNotFound FSharpFindDeclFailureReason.Unknown
        | Some (col, identIsland) -> return! results.GetDeclarationLocationAlternate (row, col, line, identIsland, false)
    }

    let getDeclarations (checkResults:FSharpCheckFileResults) (parseResults:FSharpParseFileResults) row col line = 
        let longName,residue = Parsing.findLongIdentsAndResidue(col, line)
//        Debug.WriteLine (sprintf "GetDeclarations: '%A', '%s'" longName residue)
        try
         let results =
             Async.RunSynchronously (checkResults.GetDeclarationListInfo(Some parseResults, row, col, line, longName, residue, fun (_,_) -> false),
                                     timeout = 250)
         Some (results, residue)
        with :? TimeoutException -> None

    let rec readLines stream = asyncSeq {
        let rec readLine (stream:Stream) = asyncSeq {
            let! b = stream.AsyncRead 1
            if b.[0] <> 10uy then
                yield b.[0]
                yield! readLine stream
        }
        let! bytes = AsyncSeq.toArray (readLine stream)
        yield Encoding.UTF8.GetString bytes
        yield! readLines stream
    }
 
//    let readSource stream = async {
//        let sourceLines = AsyncSeq.takeWhile (fun line -> line <> "<<EOF>>") (readLines stream)
//        let! sourceCode =
//            AsyncSeq.fold (fun (sw:StringWriter) (line:string) ->
//                sw.Write (line + "\n"); sw ) (new StringWriter ()) (sourceLines:AsyncSeq<string>)
//        return sourceCode.ToString ()
//    }

    let readLineCount = ref 0
 
    let stdin = Console.OpenStandardInput ()

    let handleCommands () = async {
//        let! projectOptions = checker.GetProjectOptionsFromScript (sourceFile, sourceText, DateTime.Now)
//        printfn "%s" (JsonConvert.SerializeObject ({CmdProjectOptions.Kind = "debug"; Log = projectOptions}))

        let allFileCheckResults = new Dictionary<string, FSharpCheckFileResults> ()
        let allFileSources = new Dictionary<string, string[]> ()
        let allFileParseResults = new Dictionary<string, FSharpParseFileResults> ()

        for line in readLines stdin do

//            printfn "IN COUNT: %i" readLineCount.Value
            printfn "LINE: %s" line
            readLineCount.Value <- readLineCount.Value + 1

            match line with
            | CmdRegex.Project project -> 
                let! projectResults = checker.ParseAndCheckProject (projectOptions)
                let json = JsonConvert.SerializeObject ({CmdResultError.Kind = "errors"; Data = projectResults.Errors})
                printfn "%s" json
//                printfn "PROJECT RESULTS %s" projectResults.Errors
                ()
            | CmdRegex.Parse file ->

                let! lines =
                    AsyncSeq.takeWhile (fun line -> line <> "<<EOF>>") (readLines stdin)
                    |> AsyncSeq.toArray

                let source =
                    let flat =
                        Array.fold (fun (sw:StringWriter) line -> sw.Write (line + "\n"); sw ) (new StringWriter ()) lines
                    flat.ToString ()

//                printfn "MATCH FILE %s" file
//                let! source = readSource stdin
//                printfn "SOURCE-CODE\n%s" (JsonConvert.SerializeObject ({Kind = "debug"; Log = source}))
//                printfn "SOURCE: %s" source
                let! fileParseResults = checker.ParseFileInProject (file, source, projectOptions)
                let! fileAnswer = checker.CheckFileInProject (fileParseResults, file, 0, source, projectOptions)

                match fileAnswer with
                | FSharpCheckFileAnswer.Succeeded fileCheckResults ->
                    allFileCheckResults.[file] <- fileCheckResults
                    allFileSources.[file] <- lines
                    allFileParseResults.[file] <- fileParseResults
                    let json = JsonConvert.SerializeObject ({CmdResultError.Kind = "errors"; Data = fileCheckResults.Errors})
                    printfn "%s" json
                | FSharpCheckFileAnswer.Aborted ->
                    ()

                ()
            | CmdRegex.Tooltip (file, row, column) ->
                let fileCheckResults = allFileCheckResults.[file]
                let lines = allFileSources.[file]
                let! tooltip = getToolTip fileCheckResults row column lines.[row - 1]

                match tooltip with
                | Some (tip, (x, y)) ->
                    let tipText = TipFormatter.formatTip tip
                    let json = JsonConvert.SerializeObject ({CmdResultTooltip.Kind = "tooltip"; Data = tipText})
                    printfn "%s" json
                | None -> ()

            | CmdRegex.Completion (file, prefix, row, column) ->
                let fileCheckResults = allFileCheckResults.[file]
                let fileParseResults = allFileParseResults.[file]
                let lines = allFileSources.[file]
                let decls = getDeclarations fileCheckResults fileParseResults row column lines.[row - 1]

                let declType glyph =
                    match glyph with
                    | 6  -> "value"
//                    | 6  -> "keyword"
                    | 24  -> "union" // union constructor
                    | 132  -> "type" // union type
                    | 74  -> "function"
                    | 0   -> "class" //
                    | 84  -> "class" //
                    | 48  -> "interface" // interface
                    | 72  -> "method"
                    | 90  -> "namespace"
                    | 102 -> "property"
                    | _   -> glyph.ToString ()

                let shouldMatchPrefix = if prefix = "" || prefix = "." then false else true
                let findMatch (text:string) = if shouldMatchPrefix then text.StartsWith prefix else false

                match decls with
                | Some (info, doc) ->

                    let help = Array.tryFind (fun (item:FSharpDeclarationListItem) -> findMatch item.Name) info.Items
                    let helpText =
                        match help with
                        | Some item -> TipFormatter.formatTip item.DescriptionText
                        | None -> ""
                    let matchDecl decl =
                        match help with
                        | Some help when help = decl -> helpText
                        | _ -> ""
                    let decls = Array.map (fun (item:FSharpDeclarationListItem) -> item.Name, declType item.Glyph, matchDecl item) info.Items

//                    TipFormatter

                    


//                    info.Items.[0].Name
                    //printfn "COMPLETION DOC: %s" doc
                    let json = JsonConvert.SerializeObject ({CmdResultCompletion.Kind = "completion"; Data = decls})
                    printfn "%s" json
                    ()
                    
//                | Some ((FSharpToolTipText tips), (x, y)) ->
//                    for tip in tips do
//                        match tip with
//                        | (FSharpToolTipElement.Single (text, doc)) ->
//                            let json = JsonConvert.SerializeObject ({CmdResultTooltip.Kind = "tooltip"; Data = text})
//                            printfn "%s" json
//                        | (FSharpToolTipElement.Group items) ->
//                            for (text, doc) in items do
//                                let json = JsonConvert.SerializeObject ({CmdResultTooltip.Kind = "tooltip"; Data = text})
//                                printfn "%s" json
//                        | _ -> ()
//                    ()
                | _ ->
                    ()
                ()
            | _ ->
//                printfn "NO MATCH"
                ()
            ()
    }

    try
        handleCommands ()
        |> Async.RunSynchronously
        |> ignore
    with
    | e -> printfn "%s" (e.ToString())



    0

