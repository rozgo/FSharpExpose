open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json.FSharp
open FSharp.Control

open System.Text.RegularExpressions
   
let (|Match|_|) (pat:string) (inp:string) =
    let m = Regex.Match(inp, pat) in
    if m.Success
    then Some (List.tail [ for g in m.Groups -> g.Value ])
    else None

let convert<'ToType> value = Convert.ChangeType(value, typeof<'ToType>) :?> 'ToType
let (|Match3|_|) (pat:string) (inp:string) : ('T1 * 'T2 * 'T3) option =
    match (|Match|_|) pat inp with
    | Some (fst :: snd :: trd :: []) -> 
        try 
            Some (convert<'T1> fst, convert<'T2> snd, convert<'T3> trd) 
        with _ -> failwith "Match3 succeeded, but with type conversion errors"
    | Some [] -> failwith "Match3 succeeded, but no groups found. Use '(.*)' to capture groups"
    | Some _ -> failwith "Match3 succeeded, but did not find exactly three matches."
    | None -> None  

// Example
let (month, day, year) : (int * int * int) =
    match DateTime.Now.ToString() with
    | Match3 "(\d*)/(\d*)/(\d*).*" (a,b,c) -> (a,b,c)
    | _ -> failwith "Match Not Found."


[<EntryPoint>]
let main argv = 
//    printfn "%A" argv

//    let projectFile = "/Users/rozgo/Projects/fsharp-edit/Test1/Test1.fsproj"
//    let sourceFile = "/Users/rozgo/Projects/fsharp-edit/Test1/Program.fs"

    let projectFile = "/Users/rozgo/Projects/SingleAppDemo/SingleAppDemo/SingleAppDemo.fsproj"
    let sourceFile = "/Users/rozgo/Projects/SingleAppDemo/SingleAppDemo/Main.fs"

    let sourceText = (new StreamReader(sourceFile)).ReadToEnd ()

    let checker = FSharpChecker.Create ()

    let projectOptions = {
        IsIncompleteTypeCheckEnvironment = false
        LoadTime = DateTime.Now
        OtherOptions =
            [|
                "--noframework"
                "-r:/Library/Frameworks/Xamarin.iOS.framework/Versions/Current/lib/mono/Xamarin.iOS/FSharp.Core.dll"
                "-r:/Library/Frameworks/Xamarin.iOS.framework/Versions/Current/lib/mono/Xamarin.iOS/mscorlib.dll"
                "-r:/Library/Frameworks/Xamarin.iOS.framework/Versions/Current/lib/mono/Xamarin.iOS/System.dll"
                "-r:/Library/Frameworks/Xamarin.iOS.framework/Versions/Current/lib/mono/Xamarin.iOS/System.Core.dll"
                "-r:/Library/Frameworks/Xamarin.iOS.framework/Versions/Current/lib/mono/Xamarin.iOS/System.Xml.dll"
                "-r:/Library/Frameworks/Xamarin.iOS.framework/Versions/Current/lib/mono/Xamarin.iOS/Xamarin.iOS.dll"
            |]
        ProjectFileName = projectFile
        ProjectFileNames =
            [|
//                "/Users/rozgo/Projects/fsharp-edit/Test1/FileTwo.fs"
//                "/Users/rozgo/Projects/fsharp-edit/Test1/Program.fs"
                "/Users/rozgo/Projects/SingleAppDemo/SingleAppDemo/AppDelegate.fs"
                "/Users/rozgo/Projects/SingleAppDemo/SingleAppDemo/ViewController.fs"
                "/Users/rozgo/Projects/SingleAppDemo/SingleAppDemo/Main.fs"
            |]
        ReferencedProjects = [||]
        UnresolvedReferences = None
        UseScriptResolutionRules = false
    }

    let projectResults = 
        checker.ParseAndCheckProject (projectOptions)
        |> Async.RunSynchronously

    projectResults.Errors
    |> Array.iter (fun e -> printfn "%A" e.Message)

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
 

    let stdin = Console.OpenStandardInput ()
//    let stdin = new StreamReader (stdin)

    

    let rec handleCommands = asyncSeq {
        for line in readLines stdin do
            printfn "LINE: %s" line

            let m = Regex.Match (line, pattern)
    }

    handleCommands
    |> Async.RunSynchronously

//    let rec readUntil chr = asyncSeq {
//        let! b = stdin.AsyncRead 1
//        if b.[0] <> chr then
//            yield b.[0]
//            yield! readUntil chr
//    }
//
//    let cmdReader = asyncSeq {
//        let! cmd = AsyncSeq.fold (fun cmd b -> b :: cmd) ([] : byte list) (readUntil 32uy)
//        let cmd = List.rev cmd
//        let str = Encoding.UTF8.GetString (Array.ofList cmd)
//        printfn "COMMAND: %A" str
//        return cmd
//    }
//
//    printfn "Read cmd"
//    cmdReader
//    |> Async.RunSynchronously
//    |> ignore

////    let bytes = stdin.re
//    let bytes = 
//        stdin
//        |> Async.RunSynchronously
//    printfn "%A" bytes



//    let projectOptions =
//        checker.GetProjectOptionsFromScript (sourceFile, sourceText, DateTime.Now, [||], false)
//        |> Async.RunSynchronously
//
//    printfn "%A" (projectOptions)

    0

