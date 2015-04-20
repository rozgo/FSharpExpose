open System
open System.Collections
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.FSharp.Compiler.SourceCodeServices
open Newtonsoft.Json.FSharp
open FSharp.Control

let (|FirstRegexGroup|_|) pattern input =
   let m = Regex.Match(input,pattern) 
   if (m.Success) then Some m.Groups.[1].Value else None 

//let (|FirstRegexGroup|_|) pattern input =
//   let m = Regex.Match(input,pattern) 
//   if (m.Success) then Some m.Groups.[1].Value else None 


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

            let m =
                match line with
                | FirstRegexGroup "^project\ \"(.*)\"$" project -> 
                       printfn "MATCH PROJECT %s" project
                | _ -> printfn "NO MATCH"
            ()
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

