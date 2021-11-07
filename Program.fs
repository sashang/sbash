namespace SBash
open System
open System.Diagnostics

module Main =
    open Domain
    open FParsec.CharParsers

    let private evalCommand (path : Path) (args : Arguments) =
        let (Path path) = path
        let (Arguments args) = args

        let pathlist = Environment.GetEnvironmentVariable("PATH").Split ([|':'|]) |> Array.toList
        try
            let where = List.find (fun x -> IO.File.Exists(x + "/" + path)) pathlist
            let startInfo = ProcessStartInfo()
            startInfo.FileName  <- where + "/" + path
            startInfo.Arguments <- args

            startInfo.RedirectStandardOutput <- false
            startInfo.RedirectStandardInput  <- false
            startInfo.UseShellExecute <- true

            let proc = new Process()
            proc.EnableRaisingEvents <- true

            proc.StartInfo <- startInfo
            proc.Start() |> ignore

            proc.WaitForExit()
            (proc.ExitCode, "")
        with
            | :? Collections.Generic.KeyNotFoundException -> (1, path + ": command not found")
    
    let private evalParameter (name : Name) (value : Value) =
        let (Name name) = name
        let (Value value) = value
        (0, "")

    let read () =
        Console.ReadLine ()

    let eval (input : string) =
        let parseResult = Parser.parse input
        match parseResult with
        | ParserResult.Success (statement, _, _) -> 
            match statement with
            | Parameter (name, value) -> evalParameter name value
            | Command (path, args) -> evalCommand path args
            | Nothing -> (0, "")
        | ParserResult.Failure (error, _, _) ->
            (0, error)

    let rec repl () =
        let args = read ()
        let (code, text) = eval args
        printfn "%s" text
        repl ()

    [<EntryPoint>]
    let main argv =
        repl ()
        0 // return an integer exit code
