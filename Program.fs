namespace SBash
open System
open System.Diagnostics

module Main =
    open Domain

    let private exec (file : string) (args : string list) =
        let pathlist = Environment.GetEnvironmentVariable("PATH").Split ([|':'|]) |> Array.toList
        try
            let where = List.find (fun x -> IO.File.Exists(x + "/" + file)) pathlist
            let startInfo = ProcessStartInfo()
            startInfo.FileName  <- where + "/" + file
            startInfo.Arguments <- match args with | [] -> "" | _ -> List.reduce (+) args

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
            | :? Collections.Generic.KeyNotFoundException -> (1, file + ": command not found")

    let read () =
        let isWhitespace c =
            System.Char.IsWhiteSpace c

        let folder (acc, current) item = 
            // if it's not whitespace then append the char to the current string being built
            if not (isWhitespace item) then
                (acc, current + string item)
            else
            // if it is whitespace the append the current word onto the accumulator (list of strings)
            // and reset the current to the empty string
                (acc @ [current], "")

        let line = Console.ReadLine ()
        line
        |> Seq.fold folder ([], "")
        |> (fun (args, last) -> args @ [last])


    let eval (line : string list) =
        let command = Parser.parse line
        match command with
        | Command (path, None) -> exec path []
        | Command (path, Some args) -> exec path args
        | Nothing -> (0, "")

    let rec repl () =
        let args = read ()
        let (code, text) = eval args
        printfn "%s" text
        repl ()

    [<EntryPoint>]
    let main argv =
        repl ()
        0 // return an integer exit code
