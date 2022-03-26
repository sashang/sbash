namespace SBash
open FSharp.Data
open System
open System.Diagnostics

module Main =
    open Domain
    open FParsec.CharParsers
    open System

    let private evalCommand (command : Command) ast =
        let (Command (path, args)) = command
        let (Path path) = path
        let (CommandArgs args) = args

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
            ast
        with
            | :? Collections.Generic.KeyNotFoundException -> ast

    let private printParameterTable table =
        Map.iter (fun key x -> printfn "key = %A, value = %A" key x) table

    // Place statement like var = "some value" into the environment table
    let private evalParamBinding (parameter : Parameter) (program : Program) =
        let (Parameter (id, value)) = parameter
        let (Program (ProgEnv env, statementBlock)) = program
        if Map.containsKey id env then
            // If a binding exists update it with the new value
            // and preserve the existing options. Options of the parameter are specified via declare or svar statements
            let (_, opts) = Map.find id env
            let table' = Map.add id (value, opts) env
            Program (ProgEnv table', statementBlock)
        else
            // No binding so add this value and id with no parameter options.
            let table' = Map.add id (value, None) env
            Program (ProgEnv table', statementBlock)

    let private paramAttrFromArg arg =
        match arg with
        | NoArg optionLetter ->
            match optionLetter with
            | "t" ->
                // for now just maps this to the csv type provider
                Some (TPParam CSV)
            | _ ->
                None
        | WithArg (optionLetter, value) ->
            match optionLetter with
            | "T" ->
                Some (TPParam(
                        match value with
                        | Identifier "csv" -> CSV
                        | _ -> failwith "unsupported structured data type."
                     ))
            | _ -> None

    let private updateParamTable id program paramAttr =
        let (Program (ProgEnv(table), statementBlock)) = program
        let table' = table.Add (id, ParameterAttributes(Value(""), paramAttr))
        printParameterTable table'
        Program (ProgEnv table', statementBlock) //return the updated table

    // Take a Declare statement and update the environment with the new variable
    // in the statement
    let private evalDeclare (ds : Declare) (program : Program) =
        let (Declare (id, args)) = ds
        match args with
        | [] -> program
        | [head] ->
            let attr = paramAttrFromArg head
            updateParamTable id program attr
        | _ -> program

    let private evalSVar (ds : SVar) (program : Program) =
        let (SVar (id, args)) = ds

        match args with
        | [] -> program
        | [head] ->
            let attr = paramAttrFromArg head
            updateParamTable id program attr
        | _ -> program

    let read () =
        Console.ReadLine ()

    let eval (input : string) program =
        let parseResult = Parser.parse input
        match parseResult with
        | Success (statement, _, _) ->
            match statement with
            | ParamBindingStatement ps ->
                evalParamBinding ps program
            | CommandStatement cs ->
                evalCommand cs program
            | DeclareStatement ds ->
                evalDeclare ds program
            | SVarStatement svar ->
                evalSVar svar program
        | Failure (error, _, _) ->
            Console.WriteLine $"Error: {error}"
            program

    let rec repl program =
        let args = read ()
        let program' = eval args program
        repl program'

    [<EntryPoint>]
    let main argv =
        let program = Program.Empty
        repl program |> ignore
        0 // return an integer exit code
