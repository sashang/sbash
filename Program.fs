namespace SBash
open FSharp.Data
open System
open System.Diagnostics

module Main =
    open Domain
    open FParsec.CharParsers

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
    
    // Place statement like var = "some value" into the environment table
    let private evalParamBinding (parameter : Parameter) (ast : AST) =
        let (Parameter (id, value)) = parameter
        let (AST (ParameterTable table, program)) = ast
        if Map.containsKey id table then
            // If a binding exists update it with the new value
            // and preserve the existing options. Options are specified via declare statements
            let (_, opts) = Map.find id table
            let table' = Map.add id (value, opts) table
            AST (ParameterTable table', program)
        else
            // No binding so add this value and id with no parameter options.
            let table' = Map.add id (value, None) table
            AST (ParameterTable table', program)

    let private paramAttrFromArg arg =
        match arg with
        | SingleArg _ ->
            None
        | ArgVal (arg, value) ->
            match arg with
            | "T" ->
                Some (TPParam(
                        match value with 
                        | Identifier "csv" -> CSV
                     ))
            | _ -> None

    let private updateParamTable id ast paramAttr =
        let (AST (ParameterTable(table), program)) = ast
        let table' = table.Add (id, ParameterAttributes(Value(""), paramAttr))
        AST (ParameterTable table', program) //return the updated AST

    // Take a Declare statement and update the AST with the new variable
    // in the statement
    let private evalDeclare (ds : Declare) (ast : AST) =
        let (Declare (id, args)) = ds

        match args with
        | [] -> ast
        | [head] ->
            let attr = paramAttrFromArg head
            updateParamTable id ast attr
        | _ -> ast

    let private evalDecltp (ds : Decltp) (ast : AST) =
        let (Decltp (id, args)) = ds

        match args with
        | [] -> ast
        | [head] ->
            let attr = paramAttrFromArg head
            updateParamTable id ast attr
        | _ -> ast

    let read () =
        Console.ReadLine ()

    let eval (input : string) ast =
        let parseResult = Parser.parse input
        match parseResult with
        | Success (statement, _, _) -> 
            match statement with
            | ParamBindingStatement ps ->
                evalParamBinding ps ast
            | CommandStatement cs ->
                evalCommand cs ast
            | DeclareStatement ds ->
                evalDeclare ds ast
            | DecltpStatement dtp ->
                evalDecltp dtp ast
        | Failure (error, _, _) ->
            Console.WriteLine $"Error: {error}"
            ast

    let rec repl ast =
        let args = read ()
        let ast' = eval args ast
        repl ast'

    [<EntryPoint>]
    let main argv =
        let ast = AST (ParameterTable Map.empty,  Program([]))
        repl ast |> ignore
        0 // return an integer exit code
