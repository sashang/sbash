namespace SBash
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
    
    let private evalParameter (parameter : Parameter) ast =
        let (Parameter (name, value)) = parameter
        let (Name name) = name
        let (Value value) = value
        ast

    let private paramPropfromArg arg =
        match arg with
        | SingleArg _ ->
            Nothing
        | ArgVal (arg, value) ->
            match arg with
            | "T" -> TPParam(
                match value with 
                | "csv" -> CSV
                | _ -> TypeProvider.Nothing
                )
            | _ -> Nothing

    // Take a Declare statement and update the AST with the new variable
    // in the statement
    let private evalDeclare (ds : Declare) (ast : AST) =
        let (Declare (Identifier(name), args)) = ds

        match args with
        | [] -> ast
        | [head] ->
            let (AST (ParameterTable(table), program)) = ast
            let paramProp = paramPropFromArg head
            let table' = table.Add (name, ASTParameter(argument, Value(""), paramProp))
            AST(table', program) //return the updated AST
        | _ -> ast

    let read () =
        Console.ReadLine ()

    let eval (input : string) ast =
        let parseResult = Parser.parse input
        match parseResult with
        | ParserResult.Success (statement, _, _) -> 
            match statement with
            | ParamStatement ps ->
                evalParameter ps ast
            | CommandStatement cs ->
                evalCommand cs ast
            | DeclareStatement ds ->
                evalDeclare ds ast
        | ParserResult.Failure (error, _, _) -> ast

    let rec repl ast () =
        let args = read ()
        let ast' = eval args ast
        repl ast' ()

    [<EntryPoint>]
    let main argv =
        let ast = AST (ParameterTable (new Map<Identifier, ASTParameter>), Program([]))
        repl ()
        0 // return an integer exit code
