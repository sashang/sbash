namespace SBash

module Parser =
    open Domain
    open FParsec

    let control : CharStream<unit> -> Reply<char> = anyOf ";\n"
    let noneControl : CharStream<unit> -> Reply<char> = noneOf ";\n"

    // Parse a parameter e.g. var=1
    let parameter  =
        regex "[^=\d][^=]+" .>> skipChar '=' .>>. regex ".+"
        |>> fun (a, b) -> Parameter (Name a, Value b)

    // command argument parser. commands are terminated with a bash control char, so we parse
    // everything that isn't a control char. Parse stops when it sees a control char.
    let commandArgs = manyChars noneControl 

    // parse a command and its args
    let command =
        regex "\w+" .>> spaces .>>. opt commandArgs .>> control
        |>> fun (a, b) ->
            match b with
            | None ->
                Command (Path a, None)
            | Some args ->
                Command (Path a, Some (Arguments args))

    let statement = opt parameter .>> command
    let parse (input : string) = run statement input