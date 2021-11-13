namespace SBash

module Parser =
    open Domain
    open FParsec

    [<Literal>]
    let literalDeclare = "declare"

    let control : CharStream<unit> -> Reply<char> = anyOf ";\n"
    let noneControl : CharStream<unit> -> Reply<char> = noneOf ";\n"

    // Parse a parameter e.g. var=1
    let parameter  =
        regex "[^=\d][^=]+" .>> skipChar '=' .>>. regex "[^ \n;]+" .>> spaces
        |>> fun (a, b) -> Parameter (Name a, Value b)

    // command argument parser. commands are terminated with a bash control char, so we parse
    // everything that isn't a control char. Parse stops when it sees a control char.
    let commandArgs = manyCharsTill noneControl control |>> CommandArgs

    // parse a command and its args
    let command =
        regex "\w+" .>> spaces .>>. commandArgs .>> spaces
        |>> fun (a, b) -> Command (Path a, b)


    let identifier = regex "[a-zA-Z]*" |>> Identifier

    let declareArgs =
        pchar '-' >>. pstring "T" .>> spaces |>> SingleArg

    let declare =
        skipString literalDeclare .>> spaces .>>. many declareArgs
        .>> spaces .>>. identifier |>> fun ((_, args), id) -> Declare (id, args)

    let declareStatement = declare .>> control |>> DeclareStatement
    let paramStatement = parameter .>> control |>> ParamStatement
    let commandStatement = command |>> CommandStatement
    let statement = attempt paramStatement <|> commandStatement

    let parse (input : string) = run statement input