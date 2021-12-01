namespace SBash

module Parser =
    open Domain
    open FParsec

    [<Literal>]
    let literalDeclare = "declare"

    let control : CharStream<unit> -> Reply<char> = anyOf ";\n"
    let noneControl : CharStream<unit> -> Reply<char> = noneOf ";\n"


    let dQuoteString =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
        let unescape c =
            match c with
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
        between (pstring "\"") (pstring "\"")
                (manyChars (normalChar <|> escapedChar))   

    let identifier =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'

        many1Satisfy2 isIdentifierFirstChar isIdentifierChar
        .>> spaces |>> Identifier

    // Parse a parameter e.g. var=1
    let parameter  =
        regex "[^=\d][^=]+" .>> skipChar '=' .>>. dQuoteString .>> spaces
        |>> fun (a, b) -> Parameter (Name a, Value b)

    // command argument parser. commands are terminated with a bash control char, so we parse
    // everything that isn't a control char. Parse stops when it sees a control char.
    let commandArgs =
        manyCharsTill noneControl control
        |>> CommandArgs


    // parse a command and its args
    let command =
        regex "\w+" .>> spaces .>>. commandArgs .>> spaces
        |>> fun (a, b) -> Command (Path a, b)


    let declareArgs =
        skipChar '-' >>. pstring "T" .>> spaces .>>.  identifier .>> spaces |>> ArgVal

    let declare =
        skipString literalDeclare .>> spaces .>>. many declareArgs
        .>> spaces .>>. identifier
        |>> fun ((_, args), id) -> Declare (id, args)

    let declareStatement = declare .>> control |>> DeclareStatement
    let paramStatement = parameter .>> control |>> ParamStatement
    let commandStatement = command |>> CommandStatement
    let statement = attempt paramStatement <|> commandStatement

    let parse (input : string) = run statement input