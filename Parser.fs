namespace SBash

module Parser =
    open Domain
    open FParsec

    [<Literal>]
    let literalDeclare = "declare"

    [<Literal>]
    let literalDecltp = "decltp"

    let skipws =
        skipManySatisfy (fun x -> x = ' ' || x = '\t')
    let control : CharStream<unit> -> Reply<char> = pchar ';' <|> newline
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
        .>> skipws |>> Identifier

    // Parse a parameter e.g. var=1
    let parameter  =
        // To test if we are parsing a parameter assignment/binding we need to see a '=' character
        // but that's the 2nd thing parsed. So we need to backtrack from that point.
        // we use .>>? to backtrack if the parse fails at '=' (i.e. the character is not '=')
        // in this case we aren't parsing a parameter. Another way of seeing this
        // is if in Bash we declared parameters with a keyword, e.g. 'param var=1'
        // in that case we won't need to backtrack because FParsec will be able to
        // determine from the 1st token parsed if this is a parameter or not.
        regex "[^=\d][^=]+" .>>? skipChar '=' .>>. dQuoteString .>> skipws
        |>> fun (a, b) -> Parameter (Name a, Value b)

    // command argument parser. commands are terminated with a bash control char, so we parse
    // everything that isn't a control char. Parse stops when it sees a control char.
    let commandArgs =
        manyCharsTill noneControl control
        |>> CommandArgs


    // parse a command and its args
    let command =
        regex "\w+" .>> skipws .>>. commandArgs .>> skipws
        |>> fun (a, b) -> Command (Path a, b)


    let declareArgs =
        skipChar '-' >>. pstring "T" .>> skipws .>>.  identifier .>> skipws |>> ArgVal

    let declare =
        skipString literalDeclare .>> skipws .>>. many declareArgs
        .>> skipws .>>. identifier
        |>> fun ((_, args), id) -> Declare (id, args)

    let decltpArgs =
        skipChar '-' >>. pstring "T" .>> skipws .>>.  identifier .>> skipws |>> ArgVal

    // decltp is like declare but it requires one arg hence many1.
    let decltp =
        skipString literalDecltp .>> skipws
        .>>. many1 decltpArgs .>> skipws
        .>>. identifier
        |>> fun ((_, args), id) -> Decltp (id, args)

    let decltpStatement = decltp .>> control |>> DecltpStatement
    let declareStatement = declare .>> control |>> DeclareStatement
    let paramStatement = parameter .>> control |>> ParamStatement
    let commandStatement = command |>> CommandStatement
    //let statement = decltpStatement <|> declareStatement <|> paramStatement <|> commandStatement
    let statement =
        decltpStatement
        <|> declareStatement
        <|> paramStatement
        <|> commandStatement

    let parse (input : string) = run statement input