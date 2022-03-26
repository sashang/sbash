namespace SBash

module Parser =
    open Domain
    open FParsec

    [<Literal>]
    let literalDeclare = "declare"

    [<Literal>]
    let keywordSVar = "svar"

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

    // Parse a parameter binding e.g. var=1
    let parameterBinding =
        // To test if we are parsing a parameter assignment/binding we need to see a '=' character
        // but that's the 2nd thing parsed. So we need to backtrack from that point.
        // we use .>>? to backtrack if the parse fails at '=' (i.e. the character is not '=')
        // in this case we aren't parsing a parameter. Another way of seeing this
        // is if in Bash we declared parameters with a keyword, e.g. 'param var=1'
        // in that case we won't need to backtrack because FParsec will be able to
        // determine from the 1st token parsed if this is a parameter or not.
        identifier .>>? skipChar '=' .>>. dQuoteString .>> skipws
        |>> fun (id, value) -> Parameter (id, Value value)

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
        skipChar '-' >>. pstring "T" .>> skipws .>>.  identifier .>> skipws |>> WithArg

    let declare =
        skipString literalDeclare .>> skipws .>>. many declareArgs
        .>> skipws .>>. identifier
        |>> fun ((_, args), id) -> Declare (id, args)

    /// <summary>
    /// Short options are things like: &lt;command&gt; -st.
    /// They don't have arguments therefore map to the NoArg constructor.
    /// </summary>
    /// <param name="options"> options: a string of characters representing the valid short options.</param>
    let shortOptions (options : string) =
        skipChar '-'
        >>. manyCharsTillApply
            (anyOf options)
            (satisfy (fun x -> x = ' ' || x = '\n' || x = ';' || x = '\t'))
            (fun str _ -> str |> Seq.map (fun c -> NoArg (string c)))

    let shortOptionsWithArg (options : string) =
        skipChar '-'
        >>. anyOf options .>> skipws
        .>>. identifier
        |>> (fun (optionLetter, optionArg) -> WithArg(string optionLetter, optionArg))

    // svar stands for structured variable. It is a varible with structued data, for example,
    // csv, json, etc... any string like data with structure.
    let svar =
        skipString keywordSVar .>> skipws
        .>>. shortOptions "st" .>> skipws
        .>>. identifier
        |>> fun ((_, args), id) -> SVar (id, List.ofSeq args)

    let svarStatement = svar .>> control |>> SVarStatement
    let declareStatement = declare .>> control |>> DeclareStatement
    let paramBindingStatement = parameterBinding .>> control |>> ParamBindingStatement
    let commandStatement = command |>> CommandStatement
    //let statement = decltpStatement <|> declareStatement <|> paramStatement <|> commandStatement
    let statement =
        svarStatement
        <|> declareStatement
        <|> paramBindingStatement
        <|> commandStatement

    let parse (input : string) = run statement input