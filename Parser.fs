namespace SBash

module Parser =
    open Domain
    open FParsec

(*
    let parseParameter (state : Program) (tokens : string list) =
        let splitResult = token.Split('=')
        let (Program (statements)) = state
        List.append statements [Parameter(splitResult.[0], splitResult.[1])]
        |> Program

    let parseCommand (state : Program) (tokens : string list) =
        let (Program (statements)) = state
        List.append statements [Command(command, args)]
        |> Program

    let parse (state : Program) (tokens : string list) =
        match tokens with
        | [] -> 
            let (Program (statements)) = state
            List.append statements [Nothing]
            |> Program
        | head::rest -> 
            if head.Contains('=') then
                parseParameter state head
            else
                parseCommand state head rest
 *)
    let str s = pstring s
    
    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    let parseParameter = regex "[a-z][A-Z]+" .>> skipChar '=' .>>. regex "[a-z][A-Z]+" 

    test parseParameter "adsad"