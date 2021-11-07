#r "nuget:FParsec"
#load "Domain.fs"
#load "Parser.fs"
(*
    for infomration about these parser primitives read the reference here: http://www.quanttec.com/fparsec/reference/parser-overview.html
    It explains things like manyChars, noneOf, anyOf etc....
    And yes it took me some time to get elementary fluency using it.
*)

open FParsec
let str s = pstring s

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let parameter = regex "[^=\d]+" .>> skipChar '=' .>>. regex ".+" 

test parameter "1var=pop" //parameters can't start with a digit - this should fail
test parameter "adsad=pop"
test parameter "adsad==pop" //bash values can contain =

let control = anyOf ";\n"

test control ";"
test control "\n"

let noneControl = noneOf ";\n"

test noneControl "1"

// command arguments are anything following the commnad that is not a bash control operator
let commandArgs = spaces >>. manyChars noneControl 

test commandArgs "-l"
test commandArgs "--list"

let command = regex "\w+" .>> spaces .>>. commandArgs

// The @ character prefixes a string that we want to be interpreted verbatim
// which means escape sequences are ignored, except where double quotes are used to escape
// single quotes.
test command @"find . -iname ""something"""
test command @"echo ""hello"""

test command @"echo ""hello""; echo ""now"""
