#r "nuget:FParsec"
#load "Domain.fs"
#load "Parser.fs"
(*
    for infomration about these parser primitives read the reference here: http://www.quanttec.com/fparsec/reference/parser-overview.html
    It explains things like manyChars, noneOf, anyOf etc....
    And yes it took me some time to get elementary fluency using it.
*)

open FParsec
open SBash
let str s = pstring s

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test Parser.parameter "1var=pop;" //parameters can't start with a digit - this should fail
test Parser.parameter "adsad=pop;"
test Parser.parameter "adsad==pop;" //bash values can contain =


test Parser.control ";"
test Parser.control "\n"

test Parser.noneControl "1"


test Parser.commandArgs "-l"
test Parser.commandArgs "--list"


// The @ character prefixes a string that we want to be interpreted verbatim
// which means escape sequences are ignored, except where double quotes are used to escape
// single quotes.
test Parser.command @"find . -iname ""something"";"
test Parser.command @"echo ""hello"";"

test Parser.command @"echo ""hello""; echo ""now"";"

