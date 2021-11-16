module Tests

open NUnit.Framework
open Swensen.Unquote
open SBash.Domain
open SBash.Parser
open FParsec

[<SetUp>]
let Setup () =
    ()


[<Test>]
let testDeclare() =
    let pass = [
        (@"declare -T csv var;", DeclareStatement(Declare(Identifier "var", [ArgVal ("T", "csv")])))
    ]
    pass
    |> List.iter (fun (expression, expected) ->
        match (run declareStatement expression ) with
        | Success (result, _, _) ->
            result =! expected
        | _ ->
            Assert.Fail($@"Failed to parse ""{statement}""")
    ) 

[<Test>]
let negTestParameter () =
    let statement = @"1vat=111;"
    match (run parameter statement ) with
    | Failure(_) ->
        ()
    | _ ->
        Assert.Fail($@"Expected parse of ""{statement}"" to fail.") 

[<Test>]
let testParameter () =
    let statements = [
        (@"adsad=pop;", Parameter(Name("adsad"), Value("pop")))
        (@"adsad==pop;", Parameter(Name("adsad"), Value("=pop")))
    ]

    statements
    |> List.iter (fun (expression, expected) ->
        match (run parameter expression) with
        | Success(param, _, _) ->
            param =! expected
            ()
        | _ ->
            Assert.Fail($"Failed to parse \"{expression}\"") 
    )

[<Test>]
let testControl () =
    let statements = [ ";";"\n" ]
    statements
    |> List.iter (fun (expression) ->
        match (run control expression) with
        | Success(_) ->
            ()
        | _ ->
            Assert.Fail($"Failed to parse \"{expression}\".") 
    )

[<Test>]
let testNoneControl () =
    let pass = [ "1";"2092"; "asldkjh asdasd"]
    pass
    |> List.iter (fun (expression) ->
        match (run noneControl expression) with
        | Success(_) ->
            ()
        | _ ->
            Assert.Fail($"Failed to parse \"{expression}\".") 
    )
    let fail= [ ";";"\n"]
    fail
    |> List.iter (fun (expression) ->
        match (run noneControl expression) with
        | Success(_) ->
            Assert.Fail($"Expected parse of \"{expression}\" to fail.") 
        | _ ->
            ()
    )

[<Test>]
let testCommandArgs () =
    let pass = [ "-l;";"--list;"; "--arg1 something --arg2 somethingelse;"]

    pass
    |> List.iter (fun (expression) ->
        match (run commandArgs expression) with
        | Success(_) ->
            ()
        | _ ->
            Assert.Fail($"Failed to parse \"{expression}\".") 
    )

[<Test>]
let testCommand () =
    let pass = [
        (@"find . -iname ""something"";", Command(Path("find"), CommandArgs(@". -iname ""something""")))
    ]

    pass
    |> List.iter (fun (expression, expected) ->
        match (run command expression) with
        | Success(result,_,_) ->
            result =! expected
            ()
        | _ ->
            Assert.Fail($"Failed to parse \"{expression}\".") 
    )