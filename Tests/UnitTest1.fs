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
    let statement = @"declare -T var;"
    match (run declareStatement statement ) with
    | Success (DeclareStatement(Declare(Identifier(id), args)), _, _) ->
        id =! "var"
        match args with
        | [SingleArg arg] -> arg =! "T"
        | _ -> Assert.Fail($"Expected argument list")
    | _ ->
        Assert.Fail($"Failed to parse \"{statement}\"") 

[<Test>]
let negTestParameter () =
    let statement = @"1vat=111;"
    match (run parameter statement ) with
    | Failure(_) ->
        ()
    | _ ->
        Assert.Fail($"Expected parse of \"{statement}\" to fail.") 

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
            Assert.Fail($"Expected parse of \"{expression}\" to fail.") 
    )