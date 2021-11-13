module tests

open NUnit.Framework
open Swensen.Unquote
open SBash.Domain
open SBash.Parser
open FParsec

[<SetUp>]
let Setup () =
    ()


[<Test>]
let declare() =
    let statement = @"declare -T var;"
    match (run declareStatement statement ) with
    | Success (DeclareStatement(Declare(Identifier(id), args)), _, _) ->
        id =! "var"
        match args with
        | [SingleArg arg] -> arg =! "T"
        | _ -> Assert.Fail($"Expected argument list")
    | _ ->
        Assert.Fail($"Failed to parse \"{statement}\"") 

