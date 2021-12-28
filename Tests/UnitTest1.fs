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
let testDecltp() =
    let pass = [
        ("svar -t csv var;", SVarStatement(SVar(Identifier "var", [WithArg ("t", Identifier "csv")])))
    ]
    let fail = [
        ("svar Var1;")
    ]
    pass
    |> List.iter (fun (expression, expected) ->
        match (run svarStatement expression) with
        | Success (result, _, _) ->
            result =! expected
        | Failure (error, parserError, state) ->
            Assert.Fail($@"Failed to parse ""{expression}"". ""{error}""")
    ) 
    fail
    |> List.iter (fun expression ->
        match (run svarStatement expression ) with
        | Failure (error, _, _) -> ()
        | Success (result, _, _)  ->
            Assert.Fail($@"Expected parse of ""{expression}"" to fail. Got ""{result}"" instead.") 
    ) 

[<Test>]
let testDeclare() =
    let pass = [
        (@"declare Var1;", DeclareStatement(Declare(Identifier "Var1", [])))
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
    match (run parameterBinding statement) with
    | Failure(_) ->
        ()
    | _ ->
        Assert.Fail($@"Expected parse of ""{statement}"" to fail.") 

[<Test>]
let testShortOptions () =
    let expr = "-st"
    let expected = seq {NoArg (string 's'); NoArg (string 't')}
    match (run (shortOptions "st") expr) with
    | Success (result, _, _) ->
        // sequences don't do structural equality, so saying s1 = s2 is going to be false.
        let test = (result, expected) ||> Seq.compareWith (fun x y -> compare x y)
        test =! 0
    | _ ->
        Assert.Fail($@"Failed to parse ""{expr}""")

[<Test>]
let testParameter () =
    let multilinetext = "first,last,email\nsashan,govender,skg@contesso.com"
    let statements = [
        (@"adsad=""pop"";", Parameter(Identifier("adsad"), Value("pop")))
        (@"adsad=""=pop"";", Parameter(Identifier("adsad"), Value("=pop")))
        ($"text=\"{multilinetext}\"", Parameter(Identifier("text"), Value(multilinetext)))
    ]

    statements
    |> List.iter (fun (expression, expected) ->
        match (run parameterBinding expression) with
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

[<Test>]
let testStatement () =
    let pass = [
        (@"find . -iname ""something"";", CommandStatement(Command(Path("find"), CommandArgs(@". -iname ""something"""))))
        ("svar -t csv var\n", SVarStatement(SVar(Identifier "var", [WithArg ("t", Identifier "csv")])))
    ]

    pass
    |> List.iter (fun (expression, expected) ->
        match (run statement expression) with
        | Success(result,_,_) ->
            result =! expected
            ()
        | _ ->
            Assert.Fail($"Failed to parse \"{expression}\".") 
    )