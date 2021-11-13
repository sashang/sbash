module Program =
    open Tests
    [<EntryPoint>]
    let main _ =
        testDeclare () 
        negTestParameter () 
        testParameter ()
        0
