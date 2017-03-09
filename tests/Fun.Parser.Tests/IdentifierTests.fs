namespace Fun.Parser.Tests
open Expecto
open FParsec
open Fun
open Fun.Parser
open Fun.Parser.Tests



    
module IdentifierTest =
    open Fun.Ast

    [<Tests>]
    let simples =
        let testSimple (s : string) v =
            let awaited = IdentifierNode([v], uint32(s.Length))
            let msg = sprintf "Should parse \"%s\" as %A" s awaited
            testCase msg <| fun () ->
                let result = run Identifier.simple s
                match result with
                | Success (res, _, _) when res = awaited -> ()
                | Success (res, _, _) -> Tests.failtestf "Expexted %A, found %A" awaited res
                | Failure(msg, _, _) -> Tests.failtestf "Error parse \"%s\" - %s" s msg
        testList "Simple identifier" [
            testSimple "Abc" "Abc"
            testSimple "_a" "_a"
            testSimple "Ab_c" "Ab_c"
            shouldFail "Invalid start letter" "'bc" Identifier.simple
        ]

    [<Tests>]
    let complex =
        let testComplex (s : string) v =
            let awaited = IdentifierNode(v, uint32(s.Length))
            let msg = sprintf "Should parse \"%s\" as %A" s awaited
            testCase msg <| fun () ->
                let result = run Identifier.complex s
                match result with
                | Success (res, _, _) when res = awaited -> ()
                | Success (res, _, _) -> Tests.failtestf "Expexted %A, found %A" awaited res
                | Failure(msg, _, _) -> Tests.failtestf "Error parse \"%s\" - %s" s msg
        testList "Complex identifier" [
            testComplex "Abc" ["Abc"]
            testComplex "Abc.Def" ["Abc"; "Def"]
            testComplex "Abc.Def.Fgh" ["Abc"; "Def"; "Fgh"]
            shouldFail "Double dot" "Abc..Def" Identifier.complex
            shouldFail "Dot started" ".Def" Identifier.complex
        ]