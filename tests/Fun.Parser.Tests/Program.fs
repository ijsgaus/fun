module Program
open Expecto
open FParsec
open Fun
open Fun.Parser
open Fun.Parser.Tests



    
module LiteralTest =
    open Fun.Ast

    [<Tests>]
    let numbers =
        let testNumber (s : string) v =
            let awaited = LiteralNode(v, uint32(s.Length))
            let msg = sprintf "Should parse \"%s\" as %A" s awaited
            testCase msg <| fun () ->
                let result = run Literal.number s
                match result with
                | Success (res, _, _) when res = awaited -> ()
                | Success (res, _, _) -> Tests.failtestf "Expexted %A, found %A" awaited res
                | Failure(msg, _, _) -> Tests.failtestf "Error parse \"%s\" - %s" s msg

        testList "Number literals" [
            testNumber "1234" (Int32 1234) 
            
            testCase "Should fail on string" <| fun () ->
                let result = run Literal.number  "1a2r"
                Expect.isFail result "Must fail on 1a2r"

            testCase "Should fail on overload" <| fun () ->
                let result = run Literal.number  "0xFFy"
                Expect.isFail result "Must fail on 0xFFy"

            testCase "Should fail on negative unsigned" <| fun () ->
                let result = run Literal.number  "-1uy"
                Expect.isFail result "Must fail on -1uy"

            testNumber "-234" (Int32 -234)
            testNumber "150uy" (UInt8 150uy)
            testNumber "0b11y" (Int8 0b11y)
            testNumber "0xAAFu" (UInt32 0xAAFu)
            testNumber "1.5" (Float 1.5)
            testNumber "1.5E-5f" (Float32 1.5E-5f)
            testNumber "3456us" (UInt16 3456us)
            testNumber "1234567UL" (UInt64 1234567UL)
        ]
    [<Tests>]
    let chars =
        let testChar (s : string) c =
            let awaited = LiteralNode(Char c, uint32(s.Length))
            let msg = sprintf "Should parse %s as %A" s awaited
            testCase msg <| fun () ->
                let result = run Literal.char s
                match result with
                | Success (res, _, _) when res = awaited -> ()
                | Success (res, _, _) -> Tests.failtestf "Expexted %A, found %A" awaited res
                | Failure(msg, _, _) -> Tests.failtestf "Error parse \"%s\" - %s" s msg
        testList "Char literals" [
           testChar "'a'" 'a'
           testChar @"'\n'" '\n'    
           testChar @"'\\'" '\\'
           testChar @"'\r'" '\r'
           testChar @"'\t'" '\t'
           testChar @"'\''" '\''
        ]


[<Tests>]
let tests = 
    testCase "yes" <| fun () ->
        let subject = "Hello world"
        Expect.equal subject "Hello world" "The strings should equal"

[<EntryPoint>]
let main argv = 
    runTestsInAssembly defaultConfig argv 
