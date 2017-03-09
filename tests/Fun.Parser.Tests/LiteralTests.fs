namespace Fun.Parser.Tests
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
            testNumber "1234" (I32 1234) 
            
            shouldFail "string" "1a2r" Literal.number
            shouldFail "overload" "0xFFy" Literal.number
            shouldFail "negative unsigned" "-1uy" Literal.number
            
            
            testNumber "-234" (I32 -234)
            testNumber "150uy" (U8 150uy)
            testNumber "0b11y" (I8 0b11y)
            testNumber "0xAAFu" (U32 0xAAFu)
            testNumber "1.5" (F64 1.5)
            testNumber "1.5E-5f" (F32 1.5E-5f)
            testNumber "3456us" (U16 3456us)
            testNumber "1234567UL" (U64 1234567UL)
        ]
    [<Tests>]
    let chars =
        let testChar (s : string) c =
            let awaited = LiteralNode(CH c, uint32(s.Length))
            let msg = sprintf "Should parse %s as %A" s awaited
            testCase msg <| fun () ->
                let result = run Literal.ch s
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
           shouldFail "on empty" "''" Literal.ch
           shouldFail "on 3 trick" "'''" Literal.ch
        ]
    [<Tests>]
    let strings =
        let testString (s : string) sr =
            let awaited = LiteralNode(STR sr, uint32(s.Length))
            let msg = sprintf "Should parse '%s' as %A" s awaited
            testCase msg <| fun () ->
                let result = run Literal.str s
                match result with
                | Success (res, _, _) when res = awaited -> ()
                | Success (res, _, _) -> Tests.failtestf "Expexted %A, found %A" awaited res
                | Failure(msg, _, _) -> Tests.failtestf "Error parse \"%s\" - %s" s msg
        testList "String literals" [
            testString "\"1234\"" "1234"
            testString "\"12\\t34\"" "12\t34"
            testString "\"12\\\"34\"" "12\"34"
            testString "\"\\u000A\"" "\n"
            testString "\"abc\\u0032ghj\"" "abc\u0032ghj"
            shouldFail "missing utf digit" "\"abc\\u032ghj\"" Literal.str
        ]