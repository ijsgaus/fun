module Program
open Expecto
open FParsec
open Fun
open Fun.Parser
open Fun.Parser.Tests



[<EntryPoint>]
let main argv = 
    runTestsInAssembly defaultConfig argv 
