// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#load "../../paket-files/include-scripts/net46/include.fsharp.core.fsx"
#load "../../paket-files/include-scripts/net46/include.main.group.fsx"
#r "../../build/Fun.dll"
#load "Combinators.fs"
open FParsec
open Fun
open Fun.Ast
open Fun.Parser

// Define your library scripting code here
printfn "%A" (run Combinators.simpleIdentifier @"Атора")
printfn "%A" (run (many1 Combinators.simpleIdentifier) @"A-->BAsd*Ijk")

