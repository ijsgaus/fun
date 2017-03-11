// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#load "../../paket-files/include-scripts/net452/include.main.group.fsx"
#r "../../build/Fun.dll"
#load "Combinators.fs"
open FParsec
open Fun.Parser

// Define your library scripting code here
run Combinators.UnicodeEscape @"\u1234"
