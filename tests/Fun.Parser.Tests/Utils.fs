namespace Fun.Parser.Tests
open FParsec
open Expecto
module Expect =
    let isSuccess result espected =
        match result with
        | Success _ -> ()
        | Failure(msg, _, _) -> Tests.failtestf "%s - %s" espected msg
    let isSuccessResultP result message f =
        match result with
        | Success (r, _, _) -> 
            match f(r) with
            | Some p -> printfn "%A" p; ()
            | _ -> Tests.failtestf "Invalid result - %s" message
        | Failure(msg, _, _) -> Tests.failtestf "%s - %s" message msg
    let isFail result message =
        match result with
        | Failure _ -> ()
        | _ -> Tests.failtestf "Parser not failed: %s" message