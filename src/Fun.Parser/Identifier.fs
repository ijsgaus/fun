namespace Fun.Parser

module Identifier =
    open FParsec
    open Tools
    open Fun.Ast

    let name stream =
        let isAsciiIdStart c = (IdentifierValidator.IsXIdStartOrSurrogate c) || (['_'] |> List.contains c)
        identifier (IdentifierOptions(isAsciiIdStart)) stream

    let simple stream = 
        (withWidth name <| fun s w -> IdentifierNode([s], w)) stream
    let complex stream = 
        (withWidth (sepBy1 name (pchar '.')) <| fun s w -> IdentifierNode(s, w)) stream

