namespace Fun.Parser

[<AutoOpen>]
module Combinators =
    open FParsec

    let withWidth parser f = 
        pipe3 getPosition parser getPosition <| fun p1 res p2 ->
            let width = uint32(p2.Index - p1.Index)
            f res width
    


(*type IParserState =
    abstract Tracer : ITracer*)