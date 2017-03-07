namespace Fun.Parser
open Fun.Ast

type ParserState = {
    SourceKind : AstSourceKind
}

module Tools =
    open FParsec
    let toSourcePos (p: Position) =
        { Offset = uint64(p.Index); Line = uint32(p.Line); Column = uint32(p.Column) }
    let takeRange (parser : Parser<_, _>)  f = 
        pipe4 getUserState getPosition parser getPosition 
            (fun st p1 res p2 ->  
                let pos =
                    match st.SourceKind with
                    | AstSourceKind.SourceFile sf -> 
                        SourceFile (sf, SourceRange(toSourcePos p1, toSourcePos p2))
                f res pos
            )


    
    