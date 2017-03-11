namespace Fun

open System.Collections.Immutable

type 't arr = ImmutableArray<'t>

module Ast =

    type IHasWidth =
        abstract member Width : uint32

    type IAstNode  = 
        inherit IHasWidth
        abstract member Chieldren : IAstNode arr

    let (|Branch|Leaf|) (node: IAstNode) =
        if node.Chieldren.Length > 0 then Branch else Leaf

    type Keywords =
        | Namespace
        | Alias
        | Open
        member __.ToCode () =
            match __ with
            | Namespace -> "namespace"
            | Alias -> "alias"
            | Open -> "open"
    
    type Whitespace = | SP | TAB | NL 

    type Comment = | Tail | Block | Doc

    type Trivia = 
        | WS of Whitespace * uint32 
        | Comment of Comment * uint32
        member __.Width = match __ with | WS(_, w) -> w | Comment(_, w) -> w
        interface IHasWidth with
            member __.Width = __.Width
            

    type OpenClose = | Open | Close

    type BracketKind = | Round | Square | Curly | Angle
        
    type SBracket = SBracket of (BracketKind * OpenClose) list 
        
    module Bracket =
        let isValid (b : SBracket) = let (SBracket l) = b in l <> []
        
        let inverse (bracket : SBracket) =
            let (SBracket lst) = bracket
            lst |> List.map (fun (t, o) ->t, if o = Open then Close else Open) |> List.rev |> SBracket
        
        let private fromPair o c oc = match oc with | Open -> o | Close -> c

        let pairToChar (knd, oc) =
            match knd with
            | Round ->  fromPair "(" ")" oc
            | Square -> fromPair "[" "]" oc
            | Curly ->  fromPair "{" "}" oc
            | Angle ->  fromPair "<" ">" oc

        let charToPair ch =
            match ch with
            | '(' -> Some (Round, Open)
            | ')' -> Some (Round, Close)
            | '[' -> Some (Square, Open)
            | ']' -> Some (Square, Close)
            | '<' -> Some (Angle, Open)
            | '>' -> Some (Angle, Close)
            | '{' -> Some (Curly, Open)
            | '}' -> Some (Curly, Close)
            | _ -> None

        let toCode bracket =
            if not (isValid bracket) then invalidArg "bracket" "Empty bracket is invalid"
            let (SBracket lst) = bracket
            lst |> List.map pairToChar |> String.concat ""
        (*let parse s =
            let lst = s |> Seq.map (fun p -> charToPair p, p) |> Seq.toList
            if lst |> List.exists (fun Option.isNone then
                Error(sprintf "Invalid bracket symbols %A" (lst |> List.filter Option.isNone |> *)
            

            
    type Divider =
        | Comma
        | Semicolon
        member __.ToCode() =
            match __ with
            | Comma -> ","
            | Semicolon -> ";"
    
    type LiteralValue =
        | I8 of int8 | U8 of uint8 | I16 of int16 | U16 of uint16 | I32 of int32 | U32 of uint32 | I64 of int64 | U64 of uint64
        | F32 of float32 | F64 of float
        | CH of char | STR of string

    type Token =
        | Keyword of Keywords * uint32 
        | Bracket of SBracket
         
//type Identifier() 

//type CompilationUnit() =
    