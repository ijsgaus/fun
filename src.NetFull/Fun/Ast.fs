namespace Fun

open System.Collections.Immutable

type 't arr = ImmutableArray<'t>

module Ast =

    type IHasWidth =
        abstract member Width : uint32

    type IAstNode  = 
        inherit IHasWidth
        abstract member Children : IAstNode arr

    let (|Branch|Leaf|) (node: IAstNode) =
        if node.Children.Length > 0 then Branch else Leaf

    type Keyword =
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
        interface IAstNode with
            member __.Width = __.Width
            member __.Children = arr.Empty
            
        
    type BracketSymbol = internal BracketSymbol of string
        
    module Bracket =
        let BracketChars =
            [
                '(', ')'
                '[', ']'
                '{', '}'
                '<', '>'
            ]

        let isValidCh ch =
            BracketChars |> List.exists (fun (oc, cc) -> oc = ch || cc = ch)
        
        let create str = 
            if (String.length str) = 0 then
                Error("Empty string")
            else
                let noValid = str |> String.filter (isValidCh >> not)
                if noValid.Length = 0 then BracketSymbol(str) |> Ok else sprintf "Invalid bracket chars \"%s\"" noValid |> Error

        let inverse bracket =
            let picker ch pair =
                match pair with
                | oc, cc when oc = ch -> Some(cc)
                | oc, cc when cc = ch -> Some(oc)
                | _ -> None
            let inverseCh ch = BracketChars |> Seq.pick (picker ch)
            let (BracketSymbol str) = bracket
            BracketSymbol(str |> String.map inverseCh)
        
        let toCode bracket = let (BracketSymbol str) = bracket in str

            
            

    
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

    


    type Name =
        private
        | Name of string
        override __.ToString() = let (Name s) = __ in s

    type Path =
        | Path of Name list 

    type Identifier = 
        | Identifier of Path * Name
        | Operator of string

    type Token =
        | Keyword of Keyword * uint32 
        | Bracket of BracketSymbol

    type private StructBag<'t> = { Data : 't }

    type private NodesBag = StructBag<ImmutableArray<IAstNode>>
    let private ChildrenProperty = System.Runtime.CompilerServices.ConditionalWeakTable<IAstNode, NodesBag>()

    type OpenStatement =
        {
            PrefixTrivia : Trivia arr;
            What : Identifier;
            SuffixTrivia : Trivia arr;
        }
        

    module Identifier =
        let initName s =
            let valid1 (str :string) =
                System.Char.IsLetter(s, 0) || ([ '$'; '#'; '_' ] |> List.contains (str.[0])) 
            let validTail (str :string) =
                let validCh ch = System.Char.IsLetterOrDigit(ch) || ch = '_'
                str |> Seq.exists (validCh >> not) |> not
            match s with
            | null -> Error("Name cannot be null")
            | "" -> Error("Name cannot bew empty string")
            | _ when valid1 s |> not -> Error("Name must start from letter or $, #, _")
            | _ when validTail s |> not -> Error("Name must contains only letter and alpha and _")
            | _ -> Ok(Name s)
        

//    type CompilationUnit() =
    