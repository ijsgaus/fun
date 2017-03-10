module Fun.Ast 

type Width = uint32

type IAstNode =
    abstract Width : Width



type LiteralValue =
    | I8 of int8
    | U8 of uint8
    | I16 of int16
    | U16 of uint16
    | I32 of int32
    | U32 of uint32
    | I64 of int64
    | U64 of uint64
    | F32 of float32
    | F64 of float
    | CH of char
    | STR of string

type LiteralNode = 
    | LiteralNode of LiteralValue * Width 
    member __.Width = let (LiteralNode (_, w)) = __ in w
    member __.Value = let (LiteralNode (v, _)) = __ in v
    interface IAstNode with
        member __.Width = __.Width
            

type IdentifierNode =
    | IdentifierNode of string * string list * Width
    member __.Width =  let (IdentifierNode (_, _, w)) = __ in w
    member __.IsMultipart = 
        match __ with 
        | IdentifierNode(_, [], _) -> false
        | _ -> true
    member __.Parts = let (IdentifierNode (n, p,_)) = __ in n :: p
    interface IAstNode with
        member __.Width = __.Width
            


        

