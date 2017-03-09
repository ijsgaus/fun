module Fun.Ast 

type Width = uint32

type IAstItem =
    abstract Width : Width
    
type IAstNode =
    inherit IAstItem


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
    member __.Width = 
        match __ with 
        | LiteralNode(_, w) -> w
    member __.Value =
        match __ with 
        | LiteralNode(v, _) -> v
    interface IAstNode with
        member __.Width = __.Width
            

type IdentifierNode =
    | IdentifierNode of parts : string list * width : Width
    member __.Width =
        match __ with 
        | IdentifierNode(_, w) -> w
    member __.IsMultipart =
        match __ with 
        | IdentifierNode([_], _) -> false
        | _ -> true
    member __.Parts = 
        match __ with 
        | IdentifierNode(p, _) -> p
    interface IAstNode with
        member __.Width = __.Width
            


        

