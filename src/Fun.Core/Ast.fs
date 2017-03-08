module Fun.Ast 

type Width = uint32

type IAstItem =
    abstract Width : Width
    
type IAstNode =
    inherit IAstItem


type LiteralValue =
    | Int8 of int8
    | UInt8 of uint8
    | Int16 of int16
    | UInt16 of uint16
    | Int32 of int32
    | UInt32 of uint32
    | Int64 of int64
    | UInt64 of uint64
    | Float32 of float32
    | Float of float
    | Char of char
    | String of string

type LiteralNode = 
    | LiteralNode of value: LiteralValue * width : Width
    interface IAstNode with
        member __.Width = 
            match __ with 
            | LiteralNode(_, w) -> w

        

