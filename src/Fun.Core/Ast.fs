namespace Fun

module Ast = 
    type SourcePos = {
        // 0 - based offset in stream
        Offset : uint64
        // 1 - based line position
        Line : uint32
        // 2 - based column position
        Column : uint32
    }

    type SourceRange = 
        | SourceRange of SourcePos * SourcePos

    

    type AstSourceKind =
        | SourceFile of string


    type AstSource =
        | SourceFile of string * SourceRange

    type IHasSource = 
        abstract Source : AstSource

    type NumberLiteral =
        | Int8 of int8 * AstSource
        | UInt8 of uint8 * AstSource
        | Int16 of int16 * AstSource
        | UInt16 of uint16 * AstSource
        | Int32 of int32 * AstSource
        | UInt32 of uint32 * AstSource
        | Int64 of int64 * AstSource
        | UInt64 of uint64 * AstSource
        | Float32 of float32 * AstSource
        | Float of float * AstSource
        member __.IsWhole = 
                match __ with
                | Int8(_, s) | UInt8(_,s) | Int16(_,s) | UInt16(_,s) | Int32(_, s) | UInt32(_, s) 
                | Int64(_, s) | UInt64(_, s)  -> true
                | Float32(_, s) | Float(_, s) -> false
        member __.IsSigned =
                match __ with
                | Int8(_, s) | Int16(_,s) | Int32(_, s) | Int64(_, s) | Float32(_, s) | Float(_, s) -> true
                | UInt8(_,s) | UInt16(_,s) | UInt32(_, s) | UInt64(_, s)  -> true
        member __.Length =
                match __ with
                | Int8(_, s) | UInt8(_,s) -> 1
                | Int16(_,s) | UInt16(_,s) -> 2
                | Int32(_, s) | UInt32(_, s) -> 4
                | Int64(_, s) | UInt64(_, s) -> 8
                | Float32(_, s) -> 4
                | Float(_, s) -> 8
        interface IHasSource with
            member __.Source =
                match __ with
                | Int8(_, s) | UInt8(_,s) | Int16(_,s) | UInt16(_,s) | Int32(_, s) | UInt32(_, s) 
                | Int64(_, s) | UInt64(_, s) | Float32(_, s) | Float(_, s) -> s
            

    type Literal =
        | Number of NumberLiteral

