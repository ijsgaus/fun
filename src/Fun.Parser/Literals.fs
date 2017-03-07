namespace Fun.Parser
module Listerals =
    open FParsec
    open Tools
    open Fun.Ast
    let int32Literal = takeRange pint32 (fun v pos -> Int32(v, pos))
    let uint32Literal = takeRange (puint32 .>> skipString "u") (fun v pos -> UInt32(v, pos))
    let int8Literal = takeRange (pint8 .>> skipString "y") (fun v pos -> Int8(v, pos))
    let uint8Literal = takeRange (puint8 .>> skipString "uy") (fun v pos -> UInt8(v, pos))
    let int16Literal = takeRange (pint16 .>> skipString "s") (fun v pos -> Int16(v, pos))
    let uint16Literal = takeRange (puint16 .>> skipString "us") (fun v pos -> UInt16(v, pos))
    let int64Literal = takeRange (pint64 .>> skipString "L") (fun v pos -> Int64(v, pos))
    let uint64Literal = takeRange (puint64 .>> skipString "UL") (fun v pos -> UInt64(v, pos))
    let floatLiteral = takeRange pfloat (fun v pos -> Float(v, pos))
    let float32Literal = takeRange (pfloat .>> skipString "f") (fun v pos -> Float32(float32(v), pos))