namespace Fun.Parser

[<RequireQualifiedAccess>]
module Literal =
    open FParsec
    open Tools
    open Fun.Ast

    let private mkLiteral f v w = LiteralNode(f(v), w)

    let literalEnd stream = 
        notFollowedBy (letter <|> digit <|> (anyOf ".")) stream
    
    let i32 stream = (withWidth pint32 <| mkLiteral I32 .>> literalEnd) stream
    
    let u32 stream = (withWidth (puint32 .>> skipString "u") <| mkLiteral U32 .>> literalEnd) stream
    
    let i8 stream = (withWidth (pint8 .>> skipString "y") <| mkLiteral I8 .>> literalEnd) stream
    
    let u8 stream = (withWidth (puint8 .>> skipString "uy") <| mkLiteral U8 .>> literalEnd) stream
    
    let i16 stream = (withWidth (pint16 .>> skipString "s") <| mkLiteral I16 .>> literalEnd) stream
    
    let u16 stream = (withWidth (puint16 .>> skipString "us") <| mkLiteral U16 .>> literalEnd) stream
    
    let i64 stream = (withWidth (pint64 .>>? skipString "L") <| mkLiteral I64 .>>? literalEnd) stream
    
    let u64 stream = (withWidth (puint64 .>>? skipString "UL") <| mkLiteral U64 .>>? literalEnd) stream
    
    let f64 stream = (withWidth pfloat <| mkLiteral F64 .>> literalEnd) stream
    
    let f32 stream = (withWidth (pfloat .>> skipString "f") <| mkLiteral (float32 >> F32) .>> literalEnd) stream

    let number stream = 
         attemptChoice [ 
            i32
            u32
            i8
            u8
            i16
            u16
            i64
            u64
            f64
            f32
         ] stream
    
    let ch stream =  
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '\'')
        let unescape c =
            match c with
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | 'b' -> '\b'
            | c -> c
        let escapedChar = pchar '\\' >>? (anyOf "\\nrtb'" |>> unescape) 
        (*let hexDigit = anyOf "0123456789ABCDEF"
        let toChar (a : char array) =
            let dict = 
                [ ('0', 0); ('1', 1); ('2', 2); ('3', 3); ('4', 4); ('5', 5); ('6', 6); ('7', 7); 
                  ('8', 8); ('9', 9); ('A', 10); ('B', 11); ('C', 12); ('D', 13); ('E', 14); ('F', 15) ] |> Map.ofList
            let i = ((dict.[a.[0]] * 16 + dict.[a.[1]]) * 16 + dict.[a.[2]]) + dict.[a.[3]]
            System.Char.ConvertFromUtf32(i)
        let unicodeChar = (pstring @"\u" <|> pstring @"\U") >>. (parray 4 hexDigit |>> toChar) *)
        let charParser = normalChar <|> escapedChar //<|> unicodeChar
        let trick = pchar '\''
        let fchar = withWidth (between trick trick charParser) <| fun c w -> LiteralNode (CH c, w)
        fchar stream
    let str stream =
        let str = pstring
        let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c // every other char is mapped to itself

        let unicodeEscape =
            /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9

            str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
            )

        let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')


        let ss = between (str "\"") (str "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)
        let fs = withWidth ss (fun s w -> LiteralNode(STR(s), w))
        fs stream
        
    