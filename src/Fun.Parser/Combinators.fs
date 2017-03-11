namespace Fun.Parser

open Fun
open Fun.Ast
open FParsec

module Combinators =
    /// <summary>
    /// hexDigit      ::= ‘0’ | … | ‘9’ | ‘A’ | … | ‘F’ | ‘a’ | … | ‘f’
    /// UnicodeEscape ::= ‘\’ ‘u’ hexDigit hexDigit hexDigit hexDigit
    /// </summary>
    
    let UnicodeEscape stream  = 
        let hex2int (c : char) = (int c &&& 15) + (int c >>> 6) * 9
        let hex42char h3 h2 h1 h0 = (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0 |> char 
        (pstring "\\u" >>. pipe4 hex hex hex hex hex42char |>> fun c -> c, 6u) stream
    
    let opchar stream = anyOf Compiler.OperatorChars stream
    
    let bracket br stream = pstring (Bracket.toCode br) stream

    

