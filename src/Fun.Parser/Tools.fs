namespace Fun.Parser
open Fun.Ast


module Tools =
    open FParsec
    
    let withWidth parser f = 
        pipe3 getPosition parser getPosition <| fun p1 res p2 ->
            let width = uint32(p2.Index - p1.Index)
            f res width
    let attemptChoice lst =
        lst |> Seq.map attempt |> choice

        
    


    
    