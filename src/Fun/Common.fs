namespace Fun

[<AutoOpen>]
module Common =
    
    type UnwrapException(msg : string, inner) = 
        inherit System.Exception(msg, inner)
        new(msg) = UnwrapException(msg, null)

    module Option =
        let unwrap error opt =
            match opt with
            | Some v -> v
            | None -> raise (UnwrapException error)
        let toResult error opt =
            match opt with
            | Some v -> Ok(v)
            | _ -> Error(error)
        let fromResult f res =
            match res with
            | Ok v -> Some(v)
            | Error e -> f e; None
        
    

    module Result =
        let unwrap res =
            match res with
            | Ok a -> a
            | Error e -> 
                match box e with
                | :? System.Exception as ex -> raise (UnwrapException(ex.Message, ex))
                | _ -> raise (UnwrapException (e.ToString()))
        let reduceSeq sq =
            let folder (rl, el) n =
                match n with
                | Ok r -> (r::rl, el)
                | Error e -> (rl, e::el)
            let (rl, el) = Seq.fold folder ([], []) sq 
            (List.rev rl, List.rev el) 
        let ofOption error opt =
            Option.toResult error opt
        let toOption f res =
            Option.fromResult f res

    open System.Runtime.CompilerServices
    open System
    [<Extension>]
    type UnwrapExtenstios private() =
        [<Extension>]
        static member Unwrap(res) = Result.unwrap res
        [<Extension>]
        static member Unwrap(opt, error) = Option.unwrap error opt

        [<Extension>]
        static member ToResult(opt, err) = Option.toResult err opt
        [<Extension>]
        static member ToOption(res) = Option.fromResult ignore res
        [<Extension>]
        static member ToOption(res, err : Action<_>) = Option.fromResult (err.Invoke) res 

    