namespace Fun.Compiler.Core

type ITracer =
    abstract Enter : routine: string -> parameters: (string * obj) seq -> unit
    abstract Exit : routine: string -> success: bool -> result: obj -> unit

type ICompilationState =
    abstract Tracer : ITracer
