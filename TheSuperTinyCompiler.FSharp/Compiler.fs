module Compiler

open Transformer
open CodeGenerator

let (>>=) = Result.(>>=)

let compile input =
    SimpleParser.parse input >>= (fun node ->
    let cnode = transformer node
    let code = generate cnode
    Result.ret code)
