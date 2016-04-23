module Compiler

open Transformer
open CodeGenerator

let (>>=) = Result.(>>=)

let compile input =
    Result.result {
        let! node = SimpleParser.parse input
        let cnode = transformer node
        let code = generate cnode
        return code
    }
