module Compiler

open Transformer
open CodeGenerator

let compile input =
    let node = SimpleParser.parse input
    match node with
    | Failure e -> e // TODO
    | Success node ->
        let cnode = transformer node
        let code = generate cnode
        code
