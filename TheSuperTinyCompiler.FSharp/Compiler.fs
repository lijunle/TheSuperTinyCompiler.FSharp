module Compiler

open Tokenizer
open Parser
open Transformer
open CodeGenerator

let compile input =
    let tokens = tokenizer input
    let node = parser tokens
    let cnode = transformer node
    let code = generate cnode
    code
