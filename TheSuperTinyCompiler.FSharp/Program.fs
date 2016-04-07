module Program

open Tokenizer
open Parser
open Transformer

[<EntryPoint>]
let main argv = 
    let input = "(add 8 (subtract 4 2))"
    printfn "%A" input

    let tokens = tokenizer input
    printfn "%A" tokens

    let node = parser tokens
    printfn "%A" node

    let cnode = transformer node
    printfn "%A" cnode

    0 // return an integer exit code
