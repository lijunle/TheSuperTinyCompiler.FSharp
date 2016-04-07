module Program

open Tokenizer
open Parser

[<EntryPoint>]
let main argv = 
    let input = "(add 8 (subtract 4 2))"
    printfn "%A" input

    let tokens = tokenizer input
    printfn "%A" tokens

    let node = parser tokens
    printfn "%A" node

    0 // return an integer exit code
