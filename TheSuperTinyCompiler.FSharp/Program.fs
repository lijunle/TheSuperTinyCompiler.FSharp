module Program

open Tokenizer

[<EntryPoint>]
let main argv = 
    let input = "(add 8 (subtract 4 2))"
    printfn "%A" input
    let tokens = tokenizer input
    printfn "%A" tokens
    0 // return an integer exit code
