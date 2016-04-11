module Program

open Compiler

[<EntryPoint>]
let main argv =
    let input = "(add 8 (subtract 4 2))"
    printfn "%s" input

    let code = compile input
    printfn "%s" code

    0 // return an integer exit code
