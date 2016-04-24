module Program

open Compiler

let (>>=) = Result.(>>=)

[<EntryPoint>]
let main argv =
    let input = "(add 8 (subtract 4 2))"
    printfn "%s" input

    let code = compile input
    match code with
    | Success v -> printfn "%s" v
    | Failure e -> printfn "[FAIL] %s" e

    0 // return an integer exit code
