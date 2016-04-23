module Program

open Compiler

let (>>=) = Result.(>>=)

[<EntryPoint>]
let main argv =
    let input = "(add 8 (subtract 4 2))"
    printfn "%s" input

    Result.result {
        let! code = compile input
        return printfn "%s" code
    } |> ignore

    0 // return an integer exit code
