module Program

open Compiler

let (>>=) = Result.(>>=)

[<EntryPoint>]
let main argv =
    let input = "(add 8 (subtract 4 2))"
    printfn "%s" input

    compile input >>= (fun code ->
    printfn "%s" code
    Result.ret ())
    |> ignore

    0 // return an integer exit code
