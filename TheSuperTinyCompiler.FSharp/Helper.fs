[<AutoOpen>]
module Helper

[<AutoOpen>]
module DomainTypes =
    type Input = {
        Value: string
        Index: int
    }

    type Result<'a> =
    | Success of 'a
    | Failure of string

    type Parser<'a> = Parser of (Input -> Result<'a * Input>)

module Input =
    let init s = { Value = s; Index = 0 }
    let next input = { input with Index = input.Index + 1 }
    let first input = input.Value.[input.Index]
    let empty input = input.Index = input.Value.Length

let parseChar c =
    let fn input =
        if Input.empty input then
            Failure "No more input"
        elif c = Input.first input then
            let next = Input.next input
            Success (c, next)
        else
            let message = sprintf "Expect %c, actual %c" c (Input.first input)
            Failure message
    Parser fn
