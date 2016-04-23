module Helper

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

let parseString (s : string) =
    let charListToString chars =
        System.String(List.toArray chars)

    s
    |> List.ofSeq
    |> List.map parseChar
    |> Parser2.sequenceList
    |> Parser2.map charListToString

let anyOf charList =
    charList
    |> List.map parseChar
    |> List.reduce Parser2.orElse

let parseInteger =
    let (>>) = Parser2.andThen
    let firstDigit = anyOf ['1'..'9']
    let digits = anyOf ['0'..'9']
    let digitsToInteger digits =
        digits |> List.toArray |> System.String |> int

    // Only support positive integer now.
    firstDigit >> Parser2.many digits
    |> Parser2.map List.Cons
    |> Parser2.map digitsToInteger

let test p s =
    let input = Input.init s
    let parser = Parser2.between (parseChar '(') parseInteger (parseChar ')')
    let result = Parser2.run parser input
    match result with
    | Failure e -> printfn "%s" e
    | Success (v, next) -> printfn "Got %d, next index %A" v next.Index
