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

module Parser =
    let run parser input =
        let (Parser fn) = parser
        fn input

    let ret v =
        let fn input =
            Success (v, input)
        Parser fn

    let andThen parser1 parser2 =
        let fn input1 =
            let result1 = run parser1 input1
            match result1 with
            | Failure e -> Failure e
            | Success (a, input2) ->
                let result2 = run parser2 input2
                match result2 with
                | Failure e -> Failure e
                | Success (b, input3) ->
                    let v = (a, b)
                    Success (v, input3)
        Parser fn

    let map f p =
        let fn input =
            let result = run p input
            match result with
            | Failure e -> Failure e
            | Success (a, next) -> Success (f a, next)
        Parser fn

    let apply f p =
        let (>>) = andThen
        f >> p
        |> map (fun (g,q) -> g q)

    let sequenceList list =
        let (<*>) = apply
        let cons head tail = head :: tail

        let initialValue = ret []
        let folder element result = (ret cons) <*> element <*> result

        List.foldBack folder list initialValue

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
    |> Parser.sequenceList
    |> Parser.map charListToString

let test p s =
    let input = Input.init s
    let parser = parseString p
    let result = Parser.run parser input
    match result with
    | Failure e -> printfn "%s" e
    | Success (v, next) -> printfn "Got %s, next index %A" v next.Index
