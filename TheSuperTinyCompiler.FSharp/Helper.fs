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

module Result =
    let bind f r =
        match r with
        | Failure e -> Failure e
        | Success v -> f v

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

    let orElse parser1 parser2 =
        let fn input =
            let result1 = run parser1 input
            match result1 with
            | Success v -> Success v
            | Failure e -> run parser2 input
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

    let between p1 p2 p3 =
        let (>>=) result f = Result.bind f result
        let fn input1 =
            run p1 input1 >>= (fun (_, input2) ->
            run p2 input2 >>= (fun (v, input3) ->
            run p3 input3 >>= (fun (_, input4) ->
            Success (v, input4))))
        Parser fn

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

let anyOf charList =
    charList
    |> List.map parseChar
    |> List.reduce Parser.orElse

let many p =
    let rec loop ret input =
        let result = Parser.run p input
        match result with
        | Failure e -> (ret, input)
        | Success (v, next) ->
            loop (v :: ret) next
    let fn input =
        let (ret, next) = loop [] input
        Success (List.rev ret, next)
    Parser fn

let parseInteger =
    let (>>) = Parser.andThen
    let firstDigit = anyOf ['1'..'9']
    let digits = anyOf ['0'..'9']
    let digitsToInteger digits =
        digits |> List.toArray |> System.String |> int

    // Only support positive integer now.
    firstDigit >> many digits
    |> Parser.map List.Cons
    |> Parser.map digitsToInteger

let test p s =
    let input = Input.init s
    let parser = Parser.between (parseChar '(') parseInteger (parseChar ')')
    let result = Parser.run parser input
    match result with
    | Failure e -> printfn "%s" e
    | Success (v, next) -> printfn "Got %d, next index %A" v next.Index
