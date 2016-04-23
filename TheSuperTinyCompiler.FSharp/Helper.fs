module Helper

let (.>>.) = Parser2.(.>>.)
let (|>>) = Parser2.(|>>)
let (>>.) = Parser2.(>>.)
let (.>>) = Parser2.(.>>)

let parseChar c =
    let equal first =
        c = first
    let message first =
        sprintf "Expect %c, actual %c" c first
    Parser2.satisfy equal message

let parseString (s : string) =
    let charListToString chars =
        System.String(List.toArray chars)

    s
    |> List.ofSeq
    |> List.map parseChar
    |> Parser2.sequenceList
    |>> charListToString

let anyOf charList =
    charList
    |> List.map parseChar
    |> List.reduce Parser2.orElse

let parseInteger =
    let firstDigit = anyOf ['1'..'9']
    let digits = anyOf ['0'..'9']
    let digitsToInteger digits =
        digits |> List.toArray |> System.String |> int

    // Only support positive integer now.
    firstDigit .>>. Parser2.many digits
    |>> List.Cons
    |>> digitsToInteger

let parseSpace =
    let message first =
        sprintf "Expect space, actual %c" first
    Parser2.satisfy System.Char.IsWhiteSpace message

let parseSpaces =
    Parser2.many parseSpace

let test s =
    let input = Input.init s
    let leftParen = parseChar '('
    let rightParen = parseChar ')'
    let integerPair = parseInteger .>> parseSpaces .>>. parseInteger
    let parser = Parser2.between leftParen integerPair rightParen
    let result = Parser2.run parser input
    match result with
    | Failure e -> printfn "%s" e
    | Success (v, next) -> printfn "Got %A, next index %A" v next.Index
