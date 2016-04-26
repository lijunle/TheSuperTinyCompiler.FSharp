module StandardParser

let (.>>.) = Parser.(.>>.)
let (|>>) = Parser.(|>>)

let character c =
    let equal first =
        c = first
    let message first =
        sprintf "Expect %c, actual %c" c first
    Parser.satisfy equal message

let anyOf charList =
    charList
    |> List.map character
    |> List.reduce Parser.orElse

let space =
    let isWhiteSpace c =
        c = ' ' || c = '\t' || c = '\r' || c = '\n'
    let message first =
        sprintf "Expect space, actual %c" first
    Parser.satisfy isWhiteSpace message

let spaces =
    Parser.many space

let charListToString chars =
    System.String(List.toArray chars)

let string (s : string) =
    s
    |> List.ofSeq
    |> List.map character
    |> Parser.sequenceList
    |>> charListToString

let integer =
    let firstDigit = anyOf ['1'..'9']
    let digits = anyOf ['0'..'9']
    let digitsToInteger digits =
        digits |> charListToString |> int

    // Only support positive integer now.
    firstDigit .>>. Parser.many digits
    |>> List.Cons
    |>> digitsToInteger
