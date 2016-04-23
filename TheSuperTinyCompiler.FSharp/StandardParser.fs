module StandardParser

let (.>>.) = Parser.(.>>.)
let (|>>) = Parser.(|>>)

let char c =
    let equal first =
        c = first
    let message first =
        sprintf "Expect %c, actual %c" c first
    Parser.satisfy equal message

let anyOf charList =
    charList
    |> List.map char
    |> List.reduce Parser.orElse

let space =
    let message first =
        sprintf "Expect space, actual %c" first
    Parser.satisfy System.Char.IsWhiteSpace message

let spaces =
    Parser.many space

let charListToString chars =
    System.String(List.toArray chars)

let string (s : string) =
    s
    |> List.ofSeq
    |> List.map char
    |> Parser.sequenceList
    |>> charListToString

let integer =
    let firstDigit = anyOf ['1'..'9']
    let digits = anyOf ['0'..'9']
    let digitsToInteger digits =
        digits |> List.toArray |> System.String |> int

    // Only support positive integer now.
    firstDigit .>>. Parser.many digits
    |>> List.Cons
    |>> digitsToInteger
