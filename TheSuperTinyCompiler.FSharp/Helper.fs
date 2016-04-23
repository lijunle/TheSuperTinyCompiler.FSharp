module Helper

let (.>>.) = Parser2.(.>>.)
let (<|>) = Parser2.orElse
let (|>>) = Parser2.(|>>)
let (>>.) = Parser2.(>>.)
let (.>>) = Parser2.(.>>)

let char c =
    let equal first =
        c = first
    let message first =
        sprintf "Expect %c, actual %c" c first
    Parser2.satisfy equal message

let anyOf charList =
    charList
    |> List.map char
    |> List.reduce Parser2.orElse

let space =
    let message first =
        sprintf "Expect space, actual %c" first
    Parser2.satisfy System.Char.IsWhiteSpace message

let spaces =
    Parser2.many space

let charListToString chars =
    System.String(List.toArray chars)

let string (s : string) =
    s
    |> List.ofSeq
    |> List.map char
    |> Parser2.sequenceList
    |>> charListToString

let integer =
    let firstDigit = anyOf ['1'..'9']
    let digits = anyOf ['0'..'9']
    let digitsToInteger digits =
        digits |> List.toArray |> System.String |> int

    // Only support positive integer now.
    firstDigit .>>. Parser2.many digits
    |>> List.Cons
    |>> digitsToInteger
    |>> Node.NumberLiteral

let leftParen = char '('

let rightParen = char ')'

let name =
    Parser2.many (anyOf ['a'..'z'])
    |>> charListToString

let value' =
    let p = Parser2.fail "Not implemented"
    let ref = ref p
    let fn input = Parser2.run !ref input // forward reference
    (Parser fn, ref)

let (value, valueRef) = value'

let call =
    leftParen >>. name .>>. Parser2.many (spaces >>. value) .>> rightParen
    |>> Node.CallExpression

valueRef :=
    call <|> integer

let program =
    Parser2.many value

let test s =
    let input = Input.init s
    let result = Parser2.run program input
    match result with
    | Failure e -> printfn "%s" e
    | Success (v, next) -> printfn "Got %A, next index %A" v next.Index
