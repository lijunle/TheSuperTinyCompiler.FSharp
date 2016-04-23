module SimpleParser

let (.>>.) = Parser2.(.>>.)
let (<|>) = Parser2.orElse
let (|>>) = Parser2.(|>>)
let (>>.) = Parser2.(>>.)
let (.>>) = Parser2.(.>>)

let integer =
    StandardParser.integer
    |>> Node.NumberLiteral

let leftParen = StandardParser.char '('

let rightParen = StandardParser.char ')'

let name =
    Parser2.many (StandardParser.anyOf ['a'..'z'])
    |>> StandardParser.charListToString

let value' =
    let p = Parser2.fail "Not implemented"
    let ref = ref p
    let fn input = Parser2.run !ref input // forward reference
    (Parser fn, ref)

let (value, valueRef) = value'

let spaces = StandardParser.spaces

let call =
    leftParen >>. name .>>. Parser2.many (spaces >>. value) .>> rightParen
    |>> Node.CallExpression

valueRef :=
    call <|> integer

let program =
    Parser2.many value
    |>> Node.Program

let parse s =
    let input = Input.init s
    let result = Parser2.run program input
    match result with
    | Failure e -> Failure e
    | Success (v, next) -> Success v
