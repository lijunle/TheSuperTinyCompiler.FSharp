module SimpleParser

let (.>>.) = Parser.(.>>.)
let (<|>) = Parser.orElse
let (|>>) = Parser.(|>>)
let (>>.) = Parser.(>>.)
let (.>>) = Parser.(.>>)

let integer =
    StandardParser.integer
    |>> Node.NumberLiteral

let leftParen = StandardParser.char '('

let rightParen = StandardParser.char ')'

let name =
    Parser.many (StandardParser.anyOf ['a'..'z'])
    |>> StandardParser.charListToString

let value' =
    let p = Parser.fail "Not implemented"
    let ref = ref p
    let fn input = Parser.run !ref input // forward reference
    (Parser fn, ref)

let (value, valueRef) = value'

let spaces = StandardParser.spaces

let call =
    leftParen >>. name .>>. Parser.many (spaces >>. value) .>> rightParen
    |>> Node.CallExpression

valueRef :=
    call <|> integer

let program =
    Parser.many value
    |>> Node.Program

let parse s =
    let input = Input.init s
    let result = Parser.run program input
    match result with
    | Failure e -> Failure e
    | Success (v, next) -> Success v
