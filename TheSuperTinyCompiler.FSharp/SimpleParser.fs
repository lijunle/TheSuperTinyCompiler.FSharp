module SimpleParser

let (.>>.) = Parser.(.>>.)
let (<|>) = Parser.orElse
let (|>>) = Parser.(|>>)
let (>>.) = Parser.(>>.)
let (.>>) = Parser.(.>>)

let integer =
    StandardParser.integer
    |>> Node.NumberLiteral

let leftParen = StandardParser.character '('

let rightParen = StandardParser.character ')'

let name =
    let charsA2Z = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']
    Parser.many (StandardParser.anyOf charsA2Z)
    |>> StandardParser.charListToString

let valueWrapper =
    let p = Parser.fail "Not implemented"
    let ref = ref p
    let fn input = Parser.run !ref input // forward reference
    (Parser fn, ref)

let (value, valueRef) = valueWrapper

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
