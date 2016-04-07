module Parser

open Tokenizer

type Node =
    | Program of body: Node list
    | CallExpression of name: string * param: Node list
    | NumberLiteral of value: int

let parser tokens =
    let rec parser' tokens =
        match tokens with
        | LeftParen :: Name name :: rest -> [CallExpression (name, parser' rest)] // space case for only one call
        | Number value :: rest -> NumberLiteral value :: parser' rest
        | _ -> [] // How to handle right parenthesis and syntax error cases?

    Program (parser' tokens)
