[<AutoOpen>]
module DomainTypes

type Input = {
    Value: string
    Index: int
}

type Result<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> =
    | Parser of (Input -> Result<'a * Input>)

type Node =
    | Program of Node list
    | CallExpression of string * Node list
    | NumberLiteral of int

type CNode =
    | Program of body : CNode list
    | ExpressionStatement of expression : CNode
    | CallExpression of callee : CNode * args : CNode list
    | Identifier of name : string
    | NumberLiteral of value : int
