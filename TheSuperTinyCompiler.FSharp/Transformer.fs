module Transformer

open Parser

type CNode =
    | Program of body : CNode list
    | ExpressionStatement of expression : CNode
    | CallExpression of callee : CNode * args : CNode list
    | Identifier of name : string
    | NumberLiteral of value : int

let transformer (ast : Node) : CNode =
    let wrapCallExpression parent callExpression =
        match parent with
        | Some (Node.CallExpression _) -> callExpression
        | _ -> ExpressionStatement callExpression

    let rec traverser (parent : Node option) (node : Node) =
        let traverserList = List.map (traverser (Some node))
        match node with
        | Node.NumberLiteral value -> NumberLiteral value
        | Node.Program body -> Program (traverserList body)
        | Node.CallExpression (name, param) ->
            let callExpression = CallExpression (Identifier name, traverserList param)
            wrapCallExpression parent callExpression

    traverser None ast
