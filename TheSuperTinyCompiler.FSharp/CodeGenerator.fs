module CodeGenerator

let rec traverse ast =
    let traverseThenConcat sep list =
        list |> List.map traverse |> String.concat sep

    match ast with
    | Program body ->  body |> traverseThenConcat "\n"
    | ExpressionStatement expression -> sprintf "%s;" (traverse expression)
    | CallExpression (callee, args) -> sprintf "%s(%s)" (traverse callee) (args |> traverseThenConcat ", ")
    | Identifier name -> name
    | NumberLiteral value -> sprintf "%i" value

let generate (ast : CNode) =
    traverse ast
