module Input

let init s =
    { Value = s; Index = 0 }

let next input =
    { input with Index = input.Index + 1 }

let first input =
    input.Value.[input.Index]

let empty input =
    input.Index = input.Value.Length
