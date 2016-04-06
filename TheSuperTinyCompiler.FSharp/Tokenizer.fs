module Tokenizer

open System

type Token = 
    | LeftParen
    | RightParen
    | Number of int
    | Name of string

// http://stackoverflow.com/questions/3722591/pattern-matching-on-the-beginning-of-a-string-in-f
let (|Prefix|_|) (p : string) (s : string) = 
    if s.StartsWith(p) then Some(s.Substring(p.Length))
    else None

let (|ExtractNumber|_|) (s : string) = 
    let rec ExtractNumber' (n : int) (rest : string) = 
        match Int32.TryParse(rest.[0..0]) with
        | true, v -> ExtractNumber' (n * 10 + v) (rest.[1..])
        | false, _ -> (n, rest)
    match ExtractNumber' 0 s with
    | (0, _) -> None
    | (n, rest) -> Some(n, rest)

let (|ExtractName|_|) (s : string) = 
    let rec ExtractName' (v : string) (rest : string) = 
        if Char.IsLetter(rest.[0]) then 
            ExtractName' (v + rest.[0].ToString()) (rest.[1..])
        else (v, rest)
    match ExtractName' "" s with
    | ("", _) -> None
    | (v, rest) -> Some(v, rest)

let rec tokenizer (input : string) = 
    match input with
    | "" -> []
    | Prefix " " rest -> tokenizer rest
    | Prefix "(" rest -> LeftParen :: tokenizer rest
    | Prefix ")" rest -> RightParen :: tokenizer rest
    | ExtractNumber(n, rest) -> Number n :: tokenizer rest
    | ExtractName(v, rest) -> Name v :: tokenizer rest
    | _ -> [] // exception?!
