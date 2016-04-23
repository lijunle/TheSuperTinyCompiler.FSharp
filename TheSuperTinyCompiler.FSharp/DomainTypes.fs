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
