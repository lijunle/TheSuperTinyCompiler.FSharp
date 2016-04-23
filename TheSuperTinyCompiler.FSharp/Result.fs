module Result

let ret r =
    Success r

let bind f r =
    match r with
    | Failure e -> Failure e
    | Success v -> f v

let (>>=) r f = bind f r

type Builder() =
    member this.Bind (r, f) = bind f r
    member this.Return x = ret x

let result = new Builder()
