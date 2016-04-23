module Result

let ret r =
    Success r

let bind f r =
    match r with
    | Failure e -> Failure e
    | Success v -> f v

let (>>=) r f = bind f r
