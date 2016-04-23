module Result

let bind f r =
    match r with
    | Failure e -> Failure e
    | Success v -> f v
