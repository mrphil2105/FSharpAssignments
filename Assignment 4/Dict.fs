module Dictionary

type Dict =
    | D of string list

let empty () = D []
let insert (s: string) (D dict) = D (s::dict)
let rec lookup (s: string) (D dict) =
    match dict with
    | [] -> false
    | x :: _ when x = s -> true
    | _ :: xs -> lookup s (D xs)
