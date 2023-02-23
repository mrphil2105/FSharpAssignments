open System

// 1.1
let sqr x = x * x

// 1.2
let pow x n = Math.Pow(x, n)

// 1.3
let rec sum =
    function
    | 0 -> 0
    | n -> n + sum (n - 1)

// 1.4
let rec fib =
    function
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)

// 1.5
let dup (s: string) = s + s

// 1.6
let rec dupn s n =
    match n with
    | 0 -> ""
    | _ -> s + dupn (s) (n - 1)

// 1.7
let rec bin (n, k) =
    if n = k || k = 0 then
        1
    else
        bin (n - 1, k - 1) + bin (n - 1, k)

// 1.8
let timediff (h, m) (h2, m2) = (h2 - h) * 60 + (m2 - m)

// 1.9
let minutes (hh, mm) = timediff (0, 0) (hh, mm)

// 1.10
let curry (f: 'a * 'b -> 'c) a b = f (a, b)
let uncurry (f: 'a -> 'b -> 'c) (a, b) = f a b

// 1.11
let empty (letter, points) pos = (letter, points)
let theLetterA : int -> char * int = empty ('A', 1)

Console.WriteLine(theLetterA 0)

Console.WriteLine(minutes (23, 1))
Console.WriteLine(timediff (12, 34) (11, 35))
Console.WriteLine(curry (fun (x, y) -> x + y) 5 3)
Console.WriteLine(uncurry (fun x y -> x + y) (5, 3))

printfn "%s" (dup "Hej ")
