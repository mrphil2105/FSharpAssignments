// 2.1

open System

let rec downto1 x =
    if x > 0 then x :: downto1 (x-1)
    else []

let rec downto2 =
    function
    | 0 -> []
    | x -> x :: downto2 (x-1)

// 2.2
let rec removeOddIdx =
    function
    | [] -> []
    | [x] -> [x]
    | x :: _ :: ys -> x :: removeOddIdx ys

// 2.3
let rec combinePair =
    function
    | [] | [_] -> []
    | x :: y :: zs -> (x, y) :: combinePair zs

// 2.4
type complex = float * float

let mkComplex a b = complex (a, b)

let complexToPair ((a, b): complex) = (a, b)

let (|+|) ((a, b): complex) ((c, d): complex) = (a+c, b + d);;
let (|*|) ((a, b): complex) ((c, d): complex) = (a*c-b*d, b*c+a*d);;

let (|-|) c ((a, b): complex) = c |+| (-a, -b);;
let (|/|) c ((a, b): complex) = c |*| (a/(a*a+b*b), -b/(a*a+b*b));;

// 2.5
let explode1 (s: string) =
    let arr = s.ToCharArray()
    List.ofArray arr

let rec explode2 =
    function
    | "" -> []
    | s -> s.Chars(0) :: explode2(s.Remove(0,1))

// 2.6
let implode cs = List.fold (fun (s: string) (c: char) -> s + string c) "" cs

let implodeRev cs = List.foldBack (fun (c: char) (s: string) -> s + string c) cs ""

// 2.7
let toUpper = explode1 >> List.map Char.ToUpper >> implode

// 2.8
let rec ack =
    function
    | 0, n -> n+1
    | m, 0 -> ack (m-1,1)
    | m, n -> ack (m-1, ack (m, n-1))

// 2.9
let time f =
    let start = DateTime.Now
    let res = f()
    let finish = DateTime.Now
    (res, finish-start)

let timeArg1 f a = (fun() -> f a) |> time

// 2.10
let rec downto3 f n e =
    match n with
    | n when n > 0 -> downto3 f (n-1) (f n e)
    | _ -> e

let fac n = downto3 (fun x acc -> x*acc) n 1

let range g n =
    if n > 0 then downto3 (fun x acc -> (g x)::acc) n []
    else []
