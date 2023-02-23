open System

type aExp =
    | N of int // Integer value
    | V of string // Variable
    | WL // Length of the word
    | PV of aExp // Point value of character at specific word index
    | Add of aExp * aExp // Addition
    | Sub of aExp * aExp // Subtraction
    | Mul of aExp * aExp // Multiplication

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

// 3.1
let rec arithEvalSimple (a: aExp) =
    match a with
    | N n -> n
    | Add (a, b) -> arithEvalSimple a + arithEvalSimple b
    | Sub (a, b) -> arithEvalSimple a - arithEvalSimple b
    | Mul (a ,b) -> arithEvalSimple a * arithEvalSimple b

// 3.2
let rec arithEvalState (a: aExp) (s: Map<string, int>) =
    match a with
    | N n -> n
    | V v -> Option.defaultValue 0 (Map.tryFind v s)
    | Add (a, b) -> arithEvalState a s + arithEvalState b s
    | Sub (a, b) -> arithEvalState a s - arithEvalState b s
    | Mul (a, b) -> arithEvalState a s * arithEvalState b s

// 3.3
type word = (char * int) list
let hello: word = [('H',4);('E',1);('L',1);('L',1);('O',1)]
let rec arithEval (a: aExp) (w: word) (s: Map<string, int>) =
    match a with
    | N n -> n
    | V v -> Option.defaultValue 0 (Map.tryFind v s)
    | WL -> w.Length
    | PV x -> snd w.[arithEval x w s]
    | Add (a, b) -> arithEval a w s + arithEval b w s
    | Sub (a, b) -> arithEval a w s - arithEval b w s
    | Mul (a, b) -> arithEval a w s * arithEval b w s

// 3.4
type cExp =
    | C of char (* Character value *)
    | ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
    | ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
    | CV of aExp (* Character lookup at word index *)

let rec charEval (c: cExp) (w: word) (s: Map<string, int>) =
    match c with
    | C c -> c
    | ToUpper c -> Char.ToUpper (charEval c w s)
    | ToLower c -> Char.ToLower (charEval c w s)
    | CV a -> fst w.[arithEval a w s]

// 3.5
type bExp =
    | TT (* true *)
    | FF (* false *)
    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)
    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)
    | IsDigit of cExp (* check for digit *)
    | IsLetter of cExp (* check for letter *)
    | IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let rec boolEval (b: bExp) (w: word) (s: Map<string, int>) =
    match b with
    | TT -> true
    | FF -> false
    | AEq (a, b) -> arithEval a w s = arithEval b w s
    | ALt (a, b) -> arithEval a w s < arithEval b w s
    | Not b -> not (boolEval b w s)
    | Conj (a, b) -> boolEval a w s && boolEval b w s
    | IsDigit c -> Char.IsDigit (charEval c w s)
    | IsLetter c -> Char.IsLetter (charEval c w s)
    | IsVowel c -> "aeiouæøåAEIOUÆØÅ".Contains (charEval c w s)

// 3.6
let isConsonant (c: cExp) = Not (IsVowel c)
