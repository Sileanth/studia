

let a = 0
let b = 1
let l = [b; a]
let f ls =
    let (x::y::_) = ls 
    (x+y)::ls

let g f x = f ( f x)
let n = g ( g ( g f)  ) l


//val a: int = 0
//val b: int = 1
//val l: int list = [1; 0]
//val f: ls: int list -> int list
//val g: f: ('a -> 'a) -> x: 'a -> 'a
//val n: int list = [34; 21; 13; 8; 5; 3; 2; 1; 1; 0]


let fs x =
    match x with
    | (a, b, c) -> a

let mid x =
    match x with
    | (a, b, c) -> b

let lst x =
    match x with
    | (a, b, c) -> c



let rec factorial n =
    match n with
    | 0 -> 1
    | n -> n * (factorial (n-1))

let x = factorial 5

let rec binomialCoefficient n k = (factorial n)/((factorial k)
    