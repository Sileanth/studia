



let rec subl x acc = function
  | [] -> acc 
  | ys :: yss -> 
    subl x ((x :: ys) :: acc) yss

and sublists xss =
  match xss with
| [] -> [[]]
| x :: xs -> 
    let xss = sublists xs in 
    subl x xss xss

type t = Leaf | Node of int * t * t 

let rec flat t acc =
  match t with
  | Leaf -> acc 
  | Node(x, l, r) -> flat l (flat r acc)

let flatten t = flat t []

let rec rev_append xs ys =
  match xs with
  | [] -> ys 
  | x :: xs -> rev_append xs (x :: ys) 

let mrev xs = rev_append xs []


