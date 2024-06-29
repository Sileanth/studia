

type tree = 
| Leaf 
| Node of tree * int * tree

let rec cbt h v =
  match h with 
  | 0 -> Leaf
  | n -> 
    let child = cbt (h-1) v in 
    Node(child, v, child)

(* 
    funkcja zużywa logarytmicznie wiele  pamięci,
    bo na kazdym z (log n) pięter jest tylko 1 unikalny node

*)


type tree = 
| Leaf
| FN of tree * int * tree



let rec bt h v = 
  match h with 
  | 0 -> Leaf
  | n -> let x = bt (h-1) v in
    match x with
    | FN(fn_2,_,fn_1) -> FN (fn_1,v,x)
    | Leaf -> FN(x,v,x)
