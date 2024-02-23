

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
    funkcja zużywa logarytmicznie wiele pamięci,
    bo na kazdym z (log n) pięter jest tylko 1 unikalny node

*)

module M = Map.Make(Int)

let logint x =
  let fx = Float.of_int x in 
  let lf = Float.log2 fx in 
  Float.to_int lf

let cbt n v = 
  let rec build n v m = 
    begin match M.find_opt n m with 
    | Some(t) -> t, m
    | None when n = 0 ->
      let t = Leaf in
      t, M.add n t m
    | None ->
      let (ln, rn) = split n in 
      let (lt, m) = build ln v m in 
      let (rt, m) = build ln v m in 
      let t = Node(lt, v, rt) in 
      t, M.add n t m
    end
  in fst (build n v M.empty)

