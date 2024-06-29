



let rec fold_left_cps (f : ('a -> 'b -> ('a -> 'c) -> 'c)) (acc : 'a) (xs : 'b list) (cont : 'a -> 'c) =
  match xs with
  | [] -> cont acc
  | x :: xs -> let ncont = (fun (acc : 'a) -> fold_left_cps f acc xs cont) in f acc x ncont 