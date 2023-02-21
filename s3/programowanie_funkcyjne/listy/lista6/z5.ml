
let rec fold_left_cps (f : ('a -> 'b -> ('a -> 'c) -> 'c)) (acc : 'a) (xs : 'b list) (cont : 'a -> 'c) =
  match xs with
  | [] -> cont acc
  | x :: xs -> let ncont = (fun (acc : 'a) -> fold_left_cps f acc xs cont) in f acc x ncont 


  let for_all p xs =
    let worker = fun acc x cont ->
      if p x then cont acc else false in
    fold_left_cps worker true xs (fun x -> x)
  
  let mult_list xs =
    let worker = (fun acc x cont ->
      if x = 0 then 0 else cont (acc + x)) in
    fold_left_cps worker 0 xs (fun x -> x)
  
  
  let sorted xs =
    let worker = (fun (res, prev) x cont ->
      begin match prev with
      | None -> (res, Some x)
      | Some z -> if x <= z then (false, None) else cont (res, (Some x)) end) in
      fold_left_cps worker (true, None) xs (fun x -> x) |> fst
      