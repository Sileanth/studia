

let revmerge cmp xs ys = 
  let rec it xs ys acc = 
    match xs, ys with 
    | [], ys -> List.rev_append ys acc 
    | xs, [] -> List.rev_append xs acc 
    | x :: xxs, y :: yys when cmp x y ->
      it xxs ys (x :: acc)
    | xs, y :: yys -> it xs yys (y :: acc)
    in it xs ys []



let asc_cmp x y = x <= y 
let desc_cmp x y = y <= x

let initial_cmps = asc_cmp, desc_cmp


(*
TODO
   potrzebny jakis wzorek z logiem od jakiego cmp zaczac
*)
let mergesort xs = 
  let xss = List.map (fun x -> [x]) xs 
  in let rec merge_row cmp xss acc = 
    match xss with
    | [] -> acc 
    | [xs] -> xs :: acc 
    | xs :: ys :: xss -> 
      merge_row cmp xss (revmerge cmp xs ys :: acc)
  in let rec it (cur_cmp, fut_cmp) xss = 
    match xss with 
    | [] -> []
    | [xs] -> xs
    | xss -> it (fut_cmp, cur_cmp) (merge_row cur_cmp xss [])
  in it initial_cmps xss