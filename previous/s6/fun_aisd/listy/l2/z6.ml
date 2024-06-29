

let rec revappend xs ys = 
  match xs with
  | [] -> ys
  | x :: xs -> revappend xs (x :: ys)

let revmerge cmp xs ys = 
  let rec it xs ys acc = 
    match xs, ys with 
    | [], ys | ys, [] -> revappend ys acc 
    | x :: xxs, y :: yys when cmp x y ->
      it xxs ys (x :: acc)
    | xs, y :: yys -> it xs yys (y :: acc)
    in it xs ys []



let asc_cmp x y = x <= y 
let desc_cmp x y = y <= x


let log2ceil n =
  let rec helper acc = function
    | 0 | 1 -> acc
    | n -> helper (acc + 1) (n / 2 + n mod 2)
  in helper 0 n

let len xs = 
  let rec aux acc = function
    | [] -> acc
    | _ :: xs -> aux (acc + 1) xs
  in aux 0 xs

let calc_initial_cmps xs = 
  let n = len xs in 
  if log2ceil n mod 2 = 0 then (asc_cmp, desc_cmp)
  else (desc_cmp, asc_cmp)

(*
   0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
   0 0 1 2 2 3 3 3 3 4  4  4  4  4  4  4  4  5

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
  in it (calc_initial_cmps xss) xss

  let tests = [ 3 ; 7 ; 2 ; 14 ; 13 ; 4 ; 2 ;10]

  let stests = mergesort tests;;