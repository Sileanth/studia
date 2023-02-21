include List

type ('a, 'b) decision =
| Drop
| Done of 'b
| Redo of 'a

let rec last x xs =
  match xs with
  | []      -> x
  | y :: xs -> last y xs

let fold_map f s l =
  let rec loop acc s xs =
    match xs with
    | [] -> (s, List.rev acc)
    | x :: xs ->
      let (s, y) = f s x in
      loop (y :: acc) s xs
  in loop [] s l

let filter_map f l =
  let rec loop acc xs =
    match xs with
    | [] -> List.rev acc
    | x :: xs ->
      begin match f x with
      | None   -> loop acc xs
      | Some y -> loop (y :: acc) xs
      end
  in loop [] l

let partition_map f l =
  let rec loop acc1 acc2 xs =
    match xs with
    | []      -> (List.rev acc1, List.rev acc2)
    | x :: xs ->
      begin match f x with
      | Either.Left  a -> loop (a :: acc1) acc2 xs
      | Either.Right b -> loop acc1 (b :: acc2) xs
      end
  in loop [] [] l

let fold_left_i f s l =
  let rec loop n s xs =
    match xs with
    | []      -> s
    | x :: xs -> loop (n+1) (f n s x) xs
  in loop 0 s l

let zip xs ys = List.map2 (fun x y -> (x, y)) xs ys

let map_cps f l cont =
  let rec loop acc xs =
    match xs with
    | [] -> cont (List.rev acc)
    | x :: xs -> f x (fun y -> loop (y :: acc) xs)
  in loop [] l

let mapi_cps f l cont =
  let rec loop acc i xs =
    match xs with
    | [] -> cont (List.rev acc)
    | x :: xs ->
      f i x (fun y -> loop (y :: acc) (i+1) xs)
  in loop [] 0 l

let fold_left_cps f st l cont =
  let rec loop st xs =
    match xs with
    | []      -> cont st
    | x :: xs -> f st x (fun st -> loop st xs)
  in loop st l

let fold_left_i_cps f st l cont =
  let rec loop i st xs =
    match xs with
    | []      -> cont st
    | x :: xs -> f i st x (fun st -> loop (i+1) st xs)
  in loop 0 st l

let map_filter_redo_cps f l cont =
  let rec loop acc xs =
    match xs with
    | [] -> cont (List.rev acc)
    | x :: xs ->
      f x (function
      | Drop   -> loop acc xs
      | Done y -> loop (y :: acc) xs
      | Redo x -> loop acc (x :: xs))
  in loop [] l
