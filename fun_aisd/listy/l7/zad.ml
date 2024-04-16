let seq_rev s =
  let rec rek s (acc : 'a Seq.t) =
    match Seq.uncons s with
    | None -> acc
    | Some(x, s) -> let tail = rek s (Seq.cons x acc) in tail
  in rek s Seq.empty

module BankQue = struct
  type 'a t = int * 'a Seq.t * int * 'a Seq.t


  let empty = 0, Seq.empty, 0, Seq.empty
  let is_empty (front, _, back, _) = front = 0 

  

  let check (fl, f, rl, r) =
      if ( fl >= rl ) then (fl, f, rl, r)
      else (fl + rl, Seq.append f (seq_rev r), 0, Seq.empty)


  let snoc x (fl, f, rl, r) = check (fl, f, rl + 1, Seq.cons x r)

  let head (_, f, _, _) = match Seq.uncons f with
      | None -> None
      | Some(x, f) -> Some x

  let tail (fl, f, rl, r) = match Seq.uncons f with
    | None -> None
    | Some(_, f) -> Some (check (fl - 1, f, rl, r))
end

module BankQueSingleQUe = struct 
  type 'a t = int * 'a Seq.t * int * 'a list

  let empty = 0, Seq.empty, 0, []
  let is_empty (front, _, _, _) = front = 0 

  let rec list_to_seq = function
    | [] -> Seq.empty
    | x :: xs -> fun () -> Cons(x, list_to_seq xs)
  

  let rec list_to_seq_susp_monolit = function
    | [] -> Seq.empty
    | x :: xs -> 
      let rec helper = function
      | [] -> Seq.empty
      | x :: xs -> Seq.cons x (helper xs)
    in fun () -> Cons(x, helper xs)

  let check (fl, f, rl, r) =
      if ( fl >= rl ) then (fl, f, rl, r)
      else (fl + rl, Seq.append f (list_to_seq r), 0, [])
    

  let snoc x (fl, f, rl, r) = check (fl, f, rl + 1, x :: r)

  let head (_, f, _, _) = match Seq.uncons f with
      | None -> None
      | Some(x, f) -> Some x

  let tail (fl, f, rl, r) = match Seq.uncons f with
    | None -> None
    | Some(_, f) -> Some (check (fl - 1, f, rl, r))

end
    
module Z2 = struct
  type 'a t = int * 'a Seq.t * int * 'a Seq.t


  let empty = 0, Seq.empty, 0, Seq.empty
  let is_empty (front, _, back, _) = front = 0 

  

  let check (fl, f, rl, r) =
      if ( 2 * fl >= rl ) then (fl, f, rl, r)
      else (fl + rl, Seq.append f (seq_rev r), 0, Seq.empty)


  let snoc x (fl, f, rl, r) = check (fl, f, rl + 1, Seq.cons x r)

  let head (_, f, _, _) = match Seq.uncons f with
      | None -> None
      | Some(x, _) -> Some x

  let tail (fl, f, rl, r) = match Seq.uncons f with
    | None -> None
    | Some(_, f) -> Some (check (fl - 1, f, rl, r))
end