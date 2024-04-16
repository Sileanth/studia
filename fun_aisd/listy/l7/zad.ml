let seq_rev s =
  let rec rek s (acc : 'a Seq.t) =
    match Seq.uncons s with
    | None -> acc
    | Some(x, s) -> rek s (Seq.cons x acc)
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
      else (fl + rl, Seq.append f (seq_rev (list_to_seq r)), 0, [])
    

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


let random_ints n = 
  let rec helper n acc = 
    if n = 0 then acc
    else helper (n - 1) (Random.int 1000 :: acc)
  in helper n []

let milion_ints = random_ints 1000000

let test_bankque () = 
  let q = ref BankQue.empty in
  for i = 0 to 1000000 do
    q := BankQue.snoc i !q
  done;
  for i = 0 to 1000000 do
    match BankQue.tail !q with
      | None -> failwith "empty"
      | Some q' -> q := q'
  done

let test_bankque_single_queue () =
  let q = ref BankQueSingleQUe.empty in
  for i = 0 to 1000000 do
    q := BankQueSingleQUe.snoc i !q
  done;
  for i = 0 to 1000000 do
    match BankQueSingleQUe.tail !q with
      | None -> failwith "empty"
      | Some q' -> q := q'
  done

let tesy_bankque_z2 () = 
  let q = ref Z2.empty in
  for i = 0 to 1000000 do
    q := Z2.snoc i !q
  done;
  for i = 0 to 1000000 do
    match Z2.tail !q with
      | None -> failwith "empty"
      | Some q' -> q := q'
  done




  (* measure time of all 3 implemnations*)
let () =
  let t1 = Sys.time () in
  test_bankque ();
  let t1e = Sys.time () -. t1 in
  let t2 = Sys.time () in
  test_bankque_single_queue ();
  let t2e = Sys.time () -. t2 in
  let t3 = Sys.time () in
  tesy_bankque_z2 ();
  let t3e = Sys.time () -. t3 in
  Printf.printf "BankQue: %f\n" (t1e);
  Printf.printf "BankQueSingleQueue: %f\n" (t2e);
  Printf.printf "Z2: %f\n" (t3e)

  (*
     BankQue: 0.261055
BankQueSingleQueue: 0.236668
Z2: 0.225218`
  *)