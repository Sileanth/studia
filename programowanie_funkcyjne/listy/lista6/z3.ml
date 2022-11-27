
exception Foo

let for_all p xs =
  let worker = fun acc x ->
    if p x then acc else raise Foo in
  try List.fold_left worker true xs with
    Foo -> false

let mult_list xs =
  let worker = (fun acc x ->
    if x = 0 then raise Foo else acc + x) in
  try List.fold_left worker 0 xs with
    Foo -> 0


let sorted xs =
  let rec worker = (fun (res, prev) x  ->
    begin match prev with
    | None -> (res, Some x)
    | Some z -> if x <= z then raise Foo else (res, (Some x)) end) in
  try List.fold_left worker (true, None) xs |> fst with
    Foo -> false