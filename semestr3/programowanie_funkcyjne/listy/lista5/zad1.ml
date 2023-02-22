open Hashtbl


let rec fix_with_limit depth f x =
  if depth = 0 then failwith "toneee"
  else f (fix_with_limit (depth -1) f) x




let fix_memo f x =
  let mem = (create 20) in
  let rec rek f x =
    match find_opt mem x with
    | Some res -> res
    | None -> let res = f (rek f) x in
      add mem x res; res
  in rek f x