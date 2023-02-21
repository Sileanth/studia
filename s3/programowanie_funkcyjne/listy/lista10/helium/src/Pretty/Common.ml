
module StrSet = Set.Make(String)

let fresh_name names x =
  if StrSet.mem x names then
    let rec aux n =
      let name = Printf.sprintf "%s%d" x n in
      if StrSet.mem name names then aux (n+1)
      else name
    in aux 0
  else x

