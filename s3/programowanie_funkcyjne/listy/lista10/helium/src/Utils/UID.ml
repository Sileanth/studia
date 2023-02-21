
type t = int64

let compare = Int64.compare

let next_fresh = ref 0L
let fresh () =
  let r = !next_fresh in
  next_fresh := Int64.succ r;
  r

let equal = Int64.equal

let to_string x = Printf.sprintf "#UID_%LX" x

module Map = Map.Make(Int64)
