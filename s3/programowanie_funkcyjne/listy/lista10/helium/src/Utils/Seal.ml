
module Int64Map = Map.Make(Int64)

type 'a key = int64

type seal =
| Seal : 'a key * 'a -> seal

type any

type t = any Int64Map.t

let next_key = ref 0L
let gen_key () =
  let key = !next_key in
  next_key := Int64.succ key;
  key

let empty = Int64Map.empty

let singleton k v = Int64Map.singleton k (Obj.magic v)
let add k v m =
  Int64Map.add k (Obj.magic v) m

let find k m =
  Obj.magic (Int64Map.find k m)

let seal key v = Seal(key, v)

let unseal (type v) (key : v key) (Seal(k, v)) : v =
  if Int64.equal key k then Obj.magic v
  else failwith "Seal keys does not match"
