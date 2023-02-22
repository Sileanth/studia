
type 'a key
type seal
type t

val gen_key : unit -> 'a key

val empty : t

val singleton : 'a key -> 'a -> t
val add : 'a key -> 'a -> t -> t

val find : 'a key -> t -> 'a

val seal   : 'a key -> 'a -> seal
val unseal : 'a key -> seal -> 'a
