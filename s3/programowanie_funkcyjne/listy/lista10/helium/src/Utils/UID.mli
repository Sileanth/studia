
type t

val compare : t -> t -> int

val fresh : unit -> t

val equal : t -> t -> bool

val to_string : t -> string

module Map : Map.S with type key = t
