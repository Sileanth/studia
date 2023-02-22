
module StrSet : Set.S with type elt = string

val fresh_name : StrSet.t -> string -> string
