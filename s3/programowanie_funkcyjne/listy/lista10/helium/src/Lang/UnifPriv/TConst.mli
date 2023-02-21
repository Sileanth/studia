type mpath = string list
type info = mpath * Kind.t
include Utils.Variable.S with type typ := info

val kind  : t -> Kind.t
val mpath : t -> mpath
val full_name : t -> string
