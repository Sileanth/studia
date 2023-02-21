
module T = Utils.Variable.Make(struct
  type typ = Type.ttype
  let default_name = "x"
end)

module Datatypes = struct
  type var = T.t
end

module type S = Utils.Variable.S
  with type typ := Type.ttype
  and  type t = T.t
include T
