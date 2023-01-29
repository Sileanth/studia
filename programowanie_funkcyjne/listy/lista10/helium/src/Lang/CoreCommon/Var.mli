
module T : Utils.Variable.S with type typ := Type.ttype

module Datatypes : sig
  type var = T.t
end

module type S = Utils.Variable.S
  with type typ := Type.ttype
  and  type t = T.t
include S
