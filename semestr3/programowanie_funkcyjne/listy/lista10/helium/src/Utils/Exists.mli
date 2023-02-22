
module type ParamType = sig
  type 'a t
end

module type S = sig
  type 'a data

  type t =
  | Pack : 'a data -> t

  module Datatypes : sig
    type ex = t =
    | Pack : 'a data -> ex
  end

  type 'r cont =
    { cont : 'a. 'a data -> 'r
    }

  val pack   : 'a data -> t
  val unpack : t -> 'r cont -> 'r
end

module Make(P : ParamType) : S with type 'a data = 'a P.t
