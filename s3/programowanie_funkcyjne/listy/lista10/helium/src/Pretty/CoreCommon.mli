open Lang.CoreCommon.All

module Env : sig
  type t
  val init : unit -> t 

  val add_var  : t -> var -> t * string
  val add_tvar : t -> 'k tvar -> t * string

  val pretty_var  : t -> var -> Box.t
  val pretty_tvar : t -> 'k tvar -> Box.t
end

val pretty_kind : int -> 'k kind -> Box.t

val pretty_type : Env.t -> int -> 'k typ -> Box.t
val pretty_type_arg : Env.t -> 'k typ -> Box.t

val prepare_typedef  : Env.t -> typedef -> Env.t
val prepare_typedefs : Env.t -> typedef list -> Env.t
val pretty_typedef   : Env.t -> ?sep:Box.t -> typedef -> Box.t
val pretty_typedefs  : Env.t -> ?sep:Box.t -> typedef list -> Box.t list
