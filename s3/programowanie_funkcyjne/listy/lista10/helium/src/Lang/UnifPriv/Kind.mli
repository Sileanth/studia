type kvar
type t

type view =
| KType
| KEffect
| KVar   of kvar
| KArrow of t * t

type kind = t
module KVar : sig
  type t = kvar

  val equal : t -> t -> bool

  val set  : t -> kind -> unit
  val set' : t -> view -> unit
end

val ktype   : t
val keffect : t

val arrow  : t -> t -> t
val arrows : t list -> t -> t

val view : t -> view

val fresh_kvar : unit -> t

val contains_kvar  : kvar -> kind -> bool 
val contains_kvar' : kvar -> view -> bool
