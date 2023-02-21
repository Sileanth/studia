
module type VarDescr = sig
  type typ

  val default_name : string
end

module type S = sig
  type typ

  type t

  val clone : ?name:string -> t -> t
  val fresh : ?name:string -> ?meta:Seal.t -> typ -> t

  val name    : t -> string
  val meta    : t -> Seal.t
  val type_of : t -> typ
  val uid     : t -> UID.t

  val equal : t -> t -> bool
  
  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
end

module Make(D : VarDescr) : S with type typ := D.typ
