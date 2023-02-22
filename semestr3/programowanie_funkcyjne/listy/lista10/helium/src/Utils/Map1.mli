
module type UIDType1 = sig
  type 'a t

  val uid : 'a t -> UID.t

  val gequal : 'a t -> 'b t -> ('a, 'b) EqDec.eq_dec
end

module type TypeFamily1 = sig
  type 'a t
end

module type S = sig
  type 'a key
  type 'v t

  val empty : 'a t
  val singleton : 'a key -> 'v -> 'v t
  val add : 'a key -> 'v -> 'v t -> 'v t

  val remove : 'a key -> 'v t -> 'v t

  val mem : 'a key -> 'v t -> bool
  val find : 'a key -> 'v t -> 'v
  val find_opt : 'a key -> 'v t -> 'v option

  module type S = sig
    type 'a v
    type t

    val empty : t
    val singleton : 'a key -> 'a v -> t
    val add : 'a key -> 'a v -> t -> t

    val remove : 'a key -> t -> t

    val mem : 'a key -> t -> bool
    val find : 'a key -> t -> 'a v
    val find_opt : 'a key -> t -> 'a v option
  end

  module Make(V : TypeFamily1) : S with type 'a v = 'a V.t
end

module Make(Key : UIDType1) : S with type 'a key = 'a Key.t
