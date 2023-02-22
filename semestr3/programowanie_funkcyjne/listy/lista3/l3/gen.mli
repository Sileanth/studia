module type Permutation =
  sig
    type key
    type t
    val apply : t -> key -> key
    val id : t
    val invert : t -> t
    val swap : key -> key -> t
    val compose : t -> t -> t
    val compare : t -> t -> int
  end

module type S = 
    sig
        type t
        val is_generated : t -> t list -> bool
    end

module Make(Perm : Permutation) : S with type t = Perm.t 
