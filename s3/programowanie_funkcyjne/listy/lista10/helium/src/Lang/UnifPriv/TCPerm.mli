(** Permutations of type constants *)
type t

val id      : t
val rev     : t -> t
val compose : t -> t -> t
val swap    : TConst.t -> TConst.t -> t

val apply : t -> TConst.t -> TConst.t

val image_of : t -> TConst.Set.t -> TConst.Set.t
val carrier  : t -> TConst.Set.t
