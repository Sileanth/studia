type t

val qvars : t -> TConst.t list
val body  : t -> Type.t
val subst : t -> (Type.tvar * Type.t) list

val apply_subst    : t -> Type.t -> Type.t

val open_s     : scope:TConst.Set.t -> t -> Type.tvar list * Type.t
val close_with : ?abs_types: TConst.t list -> Type.tvar list -> Type.t -> t

(** Same as {!close_with}, but sets to {v [] v} all the type variables that:
- have kind {v effect v};
- are in s;
- have only positive occurences
into {v [] v}. The function is useful, since it make type scheme smaller and
still equivalent.
*)
val close_down_with : ?abs_types: TConst.t list -> Type.tvar list -> Type.t -> t

(** Coerce scheme to type. Fails if scheme quanitfies some variables *)
val to_type : t -> Type.t

val tvars : t -> Type.TVar.Set.t
