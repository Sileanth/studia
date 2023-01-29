
exception Cannot_unify
exception Escapes_scope of Lang.Unif.tconst

val unify_kind : Lang.Unif.kind -> Lang.Unif.kind -> unit

val unify : Env.t -> Lang.Unif.typ -> Lang.Unif.typ -> unit

val coerce_eff :
  Env.t -> Lang.Unif.typ -> Lang.Unif.typ -> Lang.Unif.eff_coercion

val coerce_val :
  Env.t -> Lang.Unif.typ -> Lang.Unif.typ -> Lang.Unif.val_coercion
