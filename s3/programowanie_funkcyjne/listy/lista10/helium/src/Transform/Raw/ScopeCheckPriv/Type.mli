
val tr_type_fargs : Env.t ->
  Lang.Raw.type_farg list -> Env.t * Lang.Flat.type_farg list

val tr_type : Env.t -> Lang.Raw.type_expr -> Lang.Flat.type_expr

val tr_annot : Env.t -> Lang.Raw.annot -> Lang.Flat.annot

val tr_annot_opt : Env.t -> Lang.Raw.annot option -> Lang.Flat.annot option
