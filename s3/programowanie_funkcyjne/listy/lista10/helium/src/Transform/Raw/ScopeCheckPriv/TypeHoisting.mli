
type t

val build_app : pos:Utils.Position.t ->
  Lang.Flat.type_expr -> Lang.Flat.type_farg list -> Lang.Flat.type_expr

val tr_type : t -> Lang.Flat.type_expr -> Lang.Flat.type_expr

val hoist_typedefs :
  Lang.Flat.type_farg list -> Lang.Flat.typedef list ->
    t * Lang.Flat.typedef list
