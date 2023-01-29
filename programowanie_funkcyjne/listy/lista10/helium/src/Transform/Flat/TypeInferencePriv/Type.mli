
val tr_kind : Lang.Flat.kind -> Lang.Unif.kind

val check_type_fargs_kinds : Env.t ->
  (Lang.Flat.type_farg * Lang.Unif.kind) list -> Env.t * Lang.Unif.tconst list

val check_type_fargs : Env.t ->
  Lang.Flat.type_farg list -> Env.t * Lang.Unif.tconst list

val check_kind : Env.t ->
  Lang.Flat.type_expr -> Lang.Unif.kind -> Lang.Unif.typ

val check_type_field : mpos:Utils.Position.t -> Env.t ->
  Lang.Flat.type_field -> Lang.Unif.tvar -> Lang.Unif.typ
