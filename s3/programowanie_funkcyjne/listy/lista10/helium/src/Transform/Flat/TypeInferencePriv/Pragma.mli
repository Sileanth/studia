
val pragma_flag : pos:Utils.Position.t ->
  Env.t -> string ->
    (Env.t -> Lang.Unif.expr) -> Lang.Unif.expr

val pragma_val : pos:Utils.Position.t ->
  Env.t -> string -> Lang.Flat.var ->
    (Env.t -> Lang.Unif.expr) -> Lang.Unif.expr

val pragma_type : pos:Utils.Position.t ->
  Env.t -> string -> Lang.Flat.tconst ->
    (Env.t -> Lang.Unif.expr) -> Lang.Unif.expr
