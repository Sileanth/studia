
val name_of_pattern : Lang.Unif.pattern -> string
val type_of_pattern : Lang.Unif.pattern -> Lang.Unif.typ

val tr_match :
  Utils.Seal.t ->
  Env.t ->
  Lang.Explicit.var ->
  (Lang.Unif.pattern * (Env.t -> Lang.Explicit.expr)) list ->
  typ: Lang.Explicit.ttype ->
  eff: Lang.Explicit.effect ->
    Lang.Explicit.expr

val tr_matches :
  Utils.Seal.t ->
  Env.t ->
  string ->
  Lang.Explicit.var list ->
  (Env.t * Lang.Unif.pattern list * (Env.t -> Lang.Explicit.expr)) list ->
  typ: Lang.Explicit.ttype ->
  eff: Lang.Explicit.effect ->
    Lang.Explicit.expr
