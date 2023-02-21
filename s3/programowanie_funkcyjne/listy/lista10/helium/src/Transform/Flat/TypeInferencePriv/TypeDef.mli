
type td1

val prepare_typedefs : Env.t -> Lang.Flat.typedef list -> Env.t * td1 list
val check_typedefs : Env.t -> td1 list -> Env.t * SyntaxExt.typedef list

val create_ctor_proxies :
  meta: Lang.Unif.expr_meta ->
  Env.t -> SyntaxExt.typedef list ->
    (Env.t -> Lang.Unif.expr) -> Lang.Unif.expr
