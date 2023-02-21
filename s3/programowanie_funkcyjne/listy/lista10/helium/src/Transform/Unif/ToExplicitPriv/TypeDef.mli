
val prepare_env   : Env.t -> Lang.Unif.typedef -> Env.t
val prepare_env_s : Env.t -> Lang.Unif.typedef list -> Env.t

val tr_typedef : Env.t -> Lang.Unif.typedef -> Env.t * Lang.Explicit.typedef
val tr_typedefs : Env.t ->
  Lang.Unif.typedef list -> Env.t * Lang.Explicit.typedef list
