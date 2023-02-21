
val tr_expr   : Env.t -> Lang.Raw.expr -> Lang.Flat.expr
val tr_defs   : Env.t -> Lang.Raw.def list -> Env.t * Context.def list
val tr_struct : Env.t ->
  mpos:Utils.Position.t ->
  Lang.Raw.def list ->
    Context.def list * ModuleSystem.struct_def
