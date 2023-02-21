
val create_functor_type : mpos:Utils.Position.t ->
  Lang.Flat.type_farg list ->
  ModuleSig.module_type -> 
  Lang.Flat.typedef list * ModuleSig.module_type ->
    Lang.Flat.typedef list * ModuleSig.functor_s

val tr_struct_sig : Env.t ->
  mpos:Utils.Position.t -> Lang.Raw.decl list ->
    Lang.Flat.typedef list * ModuleSig.struct_s

val tr_module_type : Env.t -> Lang.Raw.module_type ->
    Lang.Flat.typedef list * ModuleSig.module_type
