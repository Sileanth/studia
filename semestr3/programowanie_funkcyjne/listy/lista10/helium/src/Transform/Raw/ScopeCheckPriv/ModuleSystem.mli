
type struct_def
type module_expr
type functor_arg

val of_namespace : mpos:Utils.Position.t -> Namespace.t -> struct_def
val to_namespace : mpath:Lang.Flat.mpath ->
  struct_def -> Context.def list * Namespace.t

val of_ns_repr : mpos:Utils.Position.t -> Namespace.module_v -> module_expr
val to_ns_repr : mpath:Lang.Flat.mpath ->
  module_expr -> Context.def list * Namespace.module_v

val as_namespace : mpath:Lang.Flat.mpath ->
  module_expr -> Context.def list * Namespace.t

val match_signature :
  Context.def list * struct_def ->
  Lang.Flat.typedef list * ModuleSig.struct_s ->
    struct_def

val match_type :
  Context.def list * module_expr ->
  Lang.Flat.typedef list * ModuleSig.module_type ->
    module_expr

val mk_struct : struct_def -> module_expr

val mk_functor_arg : pos:Utils.Position.t -> string ->
  Lang.Flat.typedef list * ModuleSig.module_type ->
    Namespace.module_v * functor_arg

val mk_functor :
  functor_arg -> Context.def list * module_expr -> module_expr

val apply_functor : pos:Utils.Position.t ->
  module_expr -> Context.def list * module_expr -> module_expr

val update_pos : module_expr -> Utils.Position.t -> module_expr
