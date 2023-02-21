
type t

type var = Namespace.var =
| VVar  of Lang.Flat.var
| VOp   of Lang.Flat.op
| VCtor of Lang.Flat.ctor

type module_v = Namespace.module_v =
| MNamespace of Namespace.t
| MFunctor   of Lang.Flat.var * ModuleSig.functor_s

val init : (t -> ?intf:string -> string -> Namespace.t * Context.def list) -> t

val new_namespace : t -> t
val extend_path   : t -> string -> t
val set_path      : t -> Lang.Flat.mpath -> t

val add_var     : t -> string -> t * Lang.Flat.var
val add_tconst  : t -> local:bool -> string -> t * Lang.Flat.tconst
val set_typedef : t -> Lang.Flat.tconst -> Namespace.t -> t

val add_op'    : t -> Lang.Flat.op -> t
val add_ctor'  : t -> Lang.Flat.ctor -> t
val add_field' : t -> Lang.Flat.field -> t

val add_namespace : t -> string -> Namespace.t -> t
val add_module    : t -> string -> module_v -> t

val allow_tvars : t -> t
val lookup_tvar : t -> Lang.Raw.ident -> Lang.Flat.tconst
val get_tvars   : t -> Lang.Flat.type_farg list

val lookup_var     : t -> Lang.Raw.mpath -> Lang.Raw.ident -> var
val lookup_op      : t -> Lang.Raw.mpath -> Lang.Raw.ident -> Lang.Flat.op
val lookup_ctor    : t -> Lang.Raw.mpath -> Lang.Raw.ident -> Lang.Flat.ctor
val lookup_field   : t -> Lang.Raw.mpath -> Lang.Raw.ident -> Lang.Flat.field
val lookup_tconst  : t -> Lang.Raw.mpath -> Lang.Raw.ident -> Lang.Flat.tconst
val lookup_typedef : t -> Lang.Raw.mpath -> Lang.Raw.ident -> Namespace.t

val lookup_infix_ctor : t -> Lang.Raw.ident -> Lang.Flat.ctor

val lookup_module    : t -> Lang.Raw.mpath -> Lang.Raw.ident -> module_v
val lookup_namespace : t -> Lang.Raw.mpath -> Lang.Raw.ident -> Namespace.t

val current_namespace : t -> Namespace.t
val current_path      : t -> string list

val open_namespace    : t -> Namespace.t -> t
val include_namespace : t -> Namespace.t -> t

val auto_open : t -> string -> t

val req_modules_rev   : t -> Context.def list list
val clean_req_modules : t -> unit
