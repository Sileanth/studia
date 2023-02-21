
type t

type var =
| VVar  of Lang.Flat.var
| VOp   of Lang.Flat.op
| VCtor of Lang.Flat.ctor

type tconst =
| TCConst   of Lang.Flat.tconst
| TCTypedef of Lang.Flat.tconst * t

and module_v =
| MNamespace of t
| MFunctor   of Lang.Flat.var * ModuleSig.functor_s

val empty : t
val merge : t -> t -> t

val add_var     : t -> string -> Lang.Flat.var -> t
val add_op      : t -> string -> Lang.Flat.op -> t
val add_ctor    : t -> string -> Lang.Flat.ctor -> t
val add_field   : t -> string -> Lang.Flat.field -> t
val add_tconst  : t -> string -> Lang.Flat.tconst -> t
val add_module  : t -> string -> module_v -> t

val set_typedef : t -> Lang.Flat.tconst -> t -> t
val add_typedef : t -> string -> Lang.Flat.tconst -> t -> t

val lookup_var     : t -> string -> var option
val lookup_op      : t -> string -> Lang.Flat.op option
val lookup_ctor    : t -> string -> Lang.Flat.ctor option
val lookup_field   : t -> string -> Lang.Flat.field option
val lookup_tconst  : t -> string -> Lang.Flat.tconst option
val lookup_module  : t -> string -> module_v option
val lookup_functor : t -> string ->
  (Lang.Flat.var * ModuleSig.functor_s) option

val lookup_tconst_base : t -> string -> tconst option
