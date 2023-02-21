open Lang.CoreCommon.Kind.Datatypes

type t

val init : unit -> t

val add_var : t -> Lang.Unif.var -> Lang.Explicit.var -> t
val add_tvar : t -> Lang.Unif.tvar -> Lang.Unif.typ -> t
val add_tconst : t -> Lang.Unif.tconst -> t * Lang.Explicit.TVar.ex
val add_tconst' : t -> Lang.Unif.tconst -> 'k Lang.Explicit.tvar -> t
val add_tconsts : t -> Lang.Unif.tconst list -> t * Lang.Explicit.TVar.ex list
val add_tconsts' : t ->
  Lang.Unif.tconst list -> Lang.Explicit.TVar.ex list -> t

val add_typedef : t -> Lang.Explicit.typedef -> t

val declare_record : t -> Lang.Unif.tconst -> string list -> t

val lookup_var : t -> Lang.Unif.var -> Lang.Explicit.var
val lookup_tvar : t -> Lang.Unif.tvar -> Lang.Unif.typ option
val lookup_tconst : t -> Lang.Unif.tconst -> Lang.Explicit.TVar.ex

val lookup_effect : t ->
  k_effect Lang.Explicit.neutral_type -> Lang.Explicit.op_decl list
val lookup_op : t ->
  k_effect Lang.Explicit.neutral_type -> int -> Lang.Explicit.op_decl

val lookup_adt : t ->
  k_type Lang.Explicit.neutral_type -> Lang.Explicit.adt_ctor list
val lookup_ctor : t ->
  k_type Lang.Explicit.neutral_type -> int -> Lang.Explicit.adt_ctor

val lookup_record : t -> Lang.Unif.tconst -> string list

val is_empty_adt : t -> k_type Lang.Explicit.neutral_type -> bool

val type_scope : t -> Lang.Unif.TConst.Set.t
