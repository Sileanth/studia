open Lang.CoreCommon.Kind.Datatypes

type predef_type =
| Type :
  { kind        : 'k kind
  ; flat_const  : Lang.Flat.tconst
  ; unif_const  : Lang.Unif.tconst
  ; core_tvar   : 'k Lang.Core.tvar
  ; untyped_var : Lang.Untyped.var option
  } -> predef_type

val register_type : predef_type -> unit

val types : unit -> predef_type list

val core_env : unit -> Lang.Core.Env.t

val register_extern : string -> Value.t -> unit
val extern_exists   : string -> bool
val get_extern      : string -> Value.t
