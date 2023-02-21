open Lang.CoreCommon.Kind.Datatypes

type 'k tr_params_result =
| Params : 'k1 kind * ('k1, 'k) Lang.Explicit.type_params ->
    'k tr_params_result

val tr_type : Env.t -> Lang.Unif.typ -> Lang.Explicit.Type.ex

val tr_ttype  : Env.t -> Lang.Unif.typ -> Lang.Explicit.ttype
val tr_effect : Env.t -> Lang.Unif.typ -> Lang.Explicit.effect

val tr_scheme : Env.t -> Lang.Unif.scheme -> Lang.Explicit.ttype

val tr_tconst : Env.t -> Lang.Unif.tconst -> 'k kind -> 'k Lang.Explicit.tvar

val tr_type_params : Env.t ->
  Lang.Unif.typ list -> 'k kind -> 'k tr_params_result

(* It may substitute only in tp1 *)
val match_type : Lang.Unif.typ -> Lang.Unif.typ -> unit
