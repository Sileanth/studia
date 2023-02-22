
type tvar_env

val create_tvar_env : unit -> tvar_env

val pretty_kind : tvar_env -> int -> Lang.Unif.kind -> Box.t
val pretty_type : tvar_env -> int -> Lang.Unif.typ -> Box.t

val pretty_arrow_effect : tvar_env -> Lang.Unif.typ -> Box.t
