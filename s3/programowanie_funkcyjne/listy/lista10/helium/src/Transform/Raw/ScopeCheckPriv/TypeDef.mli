type t

val prepare_typedef  : Env.t -> Lang.Raw.typedef -> Env.t * t
val prepare_typedefs : Env.t -> Lang.Raw.typedef list -> Env.t * t list

val tr_typedef  : Env.t -> t -> Lang.Flat.typedef
val tr_typedefs : Env.t -> t list -> Lang.Flat.typedef list

val register_typedef_fields  : Env.t -> Lang.Flat.typedef -> Env.t
val register_typedefs_fields : Env.t -> Lang.Flat.typedef list -> Env.t
