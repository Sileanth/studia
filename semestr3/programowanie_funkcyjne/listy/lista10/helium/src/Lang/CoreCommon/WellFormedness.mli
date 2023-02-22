
val type_well_formed : Env.t -> 'k Type.t -> bool

val type_params_well_formed : Env.t -> ('k1, 'k2) Type.params -> bool

val neutral_type_well_formed : Env.t -> 'k Type.neutral -> bool

val typedef_well_formed : Env.t -> TypeDef.t -> bool
