
val can_explicitly_swap :
  Env.t -> Lang.Unif.effect -> Lang.Unif.effect -> bool

val lift_effect_l :
  Lang.Unif.effect -> Lang.Unif.effect -> Lang.Unif.eff_coercion option
