open Lang.Explicit

module Env : module type of CoreCommon.Env
  with type t = CoreCommon.Env.t

val pretty_type : Env.t -> int -> 'k typ -> Box.t

val pretty_program : expr -> Box.t
