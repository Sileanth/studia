open Lang.CoreCommon.Kind.Datatypes

module type PredefinedType = sig
  type t_kind

  val unif_tp     : Lang.Unif.typ
  val unif_scheme : Lang.Unif.scheme
  val core_tp     : t_kind Lang.Core.typ

  val info : DB.predef_type
end

module Int      : PredefinedType with type t_kind := k_type
module Char     : PredefinedType with type t_kind := k_type
module String   : PredefinedType with type t_kind := k_type
module IO       : PredefinedType with type t_kind := k_effect
