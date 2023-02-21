
module Dummy : sig
  type k_type   = Dummy_KType
  type k_effect = Dummy_KEffect
  type ('k1, 'k2) k_arrow = Dummy_KArrow of 'k1 * 'k2
end

module T : sig
  type k_type   = Dummy.k_type
  type k_effect = Dummy.k_effect

  type ('k1, 'k2) k_arrow = ('k1, 'k2) Dummy.k_arrow

  type _ t =
  | KType   : k_type t
  | KEffect : k_effect t
  | KArrow  : 'k1 t * 'k2 t -> ('k1, 'k2) k_arrow t

  module Exists : Utils.Exists.S with type 'k data = 'k t
end

module Datatypes : sig
  type k_type   = T.k_type
  type k_effect = T.k_effect

  type ('k1, 'k2) k_arrow = ('k1, 'k2) T.k_arrow

  type 'k kind = 'k T.t =
  | KType   : k_type kind
  | KEffect : k_effect kind
  | KArrow  : 'k1 kind * 'k2 kind -> ('k1, 'k2) k_arrow kind
end

module type S = sig
  type k_type   = Dummy.k_type
  type k_effect = Dummy.k_effect

  type ('k1, 'k2) k_arrow = ('k1, 'k2) Dummy.k_arrow

  type 'k t = 'k T.t =
  | KType   : k_type t
  | KEffect : k_effect t
  | KArrow  : 'k1 t * 'k2 t -> ('k1, 'k2) k_arrow t

  module Exists : Utils.Exists.S
    with type 'k data = 'k t
    and  type t = T.Exists.t
  include module type of Exists.Datatypes

  val equal : 'k1 t -> 'k2 t -> ('k1, 'k2) Utils.EqDec.eq_dec
end
include S
