open Kind.Datatypes
open TVar.Datatypes

module T : sig
  type 'k t

  type ttype  = k_type t
  type effect = k_effect t

  type (_, 'k) params =
  | TP_Nil  : ('k, 'k) params
  | TP_Cons : 'k1 t * ('k0, 'k) params -> (('k1, 'k0) k_arrow, 'k) params

  type 'k head =
  | HVar : 'k tvar -> 'k head
  | HAny : 'k kind -> 'k head

  type 'k neutral =
  | TNeu : 'xk head * ('xk, 'k) params -> 'k neutral

  type at_effect = k_effect neutral

  type _ view =
  | TVEffect  : at_effect list -> k_effect view
  | TVNeutral : 'k neutral -> 'k view
  | TVArrow   : ttype * ttype * effect -> k_type view
  | TVForall  : 'k tvar * ttype -> k_type view
  | TVFun     : 'k1 tvar * 'k2 t -> ('k1, 'k2) k_arrow view

  module Exists  : Utils.Exists.S with type 'k data = 'k t
  module TVarMap : TVar.Map.S with type 'k v = 'k t
end

module Datatypes : sig
  type 'k typ = 'k T.t

  type ttype  = k_type typ
  type effect = k_effect typ

  type ('xk, 'k) type_params = ('xk, 'k) T.params =
  | TP_Nil  : ('k, 'k) type_params
  | TP_Cons : 'k1 typ * ('k0, 'k) type_params ->
      (('k1, 'k0) k_arrow, 'k) type_params

  type 'k type_head = 'k T.head =
  | HVar : 'k tvar -> 'k type_head
  | HAny : 'k kind -> 'k type_head

  type 'k neutral_type = 'k T.neutral =
  | TNeu : 'xk type_head * ('xk, 'k) type_params -> 'k neutral_type

  type at_effect = k_effect neutral_type

  type 'k type_view = 'k T.view =
  | TVEffect  : at_effect list -> k_effect type_view
  | TVNeutral : 'k neutral_type -> 'k type_view
  | TVArrow   : ttype * ttype * effect -> k_type type_view
  | TVForall  : 'k tvar * ttype -> k_type type_view
  | TVFun     : 'k1 tvar * 'k2 typ -> ('k1, 'k2) k_arrow type_view
end

module type S = sig
  type 'k t = 'k T.t

  type ttype  = k_type t
  type effect = k_effect t

  type ('xk, 'k) params = ('xk, 'k) T.params =
  | TP_Nil  : ('k, 'k) params
  | TP_Cons : 'k1 t * ('k0, 'k) params -> (('k1, 'k0) k_arrow, 'k) params

  type 'k head = 'k T.head =
  | HVar : 'k tvar -> 'k head
  | HAny : 'k kind -> 'k head

  type 'k neutral = 'k T.neutral =
  | TNeu : 'xk head * ('xk, 'k) params -> 'k neutral

  type at_effect = k_effect neutral

  type 'k view = 'k T.view =
  | TVEffect  : at_effect list -> k_effect view
  | TVNeutral : 'k neutral -> 'k view
  | TVArrow   : ttype * ttype * effect -> k_type view
  | TVForall  : 'k tvar * ttype -> k_type view
  | TVFun     : 'k1 tvar * 'k2 t -> ('k1, 'k2) k_arrow view

  module Exists : Utils.Exists.S
    with type 'k data = 'k t
    and  type t = T.Exists.t
  include module type of Exists.Datatypes

  module TVarMap : TVar.Map.S
    with type 'k v = 'k t
    and  type  t = T.TVarMap.t

  val view : 'k t -> 'k view

  (* constructors *)
  val var      : 'k tvar -> 'k t
  val any      : 'k kind -> 'k t
  val eff_pure : effect
  val eff_cons : effect -> effect -> effect
  val arrow    : ttype -> ttype -> effect -> ttype
  val forall   : 'k tvar -> ttype -> ttype
  val tfun     : 'k1 tvar -> 'k2 t -> ('k1, 'k2) k_arrow t
  val app      : ('k1, 'k2) k_arrow t -> 'k1 t -> 'k2 t

  val eff_conses : effect list -> effect
  val arrows     : ttype list -> ttype -> effect -> ttype
  val foralls    : TVar.ex list -> ttype -> ttype

  val of_neutral     : 'k neutral -> 'k t
  val to_neutral_opt : 'k t -> 'k neutral option

  val of_at_effects : at_effect list -> effect
  val to_at_effects : effect -> at_effect list

  val kind : 'k t -> 'k kind 
  val head_kind : 'k head -> 'k kind

  val rename_m : TVar.TVarMap.t -> 'k t -> 'k t
  val subst_m  : TVarMap.t -> 'k t -> 'k t

  val rename_params_m :
    TVar.TVarMap.t -> ('k1, 'k2) params -> ('k1, 'k2) params

  val rename_neutral_m :
    TVar.TVarMap.t -> 'k neutral -> 'k neutral

  val rename : 'k1 tvar -> 'k1 tvar -> 'k t -> 'k t
  val subst  : 'k1 tvar -> 'k1 t -> 'k t -> 'k t
end
include S
