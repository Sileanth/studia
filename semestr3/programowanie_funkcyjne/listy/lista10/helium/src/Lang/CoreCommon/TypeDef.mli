open Kind.Datatypes
open TVar.Datatypes
open Type.Datatypes

module T : sig
  type op_decl =
  | OpDecl of string * TVar.ex list * ttype list * ttype

  type adt_ctor =
  | ADTCtor of string * TVar.ex list * ttype list

  type (_, 'k) formal_params =
  | TDFP_Nil  : ('k, 'k) formal_params
  | TDFP_Cons : 'k1 tvar * ('k0, 'k) formal_params ->
      (('k1, 'k0) k_arrow, 'k) formal_params

  type _ body =
  | TDEffect : op_decl list  -> k_effect body
  | TDData   : adt_ctor list -> k_type body

  type t = 
  | TypeDef : 'k1 tvar * ('k1, 'k2) formal_params * 'k2 body -> t
end

module Datatypes : sig
  type op_decl = T.op_decl =
  | OpDecl of string * TVar.ex list * ttype list * ttype

  type adt_ctor = T.adt_ctor =
  | ADTCtor of string * TVar.ex list * ttype list

  type ('xk, 'k) typedef_formal_params = ('xk, 'k) T.formal_params =
  | TDFP_Nil  : ('k, 'k) typedef_formal_params
  | TDFP_Cons : 'k1 tvar * ('k0, 'k) typedef_formal_params ->
      (('k1, 'k0) k_arrow, 'k) typedef_formal_params

  type 'k typedef_body = 'k T.body =
  | TDEffect : op_decl list  -> k_effect typedef_body
  | TDData   : adt_ctor list -> k_type typedef_body

  type typedef = T.t =
  | TypeDef :
    'k1 tvar * ('k1, 'k2) typedef_formal_params * 'k2 typedef_body -> typedef
end

module type S = sig
  type op_decl = T.op_decl =
  | OpDecl of string * TVar.ex list * ttype list * ttype

  type adt_ctor = T.adt_ctor =
  | ADTCtor of string * TVar.ex list * ttype list

  type ('xk, 'k) formal_params = ('xk, 'k) T.formal_params =
  | TDFP_Nil  : ('k, 'k) formal_params
  | TDFP_Cons : 'k1 tvar * ('k0, 'k) formal_params ->
      (('k1, 'k0) k_arrow, 'k) formal_params

  type 'k body = 'k T.body =
  | TDEffect : op_decl list  -> k_effect body
  | TDData   : adt_ctor list -> k_type body

  type t = T.t =
  | TypeDef : 'k1 tvar * ('k1, 'k2) formal_params * 'k2 body -> t

  val rename_m : TVar.TVarMap.t -> t -> t

  val subst_body_m : Type.TVarMap.t -> 'k body -> 'k body
end
include S
