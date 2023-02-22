open Kind.Datatypes
open TVar.Datatypes
open Type.Datatypes

module T = struct
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

module Datatypes = struct
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
include T

let rename_op_decl_m tm (OpDecl(name, ets, inputs, output)) =
  let (tm, ets) = Utils.ListExt.fold_map (fun tm (TVar.Pack x) ->
      let y = TVar.clone x in
      (TVar.TVarMap.add x y tm, TVar.Pack y)
    ) tm ets in
  OpDecl(name, ets,
    List.map (Type.rename_m tm) inputs,
    Type.rename_m tm output)

let subst_op_decl_m sub (OpDecl(name, ets, inputs, output)) =
  let (sub, ets) = Utils.ListExt.fold_map (fun sub (TVar.Pack x) ->
      let y = TVar.clone x in
      (Type.TVarMap.add x (Type.var y) sub, TVar.Pack y)
    ) sub ets in
  OpDecl(name, ets,
    List.map (Type.subst_m sub) inputs,
    Type.subst_m sub output)

let rename_adt_ctor_m tm (ADTCtor(name, ets, tps)) =
  let (tm, ets) = Utils.ListExt.fold_map (fun tm (TVar.Pack x) ->
      let y = TVar.clone x in
      (TVar.TVarMap.add x y tm, TVar.Pack y)
    ) tm ets in
  ADTCtor(name, ets, List.map (Type.rename_m tm) tps)

let subst_adt_ctor_m sub (ADTCtor(name, ets, tps)) =
  let (sub, ets) = Utils.ListExt.fold_map (fun sub (TVar.Pack x) ->
      let y = TVar.clone x in
      (Type.TVarMap.add x (Type.var y) sub, TVar.Pack y)
    ) sub ets in
  ADTCtor(name, ets, List.map (Type.subst_m sub) tps)

let rec rename_m_formal_params : type k1 k2.
    TVar.TVarMap.t -> (k1, k2) formal_params ->
    TVar.TVarMap.t * (k1, k2) formal_params =
    fun tm params ->
  match params with
  | TDFP_Nil -> (tm, TDFP_Nil)
  | TDFP_Cons(x, params) ->
    let y = TVar.clone x in
    let tm = TVar.TVarMap.add x y tm in
    let (tm, params) = rename_m_formal_params tm params in
    (tm, TDFP_Cons(y, params))

let rename_body_m (type k) tm (body : k body) : k body =
  match body with
  | TDEffect ops -> TDEffect(List.map (rename_op_decl_m tm) ops)
  | TDData ctors -> TDData(List.map (rename_adt_ctor_m tm) ctors)

let subst_body_m (type k) sub (body : k body) : k body =
  match body with
  | TDEffect ops -> TDEffect(List.map (subst_op_decl_m sub) ops)
  | TDData ctors -> TDData(List.map (subst_adt_ctor_m sub) ctors)

let rename_m tm (TypeDef(l, params, td)) =
  let l = TVar.rename_m tm l in
  let (tm, params) = rename_m_formal_params tm params in
  let td = rename_body_m tm td in
  TypeDef(l, params, td)
