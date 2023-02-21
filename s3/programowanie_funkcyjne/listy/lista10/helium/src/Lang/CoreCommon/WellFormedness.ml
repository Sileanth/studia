open Type.Datatypes
open TypeDef.Datatypes

let rec type_well_formed : type k. Env.t -> k typ -> bool =
    fun env tp ->
  match Type.view tp with
  | TVEffect ats ->
    List.for_all (neutral_type_well_formed env) ats
  | TVNeutral nt ->
    neutral_type_well_formed env nt
  | TVArrow(tp1, tp2, eff) ->
    type_well_formed env tp1 &&
    type_well_formed env tp2 &&
    type_well_formed env eff
  | TVForall(x, tp) ->
    type_well_formed (Env.add_tvar env x) tp
  | TVFun(x, tp) ->
    type_well_formed (Env.add_tvar env x) tp

and type_params_well_formed : type k1 k2.
    Env.t -> (k1, k2) type_params -> bool =
    fun env params ->
  match params with
  | TP_Nil -> true
  | TP_Cons(tp, params) ->
    type_well_formed env tp && type_params_well_formed env params

and neutral_type_well_formed : type k.
    Env.t -> k neutral_type -> bool =
    fun env (TNeu(h, params)) ->
  type_params_well_formed env params &&
  match h with
  | HVar l -> Env.has_tvar env l
  | HAny _ -> true

let op_decl_well_formed env (OpDecl(_, ets, inputs, output)) =
  let env = Env.add_tvars env ~fresh:false ets in
  List.for_all (type_well_formed env) inputs &&
    type_well_formed env output

let adt_ctor_well_formed env (ADTCtor(_, tvs, tps)) =
  let env = Env.add_tvars env ~fresh:false tvs in
  List.for_all (type_well_formed env) tps

let typedef_well_formed env td =
  let TypeDef(l, params, body) = td in
  Env.has_tvar env l &&
  let env = Env.add_typedef_formal_params env params in
  match body with
  | TDEffect ops -> List.for_all (op_decl_well_formed env) ops
  | TDData ctors -> List.for_all (adt_ctor_well_formed env) ctors
