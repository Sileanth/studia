open Lang.Core
open Lang.Node

module Env : sig
  type t

  val empty : t

  val tr_tvar : t -> 'k tvar -> 'k tvar
  val tr_type : t -> 'k typ -> 'k typ

  val tr_types : t -> Type.ex list -> Type.ex list

  val tr_neutral_type : t -> 'k neutral_type -> 'k neutral_type

  val tr_typedef : t -> typedef -> typedef

  val tr_var : t -> var -> var

  val add_var   : t -> var -> t * var
  val add_vars  : t -> var list -> t * var list
  val add_tvar  : t -> 'k tvar -> t * 'k tvar
  val add_tvars : t -> TVar.ex list -> t * TVar.ex list
end = struct
  type t =
    { var_map  : var Var.Map.t
    ; tvar_map : TVar.TVarMap.t
    }

  let empty =
    { var_map  = Var.Map.empty
    ; tvar_map = TVar.TVarMap.empty
    }

  let tr_tvar env x =
    match TVar.TVarMap.find_opt x env.tvar_map with
    | None   -> x
    | Some y -> y

  let tr_type env tp =
    Type.rename_m env.tvar_map tp

  let tr_types env tps =
    List.map (fun (Type.Pack tp) -> Type.Pack(tr_type env tp)) tps

  let tr_neutral_type env neu =
    Type.rename_neutral_m env.tvar_map neu

  let tr_typedef env td =
    TypeDef.rename_m env.tvar_map td

  let tr_var env x =
    match Var.Map.find_opt x env.var_map with
    | None   -> x
    | Some y -> y

  let add_var env x =
    let y = Var.fresh
      ~meta: (Var.meta x)
      ~name: (Var.name x)
      (tr_type env (Var.type_of x))
    in
    { env with
      var_map = Var.Map.add x y env.var_map
    }, y

  let add_vars env xs =
    Utils.ListExt.fold_map add_var env xs

  let add_tvar env x =
    let y = TVar.clone x in
    { env with
      tvar_map = TVar.TVarMap.add x y env.tvar_map
    }, y

  let add_tvars env xs =
    Utils.ListExt.fold_map (fun env (TVar.Pack x) ->
      let (env, y) = add_tvar env x in
      (env, TVar.Pack y)
    ) env xs
end

let prepare_typedef env (TypeDef(l, _, _)) =
  fst (Env.add_tvar env l)

let prepare_typedefs env tds =
  List.fold_left prepare_typedef env tds

let tr_typedefs env tds =
  List.map (Env.tr_typedef env) tds

let rec tr_coercion env c =
  { meta = c.meta
  ; data =
    match c.data with
    | CLift l       -> CLift(Env.tr_type env l)
    | CSwap(l1, l2) -> CSwap(Env.tr_type env l1, Env.tr_type env l2)
    | CCons(l, c)   -> CCons(Env.tr_type env l, tr_coercion env c)
    | CComp(c1, c2) -> CComp(tr_coercion env c1, tr_coercion env c2)
  }

let rec tr_expr env e =
  { meta = e.meta
  ; data =
    match e.data with
    | ENum n -> ENum n
    | EChar c -> EChar c
    | EString s -> EString s
    | EVar x -> EVar(Env.tr_var env x)
    | EFun(x, e) ->
      let (env, x) = Env.add_var env x in
      EFun(x, tr_expr env e)
    | ETFun(x, e) ->
      let (env, x) = Env.add_tvar env x in
      ETFun(x, tr_expr env e)
    | EOp(neu, n, targs, args) ->
      EOp(Env.tr_neutral_type env neu, n,
        Env.tr_types env targs,
        List.map (tr_expr env) args)
    | ECtor(neu, n, targs, args) ->
      ECtor(Env.tr_neutral_type env neu, n,
        Env.tr_types env targs,
        List.map (tr_expr env) args)
    | EApp(e1, e2) -> EApp(tr_expr env e1, tr_expr env e2)
    | ETApp(e, tp) -> ETApp(tr_expr env e, Env.tr_type env tp)
    | ELet(x, e1, e2) ->
      let e1 = tr_expr env e1 in
      let (env, x) = Env.add_var env x in
      ELet(x, e1, tr_expr env e2)
    | EFix(rfs, e) ->
      let env = List.fold_left (fun env (x, _) ->
          fst (Env.add_var env x)
        ) env rfs in
      let rfs = List.map (fun (x, e) ->
          (Env.tr_var env x, tr_expr env e)
        ) rfs in
      EFix(rfs, tr_expr env e)
    | EHandle(neu, e, x, ret, hs) ->
      let neu = Env.tr_neutral_type env neu in
      let e   = tr_expr env e in
      let hs  = List.map (tr_handler env) hs in
      let (env, x) = Env.add_var env x in
      EHandle(neu, e, x, tr_expr env ret, hs)
    | EMatch(neu, e, clauses, rtp) ->
      let neu = Env.tr_neutral_type env neu in
      let e   = tr_expr env e in
      let clauses = List.map (tr_clause env) clauses in
      let rtp = Env.tr_type env rtp in
      EMatch(neu, e, clauses, rtp)
    | ETypedef(tds, e) ->
      let env = prepare_typedefs env tds in
      ETypedef(tr_typedefs env tds, tr_expr env e)
    | EAbsType(x, e) ->
      let (env, x) = Env.add_tvar env x in
      EAbsType(x, tr_expr env e)
    | ECoerce(c, e) ->
      ECoerce(tr_coercion env c, tr_expr env e)
    | EExtern(name, tp) ->
      EExtern(name, Env.tr_type env tp)
    | EReplExpr(e, seal, repl, tp, eff) ->
      let tp  = Env.tr_type env tp in
      let eff = Env.tr_type env eff in
      EReplExpr(tr_expr env e, seal,
        (fun () -> tr_expr env (repl ())), tp, eff)
    | ERepl(seal, repl, tp, eff) ->
      let tp  = Env.tr_type env tp in
      let eff = Env.tr_type env eff in
      ERepl(seal, (fun () -> tr_expr env (repl ())), tp, eff)
  }

and tr_handler env (tvs, xs, r, e) =
  let (env, tvs) = Env.add_tvars env tvs in
  let (env, xs)  = Env.add_vars env xs in
  let (env, r)   = Env.add_var env r in
  (tvs, xs, r, tr_expr env e)

and tr_clause env (tvs, xs, e) =
  let (env, tvs) = Env.add_tvars env tvs in
  let (env, xs) = Env.add_vars env xs in
  (tvs, xs, tr_expr env e)

let tr_expr e =
  tr_expr Env.empty e

let _ =
  Flow.register_transform
    ~contracts:
      [ CommonTags.unique_vars
      ; CommonTags.unique_type_vars
      ]
    ~source: flow_node
    ~target: flow_node
    tr_expr
