open Lang.Node
module S = Lang.Explicit
module T = Lang.UCPS

let rec erasable : type k. k S.kind -> bool = fun k ->
  match k with
  | S.KType   -> true
  | S.KEffect -> false
  | S.KArrow(_, k) -> erasable k

let make_node data =
  { meta = Utils.Seal.empty; data }

let rec build_effect_proxy : type k. k S.kind -> T.expr -> T.expr = fun k l ->
  match k with
  | S.KType   -> assert false
  | S.KEffect -> l
  | S.KArrow(k1, k2) ->
    if erasable k1 then build_effect_proxy k2 l
    else make_node (T.EFun(T.Var.fresh (), build_effect_proxy k2 l))

module Env : sig
  type t

  val init : unit -> t

  val add_var   : t -> S.var -> t * T.var
  val add_var'  : t -> S.var -> T.var -> t
  val add_tvar  : t -> 'k S.tvar -> t * T.var
  val add_label : t -> 'l S.tvar -> t * T.lvar

  val add_vars : t -> S.var list -> t * T.var list

  val lookup_var   : t -> S.var -> T.var
  val lookup_tvar  : t -> 'k S.tvar -> T.expr
  val lookup_label : t -> 'k S.tvar -> T.lvar

  val has_label : t -> 'k S.tvar -> bool
  val is_pure   : t -> S.Env.t -> S.effect -> bool option

  val set_purity : t -> S.effect -> bool -> t
end = struct
  type t =
    { var_env   : T.var S.Var.Map.t
    ; tvar_env  : T.expr S.TVar.Map.t
    ; label_env : T.lvar S.TVar.Map.t
    ; purity    : (S.effect * bool) list
    }

  let init () =
    let open Predef.DB in
    { var_env   = S.Var.Map.empty
    ; tvar_env  =
      List.fold_left (fun env (Type p) ->
        let kind = S.TVar.kind p.core_tvar in
        if erasable kind then env
        else
          S.TVar.Map.add p.core_tvar
            (build_effect_proxy kind (make_node T.EEffPure))
            env
        ) S.TVar.Map.empty (types ())
    ; label_env = S.TVar.Map.empty
    ; purity    =
      Utils.ListExt.filter_map (fun (Type p) ->
          (begin match S.TVar.kind p.core_tvar with
          | S.KEffect  -> Some (S.Type.var p.core_tvar, true)
          | S.KType    -> None
          | S.KArrow _ -> None
          end : (S.effect * bool) option)
        ) (types ())
    }

  let add_var' env x y =
    { env with
      var_env = S.Var.Map.add x y env.var_env
    }

  let add_var env x =
    let y = T.Var.fresh
      ~name:(S.Var.name x)
      ~meta:(S.Var.meta x)
      ()
    in (add_var' env x y, y)

  let add_tvar env x =
    let y = T.Var.fresh
      ~name:(S.TVar.name x)
      ()
    in
    { env with
      tvar_env = S.TVar.Map.add x (make_node (T.EVar y)) env.tvar_env
    }, y

  let add_label env x =
    let y = T.LVar.fresh
      ~name:(S.TVar.name x)
      ()
    in
    { env with
      label_env = S.TVar.Map.add x y env.label_env
    }, y

  let add_vars = Utils.ListExt.fold_map add_var

  let lookup_var env x =
    S.Var.Map.find x env.var_env

  let lookup_tvar env x =
    S.TVar.Map.find x env.tvar_env

  let lookup_label env x =
    S.TVar.Map.find x env.label_env

  let has_label env x =
    S.TVar.Map.mem x env.label_env

  let is_pure env senv eff =
    env.purity
    |> List.find_opt (fun (eff', _) ->
        S.Subtyping.equiv senv eff eff')
    |> (function
        | None       -> None
        | Some(_, r) -> Some r)

  let set_purity env eff r =
    { env with
      purity = (eff, r) :: env.purity
    }
end

type cont =
| C_Var  of T.var
| C_Code of string * (T.var -> T.expr)

let cont_reify cont rest =
  match cont with
  | C_Var  x -> rest x
  | C_Code(name, f) ->
    let x = T.Var.fresh ~name:"k" () in
    let y = T.Var.fresh ~name () in
    make_node (T.ELet(x, make_node (T.EFun(y, f y)), rest x))

let cont_apply cont res =
  match cont with
  | C_Var  x -> make_node (T.EApp(make_node (T.EVar x), res))
  | C_Code(name, f) ->
    let x = T.Var.fresh ~name () in
    make_node (T.ELet(x, res, f x))

let tr_head env head =
  match head with
  | S.HVar x -> Env.lookup_tvar env x
  | S.HAny k -> build_effect_proxy k (make_node T.EEffPure)

let rec tr_type : type k. Env.t -> k S.typ -> T.expr = fun env tp ->
  match S.Type.view tp with
  | S.TVArrow  _   -> assert false
  | S.TVForall _   -> assert false
  | S.TVEffect row -> tr_row env (make_node T.EEffPure) row
  | S.TVNeutral (S.TNeu(head, params)) ->
    let head = tr_head env head in
    tr_type_params env head params
  | S.TVFun(x, body) ->
    if erasable (S.TVar.kind x) then tr_type env body
    else
      let (env, x) = Env.add_tvar env x in
      make_node (T.EFun(x, tr_type env body))

and tr_row env acc row =
  match row with
  | [] -> acc
  | S.TNeu(head, params) :: row ->
    let head = tr_head env head in
    let eff  = tr_type_params env head params in
    tr_row env (make_node (T.EEffCons(acc, eff))) row

and tr_type_params :
    type k1 k2. Env.t -> T.expr -> (k1, k2) S.type_params -> T.expr =
    fun env acc params ->
  match params with
  | S.TP_Nil -> acc
  | S.TP_Cons(tp, params) ->
    let acc = 
      if erasable (S.Type.kind tp) then acc
      else make_node (T.EApp(acc, tr_type env tp))
    in
    tr_type_params env acc params

let tr_label env h =
  match h with
  | S.HVar x -> Env.lookup_label env x
  | S.HAny _ -> assert false

let ifpure_neu senv env neu tb fb =
  let S.TNeu(head, params) = neu in
  match head with
  | S.HVar x ->
    if Env.has_label env x then fb env
    else
      let tp = S.Type.of_neutral neu in
      begin match Env.is_pure env senv tp with
      | Some true  -> tb env
      | Some false -> fb env
      | None ->
        make_node (T.EIfPure(tr_type env tp,
          tb (Env.set_purity env tp true),
          fb (Env.set_purity env tp false)))
      end
  | S.HAny _ -> tb env

let rec ifpure senv env eff tb fb =
  match eff with
  | [] -> tb env
  | neu :: eff ->
    ifpure_neu senv env neu
      (fun env -> ifpure senv env eff tb fb)
      fb

let ifpure_e env e tb fb =
  ifpure
    (Utils.Seal.find S.Keys.meta_env e.meta)
    env
    (S.Type.to_at_effects (Utils.Seal.find S.Keys.expr_effect e.meta))
    tb
    fb

let if_pure_c_out env ec tb fb =
  ifpure
    (Utils.Seal.find S.Keys.meta_env ec.meta)
    env
    (S.Type.to_at_effects (Utils.Seal.find S.Keys.ecrc_out_effect ec.meta))
    tb
    fb

let if_pure_c_in env ec tb fb =
  ifpure
    (Utils.Seal.find S.Keys.meta_env ec.meta)
    env
    (S.Type.to_at_effects (Utils.Seal.find S.Keys.ecrc_in_effect ec.meta))
    tb
    fb

let rec tr_eff_coercion env ec =
  let meta = ec.meta in
  let make data = { meta; data } in
  match ec.data with
  | S.ECId _     -> make T.CId
  | S.ECLift eff -> make (T.CLift (tr_type env eff))
  | S.ECSwap(eff1, eff2) ->
    make (T.CSwap(tr_type env eff1, tr_type env eff2))
  | S.ECConsL(eff, ec) ->
    make (T.CCons(tr_type env eff, tr_eff_coercion env ec))
  | S.ECConsR(ec, _) ->
    tr_eff_coercion env ec
  | S.ECComp(ec1, ec2) ->
    make (T.CComp(tr_eff_coercion env ec1, tr_eff_coercion env ec2))

let rec tr_eff_coercion_tail env ec =
  let meta = ec.meta in
  let make data = { meta; data } in
  match ec.data with
  | S.ECId _ | S.ECSwap _ | S.ECConsR _ -> tr_eff_coercion env ec
  | S.ECLift _ -> make T.CId
  | S.ECConsL(eff, ec) ->
    make (T.CCons(tr_type env eff, tr_eff_coercion_tail env ec))
  | S.ECComp(ec1, ec2) ->
    make (T.CComp(tr_eff_coercion_tail env ec1, tr_eff_coercion_tail env ec2))

let coerce_eff env ec pure eff cont =
  match ec.data with
  | S.ECId _ -> eff env cont
  | _ ->
    let meta = ec.meta in
    let make data = { meta; data } in
    cont_reify cont (fun k ->
    if_pure_c_in env ec
      (fun env -> make (T.EApp(make (T.EVar k), pure env)))
      (fun env ->
        make (T.ECoerce(tr_eff_coercion_tail env ec,
          eff env (C_Code("_coerce_res", fun r ->
            make (T.EDone (make (T.EVar r))))),
          make (T.EVar k)))))

let rec tr_val_coercion env vc x =
  let meta = vc.meta in
  let make data = { meta; data } in
  match vc.data with
  | S.VCId -> x
  | S.VCArrow(c1, c2, ec) ->
    let z   = T.Var.fresh ~name:"_coerce_val" () in
    make (T.ELet(z, x,
    let x   = make (T.EVar z) in
    let arg = T.Var.fresh ~name:"_arrow_arg" () in
    make (T.EFun(arg,
      if_pure_c_out env ec
        (fun env ->
          tr_val_coercion env c2 (
          make (T.EApp(x,
          tr_val_coercion env c1 (make (T.EVar arg))))))
        (fun env ->
          let k = T.Var.fresh ~name:"_arrow_cont" () in
          make (T.EFun(k,
          coerce_eff env ec
            (fun env ->
              tr_val_coercion env c2 (
              make (T.EApp(x,
              tr_val_coercion env c1 (make (T.EVar arg))))))
            (fun env cont ->
              cont_reify (C_Code("_coerce_cont", fun x ->
                cont_apply cont (tr_val_coercion env c2 (make (T.EVar x)))))
              (fun k ->
              make (T.EApp(make(T.EApp(
                x,
                tr_val_coercion env c1 (make (T.EVar arg)))),
                make (T.EVar k)))))
            (C_Var k))))))))

let tr_match_clause tr env (targs, xs, e) =
  let targs = List.filter (fun (S.TVar.Pack x) ->
      not (erasable (S.TVar.kind x))
    ) targs in
  let (env, targs) = Utils.ListExt.fold_map (fun env (S.TVar.Pack x) ->
      Env.add_tvar env x
    ) env targs in
  let (env, xs) = Env.add_vars env xs in
  (targs @ xs, tr env e)

let rec tr_expr_pure env e =
  let meta = e.meta in
  let make data = { meta; data } in
  match e.data with
  | S.ENum    n  -> make (T.ENum n)
  | S.EChar   c  -> make (T.EChar c)
  | S.EString s  -> make (T.EString s)
  | S.EVar    x  -> make (T.EVar (Env.lookup_var env x))
  | S.EFun(x, e) ->
    ifpure_e env e 
      (fun env ->
        let (env, x) = Env.add_var env x in
        make (T.EFun(x, tr_expr_pure env e)))
      (fun env ->
        let (env, x) = Env.add_var env x in
        let k = T.Var.fresh ~name:"k" () in
        make (T.EFun(x, make (T.EFun(k, tr_expr_cps env e (C_Var k))))))
  | S.ETFun(x, e) ->
    if erasable (S.TVar.kind x) then
      tr_expr_pure env e
    else
      let (env, x) = Env.add_tvar env x in
      make (T.EFun(x, tr_expr_pure env e))
  | S.EOp _ ->
    failwith "Internal effect error (Explicit -> UCPS)"
  | S.ECtor(_, n, targs, args) ->
    let targs = Utils.ListExt.filter_map (fun (S.Type.Pack tp) ->
        if erasable (S.Type.kind tp) then None
        else Some (tr_type env tp)
      ) targs in
    make (T.ECtor(n, targs @ List.map (tr_expr_pure env) args))
  | S.EApp(e1, e2) ->
    make (T.EApp(tr_expr_pure env e1, tr_expr_pure env e2))
  | S.ETApp(e, tp) ->
    if erasable (S.Type.kind tp) then
      tr_expr_pure env e
    else
      make (T.EApp(tr_expr_pure env e, tr_type env tp))
  | S.ELet(x, e1, e2) | S.ELetPure(x, e1, e2) ->
    let e1 = tr_expr_pure env e1 in
    let (env, x) = Env.add_var env x in
    let e2 = tr_expr_pure env e2 in
    make (T.ELet(x, e1, e2))
  | S.EFix(rfs, e) ->
    let env = List.fold_left (fun env (x, _) ->
        fst (Env.add_var env x)
      ) env rfs in
    make (T.EFix(List.map (tr_rec_function env) rfs, tr_expr_pure env e))
  | S.EHandle(S.TNeu(l, _), e, xr, er, hs) ->
    let return_cl =
      let (env, xr) = Env.add_var env xr in
      { T.pr_arg  = xr
      ; T.pr_body = tr_expr_pure env er
      }
    in
    make (T.EPureHandle(tr_label env l,
      tr_expr_cps env e (C_Code("_handle_res", fun r ->
        make (T.EDone (make (T.EVar r))))),
      return_cl,
      List.map (tr_handler_pure env) hs))
  | S.EMatch(_, e, cls, _) ->
    make (T.EMatch(tr_expr_pure env e,
      List.map (tr_match_clause tr_expr_pure env) cls))
  | S.ETypedef(tds, e) ->
    tr_typedefs meta env tds (fun env ->
    tr_expr_pure env e)
  | S.EAbsType(x, e) ->
    let kind = S.TVar.kind x in
    if erasable kind then tr_expr_pure env e
    else
      let (env, x) = Env.add_tvar env x in
      make (T.ELet(x, build_effect_proxy kind (make T.EEffPure),
      tr_expr_pure env e))
  | S.ECoerce(vc, _, e) ->
    tr_val_coercion env vc (tr_expr_pure env e)
  | S.EExtern(name, _) -> make (T.EExtern name)
  | S.EReplExpr(e, seal, repl, _, _) ->
    make (T.EReplExpr(tr_expr_pure env e, seal, fun () ->
      tr_expr_pure env (repl ())))
  | S.ERepl(seal, repl, _, _) ->
    make (T.ERepl(seal, fun () -> tr_expr_pure env (repl ())))

and tr_handler_pure env (targs, xs, r, e) =
  let targs = List.filter (fun (S.TVar.Pack x) ->
      not (erasable (S.TVar.kind x))
    ) targs in
  let (env, targs) = Utils.ListExt.fold_map (fun env (S.TVar.Pack x) ->
      Env.add_tvar env x
    ) env targs in
  let (env, xs) = Env.add_vars env xs in
  let (env, r)  = Env.add_var env r in
  { T.ph_args = targs @ xs
  ; T.ph_res  = r
  ; T.ph_body = tr_expr_pure env e
  }

and tr_expr_cps env e cont =
  let meta = e.meta in
  let make data = { meta; data } in
  match e.data with
  | S.ENum _ | S.EChar _ | S.EString _ | S.EVar _ | S.EFun _ | S.ETFun _
  | S.ECtor _ | S.EExtern _ ->
    failwith "Internal effect error (Explicit -> UCPS)"
  | S.EOp(S.TNeu(l, _), n, targs, args) ->
    let targs = Utils.ListExt.filter_map (fun (S.Type.Pack tp) ->
        if erasable (S.Type.kind tp) then None
        else Some (tr_type env tp)
      ) targs in
    cont_reify cont (fun k ->
    make (T.EOp(tr_label env l, n,
        targs @ List.map (tr_expr_pure env) args,
        make (T.EVar k))))
  | S.EApp(e1, e2) ->
    tr_expr_cps env e1 (C_Code("_app_fun", fun func ->
    tr_expr_cps env e2 (C_Code("_app_arg", fun arg  ->
    cont_reify cont (fun k ->
    make (T.EApp(make(T.EApp(
      make (T.EVar func),
      make (T.EVar arg))),
      make (T.EVar k))))))))
  | S.ETApp(e, tp) ->
    if erasable (S.Type.kind tp) then
      tr_expr_cps env e cont
    else
      tr_expr_cps env e (C_Code("_tapp_fun", fun func ->
      cont_apply cont (make (T.EApp(
        make (T.EVar func),
        tr_type env tp)))))
  | S.ELet(x, e1, e2) ->
    tr_expr_cps env e1 (C_Code("_let_" ^ S.Var.name x, fun r ->
    let env = Env.add_var' env x r in
    tr_expr_cps env e2 cont))
  | S.ELetPure(x, e1, e2) ->
    let e1 = tr_expr_pure env e1 in
    let (env, x) = Env.add_var env x in
    make (T.ELet(x, e1, tr_expr_cps env e2 cont))
  | S.EFix(rfs, e) ->
    let env = List.fold_left (fun env (x, _) ->
        fst (Env.add_var env x)
      ) env rfs in
    make (T.EFix(List.map (tr_rec_function env) rfs, tr_expr_cps env e cont))
  | S.EHandle(S.TNeu(l, _), e, xr, er, hs) ->
    let (env', xr) = Env.add_var env xr in
    let return_cl =
      let k = T.Var.fresh ~name:"_cont" () in
      { T.r_arg  = xr
      ; T.r_cont = k
      ; T.r_body = tr_expr_cps env' er (C_Var k)
      }
    in
    cont_reify cont (fun k ->
    make (T.EHandle(tr_label env l,
      tr_expr_cps env e (C_Code("_handle_res", fun r ->
        make (T.EDone (make (T.EVar r))))),
      return_cl,
      List.map (tr_handler_cps env) hs,
      make (T.EVar k))))
  | S.EMatch(_, e, cls, _) ->
    tr_expr_cps env e (C_Code ("_match_val", fun v ->
    make (T.EMatch(make (T.EVar v),
      List.map
        (tr_match_clause (fun env e -> tr_expr_cps env e cont) env)
        cls))))
  | S.ETypedef(tds, e) ->
    tr_typedefs meta env tds (fun env ->
    tr_expr_cps env e cont)
  | S.EAbsType(x, e) ->
    let kind = S.TVar.kind x in
    if erasable kind then tr_expr_cps env e cont
    else
      let (env, x) = Env.add_tvar env x in
      make (T.ELet(x, build_effect_proxy kind (make T.EEffPure),
      tr_expr_cps env e cont))
  | S.ECoerce(vc, ec, e) ->
    coerce_eff env ec
      (fun env      -> tr_expr_pure env e)
      (fun env cont -> tr_expr_cps  env e cont)
      (C_Code("_coerce_in", fun x ->
      cont_apply cont (tr_val_coercion env vc (make (T.EVar x)))))
  | S.EReplExpr(e, seal, repl, _, _) ->
    tr_expr_cps env e (C_Code("_repl_val", fun v ->
    make (T.EReplExpr(make (T.EVar v), seal, fun () ->
      tr_expr_cps env (repl ()) cont))))
  | S.ERepl(seal, repl, _, _) ->
    make (T.ERepl(seal, fun () -> tr_expr_cps env (repl ()) cont))

and tr_handler_cps env (targs, xs, r, e) =
  let targs = List.filter (fun (S.TVar.Pack x) ->
      not (erasable (S.TVar.kind x))
    ) targs in
  let (env, targs) = Utils.ListExt.fold_map (fun env (S.TVar.Pack x) ->
      Env.add_tvar env x
    ) env targs in
  let (env, xs) = Env.add_vars env xs in
  let (env, r)  = Env.add_var env r in
  let k = T.Var.fresh ~name:"_cont" () in
  { T.h_args = targs @ xs
  ; T.h_res  = r
  ; T.h_cont = k
  ; T.h_body = tr_expr_cps env e (C_Var k)
  }

and tr_rec_function env (x, body) =
  let x = Env.lookup_var env x in
  let y = T.Var.fresh ~name:"_rec_eta" () in
  (x, y, make_node (T.EApp(tr_expr_pure env body, make_node (T.EVar y))))

and tr_typedefs meta env tds cont =
  let make data = { meta; data } in
  match tds with
  | [] -> cont env
  | S.TypeDef(l, _, S.TDEffect _) :: tds ->
    let (env, l0) = Env.add_label env l in
    let (env, l1)  = Env.add_tvar env l in
    make (T.ENewEffect(l0,
    make (T.ELet(l1, build_effect_proxy (S.TVar.kind l) (make (T.EEffId l0)),
      tr_typedefs meta env tds cont))))
  | S.TypeDef(_, _, S.TDData _) :: tds -> tr_typedefs meta env tds cont

let tr_expr e =
  tr_expr_pure (Env.init ()) e

let _ =
  Flow.register_transform
    ~require:
      [ S.Tags.expr_type_info; S.Tags.eff_coercion_types ]
    ~weight: 5.0
    ~source: S.flow_node
    ~target: T.flow_node
    tr_expr
