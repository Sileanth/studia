open Lang.Node
module S = Lang.Core
module T = Lang.Untyped

module Env : sig
  type t
  val init : unit -> t
  val add_var : t -> S.var -> t * T.var
  val add_vars : t -> S.var list -> t * T.var list
  val add_effect : t -> 'k S.tvar -> t * T.var
  val add_effect_label : t -> 'k S.tvar -> t * T.var
  val lookup_var : t -> S.var -> T.var
  val lookup_effect : t -> 'k S.tvar -> T.var
  val lookup_effect_label : t -> 'k S.tvar -> T.var
end = struct
  type t =
    { var_env : T.var S.Var.Map.t
    ; eff_env : T.var S.TVar.Map.t
    ; eff_lbl : T.var S.TVar.Map.t
    }

  let init () =
    let open Predef.DB in
    { var_env = S.Var.Map.empty
    ; eff_env =
      List.fold_left (fun env (Type p) ->
          match p.untyped_var with
          | None   -> env
          | Some x -> S.TVar.Map.add p.core_tvar x env
        ) S.TVar.Map.empty (types ())
    ; eff_lbl = S.TVar.Map.empty
    }

  let add_var env x =
    let y = T.Var.fresh
      ~name:(S.Var.name x)
      ~meta:(S.Var.meta x)
      ()
    in
    { env with
      var_env = S.Var.Map.add x y env.var_env
    }, y

  let rec add_vars env xs =
    match xs with
    | [] -> (env, [])
    | x :: xs ->
      let (env, x)  = add_var env x in
      let (env, xs) = add_vars env xs in
      (env, x :: xs)

  let add_effect env l =
    let y = T.Var.fresh
      ~name:(S.TVar.name l)
      ()
    in
    { env with
      eff_env = S.TVar.Map.add l y env.eff_env
    }, y

  let add_effect_label env l =
    let y = T.Var.fresh
      ~name:(S.TVar.name l)
      ()
    in
    { env with
      eff_lbl = S.TVar.Map.add l y env.eff_lbl
    }, y

  let lookup_var env x =
    S.Var.Map.find x env.var_env

  let lookup_effect env x =
    S.TVar.Map.find x env.eff_env

  let lookup_effect_label env x =
    S.TVar.Map.find x env.eff_lbl
end

let rec erasable : type k. k S.kind -> bool = fun k ->
  match k with
  | S.KType   -> true
  | S.KEffect -> false
  | S.KArrow(_, k) -> erasable k

let tr_effect_label env h =
  match h with
  | S.HVar x -> Env.lookup_effect_label env x
  | S.HAny _ -> assert false

let make_node data = { meta = Utils.Seal.empty; data }

let rec build_effect_proxy : type k. k S.kind -> T.expr -> T.expr = fun k l ->
  match k with
  | S.KType   -> assert false
  | S.KEffect -> l
  | S.KArrow(k1, k2) ->
    if erasable k1 then build_effect_proxy k2 l
    else make_node (T.EFun(T.Var.fresh (), build_effect_proxy k2 l))

let tr_head env head =
  match head with
  | S.HVar x -> make_node (T.EVar (Env.lookup_effect env x))
  | S.HAny k -> build_effect_proxy k (make_node T.EEffPure)

let rec tr_type_params :
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

and tr_effect_row env acc row =
  match row with
  | [] -> acc
  | S.TNeu(head, params) :: row ->
    let head = tr_head env head in
    let eff = tr_type_params env head params in
    tr_effect_row env (make_node (T.EEffCons(acc, eff))) row

and tr_type : type k. Env.t -> k S.typ -> T.expr = fun env tp ->
    match S.Type.view tp with
    | S.TVArrow _    -> assert false
    | S.TVForall _   -> assert false 
    | S.TVEffect row ->
      tr_effect_row env (make_node T.EEffPure) row
    | S.TVNeutral(S.TNeu(head, params)) ->
      tr_type_params env (tr_head env head) params
    | S.TVFun(x, tp) ->
      if erasable (S.TVar.kind x) then tr_type env tp
      else
        let (env, x) = Env.add_effect env x in
        make_node (T.EFun(x, tr_type env tp))

let rec tr_coercion env c =
  { meta = c.meta
  ; data =
    match c.data with
    | S.CLift l       -> T.ECLift(tr_type env l)
    | S.CSwap(l1, l2) -> T.ECSwap(tr_type env l1, tr_type env l2)
    | S.CCons(l, c)   -> T.ECCons(tr_type env l, tr_coercion env c)
    | S.CComp(c1, c2) -> T.ECComp(tr_coercion env c1, tr_coercion env c2)
  }

let rec tr_expr env e =
  let meta = e.meta in
  let make data = { meta; data } in
  match e.data with
  | S.ENum n -> make (T.ENum n)
  | S.EChar c -> make (T.EChar c)
  | S.EString s -> make (T.EString s)
  | S.EVar x -> make (T.EVar (Env.lookup_var env x))
  | S.EFun(x, body) ->
    let (env, y) = Env.add_var env x in
    make (T.EFun(y, tr_expr env body))
  | S.ETFun(x, body) ->
    if erasable (S.TVar.kind x) then tr_expr env body
    else
      let (env, x) = Env.add_effect env x in
      make (T.EFun(x, tr_expr env body))
  | S.EOp(S.TNeu(l, _), n, targs, args) ->
    let targs = Utils.ListExt.filter_map (fun (S.Type.Pack tp) ->
        if erasable (S.Type.kind tp) then None
        else Some (tr_type env tp)
      ) targs in
    make (T.EOp(tr_effect_label env l, n,
        targs @ List.map (tr_expr env) args))
  | S.ECtor(_, n, targs, args) ->
    let targs = Utils.ListExt.filter_map (fun (S.Type.Pack tp) ->
        if erasable (S.Type.kind tp) then None
        else Some (tr_type env tp)
      ) targs in
    make (T.ECtor(n, targs @ List.map (tr_expr env) args))
  | S.EApp(e1, e2) ->
    make (T.EApp(tr_expr env e1, tr_expr env e2))
  | S.ETApp(e, tp) ->
    if erasable (S.Type.kind tp) then tr_expr env e
    else make (T.EApp(tr_expr env e, (tr_type env tp)))
  | S.ELet(x, e1, e2) ->
    let e1 = tr_expr env e1 in
    let (env, y) = Env.add_var env x in
    make (T.ELet(y, e1, tr_expr env e2))
  | S.EFix(rfs, e) ->
    let env = List.fold_left (fun env (x, _) ->
        fst (Env.add_var env x)
      ) env rfs in
    make (T.EFix(List.map (tr_rec_function env) rfs, tr_expr env e))
  | S.EHandle(S.TNeu(l, _), e, x, ret, hs) ->
    let l  = tr_effect_label env l in
    let e  = tr_expr env e in
    let hs = List.map (fun (targs, xs, r, e) ->
        let targs = List.filter (fun (S.TVar.Pack x) ->
            not (erasable (S.TVar.kind x))
          ) targs in
        let (env, targs) = Utils.ListExt.fold_map (fun env (S.TVar.Pack x) ->
            Env.add_effect env x
          ) env targs in
        let (env, xs) = Env.add_vars env xs in
        let (env, r)  = Env.add_var env r in
        (targs @ xs, r, tr_expr env e)
      ) hs in
    let (env, x) = Env.add_var env x in
    let ret = tr_expr env ret in
    make (T.EHandle(l, e, x, ret, hs))
  | S.EMatch(_, e, cls, _) ->
    make (T.EMatch(tr_expr env e,
      List.map (fun (targs, xs, e) ->
        let targs = List.filter (fun (S.TVar.Pack x) ->
            not (erasable (S.TVar.kind x))
          ) targs in
        let (env, targs) = Utils.ListExt.fold_map (fun env (S.TVar.Pack x) ->
            Env.add_effect env x
          ) env targs in
        let (env, xs) = Env.add_vars env xs in
        (targs @ xs, tr_expr env e)
      ) cls))
  | S.ETypedef(tds, e) ->
    tr_typedefs meta env tds e
  | S.EAbsType(x, e) ->
    if erasable (S.TVar.kind x) then tr_expr env e
    else
      let (env, x) = Env.add_effect env x in
      make (T.ENewEffect(x, tr_expr env e))
  | S.ECoerce(c, e) ->
    make (T.ECoerce(tr_coercion env c, tr_expr env e))
  | S.EExtern(name, _) -> make (T.EExtern name)
  | S.EReplExpr(e, seal, repl, _, _) ->
    make (T.EReplExpr(tr_expr env e, seal, (fun () -> tr_expr env (repl ()))))
  | S.ERepl(seal, repl, _, _) ->
    make (T.ERepl(seal, (fun () -> tr_expr env (repl ()))))

and tr_rec_function env (x, body) =
  let x = Env.lookup_var env x in
  let rec loop e =
    match e.data with
    | T.EFun(y, body) -> (x, y, body)
    | T.ECoerce(_, e) -> loop e
    | _ -> assert false
  in loop (tr_expr env body)

and tr_typedefs meta env tds e =
  let make data = { meta; data } in
  match tds with
  | [] -> tr_expr env e
  | S.TypeDef(l, _, S.TDEffect _) :: tds ->
    let (env, l0) = Env.add_effect_label env l in
    let (env, l1)  = Env.add_effect env l in
    make (T.ENewEffect(l0,
    make (T.ELet(l1, build_effect_proxy (S.TVar.kind l) (make (T.EVar l0)),
        tr_typedefs meta env tds e))))
  | S.TypeDef(_, _, S.TDData _) :: tds -> tr_typedefs meta env tds e

let tr_expr e =
  tr_expr (Env.init ()) e

let _ =
  Flow.register_transform
    ~contracts: [ CommonTags.unique_vars ]
    ~source: S.flow_node
    ~target: T.flow_node
    tr_expr
