open Lang.Node
open ToExplicitPriv

module S = Lang.Unif
module T = Lang.Explicit

include Errors

let make_type_subst xs tps =
  List.fold_left2 (fun sub (T.TVar.Pack x) (T.Type.Pack tp) ->
    match T.Kind.equal (T.TVar.kind x) (T.Type.kind tp) with
    | Utils.EqDec.Equal -> T.Type.TVarMap.add x tp sub
    | Utils.EqDec.NotEqual -> failwith "kind mismatch"
  ) T.Type.TVarMap.empty xs tps

let rec select_return_handlers clauses =
  match clauses with
  | [] -> []
  | h :: clauses ->
    begin match h.data with
    | S.HReturn(pat, body) -> (pat, body) :: select_return_handlers clauses
    | S.HOp _ -> select_return_handlers clauses
    end

let rec select_handlers n clauses =
  match clauses with
  | [] -> []
  | h :: clauses ->
    begin match h.data with
    | S.HOp(m, ets, pats, r, body) when n = m ->
      (ets, pats, r, body) :: select_handlers n clauses
    | S.HReturn _ | S.HOp _ -> select_handlers n clauses
    end

let rec build_tapp env meta e args =
  match args with
  | [] -> e
  | tp :: args ->
    let T.Type.Pack tp = Type.tr_type env tp in
    build_tapp env meta
      { meta; data = T.ETApp(e, tp) }
      args

let make_ctor_proxy meta l n ets tps =
  let make_fun = Utils.ListExt.map_cps (fun tp cont ->
    let x = T.Var.fresh tp in
    { meta; data = T.EFun(x, cont { meta; data = T.EVar x })}) in
  make_fun tps (fun args ->
  { meta; data = T.ECtor(l, n, ets, args) })

let make_op_proxy meta l n ets tps =
  let make_fun = Utils.ListExt.map_cps (fun tp cont ->
    let x = T.Var.fresh tp in
    { meta; data = T.EFun(x, cont { meta; data = T.EVar x })}) in
  make_fun tps (fun args ->
  { meta; data =T.EOp(l, n, ets, args) })

let rec abstract_types env meta tcs gen =
  match tcs with
  | [] -> gen env
  | c :: tcs ->
    let (env, T.TVar.Pack x) = Env.add_tconst env c in
    let e = abstract_types env meta tcs gen in
    { meta = meta; data = T.ETFun(x, e) }

let apply_scheme_subst env subst =
  List.fold_left (fun env (x, tp) ->
    Env.add_tvar env x tp
  ) env subst

let prepare_rec_functions env rfs =
  List.fold_left (fun env (x, _) ->
    let atp = Type.tr_scheme env (S.Var.scheme x) in
    let y = T.Var.fresh ~name:(S.Var.name x) atp in
    Env.add_var env x y
  ) env rfs

let rec tr_expr env e =
  let meta = Utils.Seal.singleton Utils.Position.key e.meta.S.em_pos in
  let make data = { meta; data } in
  match e.data with
  | S.ENum n -> make (T.ENum n)
  | S.EChar c -> make (T.EChar c)
  | S.EString s -> make (T.EString s)
  | S.EVar x ->
    let scope = Env.type_scope env in
    let y = Env.lookup_var env x in
    let scheme = S.Var.scheme x in
    let (targs, tp) = S.Scheme.open_s ~scope scheme in
    Type.match_type tp e.meta.S.em_type;
    build_tapp env meta (make (T.EVar y)) (List.map S.Type.var targs)
  | S.ECtor(x, tps, n, ets) ->
    let Type.Params(kind, params) = Type.tr_type_params env tps T.KType in
    let neu = T.TNeu(T.HVar (Type.tr_tconst env x kind), params) in
    let ets = List.map (Type.tr_type env) ets in
    let T.ADTCtor(_, tvs, tps) = Env.lookup_ctor env neu n in
    let sub = make_type_subst tvs ets in
    let tps = List.map (T.Type.subst_m sub) tps in
    make_ctor_proxy meta neu n ets tps
  | S.ECoerce(vc, ec, e) -> make(
    T.ECoerce(
      Coercion.tr_val_coercion env vc,
      Coercion.tr_eff_coercion env ec,
      tr_expr env e))
  | S.EFun(pat, body) ->
    let name = PatternMatch.name_of_pattern pat in
    let tp   = PatternMatch.type_of_pattern pat in
    let x    = T.Var.fresh ~name:name (Type.tr_ttype env tp) in
    make (T.EFun(x, PatternMatch.tr_match meta env x
        [ pat, fun env -> tr_expr env body ]
        ~typ: (Type.tr_ttype  env body.meta.S.em_type)
        ~eff: (Type.tr_effect env body.meta.S.em_effect)))
  | S.EOp(l, tps, n, ets) ->
    let Type.Params(kind, params) = Type.tr_type_params env tps T.KEffect in
    let neu = T.TNeu(T.HVar (Type.tr_tconst env l kind), params) in
    let ets = List.map (Type.tr_type env) ets in
    let T.OpDecl(_, tvs, inputs, output) = Env.lookup_op env neu n in
    let sub = make_type_subst tvs ets in
    let inputs = List.map (T.Type.subst_m sub) inputs in
    make_op_proxy meta neu n ets inputs
  | S.EApp(e1, e2) ->
    make (T.EApp(tr_expr env e1, tr_expr env e2))
  | S.ELet(x, e1, e2) ->
    let scheme = S.Var.scheme x in
    let atp = Type.tr_scheme env scheme in
    let e1 = abstract_types env meta (S.Scheme.qvars scheme) (fun env ->
      let env = apply_scheme_subst env (S.Scheme.subst scheme) in
      tr_expr env e1)
    in
    let y = T.Var.fresh ~name:(S.Var.name x) atp in
    make (T.ELetPure(y, e1, tr_expr (Env.add_var env x y) e2))
  | S.EFix(rfs, rest) ->
    let env = prepare_rec_functions env rfs in
    let rfs = List.map (tr_rec_function env meta) rfs in
    make (T.EFix(rfs, tr_expr env rest))
  | S.EMatch(mexp, []) ->
    let tp = mexp.meta.S.em_type in
    let rtp = Type.tr_ttype env e.meta.S.em_type in
    begin match T.Type.to_neutral_opt (Type.tr_ttype env tp) with
    | Some neu when Env.is_empty_adt env neu ->
      make (T.EMatch(neu, tr_expr env mexp, [], rtp))
    | _ -> raise (Errors.non_empty_type e.meta.S.em_pos tp)
    end
  | S.EMatch(mexp, (((p0, _) :: _) as clauses)) ->
    let name = PatternMatch.name_of_pattern p0 in
    let tp   = PatternMatch.type_of_pattern p0 in
    let x    = T.Var.fresh ~name:name (Type.tr_ttype env tp) in
    make (T.ELet(x, tr_expr env mexp,
      PatternMatch.tr_match meta env x
        (List.map (fun (p, e) -> (p, fun env -> tr_expr env e)) clauses)
        ~typ: (Type.tr_ttype  env e.meta.S.em_type)
        ~eff: (Type.tr_effect env e.meta.S.em_effect)))
  | S.EHandle(l, tps, hexp, clauses) ->
    let Type.Params(kind, params) = Type.tr_type_params env tps T.KEffect in
    let neu = T.TNeu(T.HVar (Type.tr_tconst env l kind), params) in
    let tp = Type.tr_ttype env hexp.meta.S.em_type in
    let eff = Type.tr_effect env e.meta.S.em_effect in
    let hexp = tr_expr env hexp in
    let ops = Env.lookup_effect env neu in
    let (x, body) = tr_return_handler env meta neu clauses tp eff in
    make (T.EHandle(neu, hexp, x, body,
      List.mapi (fun i op -> tr_op_handler env meta neu i op clauses) ops))
  | S.ERecord(l, tps, fdefs) ->
    let fields = Env.lookup_record env l in
    let Type.Params(kind, params) = Type.tr_type_params env tps T.KType in
    let neu = T.TNeu(T.HVar (Type.tr_tconst env l kind), params) in
    let fields = Array.of_list
      (List.map (fun f -> Utils.Either.Left f) fields) in
    tr_field_defs env fields fdefs (fun fields ->
    let fields = Array.to_list fields in
    begin match Utils.ListExt.partition_map (fun x -> x) fields with
    | [], vals  -> make (
      T.ECoerce(
        make T.VCId,
        make (T.ECLift (Type.tr_effect env e.meta.S.em_effect)),
        make (T.ECtor(neu, 0, [], vals))))
    | fields, _ ->
      raise (Errors.non_exhaustive_record meta fields)
    end)
  | S.ETypedef(tds, e) ->
    let env = TypeDef.prepare_env_s env tds in
    let (env, tds) = TypeDef.tr_typedefs env tds in
    let e = tr_expr env e in
    make (T.ETypedef(tds, e))
  | S.EAbsType(x, e) ->
    let (env, T.TVar.Pack x) = Env.add_tconst env x in
    let e = tr_expr env e in
    make (T.EAbsType(x, e))
  | S.EExtern(name, scheme) ->
    let scope = Env.type_scope env in
    let (targs, tp) = S.Scheme.open_s ~scope scheme in
    Type.match_type tp e.meta.S.em_type;
    build_tapp env meta
      (make (T.EExtern(name, Type.tr_scheme env scheme)))
      (List.map S.Type.var targs)
  | S.EReplExpr(re, seal, repl) ->
    let re  = tr_expr env re in
    let tp  = Type.tr_ttype env e.meta.S.em_type in
    let eff = Type.tr_effect env e.meta.S.em_effect in
    make (T.EReplExpr(re, seal, (fun () -> tr_expr env (repl ())), tp, eff))
  | S.ERepl(seal, repl) ->
    let tp  = Type.tr_ttype env e.meta.S.em_type in
    let eff = Type.tr_effect env e.meta.S.em_effect in
    make (T.ERepl(seal, (fun () -> tr_expr env (repl ())), tp, eff))

and tr_rec_function env meta (x, body) =
  let scheme = S.Var.scheme x in
  let x = Env.lookup_var env x in
  let body = abstract_types env meta (S.Scheme.qvars scheme) (fun env ->
      let env = apply_scheme_subst env (S.Scheme.subst scheme) in
      tr_expr env body)
  in (x, body)

and tr_return_handler env meta l clauses rtp eff =
  match select_return_handlers clauses with
  | [] ->
    let x = T.Var.fresh rtp in
    (x, { meta; data = T.ECoerce(
          { meta; data = T.VCId },
          { meta; data = T.ECLift eff },
          { meta; data = T.EVar x })})
  | ((pat, body) :: _) as hs ->
    let name = PatternMatch.name_of_pattern pat in
    let tp   = PatternMatch.type_of_pattern pat in
    let x    = T.Var.fresh ~name:name (Type.tr_ttype env tp) in
    (x, PatternMatch.tr_match meta env x
      (List.map (fun (p, e) -> (p, fun env -> tr_expr env e)) hs)
      ~typ: (Type.tr_ttype  env body.meta.S.em_type)
      ~eff: (Type.tr_effect env body.meta.S.em_effect))

and tr_op_handler env meta l i (T.OpDecl(name, ets, inputs, _)) clauses =
  match select_handlers i clauses with
  | [] ->
    raise (Errors.non_exhaustive_handler meta name)
  | ((tvs, pats, rx, body) :: _) as hs ->
    let (ren, ets) = Utils.ListExt.fold_map (fun ren (T.TVar.Pack x) ->
        let y = T.TVar.clone x in
        (T.TVar.TVarMap.add x y ren, T.TVar.Pack y)
      ) T.TVar.TVarMap.empty ets in
    let env' = Env.add_tconsts' env tvs ets in
    let inputs = List.map (T.Type.rename_m ren) inputs in
    let xs = List.map2 (fun p tp ->
        T.Var.fresh ~name:(PatternMatch.name_of_pattern p) tp
      ) pats inputs in
    let rname = S.Var.name rx in
    let rtp   = S.Var.typ  rx in
    let r     = T.Var.fresh ~name:rname (Type.tr_ttype env' rtp) in
    (ets, xs, r, PatternMatch.tr_matches meta env name xs
      (List.map (fun (tvs, pats, rx, body) ->
        let env = Env.add_tconsts' env tvs ets in
        (env, pats, fun env -> tr_expr (Env.add_var env rx r) body)
      ) hs)
      ~typ: (Type.tr_ttype  env body.meta.S.em_type)
      ~eff: (Type.tr_effect env body.meta.S.em_effect))

and tr_field_def env fields (n, fdef) cont =
  let meta = Utils.Seal.singleton Utils.Position.key fdef.meta in
  let name =
    match fields.(n) with
    | Utils.Either.Left name -> name
    | Utils.Either.Right _   -> assert false (* Fields are defined once *)
  in
  match fdef.data with
  | S.FieldDefMono e ->
    let tp = Type.tr_ttype env e.meta.S.em_type in
    let x  = T.Var.fresh ~name tp in
    let e  = tr_expr env e in
    fields.(n) <- Utils.Either.Right { meta; data = T.EVar x };
    { meta; data = T.ELet(x, e, cont fields) }
  | S.FieldDefPoly(ets, e) ->
    let (env, ets) = Env.add_tconsts env ets in
    let tp = Type.tr_ttype env e.meta.S.em_type in
    let x_tp = T.Type.foralls ets tp in
    let x    = T.Var.fresh ~name x_tp in
    let e    = tr_expr env e in
    let e_def = List.fold_right (fun (T.TVar.Pack x) e ->
        { meta; data = T.ETFun(x, e) }
      ) ets e in
    fields.(n) <- Utils.Either.Right { meta; data = T.EVar x };
    { meta; data = T.ELetPure(x, e_def, cont fields) }

and tr_field_defs env fields fdefs cont =
  match fdefs with
  | []            -> cont fields
  | fdef :: fdefs ->
    tr_field_def  env fields fdef  (fun fields ->
    tr_field_defs env fields fdefs cont)

let tr_expr e =
  tr_expr (Env.init ()) e

let _ =
  Flow.register_transform
    ~source: S.flow_node
    ~target: T.flow_node
    tr_expr
