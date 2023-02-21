open Lang.Node
open Lang.Core
open Utils.EqDec

module B = Predef.Types
module WF = Lang.CoreCommon.WellFormedness

let as_arrow (tp : ttype) =
  match Type.view tp with
  | TVArrow(tp1, tp2, eff) -> (tp1, tp2, eff)
  | _ -> failwith "Internal type error (not an arrow type)"

let as_forall (tp : ttype) =
  match Type.view tp with
  | TVForall(x, tp) -> (TVar.Pack x, tp)
  | _ -> failwith "Internal type error (not a polymorphic type)"

let check_type_args env tvs tps =
  List.fold_left2 (fun sub (TVar.Pack x) (Type.Pack tp) ->
    assert (WF.type_well_formed env tp);
    match Kind.equal (TVar.kind x) (Type.kind tp) with
    | Equal -> Type.TVarMap.add x tp sub
    | NotEqual -> failwith "Internal kind error"
  ) Type.TVarMap.empty tvs tps

let rec coerce_effect env c eff =
  let (data, eff') = coerce_effect_aux env c.data eff in
  { meta = (c.meta
      |> Utils.Seal.add Keys.meta_env env
      |> Utils.Seal.add Keys.crc_in_effect eff
      |> Utils.Seal.add Keys.crc_out_effect eff')
  ; data = data
  }, eff'

and coerce_effect_aux env c eff =
  match c with
  | CLift l ->
    assert (WF.type_well_formed env l);
    (CLift l, Type.eff_cons l eff)
  | CSwap(l1, l2) ->
    assert (WF.type_well_formed env l1 && WF.type_well_formed env l2);
    let eff =
      match Subtyping.select_effect env l1 eff with
      | Subtyping.Selected eff | Subtyping.Opened eff -> eff
      | Subtyping.NotPossible ->
        failwith "Internal type error (missing effect)"
    in
    let eff =
      match Subtyping.select_effect env l2 eff with
      | Subtyping.Selected eff | Subtyping.Opened eff -> eff
      | Subtyping.NotPossible ->
        failwith "Internal type error (missing effect)"
    in
    (CSwap(l1, l2), Type.eff_conses [ l2 ; l1 ; eff ])
  | CCons(l, c) ->
    assert (WF.type_well_formed env l);
    let eff =
      match Subtyping.select_effect env l eff with
      | Subtyping.Selected eff | Subtyping.Opened eff -> eff
      | Subtyping.NotPossible ->
        failwith "Internal type error (missing effect)"
    in
    let (c, eff) = coerce_effect env c eff in
    (CCons(l, c), Type.eff_cons l eff)
  | CComp(c1, c2) ->
    let (c1, eff) = coerce_effect env c1 eff in
    let (c2, eff) = coerce_effect env c2 eff in
    (CComp(c1, c2), eff)


let rec infer_type env (e : expr) : expr * ttype * effect =
  let (data, tp, eff) = infer_type_aux env e.data in
  { meta = (e.meta
      |> Utils.Seal.add Keys.meta_env env
      |> Utils.Seal.add Keys.expr_type tp
      |> Utils.Seal.add Keys.expr_effect eff)
  ; data = data
  }, tp, eff

and infer_type_aux env (e : expr_data) : expr_data * ttype * effect =
  match e with
  | ENum n -> (ENum n, B.Int.core_tp, Type.eff_pure)
  | EChar c -> (EChar c, B.Char.core_tp, Type.eff_pure)
  | EString s -> (EString s, B.String.core_tp, Type.eff_pure)
  | EVar x ->
    assert (Env.has_var env x);
    (EVar x, Var.type_of x, Type.eff_pure)
  | EFun(x, body) ->
    let atp = Var.type_of x in
    assert (WF.type_well_formed env atp);
    let (body, rtp, eff) = infer_type (Env.add_var env x) body in
    (EFun(x, body), Type.arrow atp rtp eff, Type.eff_pure)
  | ETFun(x, body) ->
    assert (not (Env.has_tvar env x));
    let (body, tp, eff) = infer_type (Env.add_tvar env x) body in
    if Subtyping.match_type env eff Type.eff_pure then
      (ETFun(x, body), Type.forall x tp, Type.eff_pure)
    else
      failwith "Internal type error (effectful polymorphic function)"
  | EOp(neu, n, targs, args) ->
    assert (WF.neutral_type_well_formed env neu);
    let (OpDecl(_, tvs, inputs, output)) = Env.lookup_op env neu n in
    let sub = check_type_args env tvs targs in
    let args = List.map2 (fun e tp ->
        let tp = Type.subst_m sub tp in
        check_type_eff env e tp Type.eff_pure
      ) args inputs in
    (EOp(neu, n, targs, args), Type.subst_m sub output,
      Type.of_neutral neu)
  | ECtor(neu, n, targs, args) ->
    assert (WF.neutral_type_well_formed env neu);
    let (ADTCtor(_, tvs, tps)) = Env.lookup_ctor env neu n in
    let sub = check_type_args env tvs targs in
    let args = List.map2 (fun e tp ->
        let tp = Type.subst_m sub tp in
        check_type_eff env e tp Type.eff_pure
      ) args tps in
    (ECtor(neu, n, targs, args), Type.of_neutral neu, Type.eff_pure)
  | EApp(e1, e2) ->
    let (e1, tp, eff1) = infer_type env e1 in
    let (atp, vtp, eff3) = as_arrow tp in
    let (e2, eff2) = check_type env e2 atp in
    (EApp(e1, e2), vtp,
      Subtyping.lub_types env [ eff1 ; eff2 ; eff3 ])
  | ETApp(e, tp) ->
    assert (WF.type_well_formed env tp);
    let (e, etp, eff) = infer_type env e in
    let (TVar.Pack x, btp) = as_forall etp in
    begin match Kind.equal (TVar.kind x) (Type.kind tp) with
    | Equal -> (ETApp(e, tp), Type.subst x tp btp, eff)
    | NotEqual ->
      failwith "Internal kind error (in type application)"
    end
  | ELet(x, e1, e2) ->
    let tp1 = Var.type_of x in
    assert (WF.type_well_formed env tp1);
    let (e1, eff1) = check_type env e1 tp1 in
    let (e2, tp2, eff2) = infer_type (Env.add_var env x) e2 in
    (ELet(x, e1, e2), tp2, Subtyping.lub_type env eff1 eff2)
  | EFix(rfs, e) ->
    let env = List.fold_left (fun env (x, _) ->
        assert (WF.type_well_formed env (Var.type_of x));
        Env.add_var env x
      ) env rfs in
    let rfs = List.map (fun (x, body) ->
        assert (Expr.is_function body);
        (x, check_type_eff env body (Var.type_of x) Type.eff_pure)
      ) rfs in
    let (e, tp, eff) = infer_type env e in
    (EFix(rfs, e), tp, eff)
  | EHandle(neu, e, x, ret, hs) ->
    assert (WF.neutral_type_well_formed env neu);
    let ops = Env.lookup_effect env neu in
    let (hs, tp, eff) = infer_handlers_type env ops hs in
    let xtp = Var.type_of x in
    assert (WF.type_well_formed env xtp);
    let ret = check_type_eff (Env.add_var env x) ret tp eff in
    let e   = check_type_eff env e xtp
      (Type.eff_cons (Type.of_neutral neu) eff) in
    (EHandle(neu, e, x, ret, hs), tp, eff)
  | EMatch(neu, e, clauses, rtp) ->
    assert (WF.neutral_type_well_formed env neu);
    let (e, tp, eff) = infer_type env e in
    assert (Subtyping.match_type env tp (Type.of_neutral neu));
    assert (WF.type_well_formed env rtp);
    let ctors = Env.lookup_adt env neu in
    let (clauses, eff') = check_clauses_type env ctors clauses rtp in
    (EMatch(neu, e, clauses, rtp), rtp, Subtyping.lub_type env eff eff')
  | ETypedef(tds, e) ->
    let old_env = env in
    let env = Env.prepare_typedefs env ~fresh:true tds in
    assert (List.for_all (WF.typedef_well_formed env) tds);
    let env = Env.add_typedefs env tds in
    let (e, tp, eff) = infer_type env e in
    assert (WF.type_well_formed old_env tp);
    assert (WF.type_well_formed old_env eff);
    (ETypedef(tds, e), tp, eff)
  | EAbsType(x, e) ->
    let old_env = env in
    let env = Env.add_tvar env ~fresh:true x in
    let (e, tp, eff) = infer_type env e in
    assert (WF.type_well_formed old_env tp);
    assert (WF.type_well_formed old_env eff);
    (EAbsType(x, e), tp, eff)
  | ECoerce(c, e) ->
    let (e, tp, eff) = infer_type env e in
    let (c, eff) = coerce_effect env c eff in
    (ECoerce(c, e), tp, eff)
  | EExtern(name, tp) ->
    assert (WF.type_well_formed env tp);
    (EExtern(name, tp), tp, Type.eff_pure)
  | EReplExpr(e, seal, repl, tp, eff) ->
    assert (WF.type_well_formed env tp);
    assert (WF.type_well_formed env eff);
    let e = check_type_eff env e B.String.core_tp eff in
    (EReplExpr(e, seal,
      (fun () -> check_type_eff env (repl ()) tp eff), tp, eff),
      tp, eff)
  | ERepl(seal, repl, tp, eff) ->
    assert (WF.type_well_formed env tp);
    assert (WF.type_well_formed env eff);
    (ERepl(seal, (fun () -> check_type_eff env (repl ()) tp eff), tp, eff),
      tp, eff)

and check_type env e tp =
  let (e, tp', eff) = infer_type env e in
  if Subtyping.match_type env tp' tp then (e, eff)
  else failwith "Internal type error (type mismatch)"

and check_type_eff env e tp (eff : effect) =
  let (e, eff') = check_type env e tp in
  if Subtyping.match_type env eff' eff then e
  else failwith "Internal type error (effect mismatch)"

and infer_handler_type env (OpDecl(_, tvs1, inputs, output)) (tvs2, xs, r, e) =
  let env = Env.add_tvars env tvs2 in
  let tvs = List.map (fun (TVar.Pack x) -> (Type.Pack (Type.var x))) tvs2 in
  let sub = check_type_args env tvs1 tvs in
  assert (List.for_all2 (fun tp x ->
      let tp  = Type.subst_m sub tp in
      let xtp = Var.type_of x in
      WF.type_well_formed env xtp &&
      Subtyping.equiv env tp xtp
    ) inputs xs);
  let output = Type.subst_m sub output in
  let rtp = Var.type_of r in
  assert (WF.type_well_formed env rtp);
  match Type.view rtp with
  | TVArrow(ans, tp, eff) ->
    assert (Subtyping.equiv env ans output);
    let env = Env.add_vars env xs in
    let env = Env.add_var env r in
    let e = check_type_eff env e tp eff in
    ((tvs2, xs, r, e), tp, eff)
  | _ -> failwith "Internal type error (bad resumption type)"

and infer_handlers_type env ops hs =
  match ops, hs with
  | [], [] -> failwith "empty handler"
  | op :: ops, h :: hs ->
    let (h, tp, eff) = infer_handler_type env op h in
    let hs = check_handlers_type env ops hs tp eff in
    (h :: hs, tp, eff)
  | _ :: _, [] | [], _ :: _ -> assert false

and check_handlers_type env ops hs tp eff =
  match ops, hs with
  | [], [] -> []
  | op :: ops, h :: hs ->
    let (h, tp', eff') = infer_handler_type env op h in
    assert (Subtyping.equiv env tp tp');
    assert (Subtyping.equiv env eff eff');
    let hs = check_handlers_type env ops hs tp eff in
    h :: hs
  | _ :: _, [] | [], _ :: _ -> assert false

and check_clause_type env (ADTCtor(_, tvs1, tps)) (tvs2, xs, e) rtp =
  let env = Env.add_tvars env tvs2 in
  let tvs = List.map (fun (TVar.Pack x) -> (Type.Pack (Type.var x))) tvs2 in
  let sub = check_type_args env tvs1 tvs in
  assert (List.for_all2 (fun tp x ->
      let tp  = Type.subst_m sub tp in
      let xtp = Var.type_of x in
      WF.type_well_formed env xtp &&
      Subtyping.equiv env tp xtp
    ) tps xs);
  let env = Env.add_vars env xs in
  let (e, eff) = check_type env e rtp in
  ((tvs2, xs, e), eff)

and check_clauses_type env ctors clauses rtp =
  match ctors, clauses with
  | [], [] -> ([], Type.eff_pure)
  | ctor :: ctors, cl :: clauses ->
    let (cl, eff1) = check_clause_type env ctor cl rtp in
    let (clauses, eff2) = check_clauses_type env ctors clauses rtp in
    (cl :: clauses, Subtyping.lub_type env eff1 eff2)
  | _ :: _, [] | [], _ :: _ -> assert false

let check_expr e =
  let env = Predef.DB.core_env () in
  let eff0 = B.IO.core_tp in
  let (e, _, eff) = infer_type env e in
  if Subtyping.match_type env eff eff0 then e
  else
    failwith "Internal type error (unhandled effectes)"

let _ =
  Flow.register_transform
    ~require:   [ CommonTags.unique_type_vars ]
    ~contracts: [ Tags.expr_type_info ]
    ~preserves: [ CommonTags.unique_vars; CommonTags.unique_type_vars ]
    ~source: flow_node
    ~target: flow_node
    check_expr
