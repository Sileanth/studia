open Lang.Node
open Lang.Explicit
open Utils.EqDec

module B = Predef.Types
module WF = Lang.CoreCommon.WellFormedness

let as_arrow tp =
  match Type.view tp with
  | TVArrow(tp1, tp2, eff) -> (tp1, tp2, eff)
  | _ -> failwith "Internal type error (not an arrow type)"

let as_forall tp =
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

let rec coerce_compute_effect env c =
  let (data, eff_in, eff_out) = coerce_compute_effect_aux env c.data in
  { meta = (c.meta
      |> Utils.Seal.add Keys.meta_env env
      |> Utils.Seal.add Keys.ecrc_in_effect eff_in
      |> Utils.Seal.add Keys.ecrc_out_effect eff_out)
  ; data = data
  }, eff_in, eff_out

and coerce_compute_effect_aux env c =
  match c with
  | ECId e   ->
    assert (WF.type_well_formed env e);
    ECId e, e, e
  | ECLift e ->
    assert (WF.type_well_formed env e);
    ECLift e, Type.eff_pure, e
  | ECSwap (e1, e2) ->
    assert (WF.type_well_formed env e1);
    assert (WF.type_well_formed env e2);
    ECSwap (e1, e2), Type.eff_cons e1 e2, Type.eff_cons e2 e1
  | ECConsL (e, c)  ->
    assert (WF.type_well_formed env e);
    let (c, ef, et) = coerce_compute_effect env c
    in ECConsL (e, c), Type.eff_cons e ef, Type.eff_cons e et
  | ECConsR (c, e) ->
    assert (WF.type_well_formed env e);
    let (c, ef, et) = coerce_compute_effect env c
    in ECConsR (c, e), Type.eff_cons ef e, Type.eff_cons et e
  | ECComp (c1, c2) ->
    let (c1, e1_in, e1_out) = coerce_compute_effect env c1
    and (c2, e2_in, e2_out) = coerce_compute_effect env c2
    in if Subtyping.equiv env e1_out e2_in
       then ECComp (c1, c2), e1_in, e2_out
       else failwith "Internal type error (effect mismatch)"

let coerce_effect env c eff_in =
  assert (WF.type_well_formed env eff_in);
  let (c, e_in, e_out) = coerce_compute_effect env c in
  if Subtyping.equiv env eff_in e_in
  then c, e_out
  else failwith "Internal type error (effect mismatch)"

let coerce_effect_back env c eff_out =
  let (c, e_in, e_out) = coerce_compute_effect env c in
  if Subtyping.equiv env eff_out e_out
  then c, e_in
  else failwith "Internal type error (effect mismatch)"

let rec coerce_type env c tp =
  let (data, tp') = coerce_type_aux env c.data tp in
  { meta = (c.meta
      |> Utils.Seal.add Keys.meta_env env
      |> Utils.Seal.add Keys.vcrc_in_type tp
      |> Utils.Seal.add Keys.vcrc_out_type tp')
  ; data = data
  }, tp'

and coerce_type_back env c tp' =
  let (data, tp) = coerce_type_back_aux env c.data tp' in
  { meta = (c.meta
      |> Utils.Seal.add Keys.meta_env env
      |> Utils.Seal.add Keys.vcrc_in_type tp
      |> Utils.Seal.add Keys.vcrc_out_type tp')
  ; data = data
  }, tp

and coerce_type_aux env c tp =
  match c with
  | VCId -> (VCId, tp)
  | VCArrow(c1, c2, c3) ->
    let (tp1, tp2, eff) = as_arrow tp in
    let (c1, tp1) = coerce_type_back env c1 tp1 in
    let (c2, tp2) = coerce_type env c2 tp2 in
    let (c3, eff) = coerce_effect env c3 eff in
    (VCArrow(c1, c2, c3), Type.arrow tp1 tp2 eff)

and coerce_type_back_aux env c tp =
  match c with
  | VCId -> (VCId, tp)
  | VCArrow(c1, c2, c3) ->
    let (tp1, tp2, eff) = as_arrow tp in
    let (c1, tp1) = coerce_type env c1 tp1 in
    let (c2, tp2) = coerce_type_back env c2 tp2 in
    let (c3, eff) = coerce_effect_back env c3 eff in
    (VCArrow(c1, c2, c3), Type.arrow tp1 tp2 eff)

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
    if Subtyping.equiv env eff Type.eff_pure then
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
    let (atp, vtp, eff2) = as_arrow tp in
    if Subtyping.equiv env eff1 eff2 then begin
      let e2 = check_type_eff env e2 atp eff2 in
      (EApp(e1, e2), vtp, eff1)
    end else
      failwith "Internal type error (effect mismatch)"
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
    let (e1, eff) = check_type env e1 tp1 in
    let (e2, tp2) = infer_type' (Env.add_var env x) e2 eff in
    (ELet(x, e1, e2), tp2, eff)
  | ELetPure(x, e1, e2) ->
    let tp1 = Var.type_of x in
    assert (WF.type_well_formed env tp1);
    let e1 = check_type_eff env e1 tp1 Type.eff_pure in
    let (e2, tp2, eff) = infer_type (Env.add_var env x) e2 in
    (ELetPure(x, e1, e2), tp2, eff)
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
    let (e, tp, eff) = infer_type env e in
    let xtp = Var.type_of x in
    assert (WF.type_well_formed env xtp);
    assert (Subtyping.equiv env tp xtp);
    (* too general here, too *)
    begin match Subtyping.select_effect env (Type.of_neutral neu) eff with
    | Subtyping.Selected eff ->
      let (ret, rtp) = infer_type' (Env.add_var env x) ret eff in
      let ops = Env.lookup_effect env neu in
      let hs = List.map2 (fun
            (OpDecl(_, tvs1, inputs, output))
            (tvs2, xs, r, e) ->
          let env = Env.add_tvars env tvs2 in
          let tvs = List.map (fun (TVar.Pack x) ->
              (Type.Pack (Type.var x))) tvs2 in
          let sub = check_type_args env tvs1 tvs in
          assert (List.for_all2 (fun tp x ->
              let tp  = Type.subst_m sub tp in
              let xtp = Var.type_of x in
              WF.type_well_formed env xtp &&
              Subtyping.equiv env tp xtp
            ) inputs xs);
          let output = Type.subst_m sub output in
          let restp = Var.type_of r in
          assert (WF.type_well_formed env restp);
          assert (Subtyping.equiv env restp (Type.arrow output rtp eff));
          let env = Env.add_vars env xs in
          let env = Env.add_var env r in
          (tvs2, xs, r, check_type_eff env e rtp eff)
        ) ops hs in
      (EHandle(neu, e, x, ret, hs), rtp, eff)
    | Subtyping.Opened _ | Subtyping.NotPossible ->
      failwith "Internal type error (missing effect)"
    end
  | EMatch(neu, e, clauses, rtp) ->
    assert (WF.neutral_type_well_formed env neu);
    let (e, tp, eff) = infer_type env e in
    assert (Subtyping.equiv env tp (Type.of_neutral neu));
    assert (WF.type_well_formed env rtp);
    let ctors = Env.lookup_adt env neu in
    let clauses = List.map2 (fun (ADTCtor(_, tvs1, tps)) (tvs2, xs, e) ->
        let env = Env.add_tvars env tvs2 in
        let tvs = List.map (fun (TVar.Pack x) ->
            (Type.Pack (Type.var x))) tvs2 in
        let sub = check_type_args env tvs1 tvs in
        assert (List.for_all2 (fun tp x ->
            let tp  = Type.subst_m sub tp in
            let xtp = Var.type_of x in
            WF.type_well_formed env xtp &&
            Subtyping.equiv env tp xtp
          ) tps xs);
        let env = Env.add_vars env xs in
        (tvs2, xs, check_type_eff env e rtp eff)
      ) ctors clauses in
    (EMatch(neu, e, clauses, rtp), rtp, eff)
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
  | ECoerce(vc, ec, e) ->
    let (e, tp, eff) = infer_type env e in
    let (vc, tp)  = coerce_type env vc tp in
    let (ec, eff) = coerce_effect env ec eff in
    (ECoerce(vc, ec, e), tp, eff)
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

and infer_type' env e eff =
  let (e, tp, eff') = infer_type env e in
  if not (Subtyping.equiv env eff eff') then
    failwith "Internal type error (effect mismatch)"
  else (e, tp)

and check_type_eff env e tp eff =
  let (e, tp', eff') = infer_type env e in
  if not (Subtyping.equiv env tp tp') then
    failwith "Internal type error (type mismatch)"
  else if not (Subtyping.equiv env eff eff') then
    failwith "Internal type error (effect mismatch)"
  else e

and check_type env e tp =
  let (e, tp', eff) = infer_type env e in
  if Subtyping.equiv env tp tp' then (e, eff)
  else failwith "Internal type error (type mismatch)"

let check_expr e =
  let env = Predef.DB.core_env () in
  let eff0 = B.IO.core_tp in
  let (e, _, eff) = infer_type (Predef.DB.core_env ()) e in
  if Subtyping.equiv env eff eff0 then e
  else
    failwith "Internal type error (unhandled effectes)"

let _ =
  Flow.register_transform
    ~require:   [ CommonTags.unique_type_vars ]
    ~contracts:
      [ Tags.eff_coercion_types
      ; Tags.val_coercion_types
      ; Tags.expr_type_info
      ]
    ~preserves: [ CommonTags.unique_vars; CommonTags.unique_type_vars ]
    ~source: flow_node
    ~target: flow_node
    check_expr
