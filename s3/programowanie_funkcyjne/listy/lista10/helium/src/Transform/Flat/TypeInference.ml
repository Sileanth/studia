open Lang.Node
open TypeInferencePriv

include TypeInferencePriv.Errors

module B = Predef.Types
module S = Lang.Flat
module T = Lang.Unif

(* ========================================================================= *)
(* Coercions *)

let coerce_val pos sc env tp1 tp2 =
  try Unification.coerce_val env tp1 tp2 with
  | Unification.Cannot_unify ->
    raise (Errors.type_mismatch pos sc tp1 tp2 None)
  | Unification.Escapes_scope c ->
    raise (Errors.type_mismatch pos sc tp1 tp2 (Some c))

let unify_types pos sc env tp1 tp2 =
  try Unification.unify env tp1 tp2 with
  | Unification.Cannot_unify ->
    raise (Errors.type_mismatch pos sc tp1 tp2 None)
  | Unification.Escapes_scope c ->
    raise (Errors.type_mismatch pos sc tp1 tp2 (Some c))

let coerce_eff pos env eff1 eff2 =
  try Unification.coerce_eff env eff1 eff2 with
  | Unification.Cannot_unify ->
    raise (Errors.type_mismatch pos ExpressionEffRow eff1 eff2 None)
  | Unification.Escapes_scope c ->
    raise (Errors.type_mismatch pos ExpressionEffRow eff1 eff2 (Some c))

let coerce_expr env e tp eff =
  let pos = e.meta.T.em_pos in
  let vc = coerce_val pos Expression env e.meta.T.em_type tp in
  let ec = coerce_eff pos env e.meta.T.em_effect eff in
  T.ECoerce(vc, ec, e)

let coerce_module_val env ~pos ~dpos ~name body tp =
  let tp1 = body.meta.T.em_type in
  match Unification.coerce_val env tp1 tp with
  | vc ->
    { meta =
      { T.em_pos    = pos
      ; T.em_type   = tp
      ; T.em_effect = body.meta.T.em_effect
      }
    ; data = T.ECoerce(vc, T.ECId body.meta.T.em_effect, body)
    }
  | exception (Unification.Cannot_unify | Unification.Escapes_scope _) ->
    raise (Errors.value_sig_mismatch pos dpos name tp1 tp)

(* ========================================================================= *)
(* Patterns *)

let guess_record_pattern_type env fpats =
  let S.FieldPat(fld, _) = (List.hd fpats).data in
  let fld = Env.lookup_field env fld in
  let scope = Env.type_scope env in
  (fld.Env.fld_record,
    (List.map (fun c -> T.Type.fresh_tvar ~scope (T.TConst.kind c))
      fld.Env.fld_record_args))

let gen_pattern_ltypes env ltps ets =
  match ltps with
  | [] ->
    Utils.ListExt.fold_map (fun env c ->
        let name = "#" ^ T.TConst.name c in
        let kind = T.TConst.kind c in
        let (env, c') = Env.add_tconst' env name kind in
        (env, (c, c'))
    ) env ets
  | _ ->
    let (env, ltps) = Type.check_type_fargs env ltps in
    (env, Utils.ListExt.zip ets ltps)

let rec check_pattern env pat tp =
  let (env, data) =
    match pat.data with
    | S.PWildcard -> (env, T.PWildcard (T.Scheme.close_with [] tp))
    | S.PVar x ->
      let (env, y) = Env.add_var env x (T.Scheme.close_with [] tp) in
      (env, T.PVar y)
    | S.PCtor(cl, ltps, ps) ->
      let ctor = Env.lookup_ctor env cl in
      check_ctor_pattern env
        ~pos:pat.meta 
        ~name:(S.Ctor.name cl)
        ctor ltps ps tp
    | S.PList [] ->
      let lt = Env.list_type ~pos:pat.meta env in
      check_ctor_pattern env
        ~pos:pat.meta
        ~name:lt.Env.lt_nil_name
        lt.Env.lt_nil_ctor [] [] tp
    | S.PList(p :: ps) ->
      let lt = Env.list_type ~pos:pat.meta env in
      check_ctor_pattern env
        ~pos:pat.meta
        ~name:lt.Env.lt_cons_name
        lt.Env.lt_cons_ctor [] [ p; { pat with data = S.PList ps } ] tp
    | S.PRecord fpats ->
      let (l, targs) = guess_record_pattern_type env fpats in
      unify_types pat.meta Pattern env tp
        (T.Type.apps (T.Type.tconst l) targs);
      let (env, fpats) = check_field_patterns tp env fpats in
      (env, T.PRecord(l, targs, fpats))
    | S.PAnnot(p, tp') ->
      let pos = pat.meta in
      let tp' = Type.check_kind env tp' T.Kind.ktype in
      let vc = coerce_val pos Pattern env tp tp' in
      let (env, p) = check_pattern env p tp' in
      (env, T.PCoerce(vc, p, tp))
  in (env, { meta = pat.meta; data = data })

and check_ctor_pattern env ~pos ~name ctor ltps ps tp =
  let T.ADTCtor(_, ets, tps) = ctor.Env.ctor_decl.data in
  if List.length tps <> List.length ps then
    raise (Errors.ctor_arity pos name (List.length tps) (List.length ps))
  else if List.length ltps > 0 && List.length ltps <> List.length ets then
    raise (Errors.ctor_type_arity pos name
      (List.length ltps) (List.length ets))
  else
  let scope = Env.type_scope env in
  let (env, ctypes) = gen_pattern_ltypes env ltps ets in
  let (sub, avs) =
    T.Type.opening_subst
      ~scope:  scope
      ~vtypes: ctor.Env.ctor_datatype_args
      ~ctypes: ctypes
      ()
  in
  let tps = List.map (T.Type.open_with sub) tps in
  let avs = List.map T.Type.var avs in
  let ets = List.map snd ctypes in
  unify_types pos Pattern env tp
    (T.Type.apps (T.Type.tconst ctor.Env.ctor_datatype) avs);
  let (env, ps) = check_patterns env ps tps in
  (env, T.PCtor(ctor.Env.ctor_datatype, avs, ctor.Env.ctor_index, ets, ps))

and check_field_pattern rec_tp env fpat =
  let S.FieldPat(fld_l, pat) = fpat.data in
  let fld = Env.lookup_field env fld_l in
  let T.FieldDecl(_, ets, tp) = fld.Env.fld_decl.data in
  let (sub, avs) =
    T.Type.opening_subst
      ~scope:  (Env.type_scope env)
      ~vtypes: fld.Env.fld_record_args
      ~ctypes: []
      ()
  in
  let avs = List.map T.Type.var avs in
  let tp = T.Type.open_with sub tp in
  unify_types pat.meta Field env
    (T.Type.apps (T.Type.tconst fld.Env.fld_record) avs)
    rec_tp;
  match ets with
  | [] ->
    let (env, pat) = check_pattern env pat tp in
    (env, (fld.Env.fld_index, pat))
  | _ ->
    let sch = T.Scheme.close_with ~abs_types:ets [] tp in
    let (env, p) =
      match pat.data with
      | S.PWildcard -> (env, T.PWildcard sch)
      | S.PVar x ->
        let (env, y) = Env.add_var env x sch in
        (env, T.PVar y)
      | S.PCtor _ | S.PList _ | S.PRecord _ | S.PAnnot _ ->
        raise (polymorphic_pattern pat.meta)
    in
    (env, (fld.Env.fld_index, { meta = pat.meta; data = p }))
      
and check_field_patterns rec_tp env fpats =
  Utils.ListExt.fold_map (check_field_pattern rec_tp) env fpats

and check_patterns env ps tps =
  match ps, tps with
  | [], [] -> (env, [])
  | pat :: ps, tp :: tps ->
    let (env, pat) = check_pattern env pat tp in
    let (env, ps) = check_patterns env ps tps in
    (env, pat :: ps)
  | _ -> failwith "bad arity"

(* ========================================================================= *)
(* Functions *)

type function_patterns =
  { fp_env      : Env.t
  ; fp_ltypes   : T.tconst list
  ; fp_type     : T.typ
  ; fp_body_tp  : T.typ
  ; fp_body_eff : T.effect
  ; fp_gen      : T.expr -> T.expr
  }

let check_function_patterns env ltps ps ann =
  let (env, ltps) = Type.check_type_fargs env ltps in
  match ps with
  | [] ->
    let scope   = Env.type_scope env in
    let body_tp =
      begin match ann with
      | None -> T.Type.fresh_tvar ~scope T.Kind.ktype
      | Some (S.Annot tp) -> Type.check_kind env tp T.Kind.ktype
      | Some (S.AnnotEff(_, eff)) ->
        raise (Errors.effect_annot_in_pure eff.meta)
      end in
    { fp_env      = env
    ; fp_ltypes   = ltps
    ; fp_type     = body_tp
    ; fp_body_tp  = body_tp
    ; fp_body_eff = T.Type.eff_pure
    ; fp_gen      = (fun e -> e)
    }
  | _ ->
  let rec loop env ps =
    let scope = Env.type_scope env in
    match ps with
    | [] -> assert false
    | [ pat ] ->
      let pat_tp   = T.Type.fresh_tvar ~scope T.Kind.ktype in
      let (env, p) = check_pattern env pat pat_tp in
      let (body_tp, body_eff) =
        begin match ann with
        | None ->
          ( T.Type.fresh_tvar ~scope T.Kind.ktype
          , T.Type.fresh_tvar ~scope T.Kind.keffect)
        | Some (S.Annot tp) ->
          ( Type.check_kind env tp T.Kind.ktype
          , T.Type.fresh_tvar ~scope T.Kind.keffect)
        | Some (S.AnnotEff(tp, eff)) ->
          ( Type.check_kind env tp T.Kind.ktype
          , Type.check_kind env eff T.Kind.keffect)
        end in
      let fp_type  = T.Type.arrow pat_tp body_tp body_eff in
      { fp_env      = env
      ; fp_ltypes   = ltps
      ; fp_type     = fp_type
      ; fp_body_tp  = body_tp
      ; fp_body_eff = body_eff
      ; fp_gen      = (fun e ->
        { meta =
          { T.em_pos    = pat.meta
          ; T.em_type   = fp_type
          ; T.em_effect = T.Type.eff_pure
          }
        ; data = T.EFun(p, e)
        })
      }
    | pat :: ps ->
      let pat_tp = T.Type.fresh_tvar ~scope T.Kind.ktype in
      let (env, p) = check_pattern env pat pat_tp in
      let fp = loop env ps in
      let fp_type  = T.Type.arrow pat_tp fp.fp_type T.Type.eff_pure in
      { fp_env      = fp.fp_env
      ; fp_ltypes   = fp.fp_ltypes
      ; fp_type     = fp_type
      ; fp_body_tp  = fp.fp_body_tp
      ; fp_body_eff = fp.fp_body_eff
      ; fp_gen      = (fun e ->
        { meta =
          { T.em_pos    = pat.meta
          ; T.em_type   = fp_type
          ; T.em_effect = T.Type.eff_pure
          }
        ; data = T.EFun(p, fp.fp_gen e)
        })
      }
  in loop env ps

(* ========================================================================= *)
(* Recursion *)

let create_monomorphic_schemes env rfs =
  let scope = Env.type_scope env in
  Utils.ListExt.fold_map (fun env rf ->
    let S.RecFunc(x, _, _, _, _) = rf.data in
    let tv = T.TVar.fresh ~scope T.Kind.ktype in
    let tp = T.Type.var tv in
    let (env, x) = Env.add_var env x (T.Scheme.close_with [] tp) in
    (env, (x, tv, rf))
  ) env rfs

let prepare_rec_functions env rfs =
  List.map (fun (x, tv, rf) ->
    let S.RecFunc(_, ltps, ps, ann, body) = rf.data in
    let fp = check_function_patterns env ltps ps ann in
    let scheme = T.Scheme.close_with ~abs_types:fp.fp_ltypes [] fp.fp_type in
    T.Var.update_scheme x scheme;
    (x, fp, body)
  ) rfs

let update_rec_functions_schemes env_tvars rfs =
  List.map (fun (x, ltps, tp, body) ->
    let tvars =
      T.TVar.Set.diff (T.Type.tvars tp) env_tvars |> T.TVar.Set.elements in
    let scheme = Settings.close_scheme_function ~abs_types:ltps tvars tp in
    T.Var.update_scheme x scheme;
    (x, body)
  ) rfs

(* ========================================================================= *)
(* Handlers *)

let rec guess_handled_effect env pos clauses =
  match clauses with
  | [] -> raise (Errors.empty_handler pos)
  | h :: clauses ->
    begin match h.data with
    | S.HReturn _ -> guess_handled_effect env pos clauses
    | S.HOp(op, _, _, _, _) ->
      let op = Env.lookup_op env op in
      let scope = Env.type_scope env in
      (op.Env.op_effect,
        (List.map (fun c -> T.Type.fresh_tvar ~scope (T.TConst.kind c))
          op.Env.op_effect_args))
    end

let has_return_handler clauses =
  List.exists (fun h ->
    match h.data with
    | S.HReturn _ -> true
    | S.HOp _ -> false
  ) clauses

(* ========================================================================= *)
(* Records *)

let guess_record_type env fdefs =
  match (List.hd fdefs).data with
  | S.FieldDef(fld, _) | S.FieldDefM(_, fld, _) ->
    let fld = Env.lookup_field env fld in
    let scope = Env.type_scope env in
    (fld.Env.fld_record,
      (List.map (fun c -> T.Type.fresh_tvar ~scope (T.TConst.kind c))
        fld.Env.fld_record_args))

let build_select ~pos ~vc ~sel ~rec_e ~sel_tp ~fld_tp ~eff =
  T.ECoerce(vc, T.ECId eff,
  { meta =
    { T.em_pos    = pos
    ; T.em_type   = fld_tp
    ; T.em_effect = eff
    }
  ; data = T.EApp(
    { meta =
      { T.em_pos    = pos
      ; T.em_type   = sel_tp
      ; T.em_effect = eff
      }
    ; data = T.ECoerce(
      T.VCArrow(T.VCId, T.VCId, T.ECOpen eff),
      T.ECOpen eff,
      { meta =
        { T.em_pos    = pos
        ; T.em_type   = sel_tp
        ; T.em_effect = T.Type.eff_pure
        }
      ; data = T.EVar sel
      })
    }, rec_e)
  })

(* ========================================================================= *)
(* Main *)

let check_var env pos x tp eff =
  let xtp = snd (T.Scheme.open_s
    ~scope:(Env.type_scope env)
    (T.Var.scheme x)) in
  coerce_expr env
  { meta =
    { T.em_pos    = pos
    ; T.em_type   = xtp
    ; T.em_effect = T.Type.eff_pure
    }
  ; data = T.EVar x
  } tp eff

let rec check_type env e tp eff =
  let meta =
    { T.em_pos    = e.meta
    ; T.em_type   = tp
    ; T.em_effect = eff
    } in
  { meta = meta
  ; data =
    match e.data with
    | S.ENum n -> coerce_expr env
      { meta =
        { T.em_pos    = e.meta
        ; T.em_type   = B.Int.unif_tp
        ; T.em_effect = T.Type.eff_pure
        }
      ; data = T.ENum n
      } tp eff
    | S.EChar c -> coerce_expr env
      { meta =
        { T.em_pos    = e.meta
        ; T.em_type   = B.Char.unif_tp
        ; T.em_effect = T.Type.eff_pure
        }
      ; data = T.EChar c
      } tp eff
    | S.EString s -> coerce_expr env
      { meta =
        { T.em_pos    = e.meta
        ; T.em_type   = B.String.unif_tp
        ; T.em_effect = T.Type.eff_pure
        }
      ; data = T.EString s
      } tp eff
    | S.EList [] ->
      let pos = e.meta in
      let lt = Env.list_type ~pos env in
      check_var env pos lt.Env.lt_nil_proxy tp eff
    | S.EList es ->
      let pos = e.meta in
      let lt = Env.list_type ~pos:e.meta env in
      let scope = Env.type_scope env in
      let elt_tp  = T.Type.fresh_tvar ~scope T.Kind.ktype in
      let rest_tp = T.Type.arrow tp tp eff in
      let cons_tp = T.Type.arrow elt_tp rest_tp eff in
      let make data typ =
        { meta =
          { T.em_pos    = pos
          ; T.em_type   = typ
          ; T.em_effect = eff
          }
        ; data = data
        } in
      let cons = make
        (check_var env pos lt.Env.lt_cons_proxy cons_tp eff)
        cons_tp in
      let nil = check_var env pos lt.Env.lt_nil_proxy tp eff in
      let es  = List.map (fun e -> check_type env e elt_tp eff) es in
      List.fold_right (fun e es ->
          let app1 = make (T.EApp(cons, e)) rest_tp in
          T.EApp(app1, make es tp)
        ) es nil
    | S.EVar x ->
      let pos = e.meta in
      let x   = Env.lookup_var env x in
      check_var env pos x tp eff
    | S.EOp x ->
      let pos = e.meta in
      let x   = Env.lookup_op_proxy env x in
      check_var env pos x tp eff
    | S.ECtor x ->
      let pos = e.meta in
      let x   = Env.lookup_ctor_proxy env x in
      check_var env pos x tp eff
    | S.EModule(c, tps) ->
      let ctor = Env.lookup_ctor env c in
      let T.ADTCtor(_, ets, ctps) = ctor.Env.ctor_decl.data in
      (* Instantiate module parameters (added by hoisting)
         with fresh variables *)
      let (sub, hts) = T.Type.opening_subst
          ~scope:  (Env.type_scope env)
          ~vtypes: ctor.Env.ctor_datatype_args
          ~ctypes: []
          ()
      in
      let ctps = List.map (T.Type.open_with sub) ctps in
      (* Instantiate module-constructor parameters *)
      let (sub, avs) = T.Type.opening_subst
          ~scope:  (Env.type_scope env)
          ~vtypes: ets
          ~ctypes: []
          ()
      in
      let ctps = List.map (T.Type.open_with sub) ctps in
      let tps = List.map2 (Type.check_type_field ~mpos:e.meta env) tps avs in
      let margs = List.map T.Type.var hts in
      let mexp =
        { meta =
          { T.em_pos    = e.meta
          ; T.em_type   = T.Type.arrows ctps
              (T.Type.apps
                (T.Type.tconst ctor.Env.ctor_datatype)
                margs)
              T.Type.eff_pure
          ; T.em_effect = T.Type.eff_pure
          }
        ; data =
          T.ECtor(ctor.Env.ctor_datatype, margs, ctor.Env.ctor_index, tps)
        } in
      coerce_expr env mexp tp eff
    | S.EFun(ps, ann, body) ->
      assert (ps <> []);
      let pos = e.meta in
      let fp = check_function_patterns env [] ps ann in
      let vc = coerce_val pos Expression env fp.fp_type tp in
      let body = check_type fp.fp_env body fp.fp_body_tp fp.fp_body_eff in
      T.ECoerce(vc, T.ECOpen eff, fp.fp_gen body)
    | S.EApp(e1, e2) ->
      let scope = Env.type_scope env in
      let arg_tp = T.Type.fresh_tvar ~scope T.Kind.ktype in
      let e1 = check_type env e1 (T.Type.arrow arg_tp tp eff) eff in
      let e2 = check_type env e2 arg_tp eff in
      T.EApp(e1, e2)
    | S.ELet(x, ltps, ps, ann, e1, e2) ->
      assert (ps <> [] || S.Expr.is_value e1);
      let fp = check_function_patterns env ltps ps ann in
      let e1 = check_type fp.fp_env e1 fp.fp_body_tp fp.fp_body_eff in
      let tvars =
          T.TVar.Set.diff (T.Type.tvars fp.fp_type) (Env.tvars env)
          |> T.TVar.Set.elements
      in
      let scheme = Settings.close_scheme_function
          ~abs_types:fp.fp_ltypes tvars fp.fp_type in
      let (env, z) = Env.add_var env x scheme in
      let e2 = check_type env e2 tp eff in
      T.ELet(z, fp.fp_gen e1, e2)
    | S.ELetPat(pat, ann, e1, e2) ->
      let scope = Env.type_scope env in
      let pat_tp = T.Type.fresh_tvar ~scope T.Kind.ktype in
      let (env', pat) = check_pattern env pat pat_tp in
      let raw_e1 =
        begin match ann with
        | None     -> e1
        | Some ann -> { meta = e1.meta; data = S.EAnnot(e1, ann) }
        end in
      let e1 = check_type env raw_e1 pat_tp eff in
      let e2 = check_type env' e2 tp eff in
      T.EMatch(e1, [ (pat, e2) ])
    | S.ELetRec(rfs, rest) ->
      let (env1, rfs) = create_monomorphic_schemes env rfs in
      let rfs = prepare_rec_functions env1 rfs in
      let rfs = check_rec_functions rfs in
      let tvars = Env.tvars env in
      let rfs = update_rec_functions_schemes tvars rfs in
      T.EFix(rfs, check_type env1 rest tp eff)
    | S.EUIf(e1, e2) ->
      let bt = Env.bool_type ~pos:e.meta env in
      let ut = Env.unit_type ~pos:e.meta env in
      let e1 = check_type env e1 bt.Env.bt_type eff in
      let e2 = check_type env e2 ut.Env.ut_type eff in
      let vc = coerce_val e.meta Expression env ut.Env.ut_type tp in
      T.EMatch(e1,
        [ bt.Env.bt_true_pat,
          { meta; data = T.ECoerce(vc, T.ECId e2.meta.T.em_effect, e2) }
        ; bt.Env.bt_false_pat,
          { meta; data = T.ECoerce(vc, T.ECOpen eff, ut.Env.ut_value) }
        ])
    | S.EIf(e1, e2, e3) ->
      let bt = Env.bool_type ~pos:e.meta env in
      let e1 = check_type env e1 bt.Env.bt_type eff in
      let e2 = check_type env e2 tp eff in
      let e3 = check_type env e3 tp eff in
      T.EMatch(e1,
        [ bt.Env.bt_true_pat,  e2
        ; bt.Env.bt_false_pat, e3
        ])
    | S.EHandle(body, clauses) ->
      let scope = Env.type_scope env in
      let (lc, args) = guess_handled_effect env e.meta clauses in
      let l = T.Type.apps (T.Type.tconst lc) args in
      let body_tp =
        if has_return_handler clauses then
          T.Type.fresh_tvar ~scope T.Kind.ktype
        else tp
      in
      let body = check_type env body body_tp (T.Type.eff_cons l eff) in
      let clauses = List.map (fun h ->
          check_handler env l h body_tp tp eff
        ) clauses in
      T.EHandle(lc, args, body, clauses)
    | S.EHandleWith(e1, e2) ->
      let scope = Env.type_scope env in
      let ut    = Env.unit_type ~pos:e.meta env in 
      let tp1   = T.Type.fresh_tvar ~scope T.Kind.ktype in
      let eff1  = T.Type.fresh_tvar ~scope T.Kind.keffect in
      let ftp   = T.Type.arrow ut.Env.ut_type tp1 eff1 in
      let htp   = T.Type.arrow ftp tp eff in
      let e2 = check_type env e2 htp eff in
      let e1 = check_type env e1 tp1 eff1 in
      T.EApp(e2,
        { meta =
          { T.em_pos    = e.meta
          ; T.em_type   = ftp
          ; T.em_effect = eff
          }
        ; data = T.ECoerce(T.VCId, T.ECOpen eff,
          { meta =
            { T.em_pos    = e.meta
            ; T.em_type   = ftp
            ; T.em_effect = T.Type.eff_pure
            }
          ; data = T.EFun(
            { meta = e.meta
            ; data = T.PWildcard (T.Scheme.close_with [] ut.Env.ut_type)
            }, e1)
          })
        })
    | S.EHandler clauses ->
      let scope = Env.type_scope env in
      let ut    = Env.unit_type ~pos:e.meta env in 
      let (lc, args) = guess_handled_effect env e.meta clauses in
      let l    = T.Type.apps (T.Type.tconst lc) args in
      let tp2  = T.Type.fresh_tvar ~scope T.Kind.ktype in
      let eff2 = T.Type.fresh_tvar ~scope T.Kind.keffect in
      let tp1  =
        if has_return_handler clauses then
          T.Type.fresh_tvar ~scope T.Kind.ktype
        else tp2
      in
      let eff1 = T.Type.eff_cons l eff2 in
      let thunk_type   = T.Type.arrow ut.Env.ut_type tp1 eff1 in
      let handler_type = T.Type.arrow thunk_type tp2 eff2 in
      let vc = coerce_val e.meta Expression env handler_type tp in
      let clauses = List.map (fun h ->
          check_handler env l h tp1 tp2 eff2
        ) clauses in
      let thunk = T.Var.fresh (T.Scheme.close_with [] thunk_type) in
      let make tp eff data =
        { meta =
          { T.em_pos    = e.meta
          ; T.em_type   = tp
          ; T.em_effect = eff
          }
        ; data = data
        } in
      T.ECoerce(vc, T.ECOpen eff,
        make handler_type T.Type.eff_pure
        (T.EFun({ meta = e.meta; data = T.PVar thunk },
          make tp2 eff2
          (T.EHandle(lc, args,
            make tp1 eff1 (T.EApp(
              make thunk_type eff1
                (T.ECoerce(T.VCId, T.ECOpen eff1,
                  make thunk_type T.Type.eff_pure (T.EVar thunk))),
              make ut.Env.ut_type eff1
                (T.ECoerce(T.VCId, T.ECOpen eff1, ut.Env.ut_value)))),
            clauses)))))
    | S.EMatch(e, clauses) ->
      let scope = Env.type_scope env in
      let etp = T.Type.fresh_tvar ~scope T.Kind.ktype in
      let e = check_type env e etp eff in
      let clauses = List.map (fun { data = S.Clause(pat, body) } ->
          let (env, pat) = check_pattern env pat etp in
          let body = check_type env body tp eff in
          (pat, body)
        ) clauses in
      T.EMatch(e, clauses)
    | S.ESelect(rec_e, fld) ->
      let sel = Env.lookup_field_sel env fld in
      let (_, sel_tp) = T.Scheme.open_s
          ~scope: (Env.type_scope env)
          (T.Var.scheme sel) in
      begin match T.Type.view sel_tp with
      | T.TArrow(rec_tp, fld_tp, _) ->
        let vc = coerce_val e.meta Expression env fld_tp tp in
        let rec_e = check_type env rec_e rec_tp eff in
        build_select ~pos:e.meta ~vc ~sel ~rec_e ~sel_tp ~fld_tp ~eff
      | _ -> assert false
      end
    | S.ESelectF(rec_e, fld_l, tps) ->
      let fld = Env.lookup_field env fld_l in
      let T.FieldDecl(_, ets, ftp) = fld.Env.fld_decl.data in
      (* Instantiate record parameters (added by hoisting)
         with fresh variables *)
      let (sub, hts) = T.Type.opening_subst
          ~scope:  (Env.type_scope env)
          ~vtypes: fld.Env.fld_record_args
          ~ctypes: []
          ()
      in
      let ftp = T.Type.open_with sub ftp in
      (* Instantiate type-parameters of functor *)
      let (sub, avs) = T.Type.opening_subst
          ~scope:  (Env.type_scope env)
          ~vtypes: ets
          ~ctypes: []
          ()
      in
      let ftp = T.Type.open_with sub ftp in
      let _ = List.map2 (Type.check_type_field ~mpos:e.meta env) tps avs in
      (* Reuse typechecker for regular fields *)
      let exp = { meta = e.meta; data = S.ESelect(rec_e, fld_l) } in
      let exp = check_type env exp ftp eff in
      coerce_expr env exp tp eff
    | S.ERecord fdefs ->
      let (rec_l, rec_args) = guess_record_type env fdefs in
      unify_types e.meta Expression env
        (T.Type.apps (T.Type.tconst rec_l) rec_args)
        tp;
      let fdefs = List.map (fun fdef ->
          check_field_def env fdef tp eff
        ) fdefs in
      T.ERecord(rec_l, rec_args, fdefs)
    | S.ETypedef(tds, e) ->
      let (env, tds) = TypeDef.prepare_typedefs env tds in
      let (env, tds) = TypeDef.check_typedefs env tds in
      T.ETypedef(SyntaxExt.get_typedefs tds,
        TypeDef.create_ctor_proxies ~meta env tds (fun env ->
        check_type env e tp eff))
    | S.ETypeAlias(x, args, body, e) ->
      let (env', args) = Type.check_type_fargs env args in
      let rkind = T.Kind.fresh_kvar () in
      let body  = Type.check_kind env' body rkind in
      let tpv = T.Type.tfuns args body in
      (check_type (Env.add_type_alias env x tpv) e tp eff).data
    | S.EAbsType(x, kind, e) ->
      let kind = Type.tr_kind kind in
      let (env, x) = Env.add_tconst env ~fresh:true ~kind x in
      let e = check_type env e tp eff in
      T.EAbsType(x, e)
    | S.EAnnot(ae, S.AnnotEff(tp', eff')) ->
      let pos  = e.meta in
      let tp'  = Type.check_kind env tp' T.Kind.ktype in
      let eff' = Type.check_kind env eff' T.Kind.keffect in
      let vc   = coerce_val pos Expression env tp' tp in
      let ec   = coerce_eff pos env eff' eff in
      let ae   = check_type env ae tp' eff' in
      T.ECoerce(vc, ec, ae)
    | S.EAnnot(ae, S.Annot tp') ->
      let pos  = e.meta in
      let tp'  = Type.check_kind env tp' T.Kind.ktype in
      let vc   = coerce_val pos Expression env tp' tp in
      let ae   = check_type env ae tp' eff in
      T.ECoerce(vc, T.ECId ae.meta.T.em_effect, ae)
    | S.EExtern(name, targs, etp) ->
      let (env1, targs) = Type.check_type_fargs env targs in
      let etp = Type.check_kind env1 etp T.Kind.ktype in
      let scheme = T.Scheme.close_with ~abs_types: targs [] etp in
      coerce_expr env
      { meta =
        { T.em_pos    = e.meta
        ; T.em_type   =
            snd (T.Scheme.open_s ~scope:(Env.type_scope env) scheme)
        ; T.em_effect = T.Type.eff_pure
        }
      ; data = T.EExtern(name, scheme)
      } tp eff
    | S.EPragmaFlag(flag, rest) ->
      (Pragma.pragma_flag ~pos:e.meta env flag (fun env ->
        check_type env rest tp eff)).data
    | S.EPragmaVal(flag, x, rest) ->
      (Pragma.pragma_val ~pos:e.meta env flag x (fun env ->
        check_type env rest tp eff)).data
    | S.EPragmaType(flag, x, rest) ->
      (Pragma.pragma_type ~pos:e.meta env flag x (fun env ->
        check_type env rest tp eff)).data
    | S.EReplExpr(e, repl) ->
      let scope = Env.type_scope env in
      let tp'  = T.Type.fresh_tvar ~scope T.Kind.ktype in
      let seal = Utils.Seal.singleton T.Keys.repl_effect eff
              |> Utils.Seal.add T.Keys.repl_expr_type tp' in
      let e = check_type env e tp' eff in
      let e = Printer.build_printer env e (check_type env) in
      T.EReplExpr(e, seal, (fun () -> check_type env (repl ()) tp eff))
    | S.ERepl repl ->
      let seal = Utils.Seal.singleton T.Keys.repl_effect eff in
      T.ERepl (seal, (fun () -> check_type env (repl ()) tp eff))
  }

and check_rec_functions rfs =
  List.map (fun (x, fp, body) ->
    let body = check_type fp.fp_env body fp.fp_body_tp fp.fp_body_eff in
    (x, fp.fp_ltypes, fp.fp_type, fp.fp_gen body)
  ) rfs

and check_handler env l h body_tp ans_tp eff =
  { meta = h.meta
  ; data =
    match h.data with
    | S.HReturn(pat, body) ->
      let (env, pat) = check_pattern env pat body_tp in
      let body = check_type env body ans_tp eff in
      T.HReturn(pat, body)
    | S.HOp(op_l, ltps, pats, r, body) ->
      let op = Env.lookup_op env op_l in
      let T.OpDecl(_, ets, inputs, output) = op.Env.op_decl.data in
      if List.length inputs <> List.length pats then
        raise (Errors.op_arity h.meta (S.Op.name op_l)
          (List.length inputs) (List.length pats))
      else if List.length ltps > 0 && List.length ltps <> List.length ets then
        raise (Errors.op_type_arity h.meta (S.Op.name op_l)
          (List.length ltps) (List.length ets))
      else
      let scope = Env.type_scope env in
      let (env, ctypes) = gen_pattern_ltypes env ltps ets in
      let (sub, avs) =
        T.Type.opening_subst
          ~scope:  scope
          ~vtypes: op.Env.op_effect_args
          ~ctypes: ctypes
          ()
      in
      let inputs = List.map (T.Type.open_with sub) inputs in
      let output = T.Type.open_with sub output in
      let ets = List.map snd ctypes in
      let l' = T.Type.apps
          (T.Type.tconst op.Env.op_effect)
          (List.map T.Type.var avs)
      in
      unify_types h.meta Handler env l' l;
      let (env, pats) = check_patterns env pats inputs in
      let resume_tp = T.Type.arrow output ans_tp eff in
      let (env, r) = Env.add_var env r (T.Scheme.close_with [] resume_tp) in
      let body = check_type env body ans_tp eff in
      T.HOp(op.Env.op_index, ets, pats, r, body)
  }

and check_field_def env fdef rec_tp eff =
  let (fld_l, body, dpos) =
    match fdef.data with
    | S.FieldDef(fld_l, body) -> (fld_l, body, None)
    | S.FieldDefM(dpos, fld_l, body) -> (fld_l, body, Some dpos)
  in
  let fld = Env.lookup_field env fld_l in
  let T.FieldDecl(name, ets, tp) = fld.Env.fld_decl.data in
  match ets, dpos with
  | [], None ->
    let (sub, avs) =
      T.Type.opening_subst
        ~scope:  (Env.type_scope env)
        ~vtypes: fld.Env.fld_record_args
        ~ctypes: []
        ()
    in
    let avs = List.map T.Type.var avs in
    let tp = T.Type.open_with sub tp in
    unify_types fdef.meta Field env
      (T.Type.apps (T.Type.tconst fld.Env.fld_record) avs)
      rec_tp;
    let body = check_type env body tp eff in
    (fld.Env.fld_index, { meta = fdef.meta; data = T.FieldDefMono body })
  | _ when not (S.Expr.is_value body) ->
    raise (Errors.non_value_poly_field body.meta name)
  | _ ->
    let (env, ctypes) = gen_pattern_ltypes env [] ets in
    let (sub, avs) =
      T.Type.opening_subst
        ~scope:  (Env.type_scope env)
        ~vtypes: fld.Env.fld_record_args
        ~ctypes: ctypes
        ()
    in
    let ets = List.map snd ctypes in
    let avs = List.map T.Type.var avs in
    let tp = T.Type.open_with sub tp in
    unify_types fdef.meta Field env
      (T.Type.apps (T.Type.tconst fld.Env.fld_record) avs)
      rec_tp;
    let body =
      match dpos with
      | None -> check_type env body tp T.Type.eff_pure
      | Some dpos ->
        let body_tp = T.Type.fresh_tvar
          ~scope:(Env.type_scope env) T.Kind.ktype in
        let body = check_type env body body_tp T.Type.eff_pure in
        coerce_module_val env
          ~pos:  fdef.meta
          ~dpos: dpos
          ~name: name
          body tp
    in
    (fld.Env.fld_index,
      { meta = fdef.meta
      ; data = T.FieldDefPoly(ets, body)
      })

let tr_expr e =
  let env = Env.init () in
  let tp  = T.Type.fresh_tvar ~scope:(Env.type_scope env) T.Kind.ktype in
  let eff = B.IO.unif_tp in
  check_type env e tp eff

let _ =
  Flow.register_transform
    ~source: S.flow_node
    ~target: T.flow_node
    tr_expr
