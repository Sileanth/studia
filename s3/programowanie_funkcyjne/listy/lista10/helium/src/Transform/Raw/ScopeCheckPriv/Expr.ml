open Lang.Node
module C = Context
module S = Lang.Raw
module T = Lang.Flat

let check_extern pos name =
  if Predef.DB.extern_exists name.data then ()
  else
    raise (Errors.unknown_extern pos name.data)

let is_var_pattern pat =
  match pat.data with
  | S.PVar _ | S.POper _ | S.PUOper _ -> true
  | _ -> false

let var_pattern_name pat =
  match pat.data with
  | S.PVar   x  -> x
  | S.POper  op -> S.binary_op_var op
  | S.PUOper op -> S.unary_op_var  op
  | _ -> assert false

let rec tr_pattern env pat =
  let make data = { meta = pat.meta; data } in
  match pat.data with
  | S.PParen pat ->
    let (env, pat) = tr_pattern env pat in
    (env, make pat.data)
  | S.PWildcard -> (env, make T.PWildcard)
  | S.PVar x    ->
    let (env, x) = Env.add_var env x in
    (env, make (T.PVar x))
  | S.PCtor(mpath, S.CNName c, ps) ->
    let c = Env.lookup_ctor env mpath c in
    let (env, ltps, ps) = tr_types_and_patterns env ps in
    (env, make (T.PCtor(c, ltps, ps)))
  | S.PCtor(mpath, S.CNOper op, ps) ->
    let c = Env.lookup_infix_ctor env op in
    let (env, ltps, ps) = tr_types_and_patterns env ps in
    (env, make (T.PCtor(c, ltps, ps)))
  | S.PBOp(p1, op, p2) ->
    let c = Env.lookup_infix_ctor env op in
    let (env, p1) = tr_pattern env p1 in
    let (env, p2) = tr_pattern env p2 in
    (env, make (T.PCtor(c, [], [ p1; p2 ])))
  | S.POper op ->
    let (env, x) = Env.add_var env (S.binary_op_var op) in
    (env, make (T.PVar x))
  | S.PUOper op ->
    let (env, x) = Env.add_var env (S.unary_op_var op) in
    (env, make (T.PVar x))
  | S.PList ps ->
    let (env, ps) = tr_patterns env ps in
    (env, make (T.PList ps))
  | S.PRecord fpats ->
    Uniqueness.check_field_pat_uniqueness fpats;
    let (env, fpats) = tr_field_patterns env fpats in
    (env, make (T.PRecord fpats))
  | S.PAnnot(pat, tp) ->
    let (env1, pat) = tr_pattern env pat in
    let tp = Type.tr_type env tp in
    (env1, make (T.PAnnot(pat, tp)))
  | S.PTypes _ -> raise (Errors.pattern_expected pat.meta)

and tr_field_pattern env fpat =
  let make data = { meta = fpat.meta; data } in
  let S.FieldPat(mpath, fld, pat) = fpat.data in
  let fld = Env.lookup_field env mpath fld in
  let (env, pat) = tr_pattern env pat in
  (env, make (T.FieldPat(fld, pat)))

and tr_field_patterns env fpats =
  Utils.ListExt.fold_map tr_field_pattern env fpats

and tr_types_and_patterns env ps =
  match ps with
  | { data = S.PTypes ltps } :: ps ->
    let (env, ltps) = Type.tr_type_fargs env ltps in
    let (env, ps) = tr_patterns env ps in
    (env, ltps, ps)
  | _ ->
    let (env, ps) = tr_patterns env ps in
    (env, [], ps)

and tr_patterns env ps =
  Utils.ListExt.fold_map tr_pattern env ps

let prepare_rec_functions env rfs =
  Utils.ListExt.fold_map (fun env rf ->
    let S.RecFunc(x, _, _, _) = rf.data in
    let (env, x) = Env.add_var env x.data in
    (env, (x, rf))
  ) env rfs

let tr_resume env resume =
  Env.add_var env (match resume with None -> "resume" | Some x -> x.data)

let tr_var env mpath name =
  match Env.lookup_var env mpath name with
  | Env.VVar  x -> T.EVar x
  | Env.VOp  op -> T.EOp op
  | Env.VCtor c -> T.ECtor c

let rec tr_expr env e =
  { meta = e.meta
  ; data =
    match e.data with
    | S.EVar(mpath, x) -> tr_var env mpath x
    | S.EParen e -> (tr_expr env e).data
    | S.ECtor(mpath, c) ->
      let c = Env.lookup_ctor env mpath c in
      T.ECtor c
    | S.EOper(mpath, op) ->
      tr_var env mpath { meta = op.meta; data = S.binary_op_var op }
    | S.EUOper(mpath, op) ->
      tr_var env mpath { meta = op.meta; data = S.unary_op_var op }
    | S.ENum n    -> T.ENum n
    | S.EChar c   -> T.EChar c
    | S.EString s -> T.EString s
    | S.EList es  -> T.EList (List.map (tr_expr env) es)
    | S.EFun(ps, ann, body) ->
      let (env, ps) = tr_patterns env ps in
      let ann  = Type.tr_annot_opt env ann in
      let body = tr_expr env body in
      T.EFun(ps, ann, body)
    | S.EApp(e1, e2) ->
      let e1 = tr_expr env e1 in
      let e2 = tr_expr env e2 in
      T.EApp(e1, e2)
    | S.EBOp(e1, op, e2) ->
      let op =
        { meta = op.meta
        ; data = tr_var env [] { meta = op.meta; data = S.binary_op_var op }
        } in
      let e1 = tr_expr env e1 in
      let e2 = tr_expr env e2 in
      T.EApp(
        { meta = Utils.Position.join e1.meta op.meta
        ; data = T.EApp(op, e1)
        }, e2)
    | S.EUOp(op, e1) ->
      let op =
        { meta = op.meta
        ; data = tr_var env [] { meta = op.meta; data = S.unary_op_var op }
        } in
      let e1 = tr_expr env e1 in
      T.EApp(op, e1)
    | S.EUIf(e1, e2) ->
      let e1 = tr_expr env e1 in
      let e2 = tr_expr env e2 in
      T.EUIf(e1, e2)
    | S.EIf(e1, e2, e3) ->
      let e1 = tr_expr env e1 in
      let e2 = tr_expr env e2 in
      let e3 = tr_expr env e3 in
      T.EIf(e1, e2, e3)
    | S.EHandle(e, hs) ->
      let e  = tr_expr env e in
      let hs = List.map (tr_handler env) hs in
      T.EHandle(e, hs)
    | S.EHandleWith(e1, e2) ->
      let e1 = tr_expr env e1 in
      let e2 = tr_expr env e2 in
      T.EHandleWith(e1, e2)
    | S.EHandler hs ->
      T.EHandler(List.map (tr_handler env) hs)
    | S.EMatch(e, cls) ->
      let e   = tr_expr env e in
      let cls = List.map (tr_clause env) cls in
      T.EMatch(e, cls)
    | S.ESelect(e, mpath, fld) ->
      let e   = tr_expr env e in
      let fld = Env.lookup_field env mpath fld in
      T.ESelect(e, fld)
    | S.ERecord fdefs ->
      Uniqueness.check_field_def_uniqueness fdefs;
      let fdefs = List.map (tr_field_def env) fdefs in
      T.ERecord fdefs
    | S.EDefs(defs, e) ->
      let (env, defs) = tr_defs env defs in
      let e = tr_expr env e in
      (C.plug defs e).data
    | S.EAnnot(e, ann) ->
      let e   = tr_expr env e in
      let ann = Type.tr_annot env ann in
      T.EAnnot(e, ann)
    | S.EExtern(name, targs, tp) ->
      check_extern e.meta name;
      let (env, targs) = Type.tr_type_fargs env targs in
      let tp = Type.tr_type env tp in
      T.EExtern(name.data, targs, tp)
  }

and tr_rec_function env (x, rf) =
  { meta = rf.meta
  ; data =
    let S.RecFunc(_, ps, ann, body) = rf.data in
    match tr_types_and_patterns env ps with
    | _, _, [] ->
      raise (Errors.missing_rec_func_arg rf.meta)
    | env, ltps, ps ->
      let ann  = Type.tr_annot_opt env ann in
      let body = tr_expr env body in
      T.RecFunc(x, ltps, ps, ann, body)
  }

and tr_handler env h =
  { meta = h.meta
  ; data =
    match h.data with
    | S.HReturn(pat, body) ->
      let (env, pat) = tr_pattern env pat in
      T.HReturn(pat, tr_expr env body)
    | S.HOp(mpath, op, ps, resume, body) ->
      let op = Env.lookup_op env mpath op in
      let (env, ltps, ps) = tr_types_and_patterns env ps in
      let (env, resume) = tr_resume env resume in
      T.HOp(op, ltps, ps, resume, tr_expr env body)
  }

and tr_clause env cl =
  { meta = cl.meta
  ; data =
    let S.Clause(pat, body) = cl.data in
    let (env, pat) = tr_pattern env pat in
    T.Clause(pat, tr_expr env body)
  }

and tr_field_def env fdef =
  { meta = fdef.meta
  ; data =
    let S.FieldDef(mpath, fld, body) = fdef.data in
    let fld = Env.lookup_field env mpath fld in
    T.FieldDef(fld, tr_expr env body)
  }

and tr_def env def =
  let make data = { meta = def.meta; data = data } in
  match def.data with
  | S.DefLet([], _, _) -> assert false
  | S.DefLet(p1 :: ps, ann, e) when is_var_pattern p1 ->
    let (env1, x) = Env.add_var env (var_pattern_name p1) in
    let (env, ltps, ps) = tr_types_and_patterns env ps in
    let ann = Type.tr_annot_opt env ann in
    let e = tr_expr env e in
    begin match ps with
    | _ :: _ -> (env1, [ make (C.DefLet(x, ltps, ps, ann, e)) ])
    | [] ->
      if T.Expr.is_value e then 
        (env1, [ make (C.DefLet(x, ltps, [], ann, e)) ])
      else if ltps = [] then
        let p1 = { meta = p1.meta; data = T.PVar x } in
        (env1, [ make (C.DefLetPat(p1, ann, e)) ])
      else
        raise (Errors.polymorphic_computation (List.hd ps).meta)
    end
  | S.DefLet([ p ], ann, e) ->
    let (env1, p) = tr_pattern env p in
    let ann = Type.tr_annot_opt env ann in
    let e = tr_expr env e in
    (env1, [ make (C.DefLetPat(p, ann, e)) ])
  | S.DefLet(({ data = S.PCtor(mpath, x, []) } as p1) :: ((_ :: _) as ps),
      ann, e) ->
    let last = Utils.ListExt.last p1 ps in
    let p =
      { meta = Utils.Position.join p1.meta last.meta
      ; data = S.PCtor(mpath, x, ps)
      } in
    tr_def env { meta = def.meta; data = S.DefLet([p], ann, e) }
  | S.DefLet(_ :: p :: _, _, _) ->
    raise (Errors.illegal_abstraction p.meta)
  | S.DefLetRec rfs ->
    Uniqueness.check_recfun_uniqueness rfs;
    let (env, rfs) = prepare_rec_functions env rfs in
    let rfs = List.map (tr_rec_function env) rfs in
    (env, [ make (C.DefLetRec rfs) ])
  | S.DefHandle hs ->
    let hs = List.map (tr_handler env) hs in
    (env, [ make (C.DefHandle hs) ])
  | S.DefHandleWith e ->
    let e = tr_expr env e in
    (env, [ make (C.DefHandleWith e) ])
  | S.DefTypedef td ->
    let (env1, td) = TypeDef.prepare_typedef env td in
    let td  = TypeDef.tr_typedef env td in
    let env = TypeDef.register_typedef_fields env1 td in
    (env, [ make (C.DefTypedef [ td ]) ])
  | S.DefTypedefRec tds ->
    Uniqueness.check_typedef_uniqueness tds;
    let (env, tds) = TypeDef.prepare_typedefs env tds in
    let tds = TypeDef.tr_typedefs env tds in
    let env = TypeDef.register_typedefs_fields env tds in
    (env, [ make (C.DefTypedef tds) ])
  | S.DefTypeAlias(x, args, tp) ->
    let (env', args) = Type.tr_type_fargs env args in
    let tp = Type.tr_type env' tp in
    let (env, x) = Env.add_tconst env ~local:false x.data in
    (env, [ make (C.DefTypeAlias(x, args, tp)) ])
  | S.DefAbsMType x ->
    let (env, x) = Env.add_tconst env ~local:false x.data in
    (env, [ make (C.DefAbsType(x, T.KType)) ])
  | S.DefAbsMEffect x ->
    let (env, x) = Env.add_tconst env ~local:false x.data in
    (env, [ make (C.DefAbsType(x, T.KEffect)) ])
  | S.DefAbsType(x, kind) | S.DefAbsEffect(x, kind) ->
    let (env, x) = Env.add_tconst env ~local:false x.data in
    (env, [ make (C.DefAbsType(x, kind)) ])
  | S.DefModule(m, args, msig_opt, mexp) ->
    let env1 = Env.extend_path env m.data in
    let (defs1, mexp) = tr_mod_functor env1 args (fun env1 ->
      let cast_module = tr_module_sig_opt env1 msig_opt in
      cast_module (tr_module_expr env1 mexp)) in
    let (defs2, modl) =
      ModuleSystem.to_ns_repr ~mpath:(Env.current_path env1) mexp in
    let env = Env.add_module env m.data modl in
    (env, defs1 @ defs2)
  | S.DefOpen mexp ->
    let (defs1, mexp) = tr_module_expr env mexp in
    let (defs2, ns)   = ModuleSystem.as_namespace ~mpath:[] mexp in
    let env = Env.open_namespace env ns in
    (env, defs1 @ defs2)
  | S.DefOpenType(mpath, x) ->
    let ns  = Env.lookup_typedef env mpath x in
    let env = Env.open_namespace env ns in
    (env, [])
  | S.DefInclude mexp ->
    let (defs1, mexp) = tr_module_expr env mexp in
    let (defs2, ns)   =
      ModuleSystem.as_namespace ~mpath:(Env.current_path env) mexp in
    let env = Env.include_namespace env ns in
    (env, defs1 @ defs2)
  | S.DefIncludeType(mpath, x) ->
    let ns  = Env.lookup_typedef env mpath x in
    let env = Env.include_namespace env ns in
    (env, [])
  | S.DefPragmaFlag flag ->
    (env, [ make (C.DefPragmaFlag flag) ])
  | S.DefPragmaVal(flag, mpath, x) ->
    begin match Env.lookup_var env mpath x with
    | Env.VVar x ->
      (env, [ make (C.DefPragmaVal(flag, x)) ])
    | _ -> failwith "'pragma val' constract supports only variables."
    end
  | S.DefPragmaType(flag, mpath, x) ->
    let x = Env.lookup_tconst env mpath x in
    (env, [ make (C.DefPragmaType(flag, x)) ])

and tr_defs env defs =
  match defs with
  | [] -> (env, [])
  | def :: defs ->
    let (env, defs1) = tr_def env def in
    let (env, defs2) = tr_defs env defs in
    (env, defs1 @ defs2)

and tr_struct env ~mpos defs =
  let env = Env.new_namespace env in
  let (env, defs) = tr_defs env defs in
  let str = ModuleSystem.of_namespace ~mpos (Env.current_namespace env) in
  (defs, str)

and tr_module_expr env mexp =
  let mpos = mexp.meta in
  match mexp.data with
  | S.ModConst(mpath, m) ->
    let modl = Env.lookup_module env mpath m in
    ([], ModuleSystem.of_ns_repr ~mpos modl)
  | S.ModStruct defs ->
    let (defs, str) = tr_struct env ~mpos defs in
    (defs, ModuleSystem.mk_struct str) 
  | S.ModAnnot(mexp, msig) ->
    let mexp = tr_module_expr env mexp in
    let msig = ModuleType.tr_module_type env msig in
    let mexp = ModuleSystem.match_type mexp msig in
    let mexp = ModuleSystem.update_pos mexp mpos in
    ([], mexp)
  | S.ModFunctor(args, msig_opt, body) ->
    let (defs, mexp) = tr_mod_functor env args (fun env ->
      let cast_module = tr_module_sig_opt env msig_opt in
      let body = tr_module_expr env body in
      cast_module body)
    in
    let mexp = ModuleSystem.update_pos mexp mpos in
    (defs, mexp)
  | S.ModApp(mexp1, mexp2) ->
    let (defs1, mexp1) = tr_module_expr env mexp1 in
    let mexp2 = tr_module_expr (Env.set_path env []) mexp2 in
    let mexp = ModuleSystem.apply_functor ~pos:mpos mexp1 mexp2 in
    (defs1, mexp)

and tr_mod_functor env args gen_body =
  match args with
  | [] -> gen_body env
  | arg :: args ->
    ([], tr_mod_functor1 env arg (fun env ->
      tr_mod_functor env args gen_body))

and tr_mod_functor1 env arg gen_body =
  let S.MArg(x, msig) = arg.data in
  let env1 = Env.set_path env [ x.data ] in
  let msig = ModuleType.tr_module_type env1 msig in
  let (modl, arg) = ModuleSystem.mk_functor_arg ~pos:arg.meta x.data msig in
  let env  = Env.add_module env x.data modl in
  let body = gen_body (Env.set_path env []) in
  ModuleSystem.mk_functor arg body

and tr_module_sig_opt env msig_opt =
  match msig_opt with
  | None -> fun p -> p
  | Some msig ->
    let msig = ModuleType.tr_module_type env msig in
    fun mexp -> ([], ModuleSystem.match_type mexp msig)
