open Lang.Node
module C = Context
module T = Lang.Flat
module M = ModuleSig

type 'typ mexp =
  { m_pos   : Utils.Position.t
  ; m_types : T.typedef list
  ; m_body  : T.expr
  ; m_type  : 'typ
  }

type struct_def =
| StrUnpacked of Utils.Position.t * Namespace.t
| StrPacked   of M.struct_s mexp

type module_expr =
| MStruct of Utils.Position.t * Namespace.t
| MPacked of M.module_type mexp

type functor_arg =
  { fa_var    : T.var
  ; fa_types  : T.typedef list
  ; fa_targs  : T.type_farg list
  ; fa_type   : M.module_type
  ; fa_unpack : C.def list
  }

(* ========================================================================= *)

let with_typedefs make tds rest =
  match tds with
  | []     -> rest
  | _ :: _ -> make (C.DefTypedef tds) :: rest

let struct_def_pos str =
  match str with
  | StrUnpacked(pos, _) -> pos
  | StrPacked str -> str.m_pos

let module_expr_pos mexp =
  match mexp with
  | MStruct(pos, _) -> pos
  | MPacked mexp -> mexp.m_pos

(* ========================================================================= *)

let rec decls_to_namespace ~mpath decls =
  let rec loop acc decls =
    match decls with
    | [] -> acc
    | decl :: decls -> loop (tr_decl acc decl) decls
  and tr_decl (ns, targs, fpats) decl =
    let make data = { meta = decl.meta; data = data } in
    match decl.data with
    | M.DeclType(x, kind) ->
      let name = T.TConst.name x in
      let y    = T.TConst.fresh ~name mpath in
      let ns   = Namespace.add_tconst ns name y in
      (ns, make (T.TFA_Annot(y, kind)) :: targs, fpats)
    | M.DeclVal(fld, _, _, _) ->
      let name = T.Field.name fld in
      let x    = T.Var.fresh ~name () in
      let ns   = Namespace.add_var ns name x in
      (ns, targs, make (T.FieldPat(fld, make (T.PVar x))) :: fpats)
  in
  let (ns, targs, fpats) = loop (Namespace.empty, [], []) decls in
  (ns, List.rev targs, List.rev fpats)

(* ========================================================================= *)

let struct_impl_pattern ~mpos str fpats =
  { meta = mpos
  ; data =
    match str.M.ss_impl_kind with
    | M.ImplRecord  -> T.PRecord fpats
    | M.ImplEmpty _ -> T.PWildcard
  }

let struct_sig_pattern ~mpos str targs impl_pat =
  { meta = mpos
  ; data = T.PCtor(str.M.ss_repr_ctor, targs, [ impl_pat ])
  }

(* ========================================================================= *)

let of_namespace ~mpos ns = StrUnpacked(mpos, ns)

let to_namespace ~mpath str =
  match str with
  | StrUnpacked(_, ns) -> ([], ns)
  | StrPacked mexp ->
    let mpos = mexp.m_pos in
    let make data = { meta = mpos; data = data } in
    let str = mexp.m_type in
    let (ns, targs, fpats) = decls_to_namespace ~mpath str.M.ss_decls in
    let pat1 = struct_impl_pattern ~mpos str fpats in
    let pat2 = struct_sig_pattern ~mpos str targs pat1 in
    ( with_typedefs make mexp.m_types
      [ make (C.DefLetPat(pat2, None, mexp.m_body)) ]
    , ns)

let of_ns_repr ~mpos modl =
  match modl with
  | Namespace.MNamespace ns -> MStruct(mpos, ns)
  | Namespace.MFunctor(x, fs) ->
    MPacked
    { m_pos   = mpos
    ; m_types = []
    ; m_body  = { meta = mpos; data = T.EVar x }
    ; m_type  = M.TFunctor fs
    }

let to_ns_repr ~mpath mexp =
  match mexp with
  | MStruct(_, ns) -> ([], Namespace.MNamespace ns)
  | MPacked mexp   ->
    let make data = { meta = mexp.m_pos; data = data } in
    begin match mexp.m_type with
    | M.TStruct str ->
      let str = StrPacked { mexp with m_type = str } in
      let (defs, ns) = to_namespace ~mpath str in
      (defs, Namespace.MNamespace ns)
    | M.TFunctor fctr ->
      let x = T.Var.fresh ~name:"_Functor" () in
      ( with_typedefs make mexp.m_types
        [ make (C.DefLetPat(make (T.PVar x), None, mexp.m_body)) ]
      , Namespace.MFunctor(x, fctr))
    end

let as_namespace ~mpath mexp =
  match to_ns_repr ~mpath mexp with
  | (defs, Namespace.MNamespace ns) -> (defs, ns)
  | (_, Namespace.MFunctor _) ->
    raise (Errors.module_kind_mismatch (module_expr_pos mexp)
      Errors.MKFunctor Errors.MKStruct)

(* ========================================================================= *)

let pack_struct_into_impl ~mpos ns ssig =
  let make data = { meta = mpos; data = data } in
  let rec loop acc ns decls =
    match decls with
    | [] -> acc
    | decl :: decls -> loop (tr_decl acc ns decl) ns decls
  and tr_decl acc ns decl =
    match decl.data with
    | M.DeclType _ -> acc
    | M.DeclVal(fld, _, _, _) ->
      let name = T.Field.name fld in
      let body =
        match Namespace.lookup_var ns name with
        | None -> raise (Errors.value_not_provided mpos decl.meta name)
        | Some (Env.VVar  x) -> make (T.EVar  x)
        | Some (Env.VOp  op) -> make (T.EOp  op)
        | Some (Env.VCtor c) -> make (T.ECtor c)
      in
      make (T.FieldDefM(decl.meta, fld, body)) :: acc
  in
  let flds = List.rev (loop [] ns ssig.M.ss_decls) in
  match ssig.M.ss_impl_kind with
  | M.ImplRecord  -> make (T.ERecord flds)
  | M.ImplEmpty c -> make (T.ECtor c)

let pack_struct_types ~mpos ns ssig =
  let make data = { meta = mpos; data = data } in
  let rec loop acc ns decls =
    match decls with
    | [] -> acc
    | decl :: decls -> loop (tr_decl acc ns decl) ns decls
  and tr_decl acc ns decl =
    match decl.data with
    | M.DeclVal _ -> acc
    | M.DeclType(x, _) ->
      let name = T.TConst.name x in
      begin match Namespace.lookup_tconst ns name with
      | None -> raise (Errors.type_not_provided mpos decl.meta name)
      | Some x ->
        { meta = decl.meta
        ; data = T.TField(name, make (T.TConst x))
        } :: acc
      end
  in
  List.rev (loop [] ns ssig.M.ss_decls)

let pack_struct_into_repr ~mpos ns ssig impl =
  let make data = { meta = mpos; data = data } in
  let targs = pack_struct_types ~mpos ns ssig in
  make (T.EApp(
    make (T.EModule(ssig.M.ss_repr_ctor, targs)),
    impl))

let match_signature_ns ~mpos defs ns (tds, ssig) =
  let body = pack_struct_into_impl ~mpos ns ssig in
  let body = pack_struct_into_repr ~mpos ns ssig body in
  StrPacked
  { m_pos   = mpos
  ; m_types = tds
  ; m_body  = Context.plug defs body
  ; m_type  = ssig
  }

let rec match_functor (defs, fctr) (tds, fsig) =
  let pos = fctr.m_pos in
  let (argm, arg) = mk_functor_arg ~pos "M" ([], fsig.M.fs_arg_type) in
  let argm = of_ns_repr ~mpos:pos argm in
  let body = apply_functor_aux ~pos fctr ([], argm) in
  let body = match_type ([], body) ([], fsig.M.fs_val_type) in
  mk_functor_aux arg (defs, body) (tds, fsig)

and match_functor_type (defs, mexp) fsig =
  let fctr = as_functor mexp in
  match_functor (defs, fctr) fsig

and match_signature (defs1, str) ssig =
  let (defs2, ns) = to_namespace ~mpath:[] str in
  match_signature_ns ~mpos:(struct_def_pos str) (defs2 @ defs1) ns ssig

(* ========================================================================= *)

and mk_struct str =
  match str with
  | StrUnpacked(mpos, ns) -> MStruct(mpos, ns)
  | StrPacked str -> MPacked { str with m_type = M.TStruct str.m_type }

and match_type (defs, mexp) (tds, msig) =
  match mexp, msig with
  | _, M.TFunctor fsig ->
    let fctr = match_functor_type (defs, mexp) (tds, fsig) in
    MPacked { fctr with m_type = M.TFunctor fctr.m_type }
  | MStruct(mpos, ns), M.TStruct ssig ->
    mk_struct (match_signature (defs, StrUnpacked(mpos, ns)) (tds, ssig))
  | MPacked mexp, M.TStruct ssig ->
    begin match mexp.m_type with
    | M.TStruct ssig0 ->
      let str = { mexp with m_type = ssig0 } in
      mk_struct (match_signature (defs, StrPacked str) (tds, ssig))
    | M.TFunctor _ ->
      raise (Errors.module_kind_mismatch mexp.m_pos
        Errors.MKFunctor Errors.MKStruct)
    end

(* ========================================================================= *)

and mk_functor_arg ~pos x (tds, msig) =
  let make data = { meta = pos; data = data } in
  let var = T.Var.fresh ~name:x () in
  match msig with
  | M.TStruct str ->
    let (ns, targs, fpats) = decls_to_namespace ~mpath:[x] str.M.ss_decls in
    let pat = struct_impl_pattern ~mpos:pos str fpats in
    let arg =
      { fa_var    = var
      ; fa_types  = tds
      ; fa_targs  = targs
      ; fa_type   = msig
      ; fa_unpack =
        [ make (C.DefLetPat(pat, None, make (T.EVar var))) ]
      }
    in (Namespace.MNamespace ns, arg)
  | M.TFunctor fctr ->
    let arg =
      { fa_var    = var
      ; fa_types  = tds
      ; fa_targs  = []
      ; fa_type   = msig
      ; fa_unpack = []
      }
    in (Namespace.MFunctor(var, fctr), arg)

and mk_functor_aux arg (defs, body) (tds, fctr) =
  match body with
  | MStruct(pos, _) ->
    raise (Errors.functor_without_val_type pos)
  | MPacked modl ->
    let make data = { meta = modl.m_pos; data = data } in
    let functor_var = T.Var.fresh ~name:"_functor" () in
    let functor_expr =
      make (T.ELet(functor_var,
        arg.fa_targs, [ make (T.PVar arg.fa_var) ], None,
        C.plug arg.fa_unpack (C.plug defs modl.m_body),
      make (T.ERecord(
      [ make (T.FieldDef(fctr.M.fs_repr_field, make (T.EVar functor_var)))
      ]))))
    in
    { m_pos   = modl.m_pos
    ; m_types = arg.fa_types @ tds
    ; m_body  = functor_expr
    ; m_type  = fctr
    }

and mk_functor arg (defs, body) =
  match body with
  | MStruct(pos, _) ->
    raise (Errors.functor_without_val_type pos)
  | MPacked modl ->
    let fctr =
      ModuleType.create_functor_type ~mpos:modl.m_pos
        arg.fa_targs arg.fa_type (modl.m_types, modl.m_type) in
    let fctr = mk_functor_aux arg (defs, body) fctr in
    MPacked { fctr with m_type = M.TFunctor fctr.m_type }

(* ========================================================================= *)

and as_functor mexp =
  match mexp with
  | MStruct(pos, ns) ->
    begin match Namespace.lookup_functor ns "this" with
    | None ->
      raise (Errors.module_kind_mismatch pos
        Errors.MKStruct Errors.MKFunctor)
    | Some(x, fctr) ->
      { m_pos   = pos
      ; m_types = []
      ; m_body  = { meta = pos; data = T.EVar x }
      ; m_type  = fctr
      }
    end
  | MPacked modl ->
    begin match modl.m_type with
    | M.TFunctor fctr -> { modl with m_type = fctr }
    | M.TStruct _ ->
      raise (Errors.module_kind_mismatch modl.m_pos
        Errors.MKStruct Errors.MKFunctor)
    end

and apply_functor_str ~pos fctr ssig (defs, mexp) =
  let make data = { meta = pos; data = data } in
  let apply_unpacked defs mpos ns =
    let arg   = pack_struct_into_impl ~mpos ns ssig in
    let targs = pack_struct_types ~mpos ns ssig in
    let app_body = Context.plug defs (make (T.EApp(
        make (T.ESelectF(fctr.m_body, fctr.m_type.M.fs_repr_field, targs)),
        arg))) in
    MPacked
    { m_pos   = pos
    ; m_types = fctr.m_types
    ; m_body  = app_body
    ; m_type  = fctr.m_type.M.fs_val_type
    } in
  match mexp with
  | MStruct(mpos, ns) -> apply_unpacked defs mpos ns 
  | MPacked mexp ->
    begin match mexp.m_type with
    | M.TStruct ssig ->
      let str = StrPacked { mexp with m_type = ssig } in
      let (defs2, ns) = to_namespace ~mpath:[] str in
      apply_unpacked (defs2 @ defs) mexp.m_pos ns
    | M.TFunctor _ ->
      raise (Errors.module_kind_mismatch mexp.m_pos
        Errors.MKFunctor Errors.MKStruct)
    end

and apply_functor_aux ~pos fctr marg =
  let make data = { meta = pos; data = data } in
  match fctr.m_type.M.fs_arg_type with
  | M.TStruct ssig ->
    apply_functor_str ~pos fctr ssig marg
  | M.TFunctor ftype ->
    let marg = match_functor_type marg ([], ftype) in
    let app_body = make (T.EApp(
        make (T.ESelectF(fctr.m_body, fctr.m_type.M.fs_repr_field, [])),
        (Context.plug (with_typedefs make marg.m_types []) marg.m_body))) in
    MPacked
    { m_pos   = pos
    ; m_types = fctr.m_types
    ; m_body  = app_body
    ; m_type  = fctr.m_type.M.fs_val_type
    }

and apply_functor ~pos mexp1 marg =
  let fctr = as_functor mexp1 in
  apply_functor_aux ~pos fctr marg

(* ========================================================================= *)

let update_pos mexp pos =
  match mexp with
  | MStruct(_, ns) -> MStruct(pos, ns)
  | MPacked modl   -> MPacked { modl with m_pos = pos }
