open Lang.Node
module S = Lang.Raw
module T = Lang.Flat
module M = ModuleSig

type acc =
  { abs_types : T.type_farg list
  ; fields    : T.field_decl list
  }

let empty_acc =
  { abs_types = []
  ; fields    = []
  }

(* ========================================================================= *)

let decls_to_namespace decls =
  let rec loop acc decls =
    match decls with
    | [] -> acc
    | decl :: decls -> loop (tr_decl acc decl) decls
  and tr_decl (ns, targs) decl =
    let make data = { meta = decl.meta; data = data } in
    match decl.data with
    | M.DeclType(x, kind) ->
      let name = T.TConst.name x in
      let ns = Namespace.add_tconst ns name x in
      (ns, make (T.TFA_Annot(x, kind)) :: targs)
    | M.DeclVal(fld, x, _, _) ->
      let name = T.Field.name fld in
      let ns = Namespace.add_var ns name x in
      (ns, targs)
  in
  let (ns, targs) = loop (Namespace.empty, []) decls in
  (ns, List.rev targs)

let to_ns_repr msig =
  match msig with
  | M.TStruct str ->
    let (ns, targs) = decls_to_namespace str.M.ss_decls in
    (Namespace.MNamespace ns, targs)
  | M.TFunctor fctr ->
    (Namespace.MFunctor(T.Var.fresh (), fctr), [])

(* ========================================================================= *)

let rec tr_decl env acc decl =
  let make data = { meta = decl.meta; data = data } in
  let add_type_decl x kind =
    let (env, x) = Env.add_tconst env ~local:false x.data in
    let acc =
      { acc with
        abs_types = make (T.TFA_Annot(x, kind)) :: acc.abs_types
      } in
    (env, acc, [ make (M.DeclType(x, kind)) ])
  in
  let add_val_decl name ets tp =
    let (env1, ets) = Type.tr_type_fargs env ets in
    let env1 = Env.allow_tvars env1 in
    let tp   = Type.tr_type env1 tp in
    let (env, y) = Env.add_var env name in
    let fld = T.Field.fresh ~name () in
    let ets = ets @ Env.get_tvars env1 in
    let acc =
      { acc with
        fields = make (T.FieldDecl(fld, ets, tp)) :: acc.fields
      } in
    (env, acc, [ make (M.DeclVal(fld, y, ets, tp)) ])
  in
  match decl.data with
  | S.DeclMType x ->
    add_type_decl x T.KType
  | S.DeclMEffect x ->
    add_type_decl x T.KEffect
  | S.DeclType(x, kind) | S.DeclEffect(x, kind) ->
    add_type_decl x kind
  | S.DeclVal(x, ets, tp) ->
    add_val_decl x.data ets tp
  | S.DeclOper(op, ets, tp) ->
    add_val_decl (S.binary_op_var op) ets tp
  | S.DeclUOper(op, ets, tp) ->
    add_val_decl (S.unary_op_var op) ets tp
  | S.DeclOpen(mpath, m) ->
    let ns = Env.lookup_namespace env mpath m in
    (Env.open_namespace env ns, acc, [])
  | S.DeclOpenType(mpath, x) ->
    let ns  = Env.lookup_typedef env mpath x in
    (Env.open_namespace env ns, acc, [])

let rec tr_decls env acc decls =
  match decls with
  | [] -> (env, acc, [])
  | decl :: decls ->
    let (env, acc, decls1) = tr_decl env acc decl in
    let (env, acc, decls2) = tr_decls env acc decls in
    (env, acc, decls1 @ decls2)

(* ========================================================================= *)

let create_impl_kind acc =
  match acc.fields with
  | []     -> M.ImplEmpty (T.Ctor.fresh ~name:"_Nope" ())
  | _ :: _ -> M.ImplRecord

let create_impl_typedef ~mpos impl_type impl_kind abs_types fields =
  let make data = { meta = mpos; data = data } in
  match impl_kind with
  | M.ImplRecord ->
    make (T.TDRecord(impl_type, abs_types, fields))
  | M.ImplEmpty c ->
    make (T.TDData(impl_type, abs_types, [ make (T.ADTCtor(c, [], [])) ]))

let create_repr_typedef ~mpos repr_type repr_ctor abs_types impl_type =
  let make data = { meta = mpos; data = data } in
  let impl_type = make (T.TConst impl_type) in
  let impl_type = List.fold_left (fun tp arg ->
      make (T.TApp(tp, make (T.TConst (T.TypeFArg.var arg))))
    ) impl_type abs_types in
  make (T.TDData(repr_type, [],
    [ make (T.ADTCtor(repr_ctor, abs_types, [ impl_type ])) ]))

let create_functor_type ~mpos targs arg_type (tds, val_type) =
  let make data = { meta = mpos; data = data } in
  let repr_type  = T.TConst.fresh ~name:"_Functor" [] in
  let repr_field = T.Field.fresh ~name:"_apply" () in
  let in_tconst  = ModuleSig.impl_type arg_type in
  let in_type    =
    TypeHoisting.build_app ~pos:mpos (make (T.TConst in_tconst)) targs in
  let out_tconst = ModuleSig.repr_type val_type in
  let (hoist, tds) = TypeHoisting.hoist_typedefs targs tds in
  let fdecl = make (T.FieldDecl(repr_field, targs,
      make (T.TArrowPure(in_type,
        TypeHoisting.tr_type hoist (make (T.TConst out_tconst)))))) in
  let repr_td = make (T.TDRecord(repr_type, [], [ fdecl ])) in
  let fs =
    { M.fs_repr_type  = repr_type
    ; M.fs_repr_field = repr_field
    ; M.fs_arg_type   = arg_type
    ; M.fs_val_type   = val_type
    }
  in (repr_td :: tds, fs)

(* ========================================================================= *)

let tr_struct_sig env ~mpos decls =
  let env = Env.new_namespace env in
  let (_, acc, decls) = tr_decls env empty_acc decls in
  let abs_types = List.rev acc.abs_types in
  let fields    = List.rev acc.fields in
  let impl_type = T.TConst.fresh ~name:"_Impl" [] in
  let impl_kind = create_impl_kind acc in
  let repr_type = T.TConst.fresh ~name:"_Sig" [] in
  let repr_ctor = T.Ctor.fresh ~name:"_Sig" () in
  let impl_td =
    create_impl_typedef ~mpos impl_type impl_kind abs_types fields in
  let repr_td =
    create_repr_typedef ~mpos repr_type repr_ctor abs_types impl_type in
  let str =
    { M.ss_repr_type = repr_type
    ; M.ss_repr_ctor = repr_ctor
    ; M.ss_impl_type = impl_type
    ; M.ss_impl_kind = impl_kind
    ; M.ss_decls     = decls
    } in
  ([impl_td; repr_td], str)

let rec tr_module_type env msig =
  match msig.data with
  | S.MTStruct decls ->
    let (tds, ssig) = tr_struct_sig env ~mpos:msig.meta decls in
    (tds, M.TStruct ssig)
  | S.MTFunctor(args, body) ->
    tr_functor ~mpos:msig.meta env args (fun env -> tr_module_type env body)

and tr_functor ~mpos env args gen_body =
  match args with
  | []          -> gen_body env
  | arg :: args ->
    tr_functor1 ~mpos env arg (fun env -> tr_functor ~mpos env args gen_body)

and tr_functor1 ~mpos env arg gen_body =
  let S.MArg(x, msig) = arg.data in
  let env1 = Env.set_path env [ x.data ] in
  let (tds1, arg_type) = tr_module_type env1 msig in
  let (modl, targs) = to_ns_repr arg_type in
  let val_type = gen_body (Env.add_module (Env.set_path env []) x.data modl) in
  let (tds2, fs) = create_functor_type ~mpos targs arg_type val_type in
  (tds1 @ tds2, M.TFunctor fs)
