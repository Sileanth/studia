open Lang.Node
module S = Lang.Raw
module T = Lang.Flat

type t = T.tconst * S.typedef

let prepare_typedef env td =
  match td.data with
  | S.TDEffect(name, _, _) | S.TDData(name, _, _) | S.TDRecord(name, _, _) ->
    let (env, x) = Env.add_tconst env ~local:false name.data in
    (env, (x, td))

let prepare_typedefs env tds =
  Utils.ListExt.fold_map prepare_typedef env tds

let tr_op_decl env op =
  { meta = op.meta
  ; data =
    let S.OpDecl(name, ets, inputs, output) = op.data in
    let op = T.Op.fresh ~name:name.data () in
    let (env, ets) = Type.tr_type_fargs env ets in
    let env = Env.allow_tvars env in
    let inputs = List.map (Type.tr_type env) inputs in
    let output = Type.tr_type env output in
    T.OpDecl(op, ets @ Env.get_tvars env, inputs, output)
  }

let tr_adt_ctor env ctor =
  { meta = ctor.meta
  ; data =
    let S.ADTCtor(name, ets, tps) = ctor.data in
    let ctor =
      match name with
      | S.CNName name -> T.Ctor.fresh ~name:name.data ()
      | S.CNOper op   -> T.Ctor.fresh ~name:(S.binary_op_var op) ()
    in
    let (env, ets) = Type.tr_type_fargs env ets in
    let env = Env.allow_tvars env in
    let tps = List.map (Type.tr_type env) tps in
    T.ADTCtor(ctor, ets @ Env.get_tvars env, tps)
  }

let tr_field_decl env fld =
  { meta = fld.meta
  ; data =
    let S.FieldDecl(name, ets, tp) = fld.data in
    let fld = T.Field.fresh ~name:name.data () in
    let (env, ets) = Type.tr_type_fargs env ets in
    let env = Env.allow_tvars env in
    let tp = Type.tr_type env tp in
    T.FieldDecl(fld, ets @ Env.get_tvars env, tp)
  }

let tr_typedef env (x, td) =
  { meta = td.meta
  ; data =
    match td.data with
    | S.TDEffect(_, args, ops) ->
      Uniqueness.check_op_uniqueness ops;
      let (env, args) = Type.tr_type_fargs env args in
      let ops = List.map (tr_op_decl env) ops in
      T.TDEffect(x, args, ops)
    | S.TDData(_, args, ctors) ->
      Uniqueness.check_ctor_uniqueness ctors;
      let (env, args) = Type.tr_type_fargs env args in
      let ctors = List.map (tr_adt_ctor env) ctors in
      T.TDData(x, args, ctors)
    | S.TDRecord(_, args, fields) ->
      Uniqueness.check_field_decl_uniqueness fields;
      let (env, args) = Type.tr_type_fargs env args in
      let fields = List.map (tr_field_decl env) fields in
      T.TDRecord(x, args, fields)
  }

let tr_typedefs env tds =
  List.map (tr_typedef env) tds

let register_op env op =
  let T.OpDecl(op, _, _, _) = op.data in
  Env.add_op' env op

let register_ctor env ctor =
  let T.ADTCtor(ctor, _, _) = ctor.data in
  Env.add_ctor' env ctor

let register_field env fld =
  let T.FieldDecl(fld, _, _) = fld.data in
  Env.add_field' env fld

let register_typedef_fields env td =
  let env1 = Env.new_namespace env in
  let (x, env1) =
    match td.data with
    | T.TDEffect(x, _, ops) ->
      (x, List.fold_left register_op env1 ops)
    | T.TDData(x, _, ctors) ->
      (x, List.fold_left register_ctor env1 ctors)
    | T.TDRecord(x, _, fields) ->
      (x, List.fold_left register_field env1 fields)
  in
  let ns  = Env.current_namespace env1 in
  let env = Env.set_typedef env x ns in
  let env = Env.include_namespace env ns in
  let ns  = Namespace.add_typedef ns "this" x ns in
  Env.add_namespace env (T.TConst.name x) ns

let register_typedefs_fields env tds =
  List.fold_left register_typedef_fields env tds
