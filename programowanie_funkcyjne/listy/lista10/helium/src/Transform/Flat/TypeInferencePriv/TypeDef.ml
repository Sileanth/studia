open Lang.Node

module S = Lang.Flat
module T = Lang.Unif
module E = SyntaxExt

type td1 = T.tconst * (S.type_farg * T.kind) list * S.typedef

(* ========================================================================= *)
(* preparing *)

let prepare_typedef env td =
  match td.data with
  | S.TDEffect(l, args, _) ->
    let args  = List.map (fun arg -> (arg, T.Kind.fresh_kvar ())) args in
    let kinds = List.map snd args in
    let kind  = T.Kind.arrows kinds T.Kind.keffect in
    let (env, l) = Env.add_tconst env ~fresh:true ~kind:kind l in
    (env, (l, args, td))
  | S.TDData(l, args, _) ->
    let args  = List.map (fun arg -> (arg, T.Kind.fresh_kvar ())) args in
    let kinds = List.map snd args in
    let kind  = T.Kind.arrows kinds T.Kind.ktype in
    let (env, l) = Env.add_tconst env ~fresh:true ~kind:kind l in
    (env, (l, args, td))
  | S.TDRecord(l, args, _) ->
    let args  = List.map (fun arg -> (arg, T.Kind.fresh_kvar ())) args in
    let kinds = List.map snd args in
    let kind  = T.Kind.arrows kinds T.Kind.ktype in
    let (env, l) = Env.add_tconst env ~fresh:true ~kind:kind l in
    (env, (l, args, td))

let prepare_typedefs env tds =
  Utils.ListExt.fold_map prepare_typedef env tds

(* ========================================================================= *)
(* checking *)

let check_effect_op env op =
  let S.OpDecl(op_l, ets, inputs, output) = op.data in
  let name = S.Op.name op_l in
  let (env, ets) = Type.check_type_fargs env ets in
  let inputs = List.map (fun input ->
      Type.check_kind env input T.Kind.ktype) inputs in
  let output = Type.check_kind env output T.Kind.ktype in
  let op =
    { meta = op.meta
    ; data = T.OpDecl(name, ets, inputs, output)
    }
  in (op_l, op)

let define_effect_ops env l args ops =
  Utils.ListExt.fold_left_i (fun n env (op_l, op) ->
    Env.add_op env op_l
      { Env.op_effect      = l
      ; Env.op_effect_args = args
      ; Env.op_index       = n
      ; Env.op_decl        = op
      }) env ops

let check_adt_ctor env ctor =
  let S.ADTCtor(ctor_l, ets, tps) = ctor.data in
  let name = S.Ctor.name ctor_l in
  let (env, ets) = Type.check_type_fargs env ets in
  let tps = List.map (fun tp -> Type.check_kind env tp T.Kind.ktype) tps in
  let ctor =
    { meta = ctor.meta
    ; data = T.ADTCtor(name, ets, tps)
    }
  in (ctor_l, ctor)

let define_adt_ctors env l args ctors =
  Utils.ListExt.fold_left_i (fun n env (ctor_l, ctor) ->
    Env.add_ctor env ctor_l
      { Env.ctor_datatype      = l
      ; Env.ctor_datatype_args = args
      ; Env.ctor_index         = n
      ; Env.ctor_decl          = ctor
      }) env ctors

let check_field_decl env fld =
  let S.FieldDecl(fld_l, ets, tp) = fld.data in
  let name = S.Field.name fld_l in
  let (env, ets) = Type.check_type_fargs env ets in
  let tp = Type.check_kind env tp T.Kind.ktype in
  let fld =
    { meta = fld.meta
    ; data = T.FieldDecl(name, ets, tp)
    }
  in (fld_l, fld)

let define_fields env l args fields =
  Utils.ListExt.fold_left_i (fun n env (fld_l, fld) ->
    Env.add_field env fld_l
      { Env.fld_record      = l
      ; Env.fld_record_args = args
      ; Env.fld_index       = n
      ; Env.fld_decl        = fld
      }) env fields

let check_typedef env (l, args, td) =
  match td.data with
  | S.TDEffect(_, _, ops) ->
    let (def_env, args) = Type.check_type_fargs_kinds env args in
    let ops = List.map (check_effect_op def_env) ops in
    let env = define_effect_ops env l args ops in
    let td  = E.TDEffect(l, args, ops) in
    let env = Env.add_typedef env l td in
    (env, td)
  | S.TDData(_, _, ctors) ->
    let (def_env, args) = Type.check_type_fargs_kinds env args in
    let ctors = List.map (check_adt_ctor def_env) ctors in
    let env = define_adt_ctors env l args ctors in
    let td  = E.TDData(l, args, ctors) in
    let env = Env.add_typedef env l td in
    (env, td)
  | S.TDRecord(_, _, fields) ->
    let (def_env, args) = Type.check_type_fargs_kinds env args in
    let fields = List.map (check_field_decl def_env) fields in
    let env = define_fields env l args fields in
    let td  = E.TDRecord(l, args, fields) in
    let env = Env.add_typedef env l td in
    (env, td)

let check_typedefs env tds =
  Utils.ListExt.fold_map check_typedef env tds

(* ========================================================================= *)
(* creating proxies *)

let create_effect_op_proxy ~meta ~eff_const ~eff_args n env (l, op) cont =
  let T.OpDecl(_, ets, inputs, output) = op.data in
  let args = List.map T.Type.tconst eff_args in
  let eff  = T.Type.apps (T.Type.tconst eff_const) args in
  let typ = T.Type.arrows inputs output eff in
  let scheme = T.Scheme.close_with ~abs_types:(eff_args @ ets) [] typ in
  let (env, x) = Env.add_op_proxy env l scheme in
  { meta = meta
  ; data = T.ELet(x,
    { meta =
      { T.em_pos    = op.meta
      ; T.em_type   = typ
      ; T.em_effect = T.Type.eff_pure
      }
    ; data = T.EOp(eff_const, args, n, List.map T.Type.tconst ets)
    }, cont env)
  }

let create_effect_op_proxies ~meta ~eff_const ~eff_args env ops cont =
  Utils.ListExt.fold_left_i_cps
    (create_effect_op_proxy ~meta ~eff_const ~eff_args)
    env ops cont

let create_adt_ctor_proxy ~meta ~data_const ~data_args n env (l, ctor) cont =
  let T.ADTCtor(_, ets, tps) = ctor.data in
  let args = List.map T.Type.tconst data_args in
  let typ = T.Type.arrows tps
      (T.Type.apps (T.Type.tconst data_const) args)
      T.Type.eff_pure in
  let scheme = T.Scheme.close_with ~abs_types:(data_args @ ets) [] typ in
  let (env, x) = Env.add_ctor_proxy env l scheme in
  { meta = meta
  ; data = T.ELet(x,
    { meta =
      { T.em_pos    = ctor.meta
      ; T.em_type   = typ
      ; T.em_effect = T.Type.eff_pure
      }
    ; data = T.ECtor(data_const, args, n, List.map T.Type.tconst ets)
    }, cont env)
  }

let create_adt_ctor_proxies ~meta ~data_const ~data_args env ctors cont =
  Utils.ListExt.fold_left_i_cps
    (create_adt_ctor_proxy ~meta ~data_const ~data_args)
    env ctors cont

let create_field_selector
    ~meta ~rec_const ~rec_args n env (l, fld) cont =
  let T.FieldDecl(_, ets, tp) = fld.data in
  let args = List.map T.Type.tconst rec_args in
  let rec_type = T.Type.apps (T.Type.tconst rec_const) args in
  let typ = T.Type.arrow rec_type tp T.Type.eff_pure in
  let scheme = T.Scheme.close_with ~abs_types:(rec_args @ ets) [] typ in
  let (env, x) = Env.add_field_sel env l scheme in
  let r = T.Var.fresh (T.Scheme.close_with ~abs_types:ets [] tp) in
  let make t data =
    { meta =
      { T.em_pos    = fld.meta
      ; T.em_type   = t
      ; T.em_effect = T.Type.eff_pure
      }
    ; data = data
    } in
  { meta = meta
  ; data = T.ELet(x, make typ (T.EFun(
      { meta = fld.meta 
      ; data = T.PRecord(rec_const, args,
          [ (n, { meta = fld.meta; data = T.PVar r }) ])
      }, make tp (T.EVar r))),
    cont env)
  }

let create_field_selectors ~meta ~rec_const ~rec_args env flds cont =
  Utils.ListExt.fold_left_i_cps
    (create_field_selector ~meta ~rec_const ~rec_args)
    env flds cont

let create_ctor_proxy ~meta env td cont =
  match td with
  | E.TDEffect(eff_const, eff_args, ops) ->
    create_effect_op_proxies ~meta ~eff_const ~eff_args env ops cont
  | E.TDData(data_const, data_args, ctors) ->
    create_adt_ctor_proxies ~meta ~data_const ~data_args env ctors cont
  | E.TDRecord(rec_const, rec_args, flds) ->
    create_field_selectors ~meta ~rec_const ~rec_args env flds cont

let create_ctor_proxies ~meta env tds cont =
  Utils.ListExt.fold_left_cps (create_ctor_proxy ~meta) env tds cont
