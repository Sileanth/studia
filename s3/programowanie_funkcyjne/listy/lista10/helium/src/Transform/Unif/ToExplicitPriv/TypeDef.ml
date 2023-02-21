open Lang.Node

module S = Lang.Unif
module T = Lang.Explicit

type 'k tr_formal_params_result =
| Params : 'k1 T.kind * ('k1, 'k) T.typedef_formal_params ->
    'k tr_formal_params_result

let rec tr_formal_params : type k.
    Env.t -> S.tconst list -> k T.kind -> Env.t * k tr_formal_params_result =
    fun env args kind ->
  match args with
  | [] -> (env, Params(kind, T.TDFP_Nil))
  | arg :: args ->
    let (env, T.TVar.Pack x) = Env.add_tconst env arg in
    let (env, Params(kind, params)) = tr_formal_params env args kind in
    (env, Params(T.KArrow(T.TVar.kind x, kind), T.TDFP_Cons(x, params)))

let prepare_env env td =
  match td with
  | S.TDEffect(x, _, _) | S.TDData(x, _, _) ->
    fst (Env.add_tconst env x)
  | S.TDRecord(x, _, flds) ->
    let (env, _) = Env.add_tconst env x in
    Env.declare_record env x (List.map (fun fld ->
        let S.FieldDecl(name, _, _) = fld.data in
        name
      ) flds)

let prepare_env_s env tds =
  List.fold_left prepare_env env tds

let tr_op_decl env op =
  match op.data with
  | S.OpDecl(name, ets, inputs, output) ->
    let (env, ets) = Env.add_tconsts env ets in
    T.OpDecl(name, ets,
      List.map (Type.tr_ttype env) inputs,
      Type.tr_ttype env output)

let tr_adt_ctor env ctor =
  match ctor.data with
  | S.ADTCtor(name, ets, tps) ->
    let (env, ets) = Env.add_tconsts env ets in
    T.ADTCtor(name, ets, List.map (Type.tr_ttype env) tps)

let tr_field_decl env fld =
  match fld.data with
  | S.FieldDecl(_, ets, tp) ->
    let (env, ets) = Env.add_tconsts env ets in
    T.Type.foralls ets (Type.tr_ttype env tp)

let tr_typedef env td =
  let td =
    match td with
    | S.TDEffect(x, args, ops) ->
      let (env', Params(kind, params)) =
        tr_formal_params env args T.KEffect in
      let x = Type.tr_tconst env x kind in
      let ops = List.map (tr_op_decl env') ops in
      T.TypeDef(x, params, T.TDEffect ops)
    | S.TDData(x, args, ctors) ->
      let (env', Params(kind, params)) = tr_formal_params env args T.KType in
      let x = Type.tr_tconst env x kind in
      let ctors = List.map (tr_adt_ctor env') ctors in
      T.TypeDef(x, params, T.TDData ctors)
    | S.TDRecord(x, args, fields) ->
      let (env', Params(kind, params)) = tr_formal_params env args T.KType in
      let x = Type.tr_tconst env x kind in
      let ctor = T.ADTCtor(T.TVar.name x, [],
          List.map (tr_field_decl env') fields) in
      T.TypeDef(x, params, T.TDData [ ctor ])
  in (Env.add_typedef env td, td)

let rec tr_typedefs env tds =
  match tds with
  | [] -> (env, [])
  | td :: tds ->
    let (env, td)  = tr_typedef  env td in
    let (env, tds) = tr_typedefs env tds in
    (env, td :: tds)
