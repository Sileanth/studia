open Lang.Node

module S = Lang.Flat
module T = Lang.Unif

let rec tr_kind k =
  match k with
  | S.KType   -> T.Kind.ktype
  | S.KEffect -> T.Kind.keffect
  | S.KArrow(k1, k2) -> T.Kind.arrow (tr_kind k1) (tr_kind k2)

let check_type_farg_kind env (arg, kind) =
  match arg.data with
  | S.TFA_Var x ->
    Env.add_tconst env ~fresh:false ~kind:kind x
  | S.TFA_Annot(x, k1) ->
    let k1 = tr_kind k1 in
    begin match Unification.unify_kind k1 kind with
    | () -> Env.add_tconst env ~fresh:false ~kind x
    | exception Unification.Cannot_unify ->
      raise (Errors.targ_kind_mismatch arg.meta k1 kind)
    end

let check_type_fargs_kinds env args =
  Utils.ListExt.fold_map check_type_farg_kind env args

let check_type_fargs env args =
  check_type_fargs_kinds env
    (List.map (fun arg -> (arg, T.Kind.fresh_kvar ())) args)

let coerce_kind pos k1 k2 =
  try Unification.unify_kind k1 k2 with
  | Unification.Cannot_unify ->
    raise (Errors.kind_mismatch pos k1 k2)

let tr_tconst env x =
  match Env.lookup_tconst env x with
  | Env.TConst c  -> T.Type.tconst c
  | Env.TAlias tp -> tp

let rec check_kind env tp kind =
  match tp.data with
  | S.TPlaceholder ->
    T.Type.fresh_tvar ~scope:(Env.type_scope env) kind
  | S.TEffPure ->
    coerce_kind tp.meta T.Kind.keffect kind;
    T.Type.eff_pure
  | S.TConst x ->
    let rtp = tr_tconst env x in
    coerce_kind tp.meta (T.Type.kind rtp) kind;
    rtp
  | S.TArrowPure(tp1, tp2) ->
    coerce_kind tp.meta T.Kind.ktype kind;
    T.Type.arrow
      (check_kind env tp1 T.Kind.ktype)
      (check_kind env tp2 T.Kind.ktype)
      T.Type.eff_pure
  | S.TArrowEff(tp1, tp2, eff) ->
    coerce_kind tp.meta T.Kind.ktype kind;
    T.Type.arrow
      (check_kind env tp1 T.Kind.ktype)
      (check_kind env tp2 T.Kind.ktype)
      (check_kind env eff T.Kind.keffect)
  | S.TEffRow eff ->
    coerce_kind tp.meta T.Kind.keffect kind;
    check_kind env eff T.Kind.keffect
  | S.TEffCons(l, eff) ->
    coerce_kind tp.meta T.Kind.keffect kind;
    T.Type.eff_cons
      (check_kind env l   T.Kind.keffect)
      (check_kind env eff T.Kind.keffect)
  | S.TApp(tp1, tp2) ->
    let k   = T.Kind.fresh_kvar () in
    let tp1 = check_kind env tp1 (T.Kind.arrow k kind) in
    let tp2 = check_kind env tp2 k in
    T.Type.app tp1 tp2

let check_type_field ~mpos env fld tv =
  let S.TField(name, tp) = fld.data in
  let kind = T.Kind.fresh_kvar () in
  let tp = check_kind env tp kind in
  try
    Unification.unify_kind kind (T.TVar.kind tv);
    Unification.unify env tp (T.Type.var tv);
    tp
  with
  | Unification.Cannot_unify ->
    raise (Errors.type_sig_mismatch mpos fld.meta name kind (T.TVar.kind tv))
