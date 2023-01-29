open Lang.Node
module S = Lang.Raw
module T = Lang.Flat

let tr_type_farg env farg =
  let make data = { meta = farg.meta; data = data } in
  match farg.data with
  | S.TFA_Var x ->
    let (env, x) = Env.add_tconst env ~local:true x in
    (env, make (T.TFA_Var x))

let tr_type_fargs env fargs =
  Utils.ListExt.fold_map tr_type_farg env fargs

let rec tr_type env tp =
  { meta = tp.meta
  ; data =
    match tp.data with
    | S.TPlaceholder -> T.TPlaceholder
    | S.TEffPure     -> T.TEffPure
    | S.TParen tp    -> (tr_type env tp).data
    | S.TVar   x     ->
      let x = Env.lookup_tvar env { meta = tp.meta; data = x } in
      T.TConst x
    | S.TConst(mpath, x) ->
      let x = Env.lookup_tconst env mpath x in
      T.TConst x
    | S.TArrowPure(tp1, tp2) ->
      let tp1 = tr_type env tp1 in
      let tp2 = tr_type env tp2 in
      T.TArrowPure(tp1, tp2)
    | S.TArrowEff(tp1, tp2, eff) ->
      let tp1 = tr_type env tp1 in
      let tp2 = tr_type env tp2 in
      let eff = tr_type env eff in
      T.TArrowEff(tp1, tp2, eff)
    | S.TEffRow eff -> T.TEffRow(tr_type env eff)
    | S.TEffCons(l, eff) ->
      let l   = tr_type env l in
      let eff = tr_type env eff in
      T.TEffCons(l, eff)
    | S.TApp(tp1, tp2) ->
      let tp1 = tr_type env tp1 in
      let tp2 = tr_type env tp2 in
      T.TApp(tp1, tp2)
  }

let tr_annot env ann =
  match ann with
  | S.Annot tp -> T.Annot (tr_type env tp)
  | S.AnnotEff(tp, eff) ->
    let tp  = tr_type env tp in
    let eff = tr_type env eff in
    T.AnnotEff(tp, eff)

let tr_annot_opt env ann =
  match ann with
  | None     -> None
  | Some ann -> Some (tr_annot env ann)
