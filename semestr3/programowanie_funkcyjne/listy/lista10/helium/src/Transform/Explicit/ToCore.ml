open Lang.Node
module S = Lang.Explicit
module T = Lang.Core

let input_type (c : S.val_coercion) =
  Utils.Seal.find S.Keys.vcrc_in_type c.meta

let rec tr_eff_coercion trail c =
  match c.data with
  | S.ECId _ -> None
  | S.ECLift l       -> if trail then None else Some { meta = c.meta; data = T.CLift l }
  | S.ECSwap(l1, l2) -> Some { meta = c.meta; data = T.CSwap(l1, l2) }
  | S.ECConsL(l, c') ->
    begin match tr_eff_coercion trail c' with
    | None    -> None
    | Some c' -> Some { meta = c.meta; data = T.CCons(l, c') }
    end
  | S.ECConsR(c', l) -> tr_eff_coercion false c'
  | S.ECComp(c1, c2) ->
    begin match tr_eff_coercion trail c1, tr_eff_coercion trail c2 with
    | None,    r       -> r
    | r,       None    -> r
    | Some c1, Some c2 -> Some { meta = c.meta; data = T.CComp(c1, c2) }
    end

let coerce_eff meta effc e =
  match effc with
  | None   -> e
  | Some c -> { meta; data = T.ECoerce(c, e) }

let coerce_val gen e =
  match gen with
  | None   -> e
  | Some g -> g e

let rec tr_val_coercion c =
  match c.data with
  | S.VCId -> None
  | S.VCArrow(c1, c2, c3) ->
    begin match
      tr_val_coercion c1, tr_val_coercion c2, tr_eff_coercion true c3
    with
    | None, None, None -> None
    | gen1, gen2, effc -> Some (fun f ->
        let x = T.Var.fresh (input_type c1) in
        { meta = c.meta
        ; data = T.EFun(x, coerce_eff c.meta effc (coerce_val gen2
          { meta = c.meta
          ; data = T.EApp(f, coerce_val gen1
            { meta = c.meta
            ; data = T.EVar x
            })
          }))
        })
    end

let coerce_expr meta c e =
  match tr_val_coercion c with
  | None     -> e
  | Some gen ->
    let x = T.Var.fresh (input_type c) in
    { meta; data = T.ELet(x, e, gen { meta; data = T.EVar x }) }

let rec tr_expr e =
  let meta = e.meta in
  let make data = { meta; data } in
  match e.data with
  | S.ENum n -> make (T.ENum n)
  | S.EChar c -> make (T.EChar c)
  | S.EString s -> make (T.EString s)
  | S.EVar x -> make (T.EVar x)
  | S.EFun(x, body)         -> make (T.EFun(x, tr_expr body))
  | S.ETFun(x, body)        -> make (T.ETFun(x, tr_expr body))
  | S.EOp(neu, n, ts, es)   -> make (T.EOp(neu, n, ts, List.map tr_expr es))
  | S.ECtor(neu, n, ts, es) -> make (T.ECtor(neu, n, ts, List.map tr_expr es))
  | S.EApp(e1, e2)          -> make (T.EApp(tr_expr e1, tr_expr e2))
  | S.ETApp(e, tp)          -> make (T.ETApp(tr_expr e, tp))
  | S.ELet(x, e1, e2)       -> make (T.ELet(x, tr_expr e1, tr_expr e2))
  | S.ELetPure(x, e1, e2)   -> make (T.ELet(x, tr_expr e1, tr_expr e2))
  | S.EFix(rfs, e) ->
    let rfs = List.map (fun (x, e) -> (x, tr_expr e)) rfs in
    make (T.EFix(rfs, tr_expr e))
  | S.EHandle(neu, e, x, ret, hs) ->
    make (T.EHandle(neu, tr_expr e, x, tr_expr ret,
      List.map (fun (tvs, xs, r, e) -> (tvs, xs, r, tr_expr e)) hs))
  | S.EMatch(neu, e, cs, rtp) ->
    make (T.EMatch(neu, tr_expr e,
      List.map (fun (tvs, xs, e) -> (tvs, xs, tr_expr e)) cs,
      rtp))
  | S.ETypedef(tds, e) -> make (T.ETypedef(tds, tr_expr e))
  | S.EAbsType(x, e)   -> make (T.EAbsType(x, tr_expr e))
  | S.ECoerce(c1, c2, e) ->
    coerce_eff meta (tr_eff_coercion true c2)
      (coerce_expr meta c1 (tr_expr e))
  | S.EExtern(name, tp) -> make (T.EExtern(name, tp))
  | S.EReplExpr(e, seal, repl, tp, eff) ->
    make (T.EReplExpr(tr_expr e, seal, (fun () -> tr_expr (repl ())), tp, eff))
  | S.ERepl(seal, repl, tp, eff) ->
    make (T.ERepl(seal, (fun () -> tr_expr (repl ())), tp, eff))

let _ =
  Flow.register_transform
    ~require:
      [ S.Tags.val_coercion_types ]
    ~preserves:
      [ S.Tags.eff_coercion_types
      ; CommonTags.unique_vars
      ; CommonTags.unique_type_vars ]
    ~source: S.flow_node
    ~target: T.flow_node
    tr_expr
