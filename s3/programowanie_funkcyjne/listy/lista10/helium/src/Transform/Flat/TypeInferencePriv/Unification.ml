module U = Lang.Unif

exception Cannot_unify
exception Escapes_scope = U.TVar.Escapes_scope

let rec unify_kind k1 k2 =
  match U.Kind.view k1, U.Kind.view k2 with
  | U.KVar x,  U.KVar y when U.KVar.equal x y -> ()
  | U.KVar x, k | k, U.KVar x ->
    if U.Kind.contains_kvar' x k then
      raise Cannot_unify
    else U.KVar.set' x k
  | U.KType,   U.KType   -> ()
  | U.KType,   _         -> raise Cannot_unify
  | U.KEffect, U.KEffect -> ()
  | U.KEffect, _         -> raise Cannot_unify
  | U.KArrow(ka1, kv1), U.KArrow(ka2, kv2) ->
    unify_kind ka1 ka2;
    unify_kind kv1 kv2
  | U.KArrow _, _        -> raise Cannot_unify

let rec can_implicitly_swap env l1 l2 =
  match U.Type.nf_view l1, U.Type.nf_view l2 with
  | U.TNF_Neu(U.HConst c1, _), U.TNF_Neu(U.HConst c2, _) ->
    Env.can_swap env c1 c2
  | U.TNF_Neu(U.HVar _, _), _ | _, U.TNF_Neu(U.HVar _, _) -> false
  | (U.TNF_EffPure | U.TNF_EffCons _ | U.TNF_Arrow _ | U.TNF_Fun _), _ ->
    assert false
  | _, (U.TNF_EffPure | U.TNF_EffCons _ | U.TNF_Arrow _ | U.TNF_Fun _) ->
    assert false

let rec select_effect env l eff =
  match U.Type.row_view eff with
  | U.RVar _ ->
    let y = U.Type.fresh_tvar ~scope:(Env.type_scope env) U.Kind.keffect in
    unify env eff (U.Type.eff_cons l y);
    y
  | U.RNil -> raise Cannot_unify
  | U.RCons(l1, eff) ->
    if can_implicitly_swap env l l1 then
      U.Type.eff_cons l1 (select_effect env l eff)
    else begin
      unify env l l1;
      eff
    end

and unify_effects env eff1 eff2 =
  match U.Type.row_view eff1, U.Type.row_view eff2 with
  | U.RVar(perm1, x), U.RVar(perm2, y) when U.TVar.equal x y ->
    let perm = U.TCPerm.compose (U.TCPerm.rev perm1) perm2 in
    U.TVar.restrict x (U.TCPerm.carrier perm)
  | U.RVar(perm, x), eff | eff, U.RVar(perm, x) ->
    let eff = U.Type.perm (U.TCPerm.rev perm) (U.Type.of_row_view eff) in
    if U.Type.contains_tvar x eff then
      raise Cannot_unify
    else U.TVar.set x eff
  | U.RNil, U.RNil -> ()
  | U.RNil, _      -> raise Cannot_unify
  | U.RCons(l1, eff1), _ ->
    let eff2 = select_effect env l1 eff2 in
    unify_effects env eff1 eff2

and unify env tp1 tp2 =
  match U.Type.view tp1, U.Type.view tp2 with
  | (U.TEffPure | U.TEffCons _), _ | _, (U.TEffPure | U.TEffCons _) ->
    unify_effects env tp1 tp2
  | U.TVar(perm1, x), U.TVar(perm2, y) when U.TVar.equal x y ->
    let perm = U.TCPerm.compose (U.TCPerm.rev perm1) perm2 in
    U.TVar.restrict x (U.TCPerm.carrier perm)
  | U.TVar(perm, x), tp | tp, U.TVar(perm, x) ->
    let tp = U.Type.perm (U.TCPerm.rev perm) (U.Type.of_view tp) in
    if U.Type.contains_tvar x tp then
      raise Cannot_unify
    else U.TVar.set x tp
  | U.TConst c1, U.TConst c2 ->
    if U.TConst.equal c1 c2 then ()
    else raise Cannot_unify
  | U.TConst _, _ -> raise Cannot_unify
  | U.TArrow(atp1, vtp1, eff1), U.TArrow(atp2, vtp2, eff2) ->
    unify env atp2 atp1;
    unify env vtp1 vtp2;
    unify env eff1 eff2
  | U.TArrow _, _ -> raise Cannot_unify
  | U.TFun(x1, tp1), U.TFun(x2, tp2) ->
    let x = U.TConst.clone x1 in
    let tp1 = U.Type.perm (U.TCPerm.swap x1 x) tp1 in
    let tp2 = U.Type.perm (U.TCPerm.swap x2 x) tp2 in
    unify (Env.extend_tconst env x) tp1 tp2
  | U.TFun _, _ -> raise Cannot_unify
  | U.TApp(tf1, ta1), U.TApp(tf2, ta2) ->
    unify_kind (U.Type.kind ta1) (U.Type.kind ta2);
    unify env tf1 tf2;
    unify env ta1 ta2
  | U.TApp _, _ -> raise Cannot_unify

let rec coerce_select_effect env l eff =
  match U.Type.row_view eff with
  | U.RVar _ ->
    let y = U.Type.fresh_tvar ~scope:(Env.type_scope env) U.Kind.keffect in
    unify env eff (U.Type.eff_cons l y);
    (None, y)
  | U.RNil -> raise Cannot_unify
  | U.RCons(l1, eff) ->
    if can_implicitly_swap env l l1 then
      let (crc1, eff) = coerce_select_effect env l eff in
      let eff = U.Type.eff_cons l1 eff in
      begin match crc1 with
      | None   -> (None, eff)
      | Some c -> (Some (U.ECCons(l1, c)), eff)
      end
    else if LiftInference.can_explicitly_swap env l l1 then
      let (crc1, eff) = coerce_select_effect env l eff in
      let eff1 = U.Type.eff_cons l1 eff in
      begin match crc1 with
      | None   -> (Some (U.ECSwap(l, l1, eff)), eff1)
      | Some c -> (Some (U.ECComp(U.ECSwap(l, l1, eff), U.ECCons(l1, c))), eff1)
      end
    else begin
      unify env l l1;
      (None, eff)
    end

let rec coerce_eff env eff1 eff2 =
  match U.Type.row_view eff1, U.Type.row_view eff2 with
  | U.RNil, U.RNil -> U.ECId eff1
  | U.RNil, _ -> U.ECOpen eff2
  | U.RCons(l1, eff1), _ ->
    let (crc1, eff2) = coerce_select_effect env l1 eff2 in
    let crc2 = U.ECCons(l1, coerce_eff env eff1 eff2) in
    begin match crc1 with
    | None   -> crc2
    | Some c -> U.ECComp(crc2, c)
    end
  | U.RVar(perm1, x), U.RVar(perm2, y) when U.TVar.equal x y ->
    let perm = U.TCPerm.compose (U.TCPerm.rev perm1) perm2 in
    U.TVar.restrict x (U.TCPerm.carrier perm);
    U.ECId eff1
  | U.RVar(perm, x), U.RCons(l, eff2) when
      not (U.Type.visible_for (U.TCPerm.image_of perm (U.TVar.scope x)) l) ->
    begin match LiftInference.lift_effect_l l eff2 with
    | None   -> raise Cannot_unify
    | Some c -> U.ECComp(coerce_eff env eff1 eff2, c)
    end 
  | U.RVar(perm, x), eff ->
    let eff = U.Type.perm (U.TCPerm.rev perm) (U.Type.of_row_view eff) in
    if U.Type.contains_tvar x eff then
      raise Cannot_unify
    else begin
      U.TVar.set x eff;
      U.ECId eff1
    end

let rec coerce_val env tp1 tp2 =
  match U.Type.view tp1, U.Type.view tp2 with
  | U.TVar(perm1, x), U.TVar(perm2, y) when U.TVar.equal x y ->
    let perm = U.TCPerm.compose (U.TCPerm.rev perm1) perm2 in
    U.TVar.restrict x (U.TCPerm.carrier perm);
    U.VCId
  | (U.TVar(perm, x), (U.TArrow _ as tp))
  | ((U.TArrow _ as tp), U.TVar(perm, x)) ->
    if U.Type.contains_tvar' x tp then
      raise Cannot_unify
    else begin
      let scope = U.TCPerm.image_of (U.TCPerm.rev perm) (Env.type_scope env) in
      let atp = U.Type.fresh_tvar ~scope U.Kind.ktype in
      let vtp = U.Type.fresh_tvar ~scope U.Kind.ktype in
      let eff = U.Type.fresh_tvar ~scope U.Kind.keffect in
      U.TVar.set' x (U.TArrow(atp, vtp, eff));
      coerce_val env tp1 tp2
    end
  | U.TVar(perm, x), tp | tp, U.TVar(perm, x) ->
    let tp = U.Type.perm (U.TCPerm.rev perm) (U.Type.of_view tp) in
    if U.Type.contains_tvar x tp then
      raise Cannot_unify
    else begin
      U.TVar.set x tp;
      U.VCId
    end
  | U.TConst c1, U.TConst c2 ->
    if U.TConst.equal c1 c2 then U.VCId
    else raise Cannot_unify
  | U.TConst _, (U.TArrow _ | U.TApp _) -> raise Cannot_unify
  | U.TArrow(atp1, vtp1, eff1), U.TArrow(atp2, vtp2, eff2) ->
    let c1 = coerce_val env atp2 atp1 in
    let c2 = coerce_val env vtp1 vtp2 in
    let c3 = coerce_eff env eff1 eff2 in
    U.VCArrow(c1, c2, c3)
  | U.TArrow _, _ -> raise Cannot_unify
  | (U.TEffPure | U.TEffCons _ | U.TFun _), _ 
  | _, (U.TEffPure | U.TEffCons _ | U.TFun _) -> assert false
  | U.TApp(tf1, ta1), U.TApp(tf2, ta2) ->
    unify_kind (U.Type.kind ta1) (U.Type.kind ta2);
    unify env tf1 tf2;
    unify env ta1 ta2;
    U.VCId
  | U.TApp _, _ -> raise Cannot_unify
