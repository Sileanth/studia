open Kind.Datatypes
open TVar.Datatypes
open Type.Datatypes

type 'eff select_result =
| Selected of 'eff
| Opened   of 'eff
| NotPossible

let tvar_equal_m (type k) tm1 tm2 (x1 : k tvar) (x2 : k tvar) =
  match TVar.TVarMap.find_opt x1 tm1, TVar.TVarMap.find_opt x2 tm2 with
  | None,    None    -> TVar.equal x1 x2
  | Some x1, Some x2 -> TVar.equal x1 x2
  | None, Some _ | Some _, None -> false

let rec can_swap_m env tm1 tm2 l1 l2 =
  match l1, l2 with
  | TNeu (h1, ps1), TNeu (h2, ps2) ->
    begin match h1, h2 with
    | HAny _, _ -> true
    | _, HAny _ -> true
    | HVar x1, HVar x2 ->
    let x1 =
      match TVar.TVarMap.find_opt x1 tm1 with
      | Some x1 -> x1
      | None    -> x1
    in
    let x2 =
      match TVar.TVarMap.find_opt x2 tm2 with
      | Some x2 -> x2
      | None    -> x2
    in
    Env.can_swap env x1 x2
    end

let rec select_at_effect_m env tm1 tm2 e1 effs =
  match effs with
  | [] -> Opened []
  | e2 :: effs ->
    if equiv_neutral_m env tm1 tm2 e1 e2
    then Selected effs
    else if can_swap_m env tm1 tm2 e1 e2
    then match select_at_effect_m env tm1 tm2 e1 effs with
         | Selected effs -> Selected (e2 :: effs)
         | Opened   effs -> Opened (e2 :: effs)
         | NotPossible   -> NotPossible
    else NotPossible

and equiv_m : type k. Env.t ->
    TVar.TVarMap.t -> TVar.TVarMap.t -> k typ -> k typ -> bool =
  fun env tm1 tm2 tp1 tp2 ->
  match Type.view tp1, Type.view tp2 with
  | TVEffect effs1, TVEffect effs2 ->
    equiv_effects_m env tm1 tm2 effs1 effs2
  | TVEffect _, _ -> false
  | TVNeutral nt1, TVNeutral nt2 ->
    equiv_neutral_m env tm1 tm2 nt1 nt2
  | TVNeutral _, _ -> false
  | TVArrow(ta1, tr1, er1), TVArrow(ta2, tr2, er2) ->
    equiv_m env tm1 tm2 ta1 ta2 &&
    equiv_m env tm1 tm2 tr1 tr2 &&
    equiv_m env tm1 tm2 er1 er2
  | TVArrow _, _ -> false
  | TVForall(x1, tp1), TVForall(x2, tp2) ->
    begin match Kind.equal (TVar.kind x1) (TVar.kind x2) with
    | Utils.EqDec.Equal ->
      let x = TVar.clone x1 in
      equiv_m
        (Env.add_tvar env x)
        (TVar.TVarMap.add x1 x tm1)
        (TVar.TVarMap.add x2 x tm2)
        tp1 tp2
    | Utils.EqDec.NotEqual -> false
    end
  | TVForall _, _ -> false
  | TVFun(x1, tp1), TVFun(x2, tp2) ->
    let x = TVar.clone x1 in
    equiv_m
      (Env.add_tvar env x)
      (TVar.TVarMap.add x1 x tm1)
      (TVar.TVarMap.add x2 x tm2)
      tp1 tp2
  | TVFun _, _ -> false

and equiv_neutral_m : type k. Env.t -> TVar.TVarMap.t -> TVar.TVarMap.t ->
    k neutral_type -> k neutral_type -> bool =
  fun env tm1 tm2 nt1 nt2 ->
  match nt1, nt2 with
  | TNeu (h1, ps1), TNeu (h2, ps2) ->
    match Kind.equal (Type.head_kind h1) (Type.head_kind h2) with
    | Utils.EqDec.NotEqual -> false
    | Utils.EqDec.Equal ->
      equiv_params_m env tm1 tm2 ps1 ps2 &&
      match h1, h2 with
      | HAny _, HAny _ -> true
      | HAny _, _      -> false
      | HVar x, HVar y -> tvar_equal_m tm1 tm2 x y
      | HVar _, _      -> false

and equiv_params_m : type k1 k2. Env.t -> TVar.TVarMap.t -> TVar.TVarMap.t ->
    (k1, k2) type_params -> (k1, k2) type_params -> bool =
  fun env tm1 tm2 ps1 ps2 ->
  match ps1, ps2 with
  | TP_Nil, TP_Nil -> true
  | TP_Nil, _      -> false
  | TP_Cons (tp1, ps1), TP_Cons (tp2, ps2) ->
    equiv_m env tm1 tm2 tp1 tp2 &&
    equiv_params_m env tm1 tm2 ps1 ps2
  | TP_Cons _, _   -> false

and equiv_effects_m env tm1 tm2 effs1 effs2 =
  match effs1 with
  | [] -> effs2 = []
  | e1 :: effs1 ->
    match select_at_effect_m env tm1 tm2 e1 effs2 with
    | Selected effs2 -> equiv_effects_m env tm1 tm2 effs1 effs2
    | Opened _ | NotPossible -> false

let rec match_type_m : type k. Env.t ->
    TVar.TVarMap.t -> TVar.TVarMap.t -> k typ -> k typ -> bool =
  fun env tm1 tm2 tp1 tp2 ->
  match Type.view tp1, Type.view tp2 with
  | TVEffect effs1, TVEffect effs2 ->
    match_effects_m env tm1 tm2 effs1 effs2
  | TVEffect _, _ -> false
  | TVNeutral nt1, TVNeutral nt2 ->
    equiv_neutral_m env tm1 tm2 nt1 nt2
  | TVNeutral _, _ -> false
  | TVArrow(ta1, tr1, er1), TVArrow(ta2, tr2, er2) ->
    match_type_m env tm2 tm1 ta2 ta1 && (* contravariant *)
    match_type_m env tm1 tm2 tr1 tr2 &&
    match_type_m env tm1 tm2 er1 er2
  | TVArrow _, _ -> false
  | TVForall(x1, tp1), TVForall(x2, tp2) ->
    begin match Kind.equal (TVar.kind x1) (TVar.kind x2) with
    | Utils.EqDec.Equal ->
      let x = TVar.clone x1 in
      match_type_m
        (Env.add_tvar env x)
        (TVar.TVarMap.add x1 x tm1)
        (TVar.TVarMap.add x2 x tm2)
        tp1 tp2
    | Utils.EqDec.NotEqual -> false
    end
  | TVForall _, _ -> false
  | TVFun(x1, tp1), TVFun(x2, tp2) ->
    let x = TVar.clone x1 in
    (* TODO: subtyping on functions? *)
    equiv_m
      (Env.add_tvar env x)
      (TVar.TVarMap.add x1 x tm1)
      (TVar.TVarMap.add x2 x tm2)
      tp1 tp2
  | TVFun _, _ -> false

and match_effects_m env tm1 tm2 effs1 effs2 =
  match effs1 with
  | [] -> true
  | e1 :: effs1 ->
    match select_at_effect_m env tm1 tm2 e1 effs2 with
    | Selected effs2 | Opened effs2 ->
      match_effects_m env tm1 tm2 effs1 effs2
    | NotPossible -> false

let select_at_effect env l ls =
  select_at_effect_m env TVar.TVarMap.empty TVar.TVarMap.empty l ls

let select_effect env e1 e2 =
  let rec aux opened ls1 ls2 =
    match ls1 with
    | []       ->
      if opened then
        Opened (Type.of_at_effects ls2)
      else
        Selected (Type.of_at_effects ls2)
    | l :: ls1 ->
      match select_at_effect env l ls2 with
      | Selected ls2 -> aux opened ls1 ls2
      | Opened   ls2 -> aux true   ls1 ls2
      | NotPossible  -> NotPossible
  in aux false (Type.to_at_effects e1) (Type.to_at_effects e2)

let equiv env tp1 tp2 =
  equiv_m env TVar.TVarMap.empty TVar.TVarMap.empty tp1 tp2

let match_type env tp1 tp2 =
  match_type_m env TVar.TVarMap.empty TVar.TVarMap.empty tp1 tp2

exception Cannot_merge

let rec lub_effects_m env tm1 tm2 effs1 effs2 : at_effect list =
  match effs1 with
  | [] -> effs2
  | e1 :: effs1 ->
    match select_at_effect_m env tm1 tm2 e1 effs2 with
    | Selected effs2 | Opened effs2 ->
      Type.rename_neutral_m tm1 e1 :: lub_effects_m env tm1 tm2 effs1 effs2
    | NotPossible -> raise Cannot_merge

let glb_effects_m env tm1 tm2 effs1 effs2 : at_effect list =
  let rec aux effs1 effs2 rem1 =
    match effs1 with
    | [] -> []
    | e1 :: effs1 ->
      match select_at_effect_m env tm1 tm2 e1 effs2 with
      | Selected effs2 ->
        if List.for_all (fun e -> can_swap_m env tm1 tm1 e e1) rem1
        then Type.rename_neutral_m tm1 e1 :: aux effs1 effs2 rem1
        else aux effs1 effs2 (e1 :: rem1)
      | Opened _ | NotPossible ->
        aux effs1 effs2 (e1 :: rem1)
  in aux effs1 effs2 []

let rec glb_type_m : type k. Env.t ->
    TVar.TVarMap.t -> TVar.TVarMap.t -> k typ -> k typ -> k typ =
  fun env tm1 tm2 tp1 tp2 ->
  match Type.view tp1, Type.view tp2 with
  | TVEffect effs1, TVEffect effs2 ->
    Type.of_at_effects (glb_effects_m env tm1 tm2 effs1 effs2)
  | TVEffect _, _ -> raise Cannot_merge
  | TVNeutral nt1, TVNeutral nt2 ->
    if equiv_neutral_m env tm1 tm2 nt1 nt2
    then Type.rename_m tm1 tp1
    else raise Cannot_merge
  | TVNeutral _, _ -> raise Cannot_merge
  | TVArrow(ta1, tr1, er1), TVArrow(ta2, tr2, er2) ->
    Type.arrow
      (lub_type_m env tm1 tm2 ta1 ta2)
      (glb_type_m env tm1 tm2 tr1 tr2)
      (glb_type_m env tm1 tm2 er1 er2)
  | TVArrow _, _ -> raise Cannot_merge
  | TVForall(x1, tp1), TVForall(x2, tp2) ->
    begin match Kind.equal (TVar.kind x1) (TVar.kind x2) with
    | Utils.EqDec.Equal ->
      let z = TVar.clone x1 in
      Type.forall z (glb_type_m
        (Env.add_tvar env z)
        (TVar.TVarMap.add x1 z tm1)
        (TVar.TVarMap.add x2 z tm2)
        tp1 tp2)
    | Utils.EqDec.NotEqual -> raise Cannot_merge
    end
  | TVForall _, _ -> raise Cannot_merge
  | TVFun _, _ ->
    (* TODO: subtyping on functions *)
    if equiv_m env tm1 tm2 tp1 tp2 then
      Type.rename_m tm1 tp1
    else raise Cannot_merge

and lub_type_m : type k. Env.t ->
    TVar.TVarMap.t -> TVar.TVarMap.t -> k typ -> k typ -> k typ =
  fun env tm1 tm2 tp1 tp2 ->
  match Type.view tp1, Type.view tp2 with
  | TVEffect effs1, TVEffect effs2 ->
    Type.of_at_effects (lub_effects_m env tm1 tm2 effs1 effs2)
  | TVEffect _, _ -> raise Cannot_merge
  | TVNeutral nt1, TVNeutral nt2 ->
    if equiv_neutral_m env tm1 tm2 nt1 nt2
    then Type.rename_m tm1 tp1
    else raise Cannot_merge
  | TVNeutral _, _ -> raise Cannot_merge
  | TVArrow(ta1, tr1, er1), TVArrow(ta2, tr2, er2) ->
    Type.arrow
      (glb_type_m env tm1 tm2 ta1 ta2)
      (lub_type_m env tm1 tm2 tr1 tr2)
      (lub_type_m env tm1 tm2 er1 er2)
  | TVArrow _, _ -> raise Cannot_merge
  | TVForall(x1, tp1), TVForall(x2, tp2) ->
    begin match Kind.equal (TVar.kind x1) (TVar.kind x2) with
    | Utils.EqDec.Equal ->
      let z = TVar.clone x1 in
      Type.forall z (lub_type_m
        (Env.add_tvar env z)
        (TVar.TVarMap.add x1 z tm1)
        (TVar.TVarMap.add x2 z tm2)
        tp1 tp2)
    | Utils.EqDec.NotEqual -> raise Cannot_merge
    end
  | TVForall _, _ -> raise Cannot_merge
  | TVFun _, _ ->
    (* TODO: subtyping on functions *)
    if equiv_m env tm1 tm2 tp1 tp2 then
      Type.rename_m tm1 tp1
    else raise Cannot_merge

let glb_type env tp1 tp2 =
  glb_type_m env TVar.TVarMap.empty TVar.TVarMap.empty tp1 tp2

let lub_type env tp1 tp2 =
  lub_type_m env TVar.TVarMap.empty TVar.TVarMap.empty tp1 tp2

let lub_types env tps =
  match tps with
  | [] -> failwith "lub_types requires at least one type"
  | tp :: tps -> 
    List.fold_left (lub_type env) tp tps
