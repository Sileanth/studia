open Utils.Either

type t =
  { s_vars  : TConst.t list
  ; s_subst : (Type.tvar * Type.t) list
  ; s_type  : Type.t
  }

let qvars sch = sch.s_vars
let body  sch = sch.s_type
let subst sch = sch.s_subst

let rec close_type_with sub tp =
  match Type.nf_view tp with
  | Type.TNF_EffPure  -> Type.eff_pure
  | Type.TNF_EffCons(eff1, eff2) ->
    Type.eff_cons (close_type_with sub eff1) (close_type_with sub eff2)
  | Type.TNF_Neu(head, args) ->
    let head =
      match head with
      | Type.HVar(perm, x) ->
        Type.perm perm
          begin match Type.TVar.Map.find_opt x sub with
          | Some tp -> tp
          | None    -> Type.var x
          end
      | Type.HConst c -> Type.tconst c
    in
    Type.apps head (List.map (close_type_with sub) args)
  | Type.TNF_Arrow(tp1, tp2, eff) ->
    Type.arrow
      (close_type_with sub tp1)
      (close_type_with sub tp2)
      (close_type_with sub eff)
  | Type.TNF_Fun(x, tp) ->
    let y = TConst.clone x in
    Type.tfun y (close_type_with sub (Type.perm (TCPerm.swap x y) tp))

let apply_subst sch tp =
  let sub = List.fold_left (fun sub (x, tp) ->
      Type.TVar.Map.add x tp sub
    ) Type.TVar.Map.empty sch.s_subst
  in close_type_with sub tp

let open_s ~scope sch =
  let (sub, tvs) = Type.opening_subst ~scope ~vtypes:sch.s_vars () in
  (tvs, Type.open_with sub sch.s_type)

let close_with ?(abs_types=[]) vars tp =
  let subst = List.map (fun x ->
      Type.TVar.freeze x;
      let c = TConst.fresh ([], Type.TVar.kind x) in
      (x, c)
    ) vars in
  let vars = List.map snd subst in
  let subst = List.map (fun (x, c) -> (x, Type.tconst c)) subst in
  let sub = List.fold_left (fun sub (x, tp) ->
      Type.TVar.Map.add x tp sub
    ) Type.TVar.Map.empty subst in
  { s_vars  = abs_types @ vars
  ; s_subst = subst
  ; s_type  = close_type_with sub tp
  }

let close_down_with ?(abs_types=[]) vars tp =
  let subst = List.map (fun x ->
      Type.TVar.freeze x;
      match Kind.view (Type.TVar.kind x) with
      | Kind.KEffect when Type.is_positive x tp ->
        (x, Right Type.eff_pure)
      | _ ->
        let c = TConst.fresh ([], Type.TVar.kind x) in
        (x, Left c)
    ) vars in
  let vars = Utils.ListExt.filter_map (fun (x, act) ->
      match act with
      | Left c  -> Some c
      | Right _ -> None
    ) subst in
  let subst = List.map (fun (x, act) ->
      match act with
      | Left  c -> (x, Type.tconst c)
      | Right t -> (x, t)
    ) subst in
  let sub = List.fold_left (fun sub (x, tp) ->
      Type.TVar.Map.add x tp sub
    ) Type.TVar.Map.empty subst in
  { s_vars  = abs_types @ vars
  ; s_subst = subst
  ; s_type  = close_type_with sub tp
  }

let to_type sch =
  if sch.s_vars = [] then sch.s_type
  else failwith "cannot coerce scheme to type"

let tvars sch =
  Type.tvars sch.s_type
