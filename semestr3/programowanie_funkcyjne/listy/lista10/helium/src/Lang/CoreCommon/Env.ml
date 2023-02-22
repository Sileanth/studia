open Kind.Datatypes
open Type.Datatypes
open TypeDef.Datatypes

type timestamp =
| At     of int
| Before of int

type 'k typedef_info =
| TD : ('k, 'rk) typedef_formal_params * 'rk typedef_body -> 'k typedef_info

module TypeDefMap = TVar.Map.Make(struct type 'k t = 'k typedef_info end)

type t =
  { time     : int
  ; vars     : Var.Set.t
  ; tvars    : timestamp TVar.Map.t
  ; typedefs : TypeDefMap.t
  }

let empty =
  { time     = 0
  ; vars     = Var.Set.empty
  ; tvars    = TVar.Map.empty
  ; typedefs = TypeDefMap.empty
  }

let add_var env x =
  { env with
    vars = Var.Set.add x env.vars
  }

let add_vars env xs =
  List.fold_left add_var env xs

let add_tvar env ?(fresh=false) x =
  let ts = if fresh then At env.time else Before env.time in
  { env with
    time     = if fresh then env.time + 1 else env.time
  ; tvars    = TVar.Map.add x ts env.tvars
  ; typedefs = TypeDefMap.remove x env.typedefs (* old value is covered *)
  }

let add_tvars env ?fresh xs =
  List.fold_left (fun env (TVar.Pack x) ->
      add_tvar env ?fresh x
    ) env xs

let rec add_typedef_formal_params : type k1 k2.
    t -> (k1, k2) typedef_formal_params -> t =
    fun env params ->
  match params with
  | TDFP_Nil -> env
  | TDFP_Cons(x, params) ->
    add_typedef_formal_params (add_tvar env x) params

let has_var env x =
  Var.Set.mem x env.vars

let has_tvar env x =
  TVar.Map.mem x env.tvars

type (_, _) match_type_params =
| MTP_Done      : ('k, 'k) match_type_params
| MTP_Saturated : (('k1, 'k2) k_arrow, 'r) type_params ->
    (('k1, 'k2) k_arrow, 'r) match_type_params
| MTP_Missing   : (('k1, 'k2) k_arrow, 'r) typedef_formal_params ->
    ('r, ('k1, 'k2) k_arrow) match_type_params

let match_type_params fparams params =
  let rec loop : type k r1 r2. Type.TVarMap.t ->
      (k, r1) typedef_formal_params ->
      (k, r2) type_params ->
        Type.TVarMap.t * (r1, r2) match_type_params =
      fun sub fparams params ->
    match fparams, params with
    | TDFP_Nil,    TP_Nil    -> (sub, MTP_Done)
    | TDFP_Nil,    TP_Cons _ -> (sub, MTP_Saturated params)
    | TDFP_Cons _, TP_Nil    -> (sub, MTP_Missing fparams)
    | TDFP_Cons(x, fparams), TP_Cons(tp, params) ->
      loop (Type.TVarMap.add x tp sub) fparams params
  in loop Type.TVarMap.empty fparams params

let lookup_effect env (TNeu(h, params) : k_effect neutral_type) =
  match h with
  | HAny _ -> assert false
  | HVar l ->
  let TD(fparams, body) = TypeDefMap.find l env.typedefs in
  let (sub, mtp) = match_type_params fparams params in
  let body = TypeDef.subst_body_m sub body in
  match mtp with
  | MTP_Done        -> let TDEffect ops = body in ops
  | MTP_Saturated _ -> (match body with _ -> .)

let lookup_op env neu n = List.nth (lookup_effect env neu) n

let lookup_adt_opt env (TNeu(h, params) : k_type neutral_type) =
  match h with
  | HAny _ -> None
  | HVar l ->
    match TypeDefMap.find_opt l env.typedefs with
    | None -> None
    | Some (TD(fparams, body)) ->
    let (sub, mtp) = match_type_params fparams params in
    let body = TypeDef.subst_body_m sub body in
    match mtp with
    | MTP_Done        -> let TDData ctors = body in Some ctors
    | MTP_Saturated _ -> (match body with _ -> .)

let lookup_adt env neu =
  match lookup_adt_opt env neu with
  | Some ctors -> ctors
  | None -> raise Not_found

let lookup_ctor env neu n = List.nth (lookup_adt env neu) n

let ts_before ts1 ts2 =
  match ts1, ts2 with
  | At t1,     At t2 -> t1 < t2
  | Before t1, At t2 -> t1 <= t2
  | _, Before _      -> false

let can_swap env l1 l2 =
  let ts1 = TVar.Map.find l1 env.tvars in
  let ts2 = TVar.Map.find l2 env.tvars in
  ts_before ts1 ts2 || ts_before ts2 ts1

let prepare_typedef env ~fresh td =
  let TypeDef(l, _, _) = td in
  add_tvar env ~fresh l

let prepare_typedefs env ~fresh tds =
  List.fold_left (prepare_typedef ~fresh) env tds

let add_typedef env td =
  let TypeDef(l, params, body) = td in
  assert (not (TypeDefMap.mem l env.typedefs));
  { env with
    typedefs = TypeDefMap.add l (TD(params, body)) env.typedefs
  }

let add_typedefs env tds =
  List.fold_left add_typedef env tds
