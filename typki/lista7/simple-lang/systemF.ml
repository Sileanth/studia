(** System F with row polymorphism *)

(** Type variables *)
module type TVar_S = sig
  type t

  val compare : t -> t -> int

  val fresh : unit -> t
end

module TVar : TVar_S = struct
  include Int

  let next_fresh = ref 0
  let fresh () =
    let x = !next_fresh in
    next_fresh := x + 1;
    x
end

(** Row variables *)
module RVar : TVar_S = TVar

type tvar = TVar.t
type rvar = RVar.t
type var  = string

type name = string

type tp =
  | TUnit
  | TEmpty
  | TBool
  | TInt
  | TVar     of tvar
  | TArrow   of tp * tp
  | TForallT of tvar * tp
  | TForallR of rvar * tp
  | TPair    of tp * tp
  | TCoPair  of tp * tp
  | TRecord  of row
  | TVariant of row

(** Row is a list ended with optional row variable *)
and row = (name * tp) list * rvar option

type expr =
  | EUnit
  | EBool   of bool
  | ENum    of int
  | EVar    of string
  | EFn     of var * tp * expr
  | EFix    of var * var * tp * tp * expr
  | EApp    of expr * expr
  | ETFn    of tvar * expr
  | ETApp   of expr * tp
  | ERFn    of rvar * expr (* Same as ETFn, but abstracts a row variable *)
  | ERApp   of expr * row  (* Same as ETApp, but applies to a row *)
  | ELet    of var * expr * expr
  | EPair   of expr * expr
  | EFst    of expr
  | ESnd    of expr
  | EInl    of expr * tp
  | EInr    of tp * expr
  | ECase   of expr * clause * clause
  | EIf     of expr * expr * expr
  | ESeq    of expr * expr
  | EAbsurd of expr * tp
  | ESelect of expr * name
  | ERecord of (name * expr) list
  | ECtor   of name * expr * row
  | EMatch  of expr * name * clause * clause
  | EMatchEmpty of expr * tp

and clause = var * expr

type program = expr

module VarMap  = Map.Make(String)
module TVarMap = Map.Make(TVar)
module RVarMap = Map.Make(RVar)

(* ========================================================================= *)
(* Type substitution *)

let rec subst tsub rsub tp =
  match tp with
  | TUnit | TEmpty | TBool | TInt -> tp
  | TVar x ->
    begin match TVarMap.find_opt x tsub with
    | None    -> tp
    | Some tp -> tp
    end
  | TArrow(tp1, tp2) ->
    TArrow(subst tsub rsub tp1, subst tsub rsub tp2)
  | TForallT(a, tp) ->
    let b = TVar.fresh () in
    TForallT(b, subst (TVarMap.add a (TVar b) tsub) rsub tp)
  | TForallR(a, tp) ->
    let b = RVar.fresh () in
    TForallR(b, subst tsub (RVarMap.add a ([], Some b) rsub) tp)
  | TPair(tp1, tp2) ->
    TPair(subst tsub rsub tp1, subst tsub rsub tp2)
  | TCoPair(tp1, tp2) ->
    TCoPair(subst tsub rsub tp1, subst tsub rsub tp2)
  | TRecord  r -> TRecord  (subst_in_row tsub rsub r)
  | TVariant r -> TVariant (subst_in_row tsub rsub r)

and subst_in_row tsub rsub (r, re) =
  let (r', re) =
    match re with
    | None -> ([], None)
    | Some x ->
      begin match RVarMap.find_opt x rsub with
      | None -> ([], Some x)
      | Some r -> r
      end
  in
  (List.map (fun (name, tp) -> (name, subst tsub rsub tp)) r @ r', re)

let subst_type tp x s =
  subst (TVarMap.singleton x s) RVarMap.empty tp

let subst_row tp x row =
  subst TVarMap.empty (RVarMap.singleton x row) tp

(* ========================================================================= *)
(* Type equality *)

let rec row_select name r =
  match r with
  | [] -> None
  | (name', tp) :: r when name' = name -> Some(tp, r)
  | (name', tp') :: r ->
    begin match row_select name r with
    | None -> None
    | Some(tp, r) -> Some(tp, (name', tp') :: r)
    end

let rec type_equal tp1 tp2 =
  match tp1, tp2 with
  | TUnit, TUnit -> true
  | TUnit, _ -> false

  | TEmpty, TEmpty -> true
  | TEmpty, _ -> false

  | TBool, TBool -> true
  | TBool, _ -> false

  | TInt, TInt -> true
  | TInt, _ -> false

  | TVar x, TVar y -> TVar.compare x y = 0
  | TVar _, _ -> false

  | TArrow(ta1, tb1), TArrow(ta2, tb2) ->
    type_equal ta1 ta2 && type_equal tb1 tb2
  | TArrow _, _ -> false

  | TForallT(a1, tp1), TForallT(a2, tp2) ->
    let a = TVar.fresh () in
    type_equal (subst_type tp1 a1 (TVar a)) (subst_type tp2 a2 (TVar a))
  | TForallT _, _ -> false

  | TForallR(a1, tp1), TForallR(a2, tp2) ->
    let a = RVar.fresh () in
    type_equal (subst_row tp1 a1 ([], Some a)) (subst_row tp2 a2 ([], Some a))
  | TForallR _, _ -> false

  | TPair(ta1, tb1), TPair(ta2, tb2) ->
    type_equal ta1 ta2 && type_equal tb1 tb2
  | TPair _, _ -> false

  | TCoPair(ta1, tb1), TCoPair(ta2, tb2) ->
    type_equal ta1 ta2 && type_equal tb1 tb2
  | TCoPair _, _ -> false

  | TRecord r1, TRecord r2 ->
    row_equal r1 r2
  | TRecord _, _ -> false

  | TVariant r1, TVariant r2 ->
    row_equal r1 r2
  | TVariant _, _ -> false

and row_equal (r1, re1) (r2, re2) =
  row_body_equal r1 r2 &&
  match re1, re2 with
  | None, None -> true
  | None, _    -> false
  | Some x1, Some x2 -> RVar.compare x1 x2 = 0
  | Some _,  _ -> false

and row_body_equal r1 r2 =
  match r1, r2 with
  | [], []     -> true
  | [], _ :: _ -> false
  | (name, tp1) :: r1, _ ->
    begin match row_select name r2 with
    | None -> false
    | Some(tp2, r2) -> type_equal tp1 tp2 && row_body_equal r1 r2
    end

(* ========================================================================= *)
(* Type checking *)

(** Typing environments *)
module Env : sig
  type t

  val empty : t

  val add_var  : t -> var -> tp -> t
  val add_tvar : t -> tvar -> t * tvar
  val add_rvar : t -> rvar -> t * rvar

  val lookup_var  : t -> var -> tp
  val lookup_tvar : t -> tvar -> tvar
  val lookup_rvar : t -> rvar -> rvar
end = struct
  type t =
    { var_map  : tp VarMap.t;
      tvar_map : tvar TVarMap.t;
      rvar_map : rvar RVarMap.t }

  let empty =
    { var_map  = VarMap.empty;
      tvar_map = TVarMap.empty;
      rvar_map = RVarMap.empty }

  let add_var env x tp =
    { env with var_map = VarMap.add x tp env.var_map }

  let add_tvar env a =
    let b = TVar.fresh () in
    { env with tvar_map = TVarMap.add a b env.tvar_map}, b

  let add_rvar env a =
    let b = RVar.fresh () in
    { env with rvar_map = RVarMap.add a b env.rvar_map}, b

  let lookup_var env x =
    match VarMap.find_opt x env.var_map with
    | None -> failwith ("Internal error: unbound variable " ^ x)
    | Some tp -> tp

  let lookup_tvar env x =
    match TVarMap.find_opt x env.tvar_map with
    | None -> failwith "Internal error: unbound type variable"
    | Some x -> x

  let lookup_rvar env x =
    match RVarMap.find_opt x env.rvar_map with
    | None -> failwith "Internal error: unbound row variable"
    | Some x -> x
end

(** Checks if type is well-scoped, and refresh its type variables according to
  * the environment *)
let rec check_well_scoped env tp =
  match tp with
  | TUnit | TEmpty | TBool | TInt -> tp
  | TVar a -> TVar (Env.lookup_tvar env a)
  | TArrow(tp1, tp2) ->
    TArrow(check_well_scoped env tp1, check_well_scoped env tp2)
  | TForallT(a, tp) ->
    let (env, a) = Env.add_tvar env a in
    TForallT(a, check_well_scoped env tp)
  | TForallR(a, tp) ->
    let (env, a) = Env.add_rvar env a in
    TForallR(a, check_well_scoped env tp)
  | TPair(tp1, tp2) ->
    TPair(check_well_scoped env tp1, check_well_scoped env tp2)
  | TCoPair(tp1, tp2) ->
    TCoPair(check_well_scoped env tp1, check_well_scoped env tp2)
  | TRecord  r -> TRecord  (check_row_well_scoped env r)
  | TVariant r -> TVariant (check_row_well_scoped env r)

and check_row_well_scoped env (r, re) =
  (List.map (fun (name, tp) -> (name, check_well_scoped env tp)) r,
   Option.map (Env.lookup_rvar env) re)

let rec infer_type env e =
  match e with
  | EUnit   -> TUnit
  | EBool _ -> TBool
  | ENum  _ -> TInt
  | EVar  x -> Env.lookup_var env x
  | EFn(x, tp, body) ->
    let tp1 = check_well_scoped env tp in
    let tp2 = infer_type (Env.add_var env x tp1) body in
    TArrow(tp1, tp2)
  | EFix(f, x, tp1, tp2, body) ->
    let tp1 = check_well_scoped env tp1 in
    let tp2 = check_well_scoped env tp2 in
    let f_tp = TArrow(tp1, tp2) in
    check_type (Env.add_var (Env.add_var env f f_tp) x tp1) body tp2;
    f_tp
  | EApp(e1, e2) ->
    begin match infer_type env e1 with
    | TArrow(tp2, tp1) ->
      check_type env e2 tp2;
      tp1
    | _ -> failwith "Internal type error"
    end
  | ETFn(a, body) ->
    let (env, b) = Env.add_tvar env a in
    let tp = infer_type env body in
    TForallT(b, tp)
  | ETApp(e, tp) ->
    begin match infer_type env e with
    | TForallT(a, body) ->
      let tp = check_well_scoped env tp in
      subst_type body a tp
    | _ -> failwith "Internal type error"
    end
  | ERApp(e, row) ->
    begin match infer_type env e with
    | TForallR(a, body) ->
      let row = check_row_well_scoped env row in
      subst_row body a row
    | _ -> failwith "Internal type error"
    end
  | ERFn(a, body) ->
    let (env, b) = Env.add_rvar env a in
    let tp = infer_type env body in
    TForallR(b, tp)
  | ELet(x, e1, e2) ->
    let tp1 = infer_type env e1 in
    infer_type (Env.add_var env x tp1) e2
  | EPair(e1, e2) ->
    let tp1 = infer_type env e1 in
    let tp2 = infer_type env e2 in
    TPair(tp1, tp2)
  | EFst e ->
    begin match infer_type env e with
    | TPair(tp1, tp2) -> tp1
    | _ -> failwith "Internal type error"
    end
  | ESnd e ->
    begin match infer_type env e with
    | TPair(tp1, tp2) -> tp2
    | _ -> failwith "Internal type error"
    end
  | EInl(e, tp2) ->
    let tp1 = infer_type env e in
    let tp2 = check_well_scoped env tp2 in
    TCoPair(tp1, tp2)
  | EInr(tp1, e) ->
    let tp1 = check_well_scoped env tp1 in
    let tp2 = infer_type env e in
    TCoPair(tp1, tp2)
  | ECase(e, (x1, e1), (x2, e2)) ->
    begin match infer_type env e with
    | TCoPair(tp1, tp2) ->
      let tp = infer_type (Env.add_var env x1 tp1) e1 in
      check_type (Env.add_var env x2 tp2) e2 tp;
      tp
    | _ -> failwith "Internal type error"
    end
  | EIf(e1, e2, e3) ->
    check_type env e1 TBool;
    let tp = infer_type env e2 in
    check_type env e3 tp;
    tp
  | ESeq(e1, e2) ->
    check_type env e1 TUnit;
    infer_type env e2
  | EAbsurd(e, tp) ->
    check_type env e TEmpty;
    check_well_scoped env tp
  | ESelect(e, l) ->
    begin match infer_type env e with
    | TRecord(r, _) ->
      begin match List.assoc_opt l r with
      | Some tp -> tp
      | None -> failwith "Internal type error"
      end
    | _ -> failwith "Internal type error"
    end
  | ERecord r ->
    TRecord(List.map (fun (name, e) -> (name, infer_type env e)) r, None)
  | ECtor(name, e, r) ->
    let (r, re) = check_row_well_scoped env r in
    TVariant((name, infer_type env e) :: r, re)
  | EMatch(e, name, (x1, e1), (x2, e2)) ->
    begin match infer_type env e with
    | TVariant(r, re) ->
      begin match row_select name r with
      | Some(tp1, r) ->
        let tp = infer_type (Env.add_var env x1 tp1) e1 in
        check_type (Env.add_var env x2 (TVariant(r, re))) e2 tp;
        tp
      | None -> failwith "Internal type error"
      end
    | _ -> failwith "Internal type error"
    end
  | EMatchEmpty(e, tp) ->
    begin match infer_type env e with
    | TVariant([], None) ->
      check_well_scoped env tp
    | _ -> failwith "Internal type error"
    end

and check_type env e tp =
  let tp' = infer_type env e in
  if type_equal tp' tp then ()
  else failwith "Internal type error"

let ensure_well_typed p =
  let _ : tp = infer_type Env.empty p in
  ()
