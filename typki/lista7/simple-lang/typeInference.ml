(** Type inference (for simple types) *)

(** Internal representation of types *)
module Type : sig
  type t
  type uvar
  type view =
    | TUnit
    | TEmpty
    | TBool
    | TInt
    | TUVar   of uvar
    | TArrow  of t * t
    | TPair   of t * t
    | TCoPair of t * t

  val fresh_uvar : unit -> t

  val t_unit   : t
  val t_empty  : t
  val t_bool   : t
  val t_int    : t
  val t_arrow  : t -> t -> t
  val t_pair   : t -> t -> t
  val t_copair : t -> t -> t

  val view : t -> view

  val set_uvar : uvar -> t -> unit
end = struct
  type t = view
  and uvar = t option ref
  and view =
    | TUnit
    | TEmpty
    | TBool
    | TInt
    | TUVar   of uvar
    | TArrow  of t * t
    | TPair   of t * t
    | TCoPair of t * t

  let fresh_uvar () = TUVar (ref None)

  let t_unit  = TUnit
  let t_empty = TEmpty
  let t_bool  = TBool
  let t_int   = TInt
  let t_arrow  tp1 tp2 = TArrow(tp1, tp2)
  let t_pair   tp1 tp2 = TPair(tp1, tp2)
  let t_copair tp1 tp2 = TCoPair(tp1, tp2)

  let rec view tp =
    match tp with
    | TUVar x ->
      begin match !x with
      | None -> tp
      | Some tp ->
        let tp = view tp in
        x := Some tp;
        tp
      end
    | _ -> tp

  let set_uvar x tp =
    match !x with
    | None -> x := Some tp
    | Some _ -> assert false
end

(* ========================================================================= *)
(* Pretty printing of types *)

(** Creates fresh pretty-printing context *)
let pp_context () = ref []

let pp_at_level l lvl str =
  if lvl > l then Printf.sprintf "(%s)" str
  else str

let rec pp_type ctx lvl tp =
  match Type.view tp with
  | TUnit  -> "Unit"
  | TEmpty -> "Empty"
  | TBool  -> "Bool"
  | TInt   -> "Int"
  | TUVar x ->
    begin match List.assq_opt x !ctx with
    | Some str -> str
    | None ->
      let name = Printf.sprintf "x%d" (List.length !ctx) in
      ctx := (x, name) :: !ctx;
      name
    end
  | TArrow(tp1, tp2) ->
    pp_at_level 0 lvl
      (Printf.sprintf "%s -> %s" (pp_type ctx 1 tp1) (pp_type ctx 0 tp2))
  | TPair(tp1, tp2) ->
    pp_at_level 2 lvl
      (Printf.sprintf "%s * %s" (pp_type ctx 3 tp1) (pp_type ctx 2 tp2))
  | TCoPair(tp1, tp2) ->
    pp_at_level 1 lvl
      (Printf.sprintf "%s + %s" (pp_type ctx 2 tp1) (pp_type ctx 1 tp2))

(* ========================================================================= *)
(* Unification *)

exception Cannot_unify

let rec contains_uvar x tp =
  match Type.view tp with
  | TUVar y -> x == y
  | TUnit | TEmpty | TBool | TInt -> false
  | TArrow(tp1, tp2) | TPair(tp1, tp2) | TCoPair(tp1, tp2) ->
    contains_uvar x tp1 || contains_uvar x tp2

let unify_with_uvar x tp =
  if contains_uvar x tp then raise Cannot_unify
  else Type.set_uvar x tp

let rec unify tp1 tp2 =
  match Type.view tp1, Type.view tp2 with
  | TUVar x, TUVar y when x == y -> ()
  | TUVar x, _ -> unify_with_uvar x tp2
  | _, TUVar x -> unify_with_uvar x tp1

  | TUnit, TUnit -> ()
  | TUnit, _ -> raise Cannot_unify

  | TEmpty, TEmpty -> ()
  | TEmpty, _ -> raise Cannot_unify

  | TBool, TBool -> ()
  | TBool, _ -> raise Cannot_unify

  | TInt, TInt -> ()
  | TInt, _ -> raise Cannot_unify

  | TArrow(ta1, tb1), TArrow(ta2, tb2) ->
    unify ta1 ta2;
    unify tb1 tb2
  | TArrow _, _ -> raise Cannot_unify

  | TPair(ta1, tb1), TPair(ta2, tb2) ->
    unify ta1 ta2;
    unify tb1 tb2
  | TPair _, _ -> raise Cannot_unify

  | TCoPair(ta1, tb1), TCoPair(ta2, tb2) ->
    unify ta1 ta2;
    unify tb1 tb2
  | TCoPair _, _ -> raise Cannot_unify

(* ========================================================================= *)
(* Type inference *)

(** Typing environments *)
module Env : sig
  type t

  val empty : t

  val extend : t -> Ast.var -> Type.t -> t

  val lookup : t -> Ast.var -> Type.t option
end = struct
  module VarMap = Map.Make(String)

  type t = Type.t VarMap.t

  let empty = VarMap.empty

  let extend env x tp = VarMap.add x tp env

  let lookup env x = VarMap.find_opt x env
end

let rec infer_type env (e : Ast.expr) =
  match e.data with
  | EUnit   -> Type.t_unit
  | EBool b -> Type.t_bool
  | ENum  n -> Type.t_int
  | EVar  x ->
    begin match Env.lookup env x with
    | Some tp -> tp
    | None ->
      Utils.report_error e "Unbound variable %s" x
    end
  | EFn(x, body) ->
    let tp1 = Type.fresh_uvar () in
    let tp2 = infer_type (Env.extend env x tp1) body in
    Type.t_arrow tp1 tp2
  | EFix(f, x, body) ->
    let tp1 = Type.fresh_uvar () in
    let tp2 = Type.fresh_uvar () in
    let f_tp = Type.t_arrow tp1 tp2 in
    check_type (Env.extend (Env.extend env f f_tp) x tp1) body tp2;
    f_tp
  | EApp(e1, e2) ->
    let tp2 = Type.fresh_uvar () in
    let tp1 = Type.fresh_uvar () in
    check_type env e1 (Type.t_arrow tp2 tp1);
    check_type env e2 tp2;
    tp1
  | ELet(x, e1, e2) ->
    let tp1 = infer_type env e1 in
    infer_type (Env.extend env x tp1) e2
  | EPair(e1, e2) ->
    let tp1 = infer_type env e1 in
    let tp2 = infer_type env e2 in
    Type.t_pair tp1 tp2
  | EFst e ->
    let tp1 = Type.fresh_uvar () in
    let tp2 = Type.fresh_uvar () in
    check_type env e (Type.t_pair tp1 tp2);
    tp1
  | ESnd e ->
    let tp1 = Type.fresh_uvar () in
    let tp2 = Type.fresh_uvar () in
    check_type env e (Type.t_pair tp1 tp2);
    tp2
  | EInl e ->
    let tp1 = infer_type env e in
    let tp2 = Type.fresh_uvar () in
    Type.t_copair tp1 tp2
  | EInr e ->
    let tp1 = Type.fresh_uvar () in
    let tp2 = infer_type env e in
    Type.t_copair tp1 tp2
  | ECase(e, (x1, e1), (x2, e2)) ->
    let tp1 = Type.fresh_uvar () in
    let tp2 = Type.fresh_uvar () in
    check_type env e (Type.t_copair tp1 tp2);
    let tp = infer_type (Env.extend env x1 tp1) e1 in
    check_type (Env.extend env x2 tp2) e2 tp;
    tp
  | EIf(e1, e2, e3) ->
    check_type env e1 Type.t_bool;
    let tp = infer_type env e2 in
    check_type env e3 tp;
    tp
  | ESeq(e1, e2) ->
    check_type env e1 Type.t_unit;
    infer_type env e2
  | EAbsurd e ->
    check_type env e Type.t_empty;
    Type.fresh_uvar ()
  | ESelect _ | ERecord _ | ECtor _ | EMatch _ | EMatchEmpty _ ->
    (* TODO: not implemented *)
    Utils.report_error e "This language feature is not supported yet!"

and check_type env (e : Ast.expr) tp =
  let tp' = infer_type env e in
  try unify tp tp' with
  | Cannot_unify ->
    let ctx = pp_context () in
    Utils.report_error e
      "This expression has type %s, but an expression was expected of type %s."
      (pp_type ctx 0 tp')
      (pp_type ctx 0 tp)

let to_system_f p =
  (* TODO: not implemented properly *)
  let _ = infer_type Env.empty p in
  SystemF.ENum 42
