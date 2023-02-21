open Node

include CoreCommon.All

type eff_coercion = (Utils.Seal.t, eff_coercion_data) node
and eff_coercion_data =
| ECId    of effect
| ECLift  of effect
| ECSwap  of effect * effect
| ECConsL of effect * eff_coercion
| ECConsR of eff_coercion * effect
| ECComp  of eff_coercion * eff_coercion

type val_coercion = (Utils.Seal.t, val_coercion_data) node
and val_coercion_data =
| VCId
| VCArrow of val_coercion * val_coercion * eff_coercion

type expr = (Utils.Seal.t, expr_data) node
and expr_data =
| ENum      of int
| EChar     of char
| EString   of string
| EVar      of var
| EFun      of var * expr
| ETFun     :  'kind tvar * expr -> expr_data
| EOp       of k_effect neutral_type * int * Type.ex list * expr list
| ECtor     of k_type neutral_type * int * Type.ex list * expr list
| EApp      of expr * expr
| ETApp     :  expr * 'kind typ -> expr_data
| ELet      of var * expr * expr
| ELetPure  of var * expr * expr
| EFix      of rec_function list * expr
| EHandle   of k_effect neutral_type * expr * var * expr * handler list
| EMatch    of k_type neutral_type * expr * clause list * ttype
| ETypedef  of typedef list * expr
| EAbsType  :  'kind tvar * expr -> expr_data
| ECoerce   of val_coercion * eff_coercion * expr
| EExtern   of string * ttype
| EReplExpr of expr * Utils.Seal.t * (unit -> expr) * ttype * effect
| ERepl     of Utils.Seal.t * (unit -> expr) * ttype * effect

and rec_function = (var * expr)

and handler = (TVar.ex list * var list * var * expr)
and clause  = (TVar.ex list * var list * expr)

module Expr = struct
  type t = expr

  let rec is_function e =
    match e.data with
    | EFun _ -> true
    | ETFun(_, e) | ECoerce(_, _, e) -> is_function e
    | ENum _ | EChar _ | EString _ | EVar _ | EOp _ | ECtor _ | EApp _
    | ETApp _ | ELet _ | ELetPure _ | EFix _ | EHandle _ | EMatch _
    | ETypedef _ | EAbsType _ | EExtern _ | EReplExpr _ | ERepl _ -> false
end

let flow_node = Flow.Node.create
  ~cmd_flag:  "-explicit"
  ~cmd_descr: " Transform to Explicit intermediate language."
  "Explicit"

module Tags = struct
  let eff_coercion_types = Core.Tags.coercion_types

  let val_coercion_types = Flow.Tag.create
    "val_coercion_types"

  let expr_type_info = Core.Tags.expr_type_info
end

module Keys = struct
  let meta_env = Core.Keys.meta_env

  let ecrc_in_effect  = Core.Keys.crc_in_effect
  let ecrc_out_effect = Core.Keys.crc_out_effect

  let vcrc_in_type  = Utils.Seal.gen_key ()
  let vcrc_out_type = Utils.Seal.gen_key ()

  let expr_type   = Core.Keys.expr_type
  let expr_effect = Core.Keys.expr_effect
end
