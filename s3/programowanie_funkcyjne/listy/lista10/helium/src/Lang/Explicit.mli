open Node

include module type of CoreCommon.All

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

module Expr : sig
  type t = expr

  val is_function : t -> bool
end

val flow_node : expr Flow.node

module Tags : sig
  val eff_coercion_types : Flow.tag
  val val_coercion_types : Flow.tag
  val expr_type_info     : Flow.tag
end

module Keys : sig
  val meta_env : Env.t Utils.Seal.key

  val ecrc_in_effect  : effect Utils.Seal.key
  val ecrc_out_effect : effect Utils.Seal.key

  val vcrc_in_type  : ttype Utils.Seal.key
  val vcrc_out_type : ttype Utils.Seal.key

  val expr_type   : ttype Utils.Seal.key
  val expr_effect : effect Utils.Seal.key
end
