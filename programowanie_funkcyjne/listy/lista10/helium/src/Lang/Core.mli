open Node

include module type of CoreCommon.All

type coercion = (Utils.Seal.t, coercion_data) node
and coercion_data =
| CLift of effect
| CSwap of effect * effect
| CCons of effect * coercion
| CComp of coercion * coercion

type expr = (Utils.Seal.t, expr_data) node
and expr_data =
| ENum      of int
| EChar     of char
| EString   of string
| EVar      of var
| EFun      of var * expr
| ETFun     : 'k tvar * expr -> expr_data
| EOp       of k_effect neutral_type * int * Type.ex list * expr list
| ECtor     of k_type neutral_type * int * Type.ex list * expr list
| EApp      of expr * expr
| ETApp     :  expr * 'k typ -> expr_data
| ELet      of var * expr * expr
| EFix      of rec_function list * expr
| EHandle   of k_effect neutral_type * expr * var * expr * handler list
| EMatch    of k_type neutral_type * expr * clause list * ttype
| ETypedef  of typedef list * expr
| EAbsType  :  'kind tvar * expr -> expr_data
| ECoerce   of coercion * expr
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
  val coercion_types : Flow.tag
  val expr_type_info : Flow.tag
end

module Keys : sig
  val crc_in_effect  : effect Utils.Seal.key
  val crc_out_effect : effect Utils.Seal.key

  val meta_env    : Env.t Utils.Seal.key
  val expr_type   : ttype Utils.Seal.key
  val expr_effect : effect Utils.Seal.key
end
