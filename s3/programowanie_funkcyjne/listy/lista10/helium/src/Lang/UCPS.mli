open Node

module Var : Utils.Variable.S with type typ := unit
type var = Var.t

module LVar : Utils.Variable.S with type typ := unit
type lvar = LVar.t

type expr = (Utils.Seal.t, expr_data) node
and expr_data =
| EEffPure
| EEffId      of lvar
| EEffCons    of expr * expr
| ENum        of int
| EChar       of char
| EString     of string
| EVar        of var
| EFun        of var * expr
| EOp         of lvar * int * expr list * expr
| ECtor       of int * expr list
| EApp        of expr * expr
| ELet        of var * expr * expr
| EFix        of rec_function list * expr
| EPureHandle of lvar * expr * pure_return_cl * pure_handler list
| EHandle     of lvar * expr * return_cl * handler list * expr
| EMatch      of expr * clause list
| EDone       of expr
| ENewEffect  of lvar * expr
| EIfPure     of expr * expr * expr
| ECoerce     of coercion * expr * expr
| EExtern     of string
| EReplExpr   of expr * Utils.Seal.t * (unit -> expr)
| ERepl       of Utils.Seal.t * (unit -> expr)

and coercion = (Utils.Seal.t, coercion_data) node
and coercion_data =
| CId
| CLift of expr
| CSwap of expr * expr
| CCons of expr * coercion
| CComp of coercion * coercion

and rec_function = var * var * expr
and clause       = var list * expr
and pure_return_cl =
  { pr_arg  : var
  ; pr_body : expr
  }
and return_cl =
  { r_arg  : var
  ; r_cont : var
  ; r_body : expr
  }
and pure_handler =
  { ph_args : var list
  ; ph_res  : var
  ; ph_body : expr
  }
and handler =
  { h_args : var list
  ; h_res  : var
  ; h_cont : var
  ; h_body : expr
  }

val flow_node : expr Flow.node
