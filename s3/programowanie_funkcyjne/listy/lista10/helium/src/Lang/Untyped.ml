open Node

module Var = Utils.Variable.Make(struct
  type typ = unit
  let default_name = "x"
end)
type var = Var.t

type expr = (Utils.Seal.t, expr_data) node
and expr_data =
| EEffPure
| EEffCons   of expr * expr
| ENum       of int
| EChar      of char
| EString    of string
| EVar       of var
| EFun       of var * expr
| EOp        of var * int * expr list
| ECtor      of int * expr list
| EApp       of expr * expr
| ELet       of var * expr * expr
| EFix       of rec_function list * expr
| EHandle    of var * expr * var * expr * handler list
| EMatch     of expr * clause list
| ECoerce    of expr * expr
| ENewEffect of var * expr
| ECLift     of expr
| ECSwap     of expr * expr
| ECCons     of expr * expr
| ECComp     of expr * expr
| EExtern    of string
| EReplExpr  of expr * Utils.Seal.t * (unit -> expr)
| ERepl      of Utils.Seal.t * (unit -> expr)

and rec_function = var * var * expr
and handler = var list * var * expr
and clause  = var list * expr

let flow_node = Flow.Node.create "Untyped"
