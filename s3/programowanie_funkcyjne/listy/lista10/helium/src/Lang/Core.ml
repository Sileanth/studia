open Node

include CoreCommon.All

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

module Expr = struct
  type t = expr

  let rec is_function e =
    match e.data with
    | EFun _ -> true
    | ETFun(_, e) | ECoerce(_, e) -> is_function e
    | ENum _ | EChar _ | EString _ | EVar _ | EOp _ | ECtor _ | EApp _
    | ETApp _ | ELet _ | EFix _ | EHandle _ | EMatch _ | ETypedef _
    | EAbsType _ | EExtern _ | EReplExpr _ | ERepl _ -> false
end

let flow_node = Flow.Node.create
  ~cmd_flag:  "-core"
  ~cmd_descr: " Transform to Core intermediate language."
  "Core"

module Tags = struct
  let coercion_types = Flow.Tag.create
    "eff_coercion_types"

  let expr_type_info = Flow.Tag.create
    ~cmd_flag:  "-typecheck"
    ~cmd_descr: " Check types at intermediate representation"
    "typecheck"
end

module Keys = struct
  let crc_in_effect  = Utils.Seal.gen_key ()
  let crc_out_effect = Utils.Seal.gen_key ()

  let meta_env    = Utils.Seal.gen_key ()
  let expr_type   = Utils.Seal.gen_key ()
  let expr_effect = Utils.Seal.gen_key ()
end
