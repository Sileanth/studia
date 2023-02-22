open Node

type kind = Raw.kind =
| KType
| KEffect
| KArrow of kind * kind

type mpath = string list

module TConst = struct
  include Utils.Variable.Make(struct
    type typ = mpath
    let default_name = "T"
  end)
  let mpath = type_of
  let full_name x =
    List.fold_right (fun m name ->
        if name = "this" then m
        else m ^ "." ^ name
      ) (mpath x) (name x)
end
type tconst = TConst.t

module Var = Utils.Variable.Make(struct
  type typ = unit
  let default_name = "x"
end)
type var = Var.t

module Op = Utils.Variable.Make(struct
  type typ = unit
  let default_name = "op"
end)
type op = Op.t

module Ctor = Utils.Variable.Make(struct
  type typ = unit
  let default_name = "C"
end)
type ctor = Ctor.t

module Field = Utils.Variable.Make(struct
  type typ = unit
  let default_name = "l"
end)
type field = Field.t

type type_farg = (Utils.Position.t, type_farg_data) node
and type_farg_data =
| TFA_Var   of tconst
| TFA_Annot of tconst * kind

type type_expr = (Utils.Position.t, type_expr_data) node
and type_expr_data =
| TPlaceholder
| TEffPure
| TConst        of tconst
| TArrowPure    of type_expr * type_expr
| TArrowEff     of type_expr * type_expr * type_expr
| TEffRow       of type_expr
| TEffCons      of type_expr * type_expr
| TApp          of type_expr * type_expr

type type_field = (Utils.Position.t, type_field_data) node
and type_field_data =
| TField of string * type_expr

type annot =
| Annot    of type_expr
| AnnotEff of type_expr * type_expr

type op_decl = (Utils.Position.t, op_decl_data) node
and op_decl_data =
| OpDecl of op * type_farg list * type_expr list * type_expr

type adt_ctor = (Utils.Position.t, adt_ctor_data) node
and adt_ctor_data =
| ADTCtor of ctor * type_farg list * type_expr list

type field_decl = (Utils.Position.t, field_decl_data) node
and field_decl_data =
| FieldDecl of field * type_farg list * type_expr

type typedef = (Utils.Position.t, typedef_data) node
and typedef_data =
| TDEffect of tconst * type_farg list * op_decl list
| TDData   of tconst * type_farg list * adt_ctor list
| TDRecord of tconst * type_farg list * field_decl list

type pattern = (Utils.Position.t, pattern_data) node
and pattern_data =
| PWildcard
| PVar    of var
| PCtor   of ctor * type_farg list * pattern list
| PList   of pattern list
| PRecord of field_pattern list
| PAnnot  of pattern * type_expr

and field_pattern = (Utils.Position.t, field_pattern_data) node
and field_pattern_data =
| FieldPat of field * pattern

type expr = (Utils.Position.t, expr_data) node
and expr_data =
| EVar        of var
| EOp         of op
| ECtor       of ctor
| EModule     of ctor * type_field list
| ENum        of int
| EChar       of char
| EString     of string
| EList       of expr list
| EFun        of pattern list * annot option * expr
| EApp        of expr * expr
| ELet        of
    var * type_farg list * pattern list * annot option * expr * expr
| ELetPat     of pattern * annot option * expr * expr
| ELetRec     of rec_function list * expr
| EUIf        of expr * expr
| EIf         of expr * expr * expr
| EHandle     of expr * handler list
| EHandleWith of expr * expr
| EHandler    of handler list
| EMatch      of expr * clause list
| ESelect     of expr * field
| ESelectF    of expr * field * type_field list
| ERecord     of field_def list
| ETypedef    of typedef list * expr
| ETypeAlias  of tconst * type_farg list * type_expr * expr
| EAbsType    of tconst * kind * expr
| EAnnot      of expr * annot
| EExtern     of string * type_farg list * type_expr
| EPragmaFlag of string * expr
| EPragmaVal  of string * var * expr
| EPragmaType of string * tconst * expr
| ERepl       of (unit -> expr)
| EReplExpr   of expr * (unit -> expr)

and rec_function = (Utils.Position.t, rec_function_data) node
and rec_function_data =
| RecFunc of var * type_farg list * pattern list * annot option * expr

and handler = (Utils.Position.t, handler_data) node
and handler_data =
| HReturn of pattern * expr
| HOp     of op * type_farg list * pattern list * var * expr

and clause = (Utils.Position.t, clause_data) node
and clause_data =
| Clause of pattern * expr

and field_def = (Utils.Position.t, field_def_data) node
and field_def_data =
| FieldDef  of field * expr
| FieldDefM of Utils.Position.t * field * expr

module TypeFArg = struct
  type t = type_farg

  let var arg =
    match arg.data with
    | TFA_Var x -> x
    | TFA_Annot(x, _) -> x
end

module Expr = struct
  type t = expr

  let rec is_value e =
    match e.data with
    | EVar _ | EOp _ | ECtor _ | EModule _ | ENum _ | EChar _ | EString _
    | EFun _ | EHandler _ | EExtern _ -> true
    | ESelect(e, _) | ESelectF(e, _, _) | ETypeAlias(_, _, _, e) | EAnnot(e, _)
    | EPragmaFlag(_, e) | EPragmaVal(_, _, e) | EPragmaType(_, _, e) ->
      is_value e
    | ERecord fdefs ->
      List.for_all (fun { data = (FieldDef(_, e) | FieldDefM(_, _, e)) } ->
        is_value e) fdefs
    | EList es -> List.for_all is_value es
    | EApp(e1, e2) -> is_ctor_app e1 && is_value e2
    | ELet _ | ELetPat _ | ELetRec _ | EUIf _ | EIf _ | EHandle _
    | EHandleWith _ | EMatch _ | ETypedef _ | EAbsType _
    | EReplExpr _ | ERepl _ -> false

  and is_ctor_app e =
    match e.data with
    | ECtor _ -> true
    | EApp(e1, e2) -> is_ctor_app e1 && is_value e2
    | _ -> false
end

let flow_node = Flow.Node.create
  ~cmd_flag:  "-flat"
  ~cmd_descr: " Transform to Flat intermediate language."
  "Flat"
