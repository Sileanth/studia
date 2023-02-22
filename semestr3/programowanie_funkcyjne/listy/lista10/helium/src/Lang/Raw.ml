
type kind =
| KType
| KEffect
| KArrow of kind * kind

type ident = (Utils.Position.t, string) Node.node

type mpath = ident list

type type_expr = (Utils.Position.t, type_expr_data) Node.node
and type_expr_data =
| TPlaceholder
| TEffPure
| TParen        of type_expr
| TVar          of string
| TConst        of mpath * ident
| TArrowPure    of type_expr * type_expr
| TArrowEff     of type_expr * type_expr * type_expr
| TEffRow       of type_expr
| TEffCons      of type_expr * type_expr
| TApp          of type_expr * type_expr

type annot =
| Annot    of type_expr
| AnnotEff of type_expr * type_expr

type ctor_name =
| CNName of ident
| CNOper of ident

type type_farg = (Utils.Position.t, type_farg_data) Node.node
and type_farg_data =
| TFA_Var of string

type op_decl = (Utils.Position.t, op_decl_data) Node.node
and op_decl_data =
| OpDecl of ident * type_farg list * type_expr list * type_expr

type adt_ctor = (Utils.Position.t, adt_ctor_data) Node.node
and adt_ctor_data =
| ADTCtor of ctor_name * type_farg list * type_expr list

type field_decl = (Utils.Position.t, field_decl_data) Node.node
and field_decl_data =
| FieldDecl of ident * type_farg list * type_expr

type typedef = (Utils.Position.t, typedef_data) Node.node
and typedef_data =
| TDEffect of ident * type_farg list * op_decl list
| TDData   of ident * type_farg list * adt_ctor list
| TDRecord of ident * type_farg list * field_decl list

type module_type = (Utils.Position.t, module_type_data) Node.node
and module_type_data =
| MTStruct  of decl list
| MTFunctor of module_arg list * module_type

and module_arg = (Utils.Position.t, module_arg_data) Node.node
and module_arg_data =
| MArg of ident * module_type

and decl = (Utils.Position.t, decl_data) Node.node
and decl_data =
| DeclMType    of ident
| DeclMEffect  of ident
| DeclType     of ident * kind
| DeclEffect   of ident * kind
| DeclVal      of ident * type_farg list * type_expr
| DeclOper     of ident * type_farg list * type_expr
| DeclUOper    of ident * type_farg list * type_expr
| DeclOpen     of mpath * ident
| DeclOpenType of mpath * ident

type pattern = (Utils.Position.t, pattern_data) Node.node
and pattern_data =
| PWildcard
| PParen  of pattern
| PVar    of string
| PCtor   of mpath * ctor_name * pattern list
| PBOp    of pattern * ident * pattern
| POper   of ident
| PUOper  of ident
| PList   of pattern list
| PRecord of field_pattern list
| PAnnot  of pattern * type_expr
| PTypes  of type_farg list

and field_pattern = (Utils.Position.t, field_pattern_data) Node.node
and field_pattern_data =
| FieldPat of mpath * ident * pattern

type expr = (Utils.Position.t, expr_data) Node.node
and expr_data =
| EParen      of expr
| EVar        of mpath * ident
| ECtor       of mpath * ident
| EOper       of mpath * ident
| EUOper      of mpath * ident
| ENum        of int
| EChar       of char
| EString     of string
| EList       of expr list
| EFun        of pattern list * annot option * expr
| EApp        of expr * expr
| EBOp        of expr * ident * expr
| EUOp        of ident * expr
| EUIf        of expr * expr
| EIf         of expr * expr * expr
| EHandle     of expr * handler list
| EHandleWith of expr * expr
| EHandler    of handler list
| EMatch      of expr * clause list
| ESelect     of expr * mpath * ident
| ERecord     of field_def list
| EDefs       of def list * expr
| EAnnot      of expr * annot
| EExtern     of ident * type_farg list * type_expr

and rec_function = (Utils.Position.t, rec_function_data) Node.node
and rec_function_data =
| RecFunc of ident * pattern list * annot option * expr

and handler = (Utils.Position.t, handler_data) Node.node
and handler_data =
| HReturn of pattern * expr
| HOp     of mpath * ident * pattern list * ident option * expr

and clause = (Utils.Position.t, clause_data) Node.node
and clause_data =
| Clause of pattern * expr

and field_def = (Utils.Position.t, field_def_data) Node.node
and field_def_data =
| FieldDef of mpath * ident * expr

and def = (Utils.Position.t, def_data) Node.node
and def_data =
| DefLet         of pattern list * annot option * expr
| DefLetRec      of rec_function list
| DefHandle      of handler list
| DefHandleWith  of expr
| DefTypedef     of typedef
| DefTypedefRec  of typedef list
| DefTypeAlias   of ident * type_farg list * type_expr
| DefAbsMType    of ident
| DefAbsMEffect  of ident
| DefAbsType     of ident * kind
| DefAbsEffect   of ident * kind
| DefModule      of ident * module_arg list * module_type option * module_expr
| DefOpen        of module_expr
| DefOpenType    of mpath * ident
| DefInclude     of module_expr
| DefIncludeType of mpath * ident
| DefPragmaFlag  of string
| DefPragmaVal   of string * mpath * ident
| DefPragmaType  of string * mpath * ident

and module_expr = (Utils.Position.t, module_expr_data) Node.node
and module_expr_data =
| ModConst   of mpath * ident
| ModStruct  of def list
| ModAnnot   of module_expr * module_type
| ModFunctor of module_arg list * module_type option * module_expr
| ModApp     of module_expr * module_expr

type file      = (Utils.Position.t, def list) Node.node
type intf_file = (Utils.Position.t, decl list) Node.node

type repl_cmd = (Utils.Position.t, repl_cmd_data) Node.node
and repl_cmd_data =
| ReplExit
| ReplExpr of expr
| ReplDef  of def

let binary_op_var op = Printf.sprintf "(%s)"  op.Node.data
let unary_op_var  op = Printf.sprintf "(%s_)" op.Node.data

let flow_node = Flow.Node.create "Raw"
