open Lang.Node
open Lang.Flat

type def = (Utils.Position.t, def_data) node
and def_data =
| DefLet        of var * type_farg list * pattern list * annot option * expr
| DefLetPat     of pattern * annot option * expr
| DefLetRec     of rec_function list
| DefHandle     of handler list
| DefHandleWith of expr
| DefTypedef    of typedef list
| DefTypeAlias  of tconst * type_farg list * type_expr
| DefAbsType    of tconst * kind
| DefPragmaFlag of string
| DefPragmaVal  of string * var
| DefPragmaType of string * tconst

val plug1 : def -> expr -> expr
val plug  : def list -> expr -> expr
