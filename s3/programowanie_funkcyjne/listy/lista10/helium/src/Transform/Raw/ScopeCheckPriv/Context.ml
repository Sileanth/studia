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

let rec plug1 def e =
  let make data =
    { meta = Utils.Position.join def.meta e.meta
    ; data = data
    }
  in
  match def.data with
  | DefLet(x, ltps, ps, ann, e1) -> make (ELet(x, ltps, ps, ann, e1, e))
  | DefLetPat(p, ann, e1)        -> make (ELetPat(p, ann, e1, e))
  | DefLetRec rfs                -> make (ELetRec(rfs, e))
  | DefHandle hs                 -> make (EHandle(e, hs))
  | DefHandleWith he             -> make (EHandleWith(e, he))
  | DefTypedef tds               -> make (ETypedef(tds, e))
  | DefTypeAlias(x, args, tp)    -> make (ETypeAlias(x, args, tp, e))
  | DefAbsType(x, kind)          -> make (EAbsType(x, kind, e))
  | DefPragmaFlag flag           -> make (EPragmaFlag(flag, e))
  | DefPragmaVal(flag, x)        -> make (EPragmaVal(flag, x, e))
  | DefPragmaType(flag, x)       -> make (EPragmaType(flag, x, e))

and plug ctx e =
  match ctx with
  | []         -> e
  | def :: ctx -> plug1 def (plug ctx e)
