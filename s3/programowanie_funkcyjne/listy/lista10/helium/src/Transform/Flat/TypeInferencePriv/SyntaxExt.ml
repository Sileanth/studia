module S = Lang.Flat
module T = Lang.Unif

type typedef =
| TDEffect of T.tconst * T.tconst list * (S.op * T.op_decl) list
| TDData   of T.tconst * T.tconst list * (S.ctor * T.adt_ctor) list
| TDRecord of T.tconst * T.tconst list * (S.field * T.field_decl) list

let get_typedef = function
  | TDEffect(l, args, ops)  -> T.TDEffect(l, args, List.map snd ops)
  | TDData(l, args, ctors)  -> T.TDData(l, args, List.map snd ctors)
  | TDRecord(l, args, flds) -> T.TDRecord(l, args, List.map snd flds)

let get_typedefs = List.map get_typedef
