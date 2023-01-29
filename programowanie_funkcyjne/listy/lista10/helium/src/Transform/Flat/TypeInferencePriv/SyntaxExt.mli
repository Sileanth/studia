open Lang.Unif

type typedef =
| TDEffect of tconst * tconst list * (Lang.Flat.op * op_decl) list
| TDData   of tconst * tconst list * (Lang.Flat.ctor * adt_ctor) list
| TDRecord of tconst * tconst list * (Lang.Flat.field * field_decl) list

val get_typedef  : typedef -> Lang.Unif.typedef
val get_typedefs : typedef list -> Lang.Unif.typedef list
