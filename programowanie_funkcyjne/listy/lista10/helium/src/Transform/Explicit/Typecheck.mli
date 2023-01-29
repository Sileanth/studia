(* This transformation makes two things at once:
1. check well-typedness of the program
2. puts types of each expressions into seal metadata
*)

val check_expr : Lang.Explicit.expr -> Lang.Explicit.expr
