open Lang.Core

(** Regenerating all the variables in order to make all bound variables unique.
*)
val tr_expr : expr -> expr
