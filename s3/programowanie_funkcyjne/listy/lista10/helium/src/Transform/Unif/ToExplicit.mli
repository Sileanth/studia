
type exval =
| EVAny
| EVCtor   of string * exval list
| EVRecord of (string * exval) list

type error =
| NonExhaustiveHandler of Utils.Position.t * string
| NonExhaustiveMatch   of Utils.Position.t * exval
| NonExhaustiveRecord  of Utils.Position.t * string list
| NonEmptyType         of Utils.Position.t * Lang.Unif.typ

exception Error of error

val tr_expr : Lang.Unif.expr -> Lang.Explicit.expr
