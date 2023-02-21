
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

val non_exhaustive_handler : Utils.Seal.t -> string -> exn
val non_exhaustive_match   : Utils.Seal.t -> exval -> exn
val non_exhaustive_record  : Utils.Seal.t -> string list -> exn
val non_empty_type : Utils.Position.t -> Lang.Unif.typ -> exn
