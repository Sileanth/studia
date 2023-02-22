
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

let non_exhaustive_handler meta name =
  Error(NonExhaustiveHandler(Utils.Seal.find Utils.Position.key meta, name))

let non_exhaustive_match meta exval =
  Error(NonExhaustiveMatch(Utils.Seal.find Utils.Position.key meta, exval))

let non_exhaustive_record meta fields =
  Error(NonExhaustiveRecord(Utils.Seal.find Utils.Position.key meta, fields))

let non_empty_type pos typ =
  Error(NonEmptyType(pos, typ))
