
val current_file_path     : unit -> string
val set_current_file_path : string -> unit

val set_args : string list -> unit
val get_args : unit -> string list

val source_dirs : unit -> string list
val lib_dirs    : unit -> string list

val add_source_dir : string -> unit
val set_lib_dirs   : string list -> unit

val prelude_module   : unit -> string option
val set_prelude      : string option -> unit

val required_libs : unit -> string list
val require_lib   : string -> unit

val add_auto_open    : string -> unit
val auto_open        : unit -> string list

type auto_lift_policy =
| ALP_No
| ALP_Normal

val simpl_schemes : bool ref
val pretty_rows   : bool ref
val auto_lift     : auto_lift_policy ref

val close_scheme_function :
  ?abs_types: Lang.Unif.tconst list ->
  Lang.Unif.tvar list -> Lang.Unif.typ -> Lang.Unif.scheme

type repl =
  { repl        : 'a. Utils.Seal.t -> (unit -> 'a) -> 'a
  ; print_value : Utils.Seal.t -> string -> unit
  }

val set_repl : repl -> unit
val repl : Utils.Seal.t -> (unit -> 'a) -> 'a
val repl_print_value : Utils.Seal.t -> string -> unit
