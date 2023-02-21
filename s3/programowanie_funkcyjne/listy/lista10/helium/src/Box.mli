
type t

val word : string -> t

val ws : t -> t
val text_indent : int -> t

val indent : int -> t -> t
val prefix : t -> t -> t
val suffix : t -> t -> t

val paren      : ?opn:t -> ?cls:t -> t -> t
val brackets   : t -> t
val braces     : t -> t
val prec_paren : int -> int -> t -> t

val box  : t list -> t
val tbox : t list -> t

val textl  : string -> t list
val textfl : ('a, unit, string, t list) format4 -> 'a

val print_stdout : t -> unit
val print_stderr : t -> unit
val print_stdout_no_nl : t -> unit
val print_stderr_no_nl : t -> unit
