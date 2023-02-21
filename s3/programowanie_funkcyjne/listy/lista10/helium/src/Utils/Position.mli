
type t

val key : t Seal.key

val nowhere : t

val of_pp     : Lexing.position -> Lexing.position -> t
val of_lexing : Lexing.position -> t

val join : t -> t -> t

val to_string : t -> string
