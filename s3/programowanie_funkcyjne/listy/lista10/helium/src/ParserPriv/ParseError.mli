
type file_path = string
type message = string

             
type parse_error =
| CannotOpenFile  of file_path * message
| CannotReadFile  of file_path * message
| UnexpectedChar  of Utils.Position.t * char
| EofInComment    of Utils.Position.t
| EofInChar       of Utils.Position.t
| EofInString     of Utils.Position.t
| InvalidNumber   of Utils.Position.t * string
| InvalidEscape   of Utils.Position.t * string
| InvalidString   of Utils.Position.t * string * string
| UnexpectedToken of Utils.Position.t * string

exception Parse_error of parse_error

val cannot_open_file : file_path -> message -> exn
val cannot_read_file : file_path -> message -> exn

val unexpected_char : Utils.Position.t -> char -> exn

val eof_in_comment  : Utils.Position.t -> exn
val eof_in_char     : Utils.Position.t -> exn
val eof_in_string   : Utils.Position.t -> exn

val invalid_number : Utils.Position.t -> string -> exn
val invalid_escape : Utils.Position.t -> string -> exn
val invalid_string : Utils.Position.t -> string -> string -> exn

val unexpected_token : Utils.Position.t -> string -> exn
