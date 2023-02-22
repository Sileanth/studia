
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

val parse_file      : file_path -> Lang.Raw.file
val parse_intf_file : file_path -> Lang.Raw.intf_file
val parse_repl_cmd  : in_channel -> Lang.Raw.repl_cmd
