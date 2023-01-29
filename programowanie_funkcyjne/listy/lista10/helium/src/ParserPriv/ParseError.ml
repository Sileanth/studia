
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

let cannot_open_file path msg =
  Parse_error (CannotOpenFile(path, msg))

let cannot_read_file path msg =
  Parse_error (CannotReadFile(path, msg))

let unexpected_char pos c =
  Parse_error (UnexpectedChar(pos, c))

let eof_in_comment pos =
  Parse_error (EofInComment pos)

let eof_in_char pos =
  Parse_error (EofInChar pos)

let eof_in_string pos =
  Parse_error (EofInString pos)

let invalid_number pos tok =
  Parse_error (InvalidNumber(pos, tok))

let invalid_escape pos tok =
  Parse_error (InvalidEscape(pos, tok))

let invalid_string pos tok err =
  Parse_error (InvalidString(pos, tok, err))

let unexpected_token pos tok =
  Parse_error (UnexpectedToken(pos, tok))
