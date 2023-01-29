
type t =
  { pos_fname      : string
  ; pos_start_line : int
  ; pos_start_col  : int
  ; pos_end_line   : int
  ; pos_end_col    : int
  }

let key = Seal.gen_key ()

let nowhere =
  { pos_fname      = "<nowhere>"
  ; pos_start_line = 0
  ; pos_start_col  = 0
  ; pos_end_line   = 0
  ; pos_end_col    = 0
  }

let of_pp p1 p2 =
  { pos_fname      = p1.Lexing.pos_fname
  ; pos_start_line = p1.Lexing.pos_lnum
  ; pos_start_col  = 1 + p1.Lexing.pos_cnum - p1.Lexing.pos_bol
  ; pos_end_line   = p2.Lexing.pos_lnum
  ; pos_end_col    = p2.Lexing.pos_cnum - p2.Lexing.pos_bol
  }

let of_lexing p =
  { pos_fname      = p.Lexing.pos_fname
  ; pos_start_line = p.Lexing.pos_lnum
  ; pos_start_col  = 1 + p.Lexing.pos_cnum - p.Lexing.pos_bol
  ; pos_end_line   = p.Lexing.pos_lnum
  ; pos_end_col    = 1 + p.Lexing.pos_cnum - p.Lexing.pos_bol
  }

let join p1 p2 =
  if p1.pos_fname = p2.pos_fname then
    { p1 with
      pos_end_line = p2.pos_end_line
    ; pos_end_col  = p2.pos_end_col
    }
  else p1

let to_string pos =
  if pos.pos_start_line <> pos.pos_end_line then
    Printf.sprintf "%s:(%d:%d)-(%d:%d)"
      pos.pos_fname
      pos.pos_start_line
      pos.pos_start_col
      pos.pos_end_line
      pos.pos_end_col
  else if pos.pos_start_col <> pos.pos_end_col then
    Printf.sprintf "%s:%d:%d-%d"
      pos.pos_fname
      pos.pos_start_line
      pos.pos_start_col
      pos.pos_end_col
  else
    Printf.sprintf "%s:%d:%d"
      pos.pos_fname
      pos.pos_start_line
      pos.pos_start_col
