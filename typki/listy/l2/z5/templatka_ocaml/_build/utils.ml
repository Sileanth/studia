(** Utility functions, mostly error reporting *)

(** Exception that aborts the interpreter *)
exception Fatal_error

(** Pretty-printer of locations *)
let string_of_pp (start_p : Lexing.position) (end_p : Lexing.position) =
  if end_p.pos_cnum - start_p.pos_cnum <= 1 then
    Printf.sprintf "%s:%d:%d"
      start_p.pos_fname
      start_p.pos_lnum
      (start_p.pos_cnum - start_p.pos_bol + 1)
  else
    Printf.sprintf "%s:%d:%d-%d"
      start_p.pos_fname
      start_p.pos_lnum
      (start_p.pos_cnum - start_p.pos_bol + 1)
      (end_p.pos_cnum - start_p.pos_bol)

(** report an error not related to any location. *)
let report_error_no_pos fmt =
  Printf.kfprintf
    (fun _ -> raise Fatal_error)
    stderr
    ("error: " ^^ fmt ^^ "\n")

(** report an error related to a location between given positions. *)
let report_error_pp start_p end_p fmt =
  Printf.kfprintf
    (fun _ -> raise Fatal_error)
    stderr
    ("%s: error: " ^^ fmt ^^ "\n")
    (string_of_pp start_p end_p)

(** report an error related to given AST node, and raise Fatal_error *)
let report_error (node : 'a Ast.node) fmt =
  report_error_pp node.start_pos node.end_pos fmt
