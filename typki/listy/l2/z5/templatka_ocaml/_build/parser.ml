(** The main module of a parser. It provides convenient wrapper for
  * a yacc-generated parser *)

(** Open input channel, pass it to given function, and close the channel
  * at the end. *)
let with_channel fname func =
  match open_in fname with
  | chan ->
    begin match func chan with
    | result -> close_in_noerr chan; result
    | exception Sys_error msg ->
      close_in_noerr chan;
      Utils.report_error_no_pos
        "cannot read file %s: %s" fname msg
    | exception ex ->
      close_in_noerr chan;
      raise ex
    end
  | exception Sys_error msg ->
    Utils.report_error_no_pos
      "cannot open file %s: %s" fname msg

(** Parse give file *)
let parse_file fname =
  with_channel fname (fun chan ->
    let lexbuf = Lexing.from_channel chan in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
    try YaccParser.file Lexer.token lexbuf with
    | Parsing.Parse_error ->
      Utils.report_error_pp
        lexbuf.Lexing.lex_start_p
        lexbuf.Lexing.lex_curr_p
        "unexpected token `%s'"
        (Lexing.lexeme lexbuf)
  )
