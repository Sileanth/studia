open ParserPriv

include ParseError

let with_in_channel fname func =
  match open_in fname with
  | chan ->
    begin match func chan with
    | result -> close_in_noerr chan; result
    | exception Sys_error msg ->
      close_in_noerr chan;
      raise (ParseError.cannot_read_file fname msg)
    | exception ex ->
      close_in_noerr chan;
      raise ex
    end
  | exception Sys_error msg ->
    raise (ParseError.cannot_open_file fname msg)

let run_parser start lexbuf =
  try start Lexer.token lexbuf with
  | Parsing.Parse_error ->
    raise (ParseError.unexpected_token
      (Utils.Position.of_pp
        lexbuf.Lexing.lex_start_p
        lexbuf.Lexing.lex_curr_p)
      (Lexing.lexeme lexbuf))

let parse fname start =
  with_in_channel fname (fun chan ->
    let lexbuf = Lexing.from_channel chan in
    lexbuf.Lexing.lex_curr_p <-
      { lexbuf.Lexing.lex_curr_p with
        Lexing.pos_fname = fname
      };
    run_parser start lexbuf
  )

let parse_file fname =
  parse fname YaccParser.file

let parse_intf_file fname =
  parse fname YaccParser.intf_file

let parse_repl_cmd chan =
  let lexbuf = Lexing.from_channel chan in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with
      Lexing.pos_fname = "repl"
    };
  run_parser YaccParser.repl_cmd lexbuf

let _ =
  Flow.register_parser
    ~extension: ".he"
    ~source:    "Helium source file"
    ~target:    Lang.Raw.flow_node
    parse_file
