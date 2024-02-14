(** OCamllex generated lexer *)

{

let report_error_lex (lexbuf : Lexing.lexbuf) fmt =
  Utils.report_error_pp lexbuf.lex_start_p lexbuf.lex_curr_p fmt

let kw_map =
  let open YaccParser in
  [ "absurd", KW_ABSURD
  ; "case",   KW_CASE
  ; "else",   KW_ELSE
  ; "false",  KW_FALSE
  ; "fix",    KW_FIX
  ; "fn",     KW_FN
  ; "fst",    KW_FST
  ; "if",     KW_IF
  ; "in",     KW_IN
  ; "inl",    KW_INL
  ; "inr",    KW_INR
  ; "let",    KW_LET
  ; "of",     KW_OF
  ; "rec",    KW_REC
  ; "snd",    KW_SND
  ; "then",   KW_THEN
  ; "true",   KW_TRUE
  ] |> List.to_seq |> Hashtbl.of_seq

let tokenize_ident str =
  match Hashtbl.find_opt kw_map str with
  | Some tok -> tok
  | None     -> YaccParser.ID str

let tokenize_number lexbuf str =
  match int_of_string_opt str with
  | Some n -> YaccParser.NUM n
  | None   ->
    report_error_lex lexbuf
      "invalid numeric literal"
}

let whitespace = ['\011'-'\r' '\t' ' ']
let digit      = ['0'-'9']
let var_start  = ['a'-'z' 'A'-'Z' '_']
let var_char   = var_start | digit | '\''

rule token = parse
    whitespace+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "(*" { block_comment 1 lexbuf }
  | "//" { skip_line lexbuf; token lexbuf }
  | "("  { YaccParser.BR_OPN    }
  | ")"  { YaccParser.BR_CLS    }
  | "=>" { YaccParser.ARROW2    }
  | "|"  { YaccParser.BAR       }
  | ","  { YaccParser.COMMA     }
  | "="  { YaccParser.EQ        }
  | ";"  { YaccParser.SEMICOLON }
  | var_start var_char* as x { tokenize_ident  x }
  | digit var_char*     as x { tokenize_number lexbuf x }
  | eof    { YaccParser.EOF }
  | _ as x {
      report_error_lex lexbuf
        "invalid character in input ('%s')"
        (Char.escaped x)
    }

and block_comment depth = parse
    '\n' { Lexing.new_line lexbuf; block_comment depth lexbuf }
  | "(*" { block_comment (depth+1) lexbuf }
  | "*)" {
      if depth = 1 then token lexbuf
      else block_comment (depth-1) lexbuf
    }
  | "//" { skip_line lexbuf; block_comment depth lexbuf }
  | eof {
      report_error_lex lexbuf
        "unexpected end of file inside a block comment"
    }
  | _ { block_comment depth lexbuf }

and skip_line = parse
    '\n' { Lexing.new_line lexbuf }
  | eof  { () }
  | _    { skip_line lexbuf }
