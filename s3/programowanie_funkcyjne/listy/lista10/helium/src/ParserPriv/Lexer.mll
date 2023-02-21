{

let op_map = Hashtbl.create 32
let _ =
  Hashtbl.add op_map "->" YaccParser.ARROW;
  Hashtbl.add op_map "=>" YaccParser.ARROW2;
  Hashtbl.add op_map "|"  YaccParser.BAR;
  Hashtbl.add op_map ":"  YaccParser.COLON;
  Hashtbl.add op_map ","  YaccParser.COMMA;
  Hashtbl.add op_map "."  YaccParser.DOT;
  Hashtbl.add op_map "="  YaccParser.EQ;
  Hashtbl.add op_map ";"  YaccParser.SEMICOLON;
  Hashtbl.add op_map ";;" YaccParser.SEMICOLON2;
  Hashtbl.add op_map "/"  YaccParser.SLASH;
  ()

let tokenize_oper str =
  try Hashtbl.find op_map str with
  | Not_found ->
    let long = String.length str >= 2 in
    begin match str.[0] with
    | ';'                                           -> YaccParser.OP_0 str
    | '<' when (long && str.[1] = '-')              -> YaccParser.OP_20 str
    | ':' when (long && str.[1] = '=')              -> YaccParser.OP_20 str
    | ','                                           -> YaccParser.OP_30 str
    | '|' when (long && str.[1] = '|')              -> YaccParser.OP_40 str
    | '&' when (long && str.[1] = '&')              -> YaccParser.OP_50 str
    | '=' | '<' | '>' | '|' | '&' | '$'	| '#' | '?' -> YaccParser.OP_60 str
    | '@' | ':' | '^' | '.'                         -> YaccParser.OP_70 str
    | '+' | '-' | '~'                               -> YaccParser.OP_80 str
    | '*' when (long && str.[1] = '*')              -> YaccParser.OP_100 str
    | '*' | '/' | '%'                               -> YaccParser.OP_90  str
    | '!'                                           -> YaccParser.OP_250 str
    | _ -> assert false
    end

let kw_map = Hashtbl.create 32
let _ =
  Hashtbl.add kw_map "and"     YaccParser.KW_AND;
  Hashtbl.add kw_map "begin"   YaccParser.KW_BEGIN;
  Hashtbl.add kw_map "data"    YaccParser.KW_DATA;
  Hashtbl.add kw_map "effect"  YaccParser.KW_EFFECT;
  Hashtbl.add kw_map "effrow"  YaccParser.KW_EFFROW;
  Hashtbl.add kw_map "elif"    YaccParser.KW_ELIF;
  Hashtbl.add kw_map "else"    YaccParser.KW_ELSE;
  Hashtbl.add kw_map "end"     YaccParser.KW_END;
  Hashtbl.add kw_map "extern"  YaccParser.KW_EXTERN;
  Hashtbl.add kw_map "fn"      YaccParser.KW_FN;
  Hashtbl.add kw_map "functor" YaccParser.KW_FUNCTOR;
  Hashtbl.add kw_map "handle"  YaccParser.KW_HANDLE;
  Hashtbl.add kw_map "if"      YaccParser.KW_IF;
  Hashtbl.add kw_map "in"      YaccParser.KW_IN;
  Hashtbl.add kw_map "include" YaccParser.KW_INCLUDE;
  Hashtbl.add kw_map "let"     YaccParser.KW_LET;
  Hashtbl.add kw_map "match"   YaccParser.KW_MATCH;
  Hashtbl.add kw_map "module"  YaccParser.KW_MODULE;
  Hashtbl.add kw_map "of"      YaccParser.KW_OF;
  Hashtbl.add kw_map "open"    YaccParser.KW_OPEN;
  Hashtbl.add kw_map "pragma"  YaccParser.KW_PRAGMA;
  Hashtbl.add kw_map "rec"     YaccParser.KW_REC;
  Hashtbl.add kw_map "return"  YaccParser.KW_RETURN;
  Hashtbl.add kw_map "sig"     YaccParser.KW_SIG;
  Hashtbl.add kw_map "struct"  YaccParser.KW_STRUCT;
  Hashtbl.add kw_map "then"    YaccParser.KW_THEN;
  Hashtbl.add kw_map "this"    YaccParser.KW_THIS;
  Hashtbl.add kw_map "type"    YaccParser.KW_TYPE;
  Hashtbl.add kw_map "val"     YaccParser.KW_VAL;
  Hashtbl.add kw_map "with"    YaccParser.KW_WITH;
  Hashtbl.add kw_map "_"       YaccParser.UNDERSCORE;
  ()

let tokenize_ident str =
  try Hashtbl.find kw_map str with
  | Not_found -> YaccParser.LID str

let dec_num = Str.regexp "^[1-9][0-9]*$"
let bin_num = Str.regexp "^0[bB][01]+$"
let oct_num = Str.regexp "^0[oO][0-7]+$"
let coc_num = Str.regexp "^0[0-7]*$"
let hex_num = Str.regexp "^0[xX][0-9a-fA-F]+$"

let digit_value d =
  if d >= '0' && d <= '9' then Char.code d - Char.code '0'
  else if d >= 'a' && d <= 'z' then Char.code d - Char.code 'a' + 10
  else if d >= 'A' && d <= 'Z' then Char.code d - Char.code 'A' + 10
  else assert false

let rec int_of_string' base acc pos str =
  if pos < String.length str then
    int_of_string' base (acc*base + digit_value str.[pos]) (pos+1) str
  else acc

let tokenize_number lexbuf str =
  if Str.string_match dec_num str 0 then
    YaccParser.NUM (int_of_string str)
  else if Str.string_match bin_num str 0 then
    YaccParser.NUM (int_of_string' 2 0 2 str)
  else if Str.string_match oct_num str 0 then
    YaccParser.NUM (int_of_string' 8 0 2 str)
  else if Str.string_match coc_num str 0 then
    YaccParser.NUM (int_of_string' 8 0 1 str)
  else if Str.string_match hex_num str 0 then
    YaccParser.NUM (int_of_string' 16 0 2 str)
  else
    raise (ParseError.invalid_number
      (Utils.Position.of_pp
        lexbuf.Lexing.lex_start_p
        lexbuf.Lexing.lex_curr_p)
      str)

let unescape_char lexbuf str =
  match str.[1] with
  | 'O' | 'o' -> YaccParser.CHR (Char.chr (int_of_string' 8 0 2 str))
  | 'X' | 'x' -> YaccParser.CHR (Char.chr (int_of_string' 16 0 2 str))
  | '\\' -> YaccParser.CHR '\\'
  | '\'' -> YaccParser.CHR '\''
  | 'n'  -> YaccParser.CHR '\n'
  | 't'  -> YaccParser.CHR '\t'
  | 'b'  -> YaccParser.CHR '\b'
  | 'r'  -> YaccParser.CHR '\r'
  | ' '  -> YaccParser.CHR ' '
  | c    -> if c >= '0' && c <='9' && int_of_string' 10 0 1 str < 256
            then YaccParser.CHR (Char.chr (int_of_string' 10 0 1 str))
            else
              raise (ParseError.invalid_escape
                (Utils.Position.of_pp
                  lexbuf.Lexing.lex_start_p
                  lexbuf.Lexing.lex_curr_p)
                str)

let tokenize_string lexbuf str =
  let str = String.concat "\n" (List.rev str) in
  try YaccParser.STR (Scanf.unescaped str) with
  | Scanf.Scan_failure s ->
    let errstr = String.sub s 7 (String.length s - 7) in
    raise (ParseError.invalid_string
      (Utils.Position.of_pp
        lexbuf.Lexing.lex_start_p
        lexbuf.Lexing.lex_curr_p)
      str errstr)

}


let whitespace = ['\011'-'\r' '\t' ' ']

let digit     = ['0'-'9']
let lid_start = ['a'-'z' '_']
let uid_start = ['A'-'Z']
let var_char  = ['a'-'z' 'A'-'Z' '_' '\''] | digit

let hexdigit  = digit | ['A'-'F'] | ['a'-'f']
let escapeNum = (digit digit digit)
              | (['o' 'O'] ['0'-'3'] ['0'-'7'] ['0'-'7'])
              | (['x' 'X'] hexdigit hexdigit)
let escape    = '\\'
  ('"' | '\\' | '\'' | 'n' | 't' | 'b' | 'r' | ' ' | escapeNum)
let string    = [^ '\n' '"' '\\'] | escape

let op_char =
  [';' ',' '=' '<' '>' '|' '&' '$' '#' '?' '!'
   '@' ':' '^' '.' '+' '-' '~' '*' '/' '%']

rule token = parse
    whitespace+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | "(*" { block_comment 1 lexbuf }
  | "//" { skip_line lexbuf; token lexbuf }
  | '('  { YaccParser.BR_OPN    }
  | ')'  { YaccParser.BR_CLS    }
  | '['  { YaccParser.SBR_OPN   }
  | ']'  { YaccParser.SBR_CLS   }
  | '{'  { YaccParser.CBR_OPN   }
  | '}'  { YaccParser.CBR_CLS   }
  | '\'' { char_lit lexbuf }
  | '"' (string* as s) { string_lit [s] lexbuf }
  | op_char+            as x { tokenize_oper  x }
  | lid_start var_char* as x { tokenize_ident x }
  | uid_start var_char* as x { YaccParser.UID x }
  | digit var_char*     as x { tokenize_number lexbuf x }
  | eof { YaccParser.EOF }
  | _ as x {
      raise (ParseError.unexpected_char
        (Utils.Position.of_lexing lexbuf.Lexing.lex_curr_p)
        x)
    }

and char_lit = parse
    "\n'" { Lexing.new_line lexbuf; YaccParser.CHR '\n' }
  | (escape as s) "'" { unescape_char lexbuf s }
  | ([^ '\\' '\''] as c) "'" { YaccParser.CHR c }
  | eof  {
      raise (ParseError.eof_in_char
        (Utils.Position.of_lexing lexbuf.Lexing.lex_curr_p))
    }
  | _ as x {
      raise (ParseError.unexpected_char
        (Utils.Position.of_lexing lexbuf.Lexing.lex_curr_p)
        x)
    }

and string_lit ss = parse
    '\n' (string* as s) { Lexing.new_line lexbuf; string_lit (s :: ss) lexbuf }
  | '"'  { tokenize_string lexbuf ss }
  | eof  {
      raise (ParseError.eof_in_string
        (Utils.Position.of_lexing lexbuf.Lexing.lex_curr_p))
    }


and block_comment depth = parse
    '\n' { Lexing.new_line lexbuf; block_comment depth lexbuf }
  | "(*" { block_comment (depth+1) lexbuf }
  | "*)" {
      if depth = 1 then token lexbuf
      else block_comment (depth-1) lexbuf
    }
  | "//" { skip_line lexbuf; block_comment depth lexbuf }
  | eof  {
      raise (ParseError.eof_in_comment
        (Utils.Position.of_lexing lexbuf.Lexing.lex_curr_p))
    }
  | _ { block_comment depth lexbuf }

and skip_line = parse
    '\n' { Lexing.new_line lexbuf }
  | eof  { () }
  | _    { skip_line lexbuf }
