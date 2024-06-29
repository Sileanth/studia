type token =
  | ID of (
# 2 "yaccParser.mly"
       string
# 6 "yaccParser.mli"
)
  | NUM of (
# 3 "yaccParser.mly"
       int
# 11 "yaccParser.mli"
)
  | BR_OPN
  | BR_CLS
  | ARROW2
  | BAR
  | COMMA
  | EQ
  | SEMICOLON
  | KW_ABSURD
  | KW_CASE
  | KW_ELSE
  | KW_FALSE
  | KW_FIX
  | KW_FN
  | KW_FST
  | KW_IF
  | KW_IN
  | KW_INL
  | KW_INR
  | KW_LET
  | KW_OF
  | KW_REC
  | KW_SND
  | KW_THEN
  | KW_TRUE
  | EOF

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
