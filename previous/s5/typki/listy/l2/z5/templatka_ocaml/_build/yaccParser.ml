type token =
  | ID of (
# 2 "yaccParser.mly"
       string
# 6 "yaccParser.ml"
)
  | NUM of (
# 3 "yaccParser.mly"
       int
# 11 "yaccParser.ml"
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

open Parsing
let _ = parse_error;;
# 14 "yaccParser.mly"

open Ast

type def =
| DLet of var * expr

let make data =
  { data      = data;
    start_pos = Parsing.symbol_start_pos ();
    end_pos   = Parsing.symbol_end_pos ();
  }

let desugar_fn args body =
  let mk_lambda arg body =
    { data      = EFn(arg.data, body);
      start_pos = arg.start_pos;
      end_pos   = body.end_pos }
  in
  List.fold_right mk_lambda args body

let desugar_fix fn args body =
  match args with
  | [] -> assert false
  | arg :: args ->
    make (EFix(fn, arg.data, desugar_fn args body))

let desugar_let_fun fn args body =
  make (DLet(fn, make (desugar_fn args body).data))

let desugar_let_rec fn args body =
  make (DLet(fn, desugar_fix fn args body))

let desugar_def def rest =
  match def.data with
  | DLet(x, body) ->
    (* Let definitions are desugared the usual way: to a lambda applied
     * to an argument. This is not the best choice, as there is no reasonable
     * location that we can assign to a lambda. Having let-definitions in Ast
     * would be better, but requires extending the minimal language. *) 
    { data = EApp(
        { data      = EFn(x, rest);
          start_pos = def.start_pos;
          end_pos   = rest.end_pos
        }, body);
      start_pos = def.start_pos;
      end_pos   = rest.end_pos
    }

let desugar_defs defs rest =
  List.fold_right desugar_def defs rest

# 93 "yaccParser.ml"
let yytransl_const = [|
  259 (* BR_OPN *);
  260 (* BR_CLS *);
  261 (* ARROW2 *);
  262 (* BAR *);
  263 (* COMMA *);
  264 (* EQ *);
  265 (* SEMICOLON *);
  266 (* KW_ABSURD *);
  267 (* KW_CASE *);
  268 (* KW_ELSE *);
  269 (* KW_FALSE *);
  270 (* KW_FIX *);
  271 (* KW_FN *);
  272 (* KW_FST *);
  273 (* KW_IF *);
  274 (* KW_IN *);
  275 (* KW_INL *);
  276 (* KW_INR *);
  277 (* KW_LET *);
  278 (* KW_OF *);
  279 (* KW_REC *);
  280 (* KW_SND *);
  281 (* KW_THEN *);
  282 (* KW_TRUE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* NUM *);
    0|]

let yylhs = "\255\255\
\002\000\003\000\003\000\004\000\004\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\007\000\007\000\007\000\
\007\000\007\000\007\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\009\000\009\000\009\000\006\000\006\000\001\000\
\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\000\000\001\000\003\000\004\000\005\000\
\013\000\006\000\002\000\003\000\001\000\002\000\002\000\002\000\
\002\000\002\000\001\000\002\000\003\000\005\000\001\000\001\000\
\001\000\001\000\004\000\005\000\006\000\001\000\002\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\024\000\023\000\000\000\000\000\000\000\026\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\033\000\000\000\000\000\000\000\019\000\000\000\020\000\
\000\000\011\000\000\000\000\000\001\000\000\000\000\000\015\000\
\000\000\017\000\018\000\000\000\000\000\016\000\032\000\000\000\
\000\000\014\000\031\000\021\000\000\000\000\000\000\000\003\000\
\000\000\000\000\000\000\000\000\000\000\006\000\012\000\000\000\
\005\000\000\000\000\000\007\000\000\000\027\000\000\000\000\000\
\022\000\000\000\008\000\000\000\028\000\000\000\000\000\010\000\
\029\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000"

let yydgoto = "\002\000\
\018\000\030\000\031\000\058\000\019\000\020\000\021\000\022\000\
\023\000"

let yysindex = "\255\255\
\067\255\000\000\000\000\000\000\004\255\067\255\067\255\000\000\
\025\255\026\255\009\255\067\255\009\255\009\255\028\255\009\255\
\000\000\000\000\031\000\035\255\031\255\000\000\029\255\000\000\
\032\255\000\000\033\255\026\255\000\000\026\255\049\255\000\000\
\034\255\000\000\000\000\044\255\057\255\000\000\000\000\067\255\
\067\255\000\000\000\000\000\000\067\255\055\255\058\255\000\000\
\067\255\067\255\067\255\054\255\026\255\000\000\000\000\060\255\
\000\000\053\255\067\255\000\000\061\255\000\000\067\255\066\255\
\000\000\074\255\000\000\067\255\000\000\067\255\080\255\000\000\
\000\000\067\255\073\255\069\255\089\255\091\255\067\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\081\255\000\000\
\000\000\000\000\000\000\000\000\000\000\008\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\079\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\013\000\000\000\253\255\077\000\000\000\081\000\
\000\000"

let yytablesize = 282
let yytable = "\001\000\
\013\000\025\000\026\000\027\000\003\000\004\000\005\000\024\000\
\033\000\003\000\004\000\005\000\002\000\006\000\007\000\002\000\
\008\000\009\000\010\000\011\000\012\000\008\000\013\000\014\000\
\015\000\028\000\029\000\016\000\036\000\017\000\039\000\003\000\
\004\000\005\000\017\000\044\000\054\000\055\000\045\000\041\000\
\047\000\056\000\048\000\008\000\029\000\060\000\061\000\062\000\
\052\000\015\000\037\000\051\000\040\000\049\000\046\000\067\000\
\017\000\053\000\050\000\069\000\057\000\063\000\059\000\065\000\
\072\000\064\000\073\000\003\000\004\000\005\000\075\000\066\000\
\068\000\070\000\071\000\080\000\006\000\007\000\076\000\008\000\
\009\000\010\000\011\000\012\000\074\000\013\000\014\000\015\000\
\077\000\078\000\016\000\032\000\017\000\034\000\035\000\079\000\
\038\000\004\000\030\000\043\000\000\000\042\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\000\000\000\013\000\013\000\
\000\000\000\000\000\000\000\000\013\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\013\000\013\000\000\000\
\000\000\013\000"

let yycheck = "\001\000\
\000\000\005\000\006\000\007\000\001\001\002\001\003\001\004\001\
\012\000\001\001\002\001\003\001\005\001\010\001\011\001\008\001\
\013\001\014\001\015\001\016\001\017\001\013\001\019\001\020\001\
\021\001\001\001\001\001\024\001\001\001\026\001\000\000\001\001\
\002\001\003\001\026\001\004\001\040\000\041\000\007\001\009\001\
\028\000\045\000\030\000\013\001\001\001\049\000\050\000\051\000\
\036\000\021\001\023\001\008\001\018\001\005\001\022\001\059\000\
\026\001\001\001\025\001\063\000\006\001\008\001\005\001\004\001\
\068\000\053\000\070\000\001\001\002\001\003\001\074\000\019\001\
\012\001\008\001\001\001\079\000\010\001\011\001\006\001\013\001\
\014\001\015\001\016\001\017\001\005\001\019\001\020\001\021\001\
\020\001\001\001\024\001\011\000\026\001\013\000\014\000\005\001\
\016\000\019\001\018\001\023\000\255\255\021\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\255\255\006\001\007\001\
\255\255\255\255\255\255\255\255\012\001\255\255\255\255\255\255\
\255\255\255\255\018\001\255\255\255\255\021\001\022\001\255\255\
\255\255\025\001"

let yynames_const = "\
  BR_OPN\000\
  BR_CLS\000\
  ARROW2\000\
  BAR\000\
  COMMA\000\
  EQ\000\
  SEMICOLON\000\
  KW_ABSURD\000\
  KW_CASE\000\
  KW_ELSE\000\
  KW_FALSE\000\
  KW_FIX\000\
  KW_FN\000\
  KW_FST\000\
  KW_IF\000\
  KW_IN\000\
  KW_INL\000\
  KW_INR\000\
  KW_LET\000\
  KW_OF\000\
  KW_REC\000\
  KW_SND\000\
  KW_THEN\000\
  KW_TRUE\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "yaccParser.mly"
     ( make _1 )
# 302 "yaccParser.ml"
               : 'id))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'id) in
    Obj.repr(
# 74 "yaccParser.mly"
              ( [ _1 ] )
# 309 "yaccParser.ml"
               : 'id_list1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'id) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'id_list1) in
    Obj.repr(
# 75 "yaccParser.mly"
              ( _1 :: _2 )
# 317 "yaccParser.ml"
               : 'id_list1))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "yaccParser.mly"
              ( () )
# 323 "yaccParser.ml"
               : 'bar_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "yaccParser.mly"
              ( () )
# 329 "yaccParser.ml"
               : 'bar_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def_list1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "yaccParser.mly"
                             ( desugar_defs _1 _3 )
# 337 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'id_list1) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "yaccParser.mly"
                             ( make (desugar_fn _2 _4).data )
# 345 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'id_list1) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "yaccParser.mly"
                                 ( desugar_fix _2 _3 _5 )
# 354 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 11 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 9 : 'bar_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _11 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _13 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "yaccParser.mly"
    ( make (ECase(_2, (_6, _8), (_11, _13))) )
# 366 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "yaccParser.mly"
    ( make (EIf(_2, _4, _6)) )
# 375 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "yaccParser.mly"
                 ( make (EAbsurd _2) )
# 382 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_app) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "yaccParser.mly"
                          ( make (ESeq(_1, _3)) )
# 390 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_app) in
    Obj.repr(
# 95 "yaccParser.mly"
           ( _1 )
# 397 "yaccParser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_app) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_simple) in
    Obj.repr(
# 99 "yaccParser.mly"
                       ( make (EApp(_1, _2)) )
# 405 "yaccParser.ml"
               : 'expr_app))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_simple) in
    Obj.repr(
# 100 "yaccParser.mly"
                       ( make (EFst _2) )
# 412 "yaccParser.ml"
               : 'expr_app))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_simple) in
    Obj.repr(
# 101 "yaccParser.mly"
                       ( make (ESnd _2) )
# 419 "yaccParser.ml"
               : 'expr_app))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_simple) in
    Obj.repr(
# 102 "yaccParser.mly"
                       ( make (EInl _2) )
# 426 "yaccParser.ml"
               : 'expr_app))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_simple) in
    Obj.repr(
# 103 "yaccParser.mly"
                       ( make (EInr _2) )
# 433 "yaccParser.ml"
               : 'expr_app))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_simple) in
    Obj.repr(
# 104 "yaccParser.mly"
              ( _1 )
# 440 "yaccParser.ml"
               : 'expr_app))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "yaccParser.mly"
                                ( make EUnit )
# 446 "yaccParser.ml"
               : 'expr_simple))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 109 "yaccParser.mly"
                                ( make (_2).data )
# 453 "yaccParser.ml"
               : 'expr_simple))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 110 "yaccParser.mly"
                                ( make (EPair(_2, _4)) )
# 461 "yaccParser.ml"
               : 'expr_simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 111 "yaccParser.mly"
           ( make (ENum _1) )
# 468 "yaccParser.ml"
               : 'expr_simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "yaccParser.mly"
           ( make (EVar _1) )
# 475 "yaccParser.ml"
               : 'expr_simple))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "yaccParser.mly"
           ( make (EBool true) )
# 481 "yaccParser.ml"
               : 'expr_simple))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "yaccParser.mly"
           ( make (EBool false) )
# 487 "yaccParser.ml"
               : 'expr_simple))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "yaccParser.mly"
                                    ( make (DLet(_2, _4))      )
# 495 "yaccParser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'id_list1) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "yaccParser.mly"
                                    ( desugar_let_fun _2 _3 _5 )
# 504 "yaccParser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'id_list1) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "yaccParser.mly"
                                    ( desugar_let_rec _3 _4 _6 )
# 513 "yaccParser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'def) in
    Obj.repr(
# 124 "yaccParser.mly"
                ( [ _1 ] )
# 520 "yaccParser.ml"
               : 'def_list1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'def) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'def_list1) in
    Obj.repr(
# 125 "yaccParser.mly"
                ( _1 :: _2 )
# 528 "yaccParser.ml"
               : 'def_list1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 129 "yaccParser.mly"
           ( _1 )
# 535 "yaccParser.ml"
               : Ast.program))
(* Entry file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
