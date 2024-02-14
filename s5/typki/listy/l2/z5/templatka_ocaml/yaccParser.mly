/** Yacc-generated parser */
%token<string> ID
%token<int> NUM
%token BR_OPN BR_CLS
%token ARROW2 BAR COMMA EQ SEMICOLON
%token KW_ABSURD KW_CASE KW_ELSE KW_FALSE KW_FIX KW_FN KW_FST KW_IF KW_IN
%token KW_INL KW_INR KW_LET KW_OF KW_REC KW_SND KW_THEN KW_TRUE
%token EOF

%type<Ast.program> file
%start file

%{

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

%}

%%

id
: ID { make $1 }
;

id_list1
: id          { [ $1 ] }
| id id_list1 { $1 :: $2 }
;

bar_opt
: /* empty */ { () }
| BAR         { () }
;

expr
: def_list1 KW_IN       expr { desugar_defs $1 $3 }
| KW_FN id_list1 ARROW2 expr { make (desugar_fn $2 $4).data }
| KW_FIX ID id_list1 ARROW2 expr { desugar_fix $2 $3 $5 }
| KW_CASE expr KW_OF
  bar_opt KW_INL ID ARROW2 expr
  BAR     KW_INR ID ARROW2 expr
    { make (ECase($2, ($6, $8), ($11, $13))) }
| KW_IF expr KW_THEN expr KW_ELSE expr
    { make (EIf($2, $4, $6)) }
| KW_ABSURD expr { make (EAbsurd $2) }
| expr_app SEMICOLON expr { make (ESeq($1, $3)) }
| expr_app { $1 }
;

expr_app
: expr_app expr_simple { make (EApp($1, $2)) }
| KW_FST   expr_simple { make (EFst $2) }
| KW_SND   expr_simple { make (ESnd $2) }
| KW_INL   expr_simple { make (EInl $2) }
| KW_INR   expr_simple { make (EInr $2) }
| expr_simple { $1 }
;

expr_simple
: BR_OPN BR_CLS                 { make EUnit }
| BR_OPN expr BR_CLS            { make ($2).data }
| BR_OPN expr COMMA expr BR_CLS { make (EPair($2, $4)) }
| NUM      { make (ENum $1) }
| ID       { make (EVar $1) }
| KW_TRUE  { make (EBool true) }
| KW_FALSE { make (EBool false) }
;

def
: KW_LET ID EQ expr                 { make (DLet($2, $4))      }
| KW_LET ID id_list1 EQ expr        { desugar_let_fun $2 $3 $5 }
| KW_LET KW_REC ID id_list1 EQ expr { desugar_let_rec $3 $4 $6 }
;

def_list1
: def           { [ $1 ] }
| def def_list1 { $1 :: $2 }
;

file
: expr EOF { $1 }
;
