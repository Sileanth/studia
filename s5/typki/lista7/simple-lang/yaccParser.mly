/** Yacc-generated parser */
%token<string> LID UID
%token<int> NUM
%token BR_OPN BR_CLS CBR_OPN CBR_CLS
%token ARROW2 BAR COMMA DOT EQ SEMICOLON
%token KW_ABSURD KW_CASE KW_ELSE KW_END KW_FALSE KW_FIX KW_FN KW_FST KW_IF
%token KW_IN KW_INL KW_INR KW_LET KW_MATCH KW_OF KW_REC KW_SND KW_THEN KW_TRUE
%token KW_WITH
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
    { data = ELet(x, body, rest);
      start_pos = def.start_pos;
      end_pos   = rest.end_pos
    }

let desugar_defs defs rest =
  List.fold_right desugar_def defs rest

%}

%%

id
: LID { make $1 }
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
| KW_FIX LID id_list1 ARROW2 expr { desugar_fix $2 $3 $5 }
| KW_CASE expr KW_OF
  bar_opt KW_INL LID ARROW2 expr
  BAR     KW_INR LID ARROW2 expr
    { make (ECase($2, ($6, $8), ($11, $13))) }
| KW_MATCH expr KW_WITH
  bar_opt clauses KW_END
    { $5 (Parsing.symbol_start_pos ()) $2 }
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
| UID      expr_simple { make (ECtor($1, $2)) }
| expr_simple { $1 }
;

expr_simple
: BR_OPN BR_CLS                 { make EUnit }
| BR_OPN expr BR_CLS            { make ($2).data }
| BR_OPN expr COMMA expr BR_CLS { make (EPair($2, $4)) }
| CBR_OPN CBR_CLS            { make (ERecord []) }
| CBR_OPN fields CBR_CLS { make (ERecord $2) }
| NUM      { make (ENum $1) }
| LID      { make (EVar $1) }
| KW_TRUE  { make (EBool true) }
| KW_FALSE { make (EBool false) }
| expr_simple DOT LID { make (ESelect($1, $3)) }
;

field
: LID EQ expr { make ($1, $3) }
;

fields
: field              { [ $1 ]   }
| field COMMA fields { $1 :: $3 }
;

clauses
: LID ARROW2     expr
  { let end_pos = Parsing.symbol_end_pos () in
    fun start_pos e ->
    { data      = ELet($1, e, $3);
      start_pos = start_pos;
      end_pos   = end_pos } }
| UID LID ARROW2 expr
  { let end_pos = Parsing.symbol_end_pos () in
    let make_at_end data = { data; start_pos = end_pos; end_pos } in
    fun start_pos e ->
    { data      = EMatch(e, $1, ($2, $4),
        ("$rest", make_at_end (EMatchEmpty(make_at_end (EVar "$rest")))));
      start_pos = start_pos;
      end_pos   = end_pos } }
| UID LID ARROW2 expr BAR clauses
  { let bar_pos = Parsing.rhs_start_pos 5 in
    let end_pos = Parsing.symbol_end_pos () in
    let make_at_bar data = { data; start_pos = bar_pos; end_pos = bar_pos } in
    fun start_pos e ->
    { data      = EMatch(e, $1, ($2, $4),
        ("$rest", $6 bar_pos (make_at_bar (EVar "$rest"))));
      start_pos = start_pos;
      end_pos   = end_pos } }
;

def
: KW_LET LID EQ expr                 { make (DLet($2, $4))      }
| KW_LET LID id_list1 EQ expr        { desugar_let_fun $2 $3 $5 }
| KW_LET KW_REC LID id_list1 EQ expr { desugar_let_rec $3 $4 $6 }
;

def_list1
: def           { [ $1 ] }
| def def_list1 { $1 :: $2 }
;

file
: expr EOF { $1 }
;
