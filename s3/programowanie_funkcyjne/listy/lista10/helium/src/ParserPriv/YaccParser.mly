%token<string> LID UID
%token<string> OP_0 OP_20 OP_30 OP_40 OP_50 OP_60 OP_70 OP_80 OP_90 OP_100
%token<string> OP_250
%token<int> NUM
%token<string> STR
%token<char> CHR
%token BR_OPN BR_CLS SBR_OPN SBR_CLS CBR_OPN CBR_CLS
%token ARROW ARROW2 BAR COLON COMMA DOT EQ SEMICOLON SEMICOLON2 SLASH
%token UNDERSCORE
%token KW_AND KW_BEGIN KW_DATA KW_EFFECT KW_EFFROW KW_ELIF KW_ELSE KW_END
%token KW_EXTERN KW_FN KW_FUNCTOR KW_HANDLE KW_IF KW_IN KW_INCLUDE KW_LET
%token KW_MATCH KW_MODULE KW_OF KW_OPEN KW_PRAGMA KW_REC KW_RETURN KW_SIG
%token KW_STRUCT KW_THEN KW_THIS KW_TYPE KW_VAL KW_WITH
%token EOF

%type<Lang.Raw.file> file
%start file

%type<Lang.Raw.intf_file> intf_file
%start intf_file

%type<Lang.Raw.repl_cmd> repl_cmd
%start repl_cmd

%{

open Lang.Node
open Lang.Raw

let current_pos () =
  Utils.Position.of_pp
    (Parsing.symbol_start_pos ())
    (Parsing.symbol_end_pos ())

let make data =
  { meta = current_pos ()
  ; data = data
  }

%}

%%

file
: def_list EOF { make $1 }
;

intf_file
: decl_list EOF { make $1 }
;

repl_cmd
: expr SEMICOLON2 { make (ReplExpr $1) }
| def  SEMICOLON2 { make (ReplDef  $1) }
| EOF             { make ReplExit      }
;

/* ========================================================================= */

bar_opt
: /* empty */ { false }
| BAR         { true  }
;

/* ========================================================================= */

lid
: LID { make $1 }
;

uid
: UID { make $1 }
;

uid_or_this
: uid     { $1 }
| KW_THIS { make "this" }
;

ctor
: uid             { $1 }
| BR_OPN BR_CLS   { make "()" }
| SBR_OPN SBR_CLS { make "[]" }
;

ctor_or_op
: ctor             { CNName $1 }
| BR_OPN op BR_CLS { CNOper $2 }
;

str
: STR { make $1 }
;

/* ========================================================================= */

mpath_uid
: uid               { ([], $1) }
| uid DOT mpath_uid { let (path, x) = $3 in ($1 :: path, x) }
;

mpath1_uid
: uid DOT mpath_uid { let (path, x) = $3 in ($1 :: path, x) }
;

mpath_lid
: lid               { ([], $1) }
| uid DOT mpath_lid { let (path, x) = $3 in ($1 :: path, x) }
;

/* ========================================================================= */

op_0
: OP_0      { make $1  }
| SEMICOLON { make ";" }
;

op_0_no_semi
: OP_0 { make $1 }
;

op_20
: OP_20 { make $1 }
;

op_30
: OP_30 { make $1  }
| COMMA { make "," }
;

op_30_no_comma
: OP_30 { make $1 }
;

op_40
: OP_40 { make $1 }
;

op_50
: OP_50 { make $1 }
;

op_60
: OP_60 { make $1  }
| EQ    { make "=" }
;

op_70
: OP_70 { make $1 }
;

op_80
: OP_80 { make $1 }
;

op_90
: OP_90 { make $1  }
| SLASH { make "/" }
;

op_100
: OP_100 { make $1 }
;

uop_150
: OP_80 { make $1 }
;

uop_250
: OP_250 { make $1 }
;

op
: op_0   { $1 }
| op_20  { $1 }
| op_30  { $1 }
| op_40  { $1 }
| op_50  { $1 }
| op_60  { $1 }
| op_70  { $1 }
| op_80  { $1 }
| op_90  { $1 }
| op_100 { $1 }
| OP_250 { make $1 }
;

/* ========================================================================= */

kind
: kind_simpl ARROW kind { KArrow($1, $3) }
| kind_simpl { $1 }
;

kind_simpl
: KW_TYPE   { KType   }
| KW_EFFECT { KEffect }
| KW_EFFROW { KEffect }
| BR_OPN kind BR_CLS { $2 }
;

/* ========================================================================= */


type_list
: type_expr                 { [ $1 ] }
| type_expr COMMA type_list { $1 :: $3 }
;

type_or_effrow
: type_expr { $1 }
| effrow    { make (TEffRow $1) }
;

type_expr
: type_app ARROW type_expr        { make (TArrowPure($1, $3)) }
| type_app ARROW effrow type_expr { make (TArrowEff($1, $4, $3)) }
| type_app { $1 }
;

type_app
: type_app type_simpl_or_effrow { make (TApp($1, $2)) }
| type_simpl { $1 }
;

type_simpl
: BR_OPN type_or_effrow BR_CLS  { make (TParen $2)  }
| LID                           { make (TVar   $1)  }
| UNDERSCORE                    { make TPlaceholder }
| KW_THIS                       { make (TConst([], make "this")) }
| mpath_uid { let (path, x) = $1 in make (TConst(path, x)) }
;

type_simpl_or_effrow
: type_simpl { $1 }
| effrow     { make (TEffRow $1) }
;

effrow
: SBR_OPN SBR_CLS               { make TEffPure  }
| SBR_OPN BAR type_expr SBR_CLS { make ($3).data }
| SBR_OPN effrow_body   SBR_CLS { make ($2).data }
;

effrow_body
: type_expr                      { make (TEffRow $1) }
| type_expr BAR   type_or_effrow { make (TEffCons($1, $3)) }
| type_expr COMMA effrow_body    { make (TEffCons($1, $3)) }
;

/* ========================================================================= */

pattern
: pattern_0 { $1 }
;

pattern_no_semi
: pattern_0_no_semi { $1 }
;

pattern_no_comma
: pattern_30_no_comma { $1 }
;

pattern_0
: pattern_10 op_0 pattern_0 { make (PBOp($1, $2, $3)) }
| pattern_10 { $1 }
;

pattern_0_no_semi
: pattern_10 op_0_no_semi pattern_0_no_semi { make (PBOp($1, $2, $3)) }
| pattern_10 { $1 }
;

pattern_10 
: pattern_20 { $1 }
;

pattern_20
: pattern_30 op_20 pattern_20 { make (PBOp($1, $2, $3)) }
| pattern_30 { $1 }
;

pattern_30
: pattern_30 COLON type_expr  { make (PAnnot($1, $3)) }
| pattern_30 op_30 pattern_40 { make (PBOp($1, $2, $3)) }
| pattern_40 { $1 }
;

pattern_30_no_comma
: pattern_30_no_comma COLON type_expr { make (PAnnot($1, $3)) }
| pattern_30_no_comma op_30_no_comma pattern_40 { make (PBOp($1, $2, $3)) }
| pattern_40 { $1 }
;

pattern_40
: pattern_50 op_40 pattern_40 { make (PBOp($1, $2, $3)) }
| pattern_50 { $1 }
;

pattern_50
: pattern_60 op_50 pattern_50 { make (PBOp($1, $2, $3)) }
| pattern_60 { $1 }
;

pattern_60
: pattern_60 op_60 pattern_70 { make (PBOp($1, $2, $3)) }
| pattern_70 { $1 }
;

pattern_70
: pattern_80 op_70 pattern_70 { make (PBOp($1, $2, $3)) }
| pattern_80 { $1 }
;

pattern_80
: pattern_80 op_80 pattern_90 { make (PBOp($1, $2, $3)) }
| pattern_90 { $1 }
;

pattern_90
: pattern_90 op_90 pattern_100 { make (PBOp($1, $2, $3)) }
| pattern_100 { $1 }
;

pattern_100
: pattern_200 op_100 pattern_100 { make (PBOp($1, $2, $3)) }
| pattern_200 { $1 }
;

pattern_200
: ctor pattern_simpl_list1 { make (PCtor([], CNName $1, $2)) }
| mpath1_pat pattern_simpl_list1
    { let (path, g) = $1 in make (g path $2) }
| pattern_simpl { $1 }
;

pattern_simpl
: BR_OPN pattern BR_CLS                  { make (PParen $2) }
| BR_OPN KW_TYPE type_farg_list1 BR_CLS  { make (PTypes $3) }
| LID                                    { make (PVar $1) }
| ctor                                   { make (PCtor([], CNName $1, [])) }
| mpath1_pat { let (path, g) = $1 in make (g path []) }
| BR_OPN op BR_CLS                       { make (POper $2)  }
| BR_OPN op UNDERSCORE BR_CLS            { make (PUOper $2) }
| CBR_OPN field_pattern_list CBR_CLS     { make (PRecord $2) }
| SBR_OPN pattern_list_comma_sep SBR_CLS { make (PList $2) }
| UNDERSCORE                             { make PWildcard }
;

pattern_list_comma_sep
: pattern_no_comma { [ $1 ] }
| pattern_no_comma COMMA pattern_list_comma_sep { $1 :: $3 }
;

mpath_pat
: ctor_or_op
    { let name = $1 in ([], (fun path args -> (PCtor(path, name, args)))) }
| mpath1_pat { $1 }
;

mpath1_pat
: uid DOT mpath_pat
    { let (path, g) = $3 in ($1 :: path, g) }
;

/* ------------------------------------------------------------------------- */

pattern_simpl_list
: /* empty */ { [] }
| pattern_simpl_list1 { $1 }
;

pattern_simpl_list1
: pattern_simpl pattern_simpl_list { $1 :: $2 }
;

/* ------------------------------------------------------------------------- */

field_pattern
: mpath_lid EQ pattern_no_semi
    { let (path, l) = $1 in make (FieldPat(path, l, $3)) }
;

field_pattern_list
: field_pattern { [ $1 ] }
| field_pattern SEMICOLON field_pattern_list { $1 :: $3 }
;

/* ========================================================================= */

type_farg
: UID { make (TFA_Var $1) }
;

type_farg_list
: /* empty */ { [] }
| type_farg_list1 { $1 }
;

type_farg_list1
: type_farg type_farg_list { $1 :: $2 }
;

type_quantifier_opt
: /* empty */ { [] }
| KW_TYPE type_farg_list1 DOT { $2 }
;

/* ========================================================================= */

op_decl
: lid COLON type_quantifier_opt type_list ARROW2 type_expr
    { make (OpDecl($1, $3, $4, $6)) }
;

op_decl_list
: op_decl_list_rev { List.rev $1 }
;

op_decl_list_rev
: op_decl { [ $1 ] }
| op_decl_list_rev SEMICOLON op_decl { $3 :: $1 }
;

/* ========================================================================= */

adt_ctor
: ctor_or_op { make (ADTCtor($1, [], [])) }
| ctor_or_op KW_OF type_quantifier_opt type_list
  { make (ADTCtor($1, $3, $4)) }
;

adt_ctors
: /* empty */   { [] }
| adt_ctor_list { $1 }
;

adt_ctor_list
: adt_ctor                   { [ $1 ]   }
| adt_ctor BAR adt_ctor_list { $1 :: $3 }
;

/* ========================================================================= */

field_decl
: lid COLON type_quantifier_opt type_expr
    { make (FieldDecl($1, $3, $4)) }
;

field_decl_list
: field_decl { [ $1 ] }
| field_decl SEMICOLON field_decl_list { $1 :: $3 }
;

/* ========================================================================= */

typedef
: KW_EFFECT uid_or_this type_farg_list EQ CBR_OPN op_decl_list CBR_CLS
    { make (TDEffect($2, $3, $6)) }
| KW_DATA uid_or_this type_farg_list EQ adt_ctors
    { make (TDData($2, $3, $5)) }
| KW_DATA uid_or_this type_farg_list EQ BAR adt_ctor_list
    { make (TDData($2, $3, $6)) }
| KW_DATA uid_or_this type_farg_list EQ CBR_OPN field_decl_list CBR_CLS
    { make (TDRecord($2, $3, $6)) }
;

typedef_rec
: KW_EFFECT KW_REC uid_or_this type_farg_list EQ CBR_OPN op_decl_list CBR_CLS
    { make (TDEffect($3, $4, $7)) }
| KW_DATA KW_REC uid_or_this type_farg_list EQ adt_ctors
    { make (TDData($3, $4, $6)) }
| KW_DATA KW_REC uid_or_this type_farg_list EQ BAR adt_ctor_list
    { make (TDData($3, $4, $7)) }
| KW_DATA KW_REC uid_or_this type_farg_list EQ CBR_OPN field_decl_list CBR_CLS
    { make (TDRecord($3, $4, $7)) }
;

typedef_rec_list
: typedef_rec and_typedef_list { $1 :: $2 }
;

and_typedef_list
: /* empty */                     { [] }
| KW_AND typedef and_typedef_list { $2 :: $3 }
;

/* ========================================================================= */

type_annot
: COLON type_expr { Annot $2 }
| COLON type_expr SLASH type_simpl_or_effrow { AnnotEff($2, $4) }
;

type_annot_opt
: /* empty */ { None    }
| type_annot  { Some $1 }
;

/* ========================================================================= */

expr
: expr_0 { $1 }
;

expr_no_semi
: expr_0_no_semi { $1 }
;

expr_no_comma
: expr_30_no_comma { $1 }
;

expr_0
: KW_FN pattern_simpl_list1 type_annot_opt ARROW2 expr_0
    { make (EFun($2, $3, $5)) }
| def_list KW_IN expr_0 { make (EDefs($1, $3)) }
| expr_10 op_0 expr_0   { make (EBOp($1, $2, $3)) }
| expr_10 { $1 }
;

expr_0_no_semi
: KW_FN pattern_simpl_list1 type_annot_opt ARROW2 expr_0_no_semi
    { make (EFun($2, $3, $5)) }
| def_list KW_IN expr_0_no_semi       { make (EDefs($1, $3)) }
| expr_10 op_0_no_semi expr_0_no_semi { make (EBOp($1, $2, $3)) }
| expr_10 { $1 }
;

expr_10 
: KW_IF expr KW_THEN expr_10 { make (EUIf($2, $4)) }
| KW_IF expr KW_THEN expr_with_else_10 else_expr { make (EIf($2, $4, $5)) }
| KW_HANDLE expr KW_WITH expr_10
    { make (EHandleWith($2, $4)) }
| expr_20 { $1 }
;

expr_with_else_10
: KW_IF expr KW_THEN expr_with_else_10 else_expr_with_else
    { make (EIf($2, $4, $5)) }
| expr_20 { $1 }
;

else_expr
: KW_ELIF expr KW_THEN expr_10 { make (EUIf($2, $4)) }
| KW_ELIF expr KW_THEN expr_with_else_10 else_expr { make (EIf($2, $4, $5)) }
| KW_ELSE expr_10 { $2 }
;

else_expr_with_else
: KW_ELIF expr KW_THEN expr_with_else_10 else_expr_with_else
    { make (EIf($2, $4, $5)) }
| KW_ELSE expr_with_else_10 { $2 }
;

expr_20
: expr_30 op_20 expr_20 { make (EBOp($1, $2, $3)) }
| expr_30 { $1 }
;

expr_30
: expr_30 type_annot { make (EAnnot($1, $2)) }
| KW_EXTERN str COLON type_quantifier_opt type_expr
    { make (EExtern($2, $4, $5)) }
| expr_30 op_30 expr_40 { make (EBOp($1, $2, $3)) }
| expr_40 { $1 }
;

expr_30_no_comma
: expr_30_no_comma type_annot { make (EAnnot($1, $2)) }
| KW_EXTERN str COLON type_quantifier_opt type_expr
    { make (EExtern($2, $4, $5)) }
| expr_30_no_comma op_30_no_comma expr_40 { make (EBOp($1, $2, $3)) }
| expr_40 { $1 }
;

expr_40
: expr_50 op_40 expr_40 { make (EBOp($1, $2, $3)) }
| expr_50 { $1 }
;

expr_50
: expr_60 op_50 expr_50 { make (EBOp($1, $2, $3)) }
| expr_60 { $1 }
;

expr_60
: expr_60 op_60 expr_70 { make (EBOp($1, $2, $3)) }
| expr_70 { $1 }
;

expr_70
: expr_80 op_70 expr_70 { make (EBOp($1, $2, $3)) }
| expr_80 { $1 }
;

expr_80
: expr_80 op_80 expr_90 { make (EBOp($1, $2, $3)) }
| expr_90 { $1 }
;

expr_90
: expr_90 op_90 expr_100 { make (EBOp($1, $2, $3)) }
| expr_100 { $1 }
;

expr_100
: expr_150 op_100 expr_100 { make (EBOp($1, $2, $3)) }
| expr_150 { $1 }
;

expr_150
: uop_150 expr_150 { make (EUOp($1, $2)) }
| expr_200 { $1 }
;

expr_200
: expr_200 expr_250 { make (EApp($1, $2)) }
| expr_250 { $1 }
;

expr_250
: uop_250 expr_250 { make (EUOp($1, $2)) }
| expr_300 { $1 }
;

expr_300
: expr_300_no_ctor DOT mpath_lid
    { let (mpath, f) = $3 in make (ESelect($1, mpath, f)) }
| expr_simpl { $1 }
;

expr_300_no_ctor
: expr_300_no_ctor DOT mpath_lid
    { let (mpath, f) = $3 in make (ESelect($1, mpath, f)) }
| expr_simpl_no_ctor { $1 }
;

expr_simpl
: mpath1_expr { let (path, g) = $1 in make (g path) }
| expr_simpl_rest { $1 }
;

expr_simpl_no_ctor
: mpath1_expr_no_ctor { let (path, g) = $1 in make (g path) }
| expr_simpl_rest { $1 }
;

expr_simpl_rest
: KW_BEGIN expr KW_END        { make (EParen $2) }
| BR_OPN expr BR_CLS          { make (EParen $2) }
| BR_OPN op BR_CLS            { make (EOper([], $2))  }
| BR_OPN op UNDERSCORE BR_CLS { make (EUOper([], $2)) }
| lid                         { make (EVar([], $1)) }
| NUM { make (ENum $1)  }
| STR { make (EString $1) }
| CHR { make (EChar $1) }
| KW_HANDLE expr KW_WITH BAR handler_list KW_END
    { make (EHandle($2, $5)) }
| KW_HANDLE BAR handler_list KW_END
    { make (EHandler $3) }
| KW_MATCH expr KW_WITH clause_list_opt KW_END
    { make (EMatch($2, $4)) }
| CBR_OPN field_def_list CBR_CLS
    { make (ERecord $2) }
| SBR_OPN expr_list_comma_sep SBR_CLS { make (EList $2) }
;

expr_list_comma_sep
: expr_no_comma                           { [ $1 ] }
| expr_no_comma COMMA expr_list_comma_sep { $1 :: $3 }
;

mpath_expr
: lid  { let x = $1 in ([], (fun path -> EVar(path, x))) }
| BR_OPN op BR_OPN
    { let x = $2 in ([], (fun path -> EOper(path, x))) }
| BR_OPN op UNDERSCORE BR_CLS
    { let x = $2 in ([], (fun path -> EUOper(path, x))) }
| mpath1_expr { $1 }
;

mpath1_expr
: ctor { let c = $1 in ([], (fun path -> ECtor(path, c))) }
| uid DOT mpath_expr
    { let (path, g) = $3 in ($1 :: path, g) }
;

mpath_expr_no_ctor
: lid  { let x = $1 in ([], (fun path -> EVar(path, x))) }
| BR_OPN op BR_OPN
    { let x = $2 in ([], (fun path -> EOper(path, x))) }
| BR_OPN op UNDERSCORE BR_CLS
    { let x = $2 in ([], (fun path -> EUOper(path, x))) }
| mpath1_expr_no_ctor { $1 }
;

mpath1_expr_no_ctor
: uid DOT mpath_expr_no_ctor
    { let (path, g) = $3 in ($1 :: path, g) }
;

/* ------------------------------------------------------------------------- */

rec_function
: lid pattern_simpl_list1 type_annot_opt EQ expr
    { make (RecFunc($1, $2, $3, $5)) }
;

rec_functions
: rec_function { [ $1 ] }
| rec_function KW_AND rec_functions { $1 :: $3 }
;

/* ------------------------------------------------------------------------- */

resume_opt
: /* empty */ { None }
| SLASH lid   { Some $2 }
;

handler
: KW_RETURN pattern ARROW2 expr { make (HReturn($2, $4)) }
| mpath_lid pattern_simpl_list resume_opt ARROW2 expr
    { let (path, op) = $1 in make (HOp(path, op, $2, $3, $5)) }
;

handler_list
: handler_list_rev { List.rev $1 }
;

handler_list_rev
: handler { [ $1 ] }
| handler_list_rev BAR handler { $3 :: $1 }
;

/* ------------------------------------------------------------------------- */

clause
: pattern ARROW2 expr { make (Clause($1, $3)) }
;

clause_list
: clause { [ $1 ] }
| clause BAR clause_list { $1 :: $3 }
;

clause_list_opt
: /* empty */         { [] }
| bar_opt clause_list { $2 }
;

/* ------------------------------------------------------------------------- */

field_def
: mpath_lid EQ expr_no_semi
    { let (path, f) = $1 in make (FieldDef(path, f, $3)) }
;

field_def_list
: field_def { [ $1 ] }
| field_def SEMICOLON field_def_list { $1 :: $3 }
;

/* ========================================================================= */

def
: KW_LET pattern_simpl_list1 type_annot_opt EQ expr
    { make (DefLet($2, $3, $5)) }
| KW_LET KW_REC rec_functions
    { make (DefLetRec $3) }
| KW_LET KW_HANDLE BAR handler_list KW_END
    { make (DefHandle $4) }
| KW_LET KW_HANDLE expr
    { make (DefHandleWith $3) }
| typedef_rec_list { make (DefTypedefRec $1) }
| typedef { make (DefTypedef $1) }
| KW_TYPE   uid_or_this type_farg_list EQ type_expr
    { make (DefTypeAlias($2, $3, $5)) }
| KW_EFFECT uid_or_this type_farg_list EQ type_expr
    { make (DefTypeAlias($2, $3, $5)) }
| KW_TYPE   uid_or_this { make (DefAbsMType $2) }
| KW_EFFECT uid_or_this { make (DefAbsMEffect $2) }
| KW_TYPE   uid_or_this COLON kind { make (DefAbsType($2, $4)) }
| KW_EFFECT uid_or_this COLON kind { make (DefAbsEffect($2, $4)) }
| KW_MODULE uid_or_this module_arg_list sig_annot_opt EQ module_expr
  { make (DefModule($2, $3, $4, $6)) }
| KW_OPEN module_expr { make (DefOpen $2) }
| KW_OPEN KW_TYPE mpath_uid
    { let (mpath, x) = $3 in make (DefOpenType(mpath, x)) }
| KW_INCLUDE module_expr { make (DefInclude $2) }
| KW_INCLUDE KW_TYPE mpath_uid
    { let (mpath, x) = $3 in make (DefIncludeType(mpath, x)) }
| KW_PRAGMA STR { make (DefPragmaFlag $2) }
| KW_PRAGMA KW_VAL STR COLON mpath_lid
    { let (mpath, x) = $5 in make (DefPragmaVal($3, mpath, x)) }
| KW_PRAGMA KW_TYPE STR COLON mpath_uid
    { let (mpath, x) = $5 in make (DefPragmaType($3, mpath, x)) }
;

def_list
: /* empty */  { [] }
| def def_list { $1 :: $2 }
;

/* ========================================================================= */

module_expr
: module_expr_0 { $1 }
;

module_expr_0
: KW_FUNCTOR module_arg_list1 sig_annot_opt ARROW2 module_expr_0
    { make (ModFunctor($2, $3, $5)) }
| module_expr_10 { $1 }
;

module_expr_10
: module_expr_10 COLON module_sig { make (ModAnnot($1, $3)) }
| module_expr_20 { $1 }
;

module_expr_20
: module_expr_20 BR_OPN module_expr BR_CLS { make (ModApp($1, $3)) }
| module_expr_simpl { $1 }
;

module_expr_simpl
: BR_OPN module_expr BR_CLS { make ($2).data }
| KW_STRUCT def_list KW_END { make (ModStruct $2) }
| KW_THIS    { make (ModConst([], make "this")) }
| UID        { make (ModConst([], make $1)) }
| mpath1_uid { let (mpath, m) = $1 in make (ModConst(mpath, m)) }
;

/* ========================================================================= */

module_arg
: BR_OPN uid COLON module_sig BR_CLS { make (MArg($2, $4)) }
;

module_arg_list
: /* empty */                { [] }
| module_arg module_arg_list { $1 :: $2 }
;

module_arg_list1
: module_arg module_arg_list { $1 :: $2 }
;

/* ========================================================================= */

sig_annot_opt
: /* empty */      { None }
| COLON module_sig { Some $2 }
;

module_sig
: BR_OPN module_sig BR_CLS { make ($2).data }
| KW_SIG decl_list KW_END { make (MTStruct $2) }
| KW_FUNCTOR module_arg_list1 ARROW2 module_sig { make (MTFunctor($2, $4)) }
;

/* ========================================================================= */

decl
: KW_TYPE   uid_or_this { make (DeclMType $2) }
| KW_EFFECT uid_or_this { make (DeclMEffect $2) }
| KW_TYPE   uid_or_this COLON kind { make (DeclType($2, $4)) }
| KW_EFFECT uid_or_this COLON kind { make (DeclEffect($2, $4)) }
| KW_VAL lid COLON type_quantifier_opt type_expr
    { make (DeclVal($2, $4, $5)) }
| KW_VAL BR_OPN op BR_CLS COLON type_quantifier_opt type_expr
    { make (DeclOper($3, $6, $7)) }
| KW_VAL BR_OPN op UNDERSCORE BR_CLS COLON type_quantifier_opt type_expr
    { make (DeclUOper($3, $7, $8)) }
| KW_OPEN mpath_uid { let (path, m) = $2 in make (DeclOpen(path, m)) }
| KW_OPEN KW_TYPE mpath_uid
    { let (mpath, x) = $3 in make (DeclOpenType(mpath, x)) }
;

decl_list
: /* empty */    { [] }
| decl decl_list { $1 :: $2 }
;
