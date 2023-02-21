open Common
open Lang.Node
open Lang.Flat

module Env : sig
  type t

  val empty : t
  val init  : unit -> t

  val add_var    : t -> var    -> t * string
  val add_op     : t -> op     -> t * string
  val add_ctor   : t -> ctor   -> t * string
  val add_field  : t -> field  -> t * string
  val add_tconst : t -> tconst -> t * string

  val pretty_var    : t -> var    -> Box.t
  val pretty_op     : t -> op     -> Box.t
  val pretty_ctor   : t -> ctor   -> Box.t
  val pretty_field  : t -> field  -> Box.t
  val pretty_tconst : t -> tconst -> Box.t
end = struct
  type t =
    { names      : StrSet.t
    ; var_env    : string Var.Map.t
    ; op_env     : string Op.Map.t
    ; ctor_env   : string Ctor.Map.t
    ; field_env  : string Field.Map.t
    ; tconst_env : string TConst.Map.t
    }

  let empty =
    { names      = StrSet.empty
    ; var_env    = Var.Map.empty
    ; op_env     = Op.Map.empty
    ; ctor_env   = Ctor.Map.empty
    ; field_env  = Field.Map.empty
    ; tconst_env = TConst.Map.empty
    }

  let add_var env x =
    let name = fresh_name env.names (Var.name x) in
    { env with
      names   = StrSet.add name env.names
    ; var_env = Var.Map.add x name env.var_env
    }, name

  let add_op env x =
    let name = fresh_name env.names (Op.name x) in
    { env with
      names  = StrSet.add name env.names
    ; op_env = Op.Map.add x name env.op_env
    }, name

  let add_ctor env x =
    let name = fresh_name env.names (Ctor.name x) in
    { env with
      names    = StrSet.add name env.names
    ; ctor_env = Ctor.Map.add x name env.ctor_env
    }, name

  let add_field env x =
    let name = fresh_name env.names (Field.name x) in
    { env with
      names     = StrSet.add name env.names
    ; field_env = Field.Map.add x name env.field_env
    }, name

  let add_tconst env x =
    let name = fresh_name env.names (TConst.full_name x) in
    { env with
      names      = StrSet.add name env.names
    ; tconst_env = TConst.Map.add x name env.tconst_env
    }, name

  let init () =
    let env = empty in
    let open Predef.DB in
    let env = List.fold_left (fun env (Type p) ->
        fst (add_tconst env p.flat_const)
      ) env (types ()) in
    env

  let pretty_var env x =
    match Var.Map.find_opt x env.var_env with
    | Some name -> Box.word name
    | None      -> Box.word "<unbound_var>"

  let pretty_op env x =
    match Op.Map.find_opt x env.op_env with
    | Some name -> Box.word name
    | None      -> Box.word "<unbound_op>"

  let pretty_ctor env x =
    match Ctor.Map.find_opt x env.ctor_env with
    | Some name -> Box.word name
    | None      -> Box.word "<unbound_ctor>"

  let pretty_field env x =
    match Field.Map.find_opt x env.field_env with
    | Some name -> Box.word name
    | None      -> Box.word "<unbound_field>"

  let pretty_tconst env x =
    match TConst.Map.find_opt x env.tconst_env with
    | Some name -> Box.word name
    | None      -> Box.word "<unbound_type>"
end

(* ========================================================================= *)

let rec pretty_kind prec k =
  match k with
  | KType   -> Box.word "type"
  | KEffect -> Box.word "effect"
  | KArrow(k1, k2) -> Box.prec_paren 0 prec (
      Box.box
      [ Box.suffix (pretty_kind 1 k1) (Box.ws (Box.word "->"))
      ; Box.ws (pretty_kind 0 k2)
      ])

(* ========================================================================= *)

let tp_prec_arrow = 0
let tp_prec_app   = 10

let rec pretty_type env prec tp =
  match tp.data with
  | TPlaceholder -> Box.word "_"
  | TConst x     -> Env.pretty_tconst env x
  | TArrowPure(tp1, tp2) ->
    Box.prec_paren tp_prec_arrow prec (
      Box.box
      [ Box.suffix (pretty_type env (tp_prec_arrow + 1) tp1)
          (Box.ws (Box.word "->"))
      ; Box.ws (pretty_type env tp_prec_arrow tp2)
      ])
  | TArrowEff(tp1, tp2, eff) ->
    Box.prec_paren tp_prec_arrow prec (
      Box.box
      [ Box.suffix (pretty_type env (tp_prec_arrow + 1) tp1)
          (Box.ws (Box.box
          [ (Box.word "->")
          ; Box.indent 2 (pretty_effrow env eff)
          ]))
      ; Box.ws (pretty_type env tp_prec_arrow tp2)
      ])
  | TEffPure | TEffRow _ | TEffCons _ ->
    pretty_effrow env tp
  | TApp(tp1, tp2) ->
    Box.prec_paren tp_prec_app prec (Box.box
      [ pretty_type env tp_prec_app tp1
      ; Box.ws (Box.indent 2 (pretty_type env (tp_prec_app + 1) tp2))
      ])

and pretty_effrow env eff =
  match eff.data with
  | TPlaceholder | TConst _ | TArrowPure _ | TArrowEff _ | TApp _ ->
    Box.paren ~opn:(Box.word "[|") ~cls:(Box.word "]")
      (pretty_type env 0 eff)
  | TEffPure -> Box.word "[]"
  | TEffRow eff -> pretty_effrow env eff
  | TEffCons(l, eff) ->
    Box.box
    (  Box.prefix (Box.word "[") (pretty_type env 0 l)
    :: pretty_effrow' env eff)

and pretty_effrow' env eff =
  match eff.data with
  | TPlaceholder | TConst _ | TArrowPure _ | TArrowEff _ | TApp _ ->
    [ Box.paren ~opn:(Box.word "|") ~cls:(Box.word "]")
        (pretty_type env 0 eff) ]
  | TEffPure -> [ Box.word "]" ]
  | TEffRow eff -> pretty_effrow' env eff
  | TEffCons(l, eff) ->
    Box.prefix (Box.word ",") (pretty_type env 0 l)
      :: pretty_effrow' env eff

(* ========================================================================= *)

let rec pretty_types_comma_sep env tps =
  match tps with
  | []     -> []
  | [ tp ] -> [ Box.ws (pretty_type env (tp_prec_arrow + 1) tp) ]
  | tp :: tps ->
    Box.ws (Box.suffix (pretty_type env (tp_prec_arrow + 1) tp)
      (Box.word ","))
    :: pretty_types_comma_sep env tps

let pretty_type_field env fld =
  match fld.data with
  | TField(name, tp) ->
    Box.box
    [ Box.suffix
        (Box.box [ Box.word "type"; Box.ws (Box.word name) ])
        (Box.ws (Box.word "="))
    ; Box.ws (Box.indent 2 (pretty_type env 0 tp))
    ]

(* ========================================================================= *)

let pretty_type_annot env annot =
  match annot with
  | Annot tp ->
    Box.suffix (Box.ws (Box.word ":")) (Box.ws (pretty_type env 0 tp))
  | AnnotEff(tp, eff) ->
    Box.box
    [ Box.suffix (Box.ws (Box.word ":")) (Box.ws (pretty_type env 0 tp))
    ; Box.suffix (Box.ws (Box.word "/")) (Box.ws (pretty_type env 0 eff))
    ]

let pretty_type_annot_opt env annot =
  match annot with
  | None -> Box.box []
  | Some annot -> pretty_type_annot env annot

(* ========================================================================= *)

let pretty_type_farg env arg =
  match arg.data with
  | TFA_Var x ->
    let (env, x) = Env.add_tconst env x in
    (env, Box.ws (Box.word x))
  | TFA_Annot(x, k) ->
    let (env, x) = Env.add_tconst env x in
    (env, Box.ws (Box.paren (Box.box
      [ Box.suffix (Box.word x)
          (Box.ws (Box.word ":"))
      ; Box.ws (pretty_kind 0 k)
      ])))

let pretty_type_fargs env args =
  Utils.ListExt.fold_map pretty_type_farg env args

let pretty_type_quantifier_opt env ets =
  match ets with
  | [] -> (env, Box.box [])
  | _  ->
    let (env, ets) = pretty_type_fargs env ets in
    (env, Box.ws (Box.suffix (Box.box
        (  Box.word "type"
        :: List.map (Box.indent 2) ets))
        (Box.word ",")))

(* ========================================================================= *)

let prepare_op_decl env op =
  match op.data with
  | OpDecl(op, _, _, _) ->
    fst (Env.add_op env op)

let pretty_op_decl env op =
  match op.data with
  | OpDecl(op, ets, tps, tp) ->
    let (env, ets) = pretty_type_quantifier_opt env ets in
    Box.box
    [ Box.suffix (Env.pretty_op env op)
        (Box.ws (Box.word ":"))
    ; Box.ws (Box.indent 2 (Box.box
      [ ets
      ; Box.suffix (Box.box (pretty_types_comma_sep env tps))
          (Box.ws (Box.word "=>"))
      ; Box.ws (Box.indent 2 (pretty_type env 0 tp))
      ]))
    ]

let rec pretty_op_decls' env ops =
  match ops with
  | [] -> [ Box.ws (Box.word "}") ]
  | op :: ops -> 
    Box.prefix (Box.word "; ") (pretty_op_decl env op) ::
    pretty_op_decls' env ops

let pretty_op_decls env ops =
  match ops with
  | [] -> [ Box.word "<empty_effect>" ]
  | op :: ops ->
    Box.prefix (Box.word "{ ") (pretty_op_decl env op) ::
    pretty_op_decls' env ops

(* ========================================================================= *)

let prepare_adt_ctor env ctor =
  match ctor.data with
  | ADTCtor(ctor, _, _) ->
    fst (Env.add_ctor env ctor)

let pretty_adt_ctor env ctor =
  match ctor.data with
  | ADTCtor(ctor, [], []) ->
    Box.ws (Box.prefix (Box.word "|")
      (Box.ws (Env.pretty_ctor env ctor)))
  | ADTCtor(ctor, ets, []) ->
    let (_, ets) = pretty_type_fargs env ets in
    Box.box
    [ Box.ws (Box.prefix (Box.word "|")
      (Box.suffix (Box.ws (Env.pretty_ctor env ctor))
        (Box.ws (Box.word "of"))))
    ; Box.ws (Box.indent 4 (Box.paren (Box.box
      (  Box.word "type"
      :: List.map (Box.indent 2) ets))))
    ]
  | ADTCtor(ctor, ets, tps) ->
    let (env, ets) = pretty_type_quantifier_opt env ets in
    Box.box
    [ Box.ws (Box.prefix (Box.word "|")
      (Box.suffix (Box.ws (Env.pretty_ctor env ctor))
        (Box.ws (Box.word "of"))))
    ; Box.indent 4 (Box.box
      (ets :: pretty_types_comma_sep env tps))
    ]

(* ========================================================================= *)

let prepare_field_decl env fld =
  match fld.data with
  | FieldDecl(fld, _, _) ->
    fst (Env.add_field env fld)

let pretty_field_decl env fld =
  match fld.data with
  | FieldDecl(fld, ets, tp) ->
    let (env, ets) = pretty_type_quantifier_opt env ets in
    Box.box
    [ Box.suffix (Env.pretty_field env fld)
        (Box.ws (Box.word ":"))
    ; Box.ws (Box.indent 2 (Box.box [ ets; pretty_type env 0 tp ]))
    ]

let rec pretty_field_decls' env flds =
  match flds with
  | [] -> [ Box.ws (Box.word "}") ]
  | fld :: flds ->
    Box.prefix (Box.word "; ") (pretty_field_decl env fld) ::
    pretty_field_decls' env flds

let pretty_field_decls env flds =
  match flds with
  | [] -> [ Box.word "<empty_record>" ]
  | fld :: flds ->
    Box.prefix (Box.word "{ ") (pretty_field_decl env fld) ::
    pretty_field_decls' env flds

(* ========================================================================= *)

let prepare_typedefs env tds =
  List.fold_left (fun env td ->
    match td.data with
    | TDEffect(x, _, ops) ->
      let env = fst (Env.add_tconst env x) in
      List.fold_left prepare_op_decl env ops
    | TDData(x, _, ctors) ->
      let env = fst (Env.add_tconst env x) in
      List.fold_left prepare_adt_ctor env ctors
    | TDRecord(x, _, fields) ->
      let env = fst (Env.add_tconst env x) in
      List.fold_left prepare_field_decl env fields
  ) env tds

let pretty_typedef env td =
  match td.data with
  | TDEffect(x, args, ops) ->
    let (env, args) = pretty_type_fargs env args in
    Box.box
    [ Box.box
      [ Box.box
        [ Box.word "effect"
        ; Box.ws (Box.indent 4 (Env.pretty_tconst env x))
        ]
      ; Box.suffix (Box.box args) (Box.ws (Box.word "="))
      ]
    ; Box.ws (Box.indent 2 (Box.box (pretty_op_decls env ops)))
    ]
  | TDData(x, args, ctors) ->
    let (env, args) = pretty_type_fargs env args in
    Box.box
    [ Box.box
      [ Box.box
        [ Box.word "type"
        ; Box.ws (Box.indent 4 (Env.pretty_tconst env x))
        ]
      ; Box.suffix (Box.box args) (Box.ws (Box.word "="))
      ]
    ; Box.box (List.map (pretty_adt_ctor env) ctors)
    ]
  | TDRecord(x, args, flds) ->
    let (env, args) = pretty_type_fargs env args in
    Box.box
    [ Box.box
      [ Box.box
        [ Box.word "type"
        ; Box.ws (Box.indent 4 (Env.pretty_tconst env x))
        ]
      ; Box.suffix (Box.box args) (Box.ws (Box.word "="))
      ]
    ; Box.ws (Box.indent 2 (Box.box (pretty_field_decls env flds)))
    ]

let rec pretty_typedefs' env tds =
  match tds with
  | [] -> []
  | td :: tds ->
    Box.ws (Box.word "and") ::
    Box.ws (pretty_typedef env td) ::
    pretty_typedefs' env tds

let pretty_typedefs env tds =
  let env = prepare_typedefs env tds in
  match tds with
  | [] -> (env, [ Box.word "<empty_typedefs>" ])
  | td :: tds ->
    (env, pretty_typedef env td :: pretty_typedefs' env tds)

(* ========================================================================= *)

let prec_stmt   = 0
let prec_if     = 10
let prec_annot  = 20
let prec_app    = 30
let prec_select = 40

(* ========================================================================= *)

let rec pretty_pattern env prec pat =
  match pat.data with
  | PWildcard -> (env, Box.word "_")
  | PVar x ->
    let (env, x) = Env.add_var env x in
    (env, Box.word x)
  | PCtor(c, [], []) -> (env, Env.pretty_ctor env c)
  | PCtor(c, ets, pats) ->
    let c = Env.pretty_ctor env c in
    let (env, pats) = pretty_patterns' env ets pats in
    (env, Box.prec_paren prec_app prec
      (Box.box [ c; Box.indent 2 (Box.box pats) ]))
  | PList [] -> (env, Box.word "[]")
  | PList(p :: ps) ->
    let (env, p)  = pretty_pattern env 0 p in
    let (env, ps) = Utils.ListExt.fold_map (fun env p ->
        pretty_pattern env 0 p
      ) env ps in
    let res =
      Box.box
      (( Box.prefix (Box.word "[") (Box.ws p)
      :: List.map (fun p ->
           Box.prefix (Box.word ";") (Box.ws p)
          ) ps
      ) @ [ Box.word "]" ])
    in (env, res)
  | PRecord flds ->
    let (env, bs) = pretty_record_pattern env flds in
    (env, Box.box bs)
  | PAnnot(p, tp) ->
    let (env, p) = pretty_pattern env (prec_annot + 1) p in
    (env, Box.prec_paren prec_annot prec (Box.box
      [ Box.suffix p (Box.ws (Box.word ":"))
      ; Box.ws (Box.indent 2 (pretty_type env 0 tp))
      ]))

and pretty_patterns env pats =
  Utils.ListExt.fold_map (fun env p ->
    let (env, b) = pretty_pattern env (prec_app + 1) p in
    (env, Box.ws b)
  ) env pats

and pretty_patterns' env ets pats =
  let (env, ets)  = pretty_type_fargs env ets in
  let (env, pats) = pretty_patterns env pats in
  let pats =
    match ets with
    | [] -> pats
    | _  -> Box.ws (Box.paren (Box.box
      (  Box.word "type"
      :: List.map (fun b -> Box.ws (Box.indent 2 b)) ets
      ))) :: pats
  in
  (env, pats)

and pretty_record_pattern env flds =
  match flds with
  | []          -> (env, [ Box.word "<empty_record>" ])
  | fld :: flds ->
    let (env, fld)  = pretty_field_pattern env fld in
    let (env, flds) = pretty_record_pattern' env flds in
    (env, Box.prefix (Box.word "{ ") fld :: flds)

and pretty_record_pattern' env flds =
  match flds with
  | []          -> (env, [ Box.ws (Box.word "}") ])
  | fld :: flds ->
    let (env, fld)  = pretty_field_pattern env fld in
    let (env, flds) = pretty_record_pattern' env flds in
    (env, Box.prefix (Box.word "; ") fld :: flds)

and pretty_field_pattern env fld =
  match fld.data with
  | FieldPat(fld, p) ->
    let (env, p) = pretty_pattern env 0 p in
    (env, Box.box
    [ Box.suffix (Env.pretty_field env fld)
        (Box.ws (Box.word "="))
    ; Box.ws (Box.indent 2 p)
    ])

(* ========================================================================= *)

let pretty_string str =
  Box.word ("\"" ^ String.escaped str ^ "\"")

let rec pretty_expr env prec e =
  match e.data with
  | EVar  x -> Env.pretty_var  env x
  | EOp   x -> Env.pretty_op   env x
  | ECtor x -> Env.pretty_ctor env x
  | EModule(c, tfs) ->
    Box.box
    (  Box.box [ Box.word "module"; Box.ws (Env.pretty_ctor env c) ]
    :: List.map (fun tf ->
        Box.ws (Box.indent 2 (pretty_type_field env tf))) tfs
    @ [ Box.ws (Box.word "end") ])
  | ENum n -> Box.word (string_of_int n)
  | EChar c -> Box.word ("'" ^ Char.escaped c ^ "'")
  | EString s -> pretty_string s
  | EList [] -> Box.word "[]"
  | EList(e :: es) ->
    Box.box
    (( Box.prefix (Box.word "[") (Box.ws (pretty_expr env 0 e))
    :: List.map (fun e ->
         Box.prefix (Box.word ";") (Box.ws (pretty_expr env 0 e))
        ) es
    ) @ [ Box.word "]" ])
  | EFun(pats, annot, body) ->
    let (env, pats) = pretty_patterns env pats in
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.word "fn"
      ; Box.ws (Box.indent 2 (Box.box pats))
      ; Box.ws (Box.indent 2 (pretty_type_annot_opt env annot))
      ; Box.ws (Box.word "=>")
      ]
    ; Box.ws (Box.indent 2 (pretty_expr env prec_stmt body))
    ])
  | EApp(e1, e2) ->
    Box.prec_paren prec_app prec (Box.box
      [ pretty_expr env prec_app e1
      ; Box.ws (Box.indent 2 (pretty_expr env (prec_app + 1) e2))
      ])
  | ELet(x, ets, pats, annot, e1, e2) ->
    let (env2, x)    = Env.add_var env x in
    let (env1, pats) = pretty_patterns' env ets pats in
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.suffix (Box.box
        [ Box.box
          [ Box.box [ Box.word "let"; Box.ws (Box.word x) ]
          ; Box.indent 4 (Box.box pats)
          ]
        ; Box.ws (pretty_type_annot_opt env1 annot)
        ]) (Box.ws (Box.word "="))
      ; Box.ws (Box.indent 2 (pretty_expr env1 0 e1))
      ; Box.ws (Box.word "in")
      ]
    ; Box.ws (pretty_expr env2 prec_stmt e2)
    ])
  | ELetPat(p, annot, e1, e2) ->
    let (env2, p) = pretty_pattern env 0 p in
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.suffix (Box.box
        [ Box.box [ Box.word "letpat"; Box.ws p ]
        ; Box.ws (pretty_type_annot_opt env annot)
        ]) (Box.ws (Box.word "="))
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e1))
      ; Box.ws (Box.word "in")
      ]
    ; Box.ws (pretty_expr env2 prec_stmt e2)
    ])
  | ELetRec([], e) ->
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.word "let"; Box.ws (Box.word "rec"); Box.ws (Box.word "in") ]
    ; Box.ws (pretty_expr env prec_stmt e)
    ])
  | ELetRec(rf :: rfs, e) ->
    let env = prepare_rec_functions env (rf :: rfs) in
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      (  pretty_rec_function
          (Box.box [ Box.word "let"; Box.ws (Box.word "rec") ])
          env rf
      :: List.map (pretty_rec_function (Box.ws (Box.word "and")) env) rfs
      @  [ Box.ws (Box.word "in") ])
    ; Box.ws (pretty_expr env prec_stmt e)
    ])
  | EUIf(e1, e2) ->
    Box.prec_paren prec_if prec (Box.box
    [ Box.box
      [ Box.word "if"
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e1))
      ; Box.ws (Box.word "then")
      ]
    ; Box.ws (Box.indent 2 (pretty_expr env prec_if e2))
    ])
  | EIf(e1, e2, e3) ->
    Box.prec_paren prec_if prec (Box.box
    ( Box.box
      [ Box.box
        [ Box.word "if"
        ; Box.ws (Box.indent 2 (pretty_expr env 0 e1))
        ; Box.ws (Box.word "then")
        ]
      ; Box.ws (Box.indent 2 (pretty_expr_with_else env e2))
      ] :: pretty_expr_else env e3))
  | EHandle(e, hs) ->
    Box.box
    ( Box.box
      [ Box.word "handle"
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e))
      ; Box.ws (Box.word "with")
      ]
    :: List.map (pretty_handler env) hs
    @ [ Box.ws (Box.word "end") ])
  | EHandleWith(e1, e2) ->
    Box.box
    [ Box.box
      [ Box.word "handle"
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e1))
      ]
    ; Box.box
      [ Box.ws (Box.word "with")
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e2))
      ; Box.ws (Box.word "end")
      ]
    ]
  | EHandler hs ->
    Box.box
    ( Box.word "handle"
    :: List.map (pretty_handler env) hs
    @ [ Box.ws (Box.word "end") ])
  | EMatch(e, cls) ->
    Box.box
    ( Box.box
      [ Box.word "match"
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e))
      ; Box.ws (Box.word "with")
      ]
    :: List.map (pretty_match_clause env) cls
    @ [ Box.ws (Box.word "end") ])
  | ESelect(e, fld) ->
    Box.prec_paren prec_select prec (Box.suffix
      (pretty_expr env prec_select e)
      (Box.box [ Box.word "."; (Env.pretty_field env fld) ]))
  | ESelectF(e, fld, tfs) ->
    Box.prec_paren prec_select prec (Box.box
    [ pretty_expr env prec_select e
    ; Box.prefix (Box.word ".")
      (Box.box
      (  Box.box [ Box.word "functor"; Box.ws (Env.pretty_field env fld) ]
      :: List.map (fun tf ->
          Box.ws (Box.indent 2 (pretty_type_field env tf))) tfs
      @ [ Box.ws (Box.word "end") ]))
    ])
  | ERecord flds ->
    Box.box (pretty_record env flds)
  | ETypedef(tds, e) ->
    let (env, tds) = pretty_typedefs env tds in
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box tds
      ; Box.ws (Box.word "in")
      ]
    ; Box.ws (pretty_expr env prec_stmt e)
    ])
  | ETypeAlias(x, args, tp, e) ->
    let (env1, x) = Env.add_tconst env x in
    let (env2, args) = pretty_type_fargs env args in
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box
        [ Box.box [ Box.word "type"; Box.ws (Box.word x) ]
        ; Box.indent 4 (Box.suffix (Box.box args) (Box.ws (Box.word ":=")))
        ]
      ; Box.ws (Box.indent 2 (pretty_type env 0 tp))
      ; Box.ws (Box.word "in")
      ]
    ; pretty_expr env1 prec_stmt e
    ])
  | EAbsType(x, kind, e) ->
    let (env, x) = Env.add_tconst env x in
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box
        [ Box.word "type"
        ; Box.box [ Box.ws (Box.word x); Box.ws (Box.word ":") ]
        ]
      ; Box.ws (Box.indent 2 (pretty_kind 0 kind))
      ; Box.ws (Box.word "in")
      ]
    ; pretty_expr env prec_stmt e
    ])
  | EAnnot(e, annot) ->
    Box.prec_paren prec_annot prec (Box.box
    [ pretty_expr env (prec_annot+1) e
    ; Box.indent 2 (pretty_type_annot env annot)
    ])
  | EExtern(name, ets, tp) ->
    let (env, ets) = pretty_type_quantifier_opt env ets in
    Box.prec_paren prec_annot prec (Box.box
    [ Box.box
      [ Box.word "extern"
      ; Box.ws (Box.indent 4 (Box.suffix
          (Box.word name)
          (Box.ws (Box.word ":"))))
      ]
    ; Box.ws (Box.indent 2 (Box.box [ ets; pretty_type env 0 tp ]))
    ])
  | EPragmaFlag(flag, e) ->
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box
        [ Box.word "pragma"
        ; Box.ws (Box.indent 4 (pretty_string flag))
        ]
      ; Box.ws (Box.word "in")
      ]
    ; Box.ws (pretty_expr env prec_stmt e)
    ])
  | EPragmaVal(flag, x, e) ->
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box
        [ Box.box
          [ Box.box [ Box.word "pragma"; Box.ws (Box.word "val") ]
          ; Box.ws (Box.indent 4
              (Box.suffix (pretty_string flag) (Box.word ":")))
          ]
        ; Box.ws (Box.indent 2 (Env.pretty_var env x))
        ]
      ; Box.ws (Box.word "in")
      ]
    ; Box.ws (pretty_expr env prec_stmt e)
    ])
  | EPragmaType(flag, x, e) ->
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box
        [ Box.box
          [ Box.box [ Box.word "pragma"; Box.ws (Box.word "type") ]
          ; Box.ws (Box.indent 4
              (Box.suffix (pretty_string flag) (Box.word ":")))
          ]
        ; Box.ws (Box.indent 2 (Env.pretty_tconst env x))
        ]
      ; Box.ws (Box.word "in")
      ]
    ; Box.ws (pretty_expr env prec_stmt e)
    ])
  | EReplExpr(e, _) ->
    Box.prec_paren prec_app prec (Box.box
      [ Box.word "$REPL_EXPR"
      ; Box.ws (Box.indent 2 (pretty_expr env (prec_app + 1) e))
      ])
  | ERepl e ->
    Box.word "$REPL"

and pretty_expr_with_else env e =
  match e.data with
  | EUIf _ -> Box.paren (pretty_expr env 0 e)
  | EIf(e1, e2, e3) ->
    Box.box (
    Box.box
    [ Box.box
      [ Box.word "if"
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e1))
      ; Box.ws (Box.word "then")
      ]
    ; Box.ws (Box.indent 2 (pretty_expr_with_else env e2))
    ] :: pretty_expr_else_with_else env e3)
  | _ -> pretty_expr env prec_if e

and pretty_expr_else env e =
  match e.data with
  | EUIf(e1, e2) ->
    Box.box
    [ Box.box
      [ Box.ws (Box.word "elif")
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e1))
      ; Box.ws (Box.word "then")
      ]
    ; Box.ws (Box.indent 2 (pretty_expr env prec_if e2))
    ] :: []
  | EIf(e1, e2, e3) ->
    Box.box
    [ Box.box
      [ Box.ws (Box.word "elif")
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e1))
      ; Box.ws (Box.word "then")
      ]
    ; Box.ws (Box.indent 2 (pretty_expr_with_else env e2))
    ] :: pretty_expr_else env e3
  | _ ->
    Box.box
    [ Box.ws (Box.word "else")
    ; Box.ws (Box.indent 2 (pretty_expr env prec_if e))
    ] :: []

and pretty_expr_else_with_else env e =
  match e.data with
  | EIf(e1, e2, e3) ->
    Box.box
    [ Box.box
      [ Box.ws (Box.word "elif")
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e1))
      ; Box.ws (Box.word "then")
      ]
    ; Box.ws (Box.indent 2 (pretty_expr_with_else env e2))
    ] :: pretty_expr_else_with_else env e3
  | _ ->
    Box.box
    [ Box.ws (Box.word "else")
    ; Box.ws (Box.indent 2 (pretty_expr_with_else env e))
    ] :: []

and prepare_rec_functions env rfs =
  List.fold_left (fun env rf ->
    match rf.data with
    | RecFunc(x, _, _, _, _) ->
      fst (Env.add_var env x)
  ) env rfs

and pretty_rec_function kw env rf =
  match rf.data with
  | RecFunc(x, ets, pats, annot, body) ->
    let (env, pats) = pretty_patterns' env ets pats in
    Box.box
    [ Box.suffix (Box.box
      [ Box.box
        [ Box.box [ kw; Box.ws (Env.pretty_var env x) ]
        ; Box.indent 4 (Box.box pats)
        ]
      ; Box.ws (pretty_type_annot_opt env annot)
      ]) (Box.ws (Box.word "="))
    ; Box.ws (Box.indent 2 (pretty_expr env 0 body))
    ]

and pretty_handler env h =
  match h.data with
  | HReturn(p, body) ->
    let (env, p) = pretty_pattern env 0 p in
    Box.box
    [ Box.box
      [ Box.prefix (Box.word "| ") (Box.word "return")
      ; Box.ws (Box.indent 4 (Box.suffix (Box.ws p)
          (Box.ws (Box.word "=>"))))
      ]
    ; Box.ws (Box.indent 2 (pretty_expr env 0 body))
    ]
  | HOp(op, ets, pats, rx, body) ->
    let (env, pats) = pretty_patterns' env ets pats in
    let (env, rx) = Env.add_var env rx in
    Box.box
    [ Box.box
      [ Box.prefix (Box.word "| ") (Env.pretty_op env op)
      ; Box.ws (Box.indent 4 (Box.box
        [ Box.suffix (Box.box pats) (Box.ws (Box.word "/"))
        ; Box.suffix
            (Box.ws (Box.word rx))
            (Box.ws (Box.word "=>"))
        ]))
      ]
    ; Box.ws (Box.indent 2 (pretty_expr env 0 body))
    ]

and pretty_match_clause env cl =
  match cl.data with
  | Clause(pat, body) ->
    let (env, pat) = pretty_pattern env 0 pat in
    Box.box
    [ Box.suffix (Box.box
        [ Box.ws (Box.word "|")
        ; Box.ws pat
        ])
      (Box.ws (Box.word "=>"))
    ; Box.ws (Box.indent 2 (pretty_expr env 0 body))
    ]

and pretty_record env flds =
  match flds with
  | []          -> [ Box.word "<empty_record>" ]
  | fld :: flds ->
    Box.prefix (Box.word "{ ")
      (pretty_field_def env fld)
    :: pretty_record' env flds

and pretty_record' env flds =
  match flds with
  | []          -> [ Box.ws (Box.word "}") ]
  | fld :: flds ->
    Box.prefix (Box.word "; ")
      (pretty_field_def env fld)
    :: pretty_record' env flds

and pretty_field_def env fld =
  match fld.data with
  | FieldDef(fld, e) ->
    Box.box
    [ Box.suffix (Env.pretty_field env fld)
        (Box.ws (Box.word "="))
    ; Box.ws (Box.indent 2 (pretty_expr env 0 e))
    ]
  | FieldDefM(_, fld, e) ->
    Box.box
    [ Box.suffix (Box.box
      [ Box.word "val"
      ; Box.ws (Env.pretty_field env fld)
      ]) (Box.ws (Box.word "="))
    ; Box.ws (Box.indent 2 (pretty_expr env 0 e))
    ]

let pretty_program e =
  pretty_expr (Env.init ()) 0 e

let _ =
  Flow.register_box_printer 
    ~source:flow_node
    pretty_program
