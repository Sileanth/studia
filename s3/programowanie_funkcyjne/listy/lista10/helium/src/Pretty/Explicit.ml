open Lang.Node
open Lang.Explicit
open Common

include CoreCommon

let prec_app    = 100
let prec_extern = 50
let prec_stmt   = 0

let rec pretty_eff_coercion env c =
  match c.data with
  | ECId eff   -> Box.box [Box.word "id"; Box.ws (Box.indent 2 (pretty_type env 6 eff))]
  | ECLift l -> Box.box
    [ Box.word "lift"
    ; Box.ws (Box.indent 2 (pretty_type env 6 l))
    ]
  | ECSwap(l1, l2) -> Box.box
    [ Box.box
      [ Box.word "swap"
      ; Box.ws (Box.indent 2 (pretty_type env 0 l1))
      ]
    ; Box.box
      [ Box.ws (Box.word "with")
      ; Box.ws (Box.indent 2 (pretty_type env 0 l2))
      ]
    ]
  | ECConsL(l, c) ->
    Box.box
    [ Box.suffix (pretty_type env 0 l) (Box.word " ·")
    ; pretty_eff_coercion env c
    ]
  | ECConsR(c, l) ->
    Box.box
    [ Box.suffix (pretty_eff_coercion env c) (Box.word " ·")
    ; pretty_type env 0 l
    ]
  | ECComp(c1, c2) ->
    Box.paren (Box.box
    [ Box.suffix (pretty_eff_coercion env c1) (Box.word ";")
    ; pretty_eff_coercion env c2
    ])

let rec pretty_val_coercion env prec c =
  match c.data with
  | VCId -> Box.word "id"
  | VCArrow(c1, c2, c3) ->
    Box.prec_paren 0 prec (Box.box
    [ Box.suffix (pretty_val_coercion env 1 c1)
        (Box.ws (Box.box
        [ (Box.word "->")
        ; Box.indent 2 (Box.brackets (pretty_eff_coercion env c3))
        ]))
    ; Box.ws (pretty_val_coercion env 0 c2)
    ])

let prepare_rec_functions env rfs =
  List.fold_left (fun env (x, _) ->
    fst (Env.add_var env x)
  ) env rfs

let rec pretty_expr env prec e =
  match e.data with
  | ENum n -> Box.word (string_of_int n)
  | EChar c -> Box.word ("'" ^ Char.escaped c ^ "'")
  | EString s -> Box.word ("\"" ^ String.escaped s ^ "\"")
  | EVar x -> Env.pretty_var env x
  | EFun _ | ETFun _ ->
    Box.prec_paren prec_stmt prec (pretty_fun env e)
  | EOp(neu, n, targs, args) ->
    Box.prec_paren prec_app prec (Box.box
    [ Box.suffix
        (Box.brackets (pretty_type env 0 (Type.of_neutral neu)))
        (Box.word ("@" ^ string_of_int n))
    ; Box.indent 2 (Box.box 
      (List.map (fun (Type.Pack tp) ->
          pretty_type_arg env tp
        ) targs))
    ; Box.indent 2 (pretty_ctor_args env args)
    ])
  | ECtor(neu, n, targs, args) ->
    Box.prec_paren prec_app prec (Box.box
    [ Box.suffix
        (Box.brackets (pretty_type env 0 (Type.of_neutral neu)))
        (Box.word ("@" ^ string_of_int n))
    ; Box.indent 2 (Box.box 
      (List.map (fun (Type.Pack tp) ->
          pretty_type_arg env tp
        ) targs))
    ; Box.indent 2 (pretty_ctor_args env args)
    ])
  | EApp(e1, e2) ->
    Box.prec_paren prec_app prec (Box.box
      [ pretty_expr env prec_app e1
      ; Box.ws (Box.indent 2 (pretty_expr env (prec_app + 1) e2))
      ])
  | ETApp(e, tp) ->
    Box.prec_paren prec_app prec (Box.box
      [ pretty_expr env prec_app e
      ; Box.ws (Box.indent 2 (pretty_type_arg env tp))
      ])
  | ELet(x, e1, e2) ->
    let (env', name) = Env.add_var env x in
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box
        [ Box.box
          [ Box.word "let"
          ; Box.ws (Box.indent 2 (Box.suffix
              (Box.word name)
              (Box.ws (Box.word ":"))))
          ]
        ; Box.ws (Box.indent 4 (Box.suffix
            (pretty_type env 0 (Var.type_of x))
            (Box.ws (Box.word "="))))
        ]
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e1))
      ; Box.ws (Box.word "in")
      ]
    ; Box.ws (pretty_expr env' 0 e2)
    ])
  | ELetPure(x, e1, e2) ->
    let (env', name) = Env.add_var env x in
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box
        [ Box.box
          [ Box.box [ Box.word "let"; Box.ws (Box.word "pure") ]
          ; Box.ws (Box.indent 2 (Box.suffix
              (Box.word name)
              (Box.ws (Box.word ":"))))
          ]
        ; Box.ws (Box.indent 4 (Box.suffix
            (pretty_type env 0 (Var.type_of x))
            (Box.ws (Box.word "="))))
        ]
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e1))
      ; Box.ws (Box.word "in")
      ]
    ; Box.ws (pretty_expr env' 0 e2)
    ])
  | EFix([], e) ->
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box [ Box.word "fix"; Box.ws (Box.word "in") ]
    ; Box.ws (pretty_expr env 0 e)
    ])
  | EFix(rf :: rfs, e) ->
    let env = prepare_rec_functions env (rf :: rfs) in
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box (pretty_rec_function env "fix" rf
        :: List.map (pretty_rec_function env "and") rfs)
      ; Box.ws (Box.word "in")
      ]
    ; Box.ws (pretty_expr env 0 e)
    ])
  | EHandle(neu, e, xr, rbody, hs) ->
    Box.box
    [ Box.box
      [ Box.box
        [ Box.word "handle"
        ; Box.indent 2 (Box.brackets (pretty_type env 0 (Type.of_neutral neu)))
        ]
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e))
      ; Box.ws (Box.word "with")
      ]
    ; Box.box
      ( let (env, xr) = Env.add_var env xr in
        Box.box
        [ Box.box
          [ Box.prefix (Box.word "| ") (Box.word "return")
          ; Box.ws (Box.indent 4 (Box.suffix
              (Box.ws (Box.word xr))
              (Box.ws (Box.word "=>"))))
          ]
        ; Box.ws (Box.indent 2 (pretty_expr env 0 rbody))
        ] ::
      List.mapi (fun i (tvs, xs, r, body) ->
        let (env, tvs)  = pretty_ctor_clause_tvars env tvs in
        let (env, vars) = pretty_ctor_clause_vars env xs in
        let (env, r) = Env.add_var env r in
        Box.box
        [ Box.box
          [ Box.prefix (Box.word "| ") (Box.word ("@" ^ string_of_int i))
          ; Box.indent 4 tvs
          ; Box.ws (Box.indent 4 (Box.box
            [ Box.suffix tvs (Box.ws (Box.word "/"))
            ; Box.suffix
                (Box.ws (Box.word r))
                (Box.ws (Box.word "=>"))
            ]))
          ]
        ; Box.ws (Box.indent 2 (pretty_expr env 0 body))
        ]
      ) hs)
    ; Box.ws (Box.word "end")
    ]
  | EMatch(neu, e, clauses, _) ->
    Box.box
    [ Box.box
      [ Box.box
        [ Box.word "match"
        ; Box.indent 2 (Box.brackets (pretty_type env 0 (Type.of_neutral neu)))
        ]
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e))
      ; Box.ws (Box.word "with")
      ]
    ; Box.box(
      List.mapi (fun i (tvs, xs, body) ->
        let (env, tvs)  = pretty_ctor_clause_tvars env tvs in
        let (env, vars) = pretty_ctor_clause_vars env xs in
        Box.box
        [ Box.box
          [ Box.prefix (Box.word "| ") (Box.word ("@" ^ string_of_int i))
          ; Box.indent 4 tvs
          ; Box.suffix (Box.indent 4 vars) (Box.ws (Box.word "=>"))
          ]
        ; Box.ws (Box.indent 2 (pretty_expr env 0 body))
        ]
      ) clauses)
    ; Box.ws (Box.word "end")
    ]
  | ETypedef(tds, e) ->
    let env = prepare_typedefs env tds in
    Box.box
    [ Box.box
      [ Box.box (pretty_typedefs env tds)
      ; Box.ws (Box.word "in")
      ]
    ; Box.ws (pretty_expr env 0 e)
    ]
  | EAbsType(x, e) ->
    let kind = TVar.kind x in
    let (env, x) = Env.add_tvar env x in
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
  | ECoerce(vc, ec, e) ->
    Box.prec_paren prec_app prec (Box.box
    [ Box.box
      [ Box.prefix (Box.word "<") (pretty_val_coercion env 0 vc)
      ; Box.prefix (Box.word "/")
          (Box.suffix (pretty_eff_coercion env ec) (Box.word ">"))
      ]
    ; Box.ws (pretty_expr env (prec_app+1) e)
    ])
  | EExtern(name, tp) ->
    Box.prec_paren prec_extern prec (Box.box
    [ Box.box
      [ Box.word "extern"
      ; Box.ws (Box.indent 4 (Box.suffix
          (Box.word name)
          (Box.ws (Box.word ":"))))
      ]
    ; Box.ws (Box.indent 2 (pretty_type env 0 tp))
    ])
  | EReplExpr(e, _, _, _, _) ->
    Box.prec_paren prec_app prec (Box.box
      [ Box.word "$REPL_EXPR"
      ; Box.ws (Box.indent 2 (pretty_expr env (prec_app + 1) e))
      ])
  | ERepl(e, _, _, _) ->
    Box.word "$REPL"

and pretty_fun env e =
  let (args, body) = pretty_fun_aux env e in
  Box.box
  [ Box.box
    [ Box.word "fn"
    ; Box.ws (Box.indent 2 (Box.box args))
    ; Box.ws (Box.word "=>")
    ]
  ; Box.ws (Box.indent 2 body)
  ]

and pretty_fun_aux env e =
  match e.data with
  | EFun(x, body) ->
    let (env', name) = Env.add_var env x in
    let (args, body) = pretty_fun_aux env' body in
    let arg =
      Box.ws (Box.paren (Box.box
      [ Box.suffix (Box.word name) (Box.ws (Box.word ":"))
      ; Box.ws (pretty_type env 0 (Var.type_of x))
      ]))
    in (arg :: args, body)
  | ETFun(x, body) ->
    let (env', name) = Env.add_tvar env x in
    let (args, body) = pretty_fun_aux env' body in
    let arg =
      Box.ws (Box.brackets (Box.box
      [ Box.suffix (Box.word name) (Box.ws (Box.word "::"))
      ; Box.ws (pretty_kind 0 (TVar.kind x))
      ]))
    in (arg :: args, body)
  | _ -> ([], pretty_expr env 0 e)

and pretty_ctor_args env es =
  match es with
  | [] -> Box.box []
  | e :: es ->
    Box.suffix 
      (Box.box
      (  Box.prefix (Box.word "(") (pretty_expr env 0 e)
      :: List.map (fun e -> Box.prefix (Box.word ",")
            (Box.ws (pretty_expr env 0 e))) es))
      (Box.word ")")

and pretty_ctor_clause_vars env vars =
  match vars with
  | [] -> (env, Box.box [])
  | x :: vars ->
    let rec loop env acc vars =
      match vars with
      | [] -> (env, List.rev acc)
      | x :: vars ->
        let (env, x) = Env.add_var env x in
        loop env (Box.prefix (Box.word ",") (Box.word x) :: acc) vars
    in
    let (env, x) = Env.add_var env x in
    let (env, bs) = loop env [Box.prefix (Box.word "(") (Box.word x)] vars in
    (env, Box.suffix (Box.box bs) (Box.word ")"))

and pretty_ctor_clause_tvars env tvs =
  let (env, bs) = Utils.ListExt.fold_map (fun env (TVar.Pack x) ->
    let (env, b) = Env.add_tvar env x in
    (env, Box.brackets (Box.word b))
  ) env tvs in
  (env, Box.box bs)

and pretty_rec_function env kw (x, e) =
  let name = Env.pretty_var env x in
  Box.box
  [ Box.box
    [ Box.box
      [ Box.word kw
      ; Box.ws (Box.indent 2 (Box.suffix name
          (Box.ws (Box.word ":"))))
      ]
    ; Box.ws (Box.indent 4 (Box.suffix
        (pretty_type env 0 (Var.type_of x))
        (Box.ws (Box.word "="))))
    ]
  ; Box.ws (Box.indent 2 (pretty_expr env 0 e))
  ]

let pretty_program e =
  pretty_expr (Env.init ()) 0 e

let _ =
  Flow.register_box_printer 
    ~source:flow_node
    pretty_program
