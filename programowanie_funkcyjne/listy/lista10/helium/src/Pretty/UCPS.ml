open Lang.Node
open Lang.UCPS
open Common

module Env : sig
  type t

  val init : unit -> t

  val add_var  : t -> var  -> t * string
  val add_lvar : t -> lvar -> t * string

  val pretty_var  : t -> var -> Box.t
  val pretty_lvar : t -> lvar -> Box.t
end = struct
  type t =
    { names    : StrSet.t
    ; var_env  : string Var.Map.t
    ; lvar_env : string LVar.Map.t
    }

  let init () = (* TODO *)
    { names    = StrSet.empty
    ; var_env  = Var.Map.empty
    ; lvar_env = LVar.Map.empty
    }

  let add_var env x =
    let name = fresh_name env.names (Var.name x) in
    { env with
      names   = StrSet.add name env.names
    ; var_env = Var.Map.add x name env.var_env
    }, name

  let add_lvar env x =
    let name = fresh_name env.names ("$" ^ LVar.name x) in
    { env with
      names    = StrSet.add name env.names
    ; lvar_env = LVar.Map.add x name env.lvar_env
    }, name

  let pretty_var env x =
    match Var.Map.find_opt x env.var_env with
    | None   -> Box.word (Printf.sprintf "<unbound:%s>" (Var.name x))
    | Some n -> Box.word n

  let pretty_lvar env x =
    match LVar.Map.find_opt x env.lvar_env with
    | None   -> Box.word (Printf.sprintf "<unbound:$%s>" (LVar.name x))
    | Some n -> Box.word n
end

let prec_stmt   = 0
let prec_extern = 10
let prec_cons   = 20
let prec_app    = 30

let pretty_string str =
  Box.word ("\"" ^ String.escaped str ^ "\"")

let prepare_rec_functions env rfs =
  List.fold_left (fun env (x, _, _) ->
    fst (Env.add_var env x)
  ) env rfs

let pretty_ctor_clause_vars env vars =
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

let rec pretty_expr env prec e =
  match e.data with
  | EEffPure -> Box.word "[pure]"
  | EEffId l -> Env.pretty_lvar env l
  | EEffCons(eff, e) ->
    Box.box
      (pretty_effect env eff
        [ Box.prefix (Box.word ",")
          (Box.suffix (Box.ws (pretty_expr env 0 e)) (Box.word "]")) ])
  | ENum n -> Box.word (string_of_int n)
  | EChar c -> Box.word ("'" ^ Char.escaped c ^ "'")
  | EString s -> pretty_string s
  | EVar x -> Env.pretty_var env x
  | EFun _ ->
    Box.prec_paren prec_stmt prec (pretty_fun env e)
  | EOp(l, n, args, e) ->
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box 
        [ Box.word "raise"
        ; Box.indent 2 (Box.suffix
            (Box.ws (Env.pretty_lvar env l))
            (Box.word ("@" ^ string_of_int n)))
        ]
      ; Box.indent 2 (pretty_ctor_args env args)
      ]
    ; Box.box
      [ Box.ws (Box.word "then")
      ; Box.ws (Box.indent 2 (pretty_expr env prec_stmt e))
      ]
    ])
  | ECtor(n, args) ->
    Box.prec_paren prec_app prec (Box.box
    [ Box.suffix
        (Box.word "cons")
        (Box.word ("@" ^ string_of_int n))
    ; Box.indent 2 (pretty_ctor_args env args)
    ])
  | EApp(e1, e2) ->
    Box.prec_paren prec_app prec (Box.box
      [ pretty_expr env prec_app e1
      ; Box.ws (Box.indent 2 (pretty_expr env (prec_app + 1) e2))
      ])
  | ELet(x, e1, e2) ->
    let (env', name) = Env.add_var env x in
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box
        [ Box.word "let"
        ; Box.ws (Box.indent 4 (Box.box
          [ Box.word name; Box.ws (Box.word "=") ]))
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
  | EPureHandle(l, e, return_cl, hs) ->
    Box.box
    [ Box.box
      [ Box.box
        [ Box.word "handle"
        ; Box.indent 2 (Box.brackets (Env.pretty_lvar env l))
        ]
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e))
      ; Box.ws (Box.word "with")
      ]
    ; Box.box
      ( let (env, xr) = Env.add_var env return_cl.pr_arg in
        Box.box
        [ Box.box
          [ Box.prefix (Box.word "| ") (Box.word "return")
          ; Box.ws (Box.indent 4 (Box.suffix
              (Box.ws (Box.word xr))
              (Box.ws (Box.word "=>"))))
          ]
        ; Box.ws (Box.indent 2 (pretty_expr env 0 return_cl.pr_body))
        ] ::
      List.mapi (fun i h ->
        let (env, vars) = pretty_ctor_clause_vars env h.ph_args in
        let (env, r) = Env.add_var env h.ph_res in
        Box.box
        [ Box.box
          [ Box.prefix (Box.word "| ") (Box.word ("@" ^ string_of_int i))
          ; Box.ws (Box.indent 4 (Box.box
            [ Box.suffix vars (Box.ws (Box.word "/"))
            ; Box.suffix
                (Box.ws (Box.word r))
                (Box.ws (Box.word "=>"))
            ]))
          ]
        ; Box.ws (Box.indent 2 (pretty_expr env 0 h.ph_body))
        ]
      ) hs)
    ; Box.ws (Box.word "end")
    ]
  | EHandle(l, e, return_cl, hs, ke) ->
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box
        [ Box.word "handle"
        ; Box.indent 2 (Box.brackets (Env.pretty_lvar env l))
        ]
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e))
      ; Box.ws (Box.word "with")
      ]
    ; Box.box
      ( let (env, xr) = Env.add_var env return_cl.r_arg in
        let (env, xk) = Env.add_var env return_cl.r_cont in
        Box.box
        [ Box.box
          [ Box.prefix (Box.word "| ") (Box.word "return")
          ; Box.ws (Box.indent 4 (Box.box
            [ Box.suffix (Box.ws (Box.word xr)) (Box.ws (Box.word "/"))
            ; Box.suffix (Box.ws (Box.word xk)) (Box.ws (Box.word "=>"))
            ]))
          ]
        ; Box.ws (Box.indent 2 (pretty_expr env 0 return_cl.r_body))
        ] ::
      List.mapi (fun i h ->
        let (env, vars) = pretty_ctor_clause_vars env h.h_args in
        let (env, r) = Env.add_var env h.h_res in
        let (env, k) = Env.add_var env h.h_cont in
        Box.box
        [ Box.box
          [ Box.prefix (Box.word "| ") (Box.word ("@" ^ string_of_int i))
          ; Box.ws (Box.indent 4 (Box.box
            [ Box.suffix vars (Box.ws (Box.word "/"))
            ; Box.suffix
                (Box.ws (Box.word r))
                (Box.ws (Box.word "/"))
            ; Box.suffix
                (Box.ws (Box.word k))
                (Box.ws (Box.word "=>"))
            ]))
          ]
        ; Box.ws (Box.indent 2 (pretty_expr env 0 h.h_body))
        ]
      ) hs)
    ; Box.box
      [ Box.ws (Box.word "then")
      ; Box.ws (Box.indent 2 (pretty_expr env prec_stmt ke))
      ]
    ])
  | EMatch(e, clauses) ->
    Box.box
    [ Box.box
      [ Box.word "match"
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e))
      ; Box.ws (Box.word "with")
      ]
    ; Box.box(
      List.mapi (fun i (xs, body) ->
        let (env, vars) = pretty_ctor_clause_vars env xs in
        Box.box
        [ Box.box
          [ Box.prefix (Box.word "| ") (Box.word ("@" ^ string_of_int i))
          ; Box.suffix (Box.indent 4 vars) (Box.ws (Box.word "=>"))
          ]
        ; Box.ws (Box.indent 2 (pretty_expr env 0 body))
        ]
      ) clauses)
    ; Box.ws (Box.word "end")
    ]
  | EDone e ->
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.word "done"
    ; Box.ws (Box.indent 2 (pretty_expr env prec_stmt e))
    ])
  | ENewEffect(l, e) ->
    let (env, name) = Env.add_lvar env l in
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.word "new"
      ; Box.ws (Box.word name)
      ; Box.ws (Box.word "in")
      ]
    ; Box.ws (pretty_expr env prec_stmt e)
    ])
  | EIfPure(e1, e2, e3) ->
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box
        [ Box.word "ifpure"
        ; Box.ws (Box.indent 2 (pretty_expr env 0 e1))
        ; Box.ws (Box.word "then")
        ]
      ; Box.ws (Box.indent 2 (pretty_expr env 0 e2))
      ]
    ; Box.box
      [ Box.ws (Box.word "else")
      ; Box.ws (Box.indent 2 (pretty_expr env prec_stmt e3))
      ]
    ])
  | ECoerce(c, e1, e2) ->
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.box
      [ Box.box
        [ Box.word "coerce"
        ; Box.ws (Box.indent 2 (pretty_expr env 0 e1))
        ; Box.ws (Box.word "with")
        ]
      ; Box.ws (Box.indent 2 (pretty_coercion env 0 c))
      ]
    ; Box.box
      [ Box.ws (Box.word "then")
      ; Box.ws (Box.indent 2 (pretty_expr env prec_stmt e2))
      ]
    ])
  | EExtern name ->
    Box.prec_paren prec_extern prec (Box.box
    [ Box.word "extern"
    ; Box.ws (Box.indent 4 (Box.word name))
    ])
  | EReplExpr(e, _, _) ->
    Box.prec_paren prec_app prec (Box.box
      [ Box.word "$REPL_EXPR"
      ; Box.ws (Box.indent 2 (pretty_expr env (prec_app + 1) e))
      ])
  | ERepl(e, _) ->
    Box.word "$REPL"

and pretty_effect env eff rest =
  match eff.data with
  | EEffPure -> Box.word "[" :: rest
  | EEffCons(eff, e) ->
      pretty_effect env eff
        (Box.prefix (Box.word ",")
          (Box.ws (pretty_expr env 0 e))
        :: rest)
  | _ ->
    Box.prefix (Box.word "[") (Box.ws (pretty_expr env 0 eff))
      :: rest

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
    let arg = Box.ws (Box.word name) in
    (arg :: args, body)
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

and pretty_rec_function env kw (x, y, e) =
  let name = Env.pretty_var env x in
  let func = { meta = Utils.Seal.empty; data = EFun(y, e) } in
  Box.box
  [ Box.box
    [ Box.word kw
    ; Box.ws (Box.indent 2 (Box.suffix name
        (Box.ws (Box.word "="))))
    ]
  ; Box.ws (Box.indent 2 (pretty_fun env func))
  ]

and pretty_coercion env prec c =
  match c.data with
  | CId     -> Box.word "id"
  | CLift e ->
    Box.prec_paren prec_app prec (Box.box
    [ Box.word "lift"
    ; Box.ws (Box.indent 2 (pretty_expr env (prec_app + 1) e))
    ])
  | CSwap(e1, e2) ->
    Box.prec_paren prec_app prec (Box.box
    [ Box.word "swap"
    ; Box.ws (Box.indent 2 (pretty_expr env (prec_app + 1) e1))
    ; Box.ws (Box.indent 2 (pretty_expr env (prec_app + 1) e2))
    ])
  | CCons(e, c) ->
    Box.prec_paren prec_cons prec (Box.box
    [ Box.suffix
      (pretty_expr env (prec_cons + 1) e)
      (Box.ws (Box.word ":"))
    ; Box.ws (pretty_coercion env prec_cons c)
    ])
  | CComp(c1, c2) ->
    Box.prec_paren prec_stmt prec (Box.box
    [ Box.suffix
      (pretty_coercion env prec_stmt c1)
      (Box.word ";")
    ; Box.ws (pretty_coercion env prec_stmt c2)
    ])

let pretty_program e =
  pretty_expr (Env.init ()) 0 e

let _ =
  Flow.register_box_printer 
    ~source:flow_node
    pretty_program
