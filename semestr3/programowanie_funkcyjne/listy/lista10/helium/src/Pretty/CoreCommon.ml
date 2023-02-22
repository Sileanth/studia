open Lang.CoreCommon.All

module Env = struct
  open Common

  type t =
    { var_names  : StrSet.t
    ; tvar_names : StrSet.t
    ; var_map    : string Var.Map.t
    ; tvar_map   : string TVar.Map.t
    }

  let empty =
    { var_names  = StrSet.empty
    ; tvar_names = StrSet.empty
    ; var_map    = Var.Map.empty
    ; tvar_map   = TVar.Map.empty
    }

  let add_var env x =
    let name = fresh_name env.var_names (Var.name x) in
    { env with
      var_names = StrSet.add name env.var_names
    ; var_map   = Var.Map.add x name env.var_map
    }, name

  let add_tvar env x =
    let name = fresh_name env.tvar_names (TVar.name x) in
    { env with
      tvar_names = StrSet.add name env.tvar_names
    ; tvar_map   = TVar.Map.add x name env.tvar_map
    }, name

  let add_tvars env xs =
    Utils.ListExt.fold_map (fun env (TVar.Pack x) -> add_tvar env x) env xs

  let init () =
    let env = empty in
    let open Predef.DB in
    let env = List.fold_left (fun env (Type p) ->
        fst (add_tvar env p.core_tvar)
      ) env (types ()) in
    env

  let pretty_var env x =
    match Var.Map.find_opt x env.var_map with
    | None   -> Box.word (Printf.sprintf "<unbound:%s>" (Var.name x))
    | Some n -> Box.word n

  let pretty_tvar env x =
    match TVar.Map.find_opt x env.tvar_map with
    | None   -> Box.word (Printf.sprintf "<unbound:%s>" (TVar.name x))
    | Some n -> Box.word n
end

let rec pretty_kind : type k. int -> k kind -> Box.t =
    fun prec k ->
  match k with
  | KType   -> Box.word "type"
  | KEffect -> Box.word "effect"
  | KArrow(k1, k2) -> Box.prec_paren 0 prec (
      Box.box
      [ Box.suffix (pretty_kind 1 k1) (Box.ws (Box.word "->"))
      ; Box.ws (pretty_kind 0 k2)
      ])

let rec pretty_type : type k. Env.t -> int -> k typ -> Box.t =
    fun env prec tp ->
  match Type.view tp with
  | TVEffect ats  -> pretty_row env ats
  | TVNeutral neu -> pretty_neutral_type env prec neu
  | TVArrow(tp1, tp2, eff) ->
    Box.prec_paren 0 prec (
      Box.box
      [ Box.suffix (pretty_type env 1 tp1)
          (Box.ws (Box.box
          [ (Box.word "->")
          ; Box.indent 2 (pretty_arrow_effect env eff)
          ]))
      ; Box.ws (pretty_type env 0 tp2)
      ])
  | TVForall(x, body) ->
    let (args, body) = pretty_forall_aux env (Type.forall x body) in
    Box.box
    [ Box.box
      [ Box.word "forall"
      ; Box.ws (Box.suffix (Box.indent 2 (Box.box args)) (Box.word ","))
      ]
    ; Box.ws (Box.indent 2 body)
    ]
  | TVFun(x, body) ->
    let (args, body) = pretty_fun_aux env (Type.tfun x body) in
    Box.box
    [ Box.box
      [ Box.word "fn"
      ; Box.ws (Box.suffix (Box.indent 2 (Box.box args)) (Box.word "=>"))
      ]
    ; Box.ws (Box.indent 2 body)
    ]

and pretty_arrow_effect env (eff : effect) =
  match Type.to_at_effects eff with
  | []  -> Box.box []
  | ats -> pretty_row env ats

and pretty_forall_aux env (tp : ttype) =
  match Type.view tp with
  | TVNeutral _ | TVArrow _ ->
      ([], pretty_type env 0 tp)
  | TVForall(x, body) ->
    let (env', name) = Env.add_tvar env x in
    let (args, body) = pretty_forall_aux env' body in
    let arg =
      Box.brackets (Box.box
      [ Box.suffix (Box.word name) (Box.ws (Box.word "::"))
      ; Box.ws (pretty_kind 0 (TVar.kind x))
      ])
    in (arg :: args, body)

and pretty_fun_aux : type k. Env.t -> k typ -> Box.t list * Box.t =
    fun env tp ->
  match Type.view tp with
  | TVEffect  _ -> ([], pretty_type env 0 tp)
  | TVNeutral _ -> ([], pretty_type env 0 tp)
  | TVArrow   _ -> ([], pretty_type env 0 tp)
  | TVForall  _ -> ([], pretty_type env 0 tp)
  | TVFun(x, body) ->
    let (env', name) = Env.add_tvar env x in
    let (args, body) = pretty_fun_aux env' body in
    let arg =
      Box.brackets (Box.box
      [ Box.suffix (Box.word name) (Box.ws (Box.word "::"))
      ; Box.ws (pretty_kind 0 (TVar.kind x))
      ])
    in (arg :: args, body)

and pretty_type_head : type k. Env.t -> k type_head -> Box.t =
    fun env h ->
  match h with
  | HVar x -> Env.pretty_tvar env x
  | HAny k -> Box.braces (Box.box
      [ Box.word "any"
      ; Box.indent 2 (Box.ws (pretty_kind 0 k))
      ])

and pretty_type_params : type k1 k2. Env.t -> (k1, k2) type_params -> Box.t list =
    fun env params ->
  match params with
  | TP_Nil -> []
  | TP_Cons(tp, params) ->
    pretty_type env 11 tp :: pretty_type_params env params

and pretty_neutral_type : type k. Env.t -> int -> k neutral_type -> Box.t =
    fun env prec (TNeu(h, params)) ->
  begin match params with
  | TP_Nil    -> pretty_type_head env h
  | TP_Cons _ ->
    Box.prec_paren 10 prec (Box.box
    (  pretty_type_head env h
    :: List.map (fun b -> Box.indent 2 (Box.ws b))
        (pretty_type_params env params)))
  end

and pretty_row env ats =
  match ats with
  | []  -> Box.word "[]"
  | l :: ats ->
    let rec loop pref l ats =
      match ats with
      | []  ->
        [ Box.prefix pref
          (Box.suffix (pretty_neutral_type env 0 l) (Box.word "]"))
        ]
      | l1 :: ats ->
        Box.prefix pref (pretty_neutral_type env 0 l)
        :: loop (Box.word ",") l1 ats
    in
    Box.box (loop (Box.word "[") l ats)

let pretty_type_arg (type k) env (tp : k typ) =
  match Type.kind tp with
  | KEffect when !Settings.pretty_rows ->
    begin match Type.to_at_effects tp with
    | []  -> Box.word "[[]]"
    | ats -> pretty_row env ats
    end
  | _       -> Box.brackets (pretty_type env 0 tp)

let prepare_typedef env (TypeDef(l, _, _)) =
  fst (Env.add_tvar env l)

let prepare_typedefs env tds =
  List.fold_left prepare_typedef env tds

let rec pretty_type_list env tps =
  match tps with
  | []     -> []
  | [ tp ] -> [ Box.ws (pretty_type env 1 tp) ]
  | tp :: tps ->
    Box.ws (Box.suffix (pretty_type env 1 tp) (Box.word ",")) ::
      (pretty_type_list env tps)

let pretty_effect_op env n (OpDecl(name, tvs, inputs, output)) =
  let (env, tvs) = Env.add_tvars env tvs in
  let op =
    Box.box
    [ Box.box
      [ Box.word (string_of_int n)
      ; Box.paren(Box.word name)
      ]
    ; Box.box (List.map (fun name -> Box.brackets (Box.word name)) tvs)
    ] in
  Box.box
  [ Box.suffix op (Box.ws (Box.word ":"))
  ; Box.ws (Box.indent 2 (Box.box
      [ Box.suffix (Box.box (pretty_type_list env inputs))
          (Box.ws (Box.word "=>"))
      ; Box.ws (pretty_type env 0 output)
      ]))
  ]

let pretty_effect_ops env ops =
  match ops with
  | [] -> Box.word "<empty_effect>"
  | op :: ops ->
    let rec pretty_effect_ops_aux n ops =
      match ops with
      | [] -> [ Box.ws (Box.word "}") ]
      | op :: ops ->
        Box.prefix (Box.word "; ") (pretty_effect_op env n op)
        :: pretty_effect_ops_aux (n+1) ops
    in
    Box.box
    (  Box.prefix (Box.word "{ ") (pretty_effect_op env 0 op)
    :: pretty_effect_ops_aux 1 ops
    )

let pretty_adt_ctors env ctors =
  List.mapi (fun n (ADTCtor(name, tvs, tps)) ->
    let (env, tvs) = Env.add_tvars env tvs in
    let ctor =
      Box.box
      [ Box.box
        [ Box.word (string_of_int n)
        ; Box.paren(Box.word name)
        ]
      ; Box.box (List.map (fun name -> Box.brackets (Box.word name)) tvs)
      ] in
    Box.ws (Box.prefix (Box.word "| ")
    begin match tps with
    | [] -> ctor
    | _  -> Box.box
      [ Box.box [ ctor; Box.indent 4 (Box.ws (Box.word "of")) ]
      ; Box.indent 2 (Box.box (pretty_type_list env tps))
      ] 
    end)
  ) ctors |> Box.box

let rec pretty_typedef_formal_params : type k1 k2.
    Env.t -> (k1, k2) typedef_formal_params -> Env.t * Box.t list =
    fun env params ->
  match params with
  | TDFP_Nil -> (env, [])
  | TDFP_Cons(x, params) ->
    let (env, name) = Env.add_tvar env x in
    let (env, params) = pretty_typedef_formal_params env params in
    let p =
      Box.brackets (Box.box
      [ Box.suffix (Box.word name) (Box.ws (Box.word "::"))
      ; Box.ws (pretty_kind 0 (TVar.kind x))
      ])
    in (env, p :: params)

let pretty_typedef env ?sep (TypeDef(l, params, body)) =
  let make_kw kw =
    match sep with
    | None -> Box.word kw
    | Some b -> Box.box [ b; Box.ws (Box.word kw) ]
  in
  let (env, params) = pretty_typedef_formal_params env params in
  match body with
  | TDEffect ops ->
    Box.box
    [ Box.box
      [ Box.box [ make_kw "effect"; Box.ws (Env.pretty_tvar env l) ]
      ; Box.ws (Box.indent 4 (Box.suffix (Box.box params)
          (Box.ws (Box.word "="))))
      ]
    ; Box.ws (Box.indent 2 (pretty_effect_ops env ops))
    ]
  | TDData ctors ->
    Box.box
    [ Box.box
      [ Box.box [ make_kw "type"; Box.ws (Env.pretty_tvar env l) ]
      ; Box.ws (Box.indent 4 (Box.suffix (Box.box params)
          (Box.ws (Box.word "="))))
      ]
    ; Box.ws (pretty_adt_ctors env ctors)
    ]

let pretty_typedefs env ?sep tds =
  match tds with
  | [] -> []
  | td :: tds ->
    pretty_typedef env ?sep td ::
    List.map (pretty_typedef env ~sep:(Box.ws (Box.word "and"))) tds
