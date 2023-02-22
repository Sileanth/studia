open Lang.Unif

type tvar_env =
  { mutable tvar_names         : string TVar.Map.t
  ; mutable fresh_tvar_name_id : int
  }

let create_tvar_env () =
  { tvar_names         = TVar.Map.empty
  ; fresh_tvar_name_id = 0
  }

let tvar_name_of_id n =
  let r = Char.code 'z' - Char.code 'a' + 1 in
  let c = Char.chr (Char.code 'a' + n mod r) in
  let n = n / r in
  if n = 0 then Printf.sprintf "%c" c
  else Printf.sprintf "%c%d" c n

let rec pretty_kind env prec k =
  match Kind.view k with
  | KVar _  -> Box.word "?"
  | KType   -> Box.word "type"
  | KEffect -> Box.word "effect"
  | KArrow(k1, k2) -> Box.prec_paren 0 prec (
      Box.box
      [ Box.suffix (pretty_kind env 1 k1) (Box.ws (Box.word "->"))
      ; Box.ws (pretty_kind env 0 k2)
      ])

let pretty_tvar env x =
  let name =
    match TVar.Map.find x env.tvar_names with
    | name -> name
    | exception Not_found ->
      let name = tvar_name_of_id env.fresh_tvar_name_id in
      env.tvar_names <- TVar.Map.add x name env.tvar_names;
      env.fresh_tvar_name_id <- env.fresh_tvar_name_id + 1;
      name
  in
  Box.word name

let rec pretty_type env prec tp =
  match Type.view tp with
  | TEffPure -> Box.word "[]"
  | TEffCons(l, eff) ->
    if !Settings.pretty_rows then
      begin match Type.row_view tp with
      | RNil       -> Box.word "[]"
      | RVar(_, x) -> pretty_tvar env x
      | RCons(l, eff) ->
        begin match Type.row_view eff with
        | RNil -> pretty_type env prec l
        | _    ->
          Box.box (pretty_row
              (Box.prefix (Box.word "[") (pretty_type env 0 l))
            env eff)
        end
      end
    else
      Box.box (pretty_effrow
          (Box.prefix (Box.word "[") (pretty_type env 0 l))
        env eff)
  | TConst c   -> Box.word (TConst.full_name c)
  | TVar(_, x) -> pretty_tvar env x
  | TArrow(tp1, tp2, eff) ->
    Box.prec_paren 0 prec (
      Box.box
      [ Box.suffix (pretty_type env 1 tp1)
          (Box.ws (Box.box
          [ (Box.word "->")
          ; Box.indent 2 (pretty_arrow_effect env eff)
          ]))
      ; Box.ws (pretty_type env 0 tp2)
      ])
  | TFun(x, body) ->
    let (args, body) = pretty_fun_aux env tp in
    Box.prec_paren 10 prec (Box.box
    [ Box.box
      [ Box.word "fn"
      ; Box.ws (Box.suffix (Box.indent 2 (Box.box args)) (Box.word "=>"))
      ]
    ; Box.ws (Box.indent 2 body)
    ])
  | TApp(tp1, tp2) ->
    Box.prec_paren 10 prec (Box.box
      [ pretty_type env 10 tp1
      ; Box.ws (Box.indent 2 (pretty_type env 11 tp2))
      ])

and pretty_arrow_effect env tp =
  if !Settings.pretty_rows then
    begin match Type.row_view tp with
    | RNil   -> Box.box []
    | RVar(_, x) -> Box.brackets (pretty_tvar env x)
    | RCons(l, eff) ->
      Box.box (pretty_row
        (Box.prefix (Box.word "[") (pretty_type env 0 l))
        env eff)
    end
  else
    begin match Type.view tp with
    | TEffPure -> Box.box []
    | TEffCons(l, eff) ->
      Box.box (pretty_effrow
        (Box.prefix (Box.word "[") (pretty_type env 0 l))
        env eff)
    | TConst _ | TVar _ | TArrow _ | TFun _ | TApp _ ->
      Box.paren 
        ~opn:(Box.word "[|") 
        ~cls:(Box.word "]")
        (pretty_type env 0 tp)
    end

(* for no-pretty_rows *)
and pretty_effrow head env eff =
  match Type.view eff with
  | TEffPure -> [ Box.suffix head (Box.word "]") ]
  | TEffCons(l, eff) ->
    head :: pretty_effrow
      (Box.prefix (Box.word ",") (pretty_type env 0 l))
      env eff
  | _ ->
    [ head
    ; Box.paren
        ~opn: (Box.word "|")
        ~cls: (Box.word "]")
        (pretty_type env 0 eff)
    ]

(* for pretty_rows *)
and pretty_row head env eff =
  match Type.row_view eff with
  | RNil   -> [ Box.suffix head (Box.word "]") ]
  | RVar(_, x) ->
    [ head
    ; Box.paren
        ~opn: (Box.word "|")
        ~cls: (Box.word "]")
        (pretty_tvar env x)
    ]
  | RCons(l, eff) ->
    head :: pretty_row
      (Box.prefix (Box.word ",") (pretty_type env 0 l))
      env eff

and pretty_fun_aux env tp =
  match Type.view tp with
  | TEffPure | TEffCons _ | TVar _ | TConst _ | TArrow _ | TApp _ ->
    ([], pretty_type env 0 tp)
  | TFun(x, body) ->
    let name = TConst.full_name x in
    let (args, body) = pretty_fun_aux env body in
    let arg =
      Box.brackets (Box.box
      [ Box.suffix (Box.word name) (Box.ws (Box.word "::"))
      ; Box.ws (pretty_kind env 0 (TConst.kind x))
      ])
    in (arg :: args, body)

