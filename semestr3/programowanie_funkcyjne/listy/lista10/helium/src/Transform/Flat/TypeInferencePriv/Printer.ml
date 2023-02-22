open Lang.Node
module T = Lang.Unif

let open_effrow e eff =
  { meta = { e.meta with T.em_effect = eff }
  ; data = T.ECoerce(T.VCId, T.ECOpen eff, e)
  }

let apply e1 e2 =
  match T.Type.view e1.meta.T.em_type with
  | T.TArrow(tp1, tp2, eff) ->
    { meta =
      { T.em_pos    = e1.meta.T.em_pos
      ; T.em_type   = tp2
      ; T.em_effect = eff
      }
    ; data = T.EApp(e1, e2)
    }
  | _ -> assert false

let rec find_printer env tp =
  let is_var x =
    match T.Type.view x with
    | T.TVar _ -> true
    | _        -> false
  in
  match T.Type.view tp with
  | T.TVar _ -> match_default_printer env tp
  | T.TApp(_, atp) when is_var atp -> match_default_printer env tp
  | _ -> match_printers env tp (Env.printers env)

and match_default_printer env tp =
  begin match Env.default_printer env with
  | None   -> None
  | Some p -> match_printer env tp p
  end

and match_printers env tp ps =
  match ps with
  | []      -> match_default_printer env tp
  | p :: ps ->
    begin match match_printer env tp p with
    | None   -> match_printers env tp ps
    | Some r -> Some r
    end

and match_printer env tp p =
  let p_type = snd (T.Scheme.open_s
    ~scope:(Env.type_scope env)
    (T.Var.scheme p)) in
  match match_printer_type env tp p_type with
  | None     -> None
  | Some gen -> Some (gen
      { meta =
        { T.em_pos    = Utils.Position.nowhere
        ; T.em_type   = p_type
        ; T.em_effect = T.Type.eff_pure
        }
      ; data = T.EVar p
      })

and match_printer_type env tp p_type =
  let scope = Env.type_scope env in
  let arg_tp = T.Type.fresh_tvar ~scope T.Kind.ktype in
  let val_tp = T.Type.fresh_tvar ~scope T.Kind.ktype in
  match
    Unification.unify env (T.Type.arrow arg_tp val_tp T.Type.eff_pure) p_type
  with
  | () ->
    begin match match_printer_type env tp val_tp with
    | None     -> None
    | Some gen ->
      begin match find_printer env arg_tp with
      | None     -> None
      | Some arg -> Some (fun e -> gen (apply e arg))
      end
    end
  | exception (Unification.Cannot_unify | Unification.Escapes_scope _) ->
    begin match Unification.unify env tp p_type with
    | () -> Some (fun e -> e)
    | exception (Unification.Cannot_unify | Unification.Escapes_scope _) ->
      None
    end

let dummy_printer e =
  { meta =
    { T.em_pos    = Utils.Position.nowhere
    ; T.em_type   = Predef.Types.String.unif_tp
    ; T.em_effect = e.meta.T.em_effect
    }
  ; data = T.EMatch(e,
    [ { meta = Utils.Position.nowhere
      ; data = T.PWildcard (T.Scheme.close_with [] e.meta.T.em_type)
      }
    , open_effrow
      { meta =
        { T.em_pos    = Utils.Position.nowhere
        ; T.em_type   = Predef.Types.String.unif_tp
        ; T.em_effect = T.Type.eff_pure
        }
      ; data = T.EString "value"
      } e.meta.T.em_effect
    ])
  }

let build_printer env e check_type =
  match Env.printer_type env, Env.printer_interp env with
  | None, _ | _, None   -> dummy_printer e
  | Some ptp, Some pexp ->
    let tp = e.meta.T.em_type in
    let printer_tp = T.Type.app (T.Type.tconst ptp) tp in
    begin match find_printer env printer_tp with
    | None   -> dummy_printer e
    | Some p ->
      let pexp_tp = T.Type.arrows
        [ printer_tp; tp ]
        Predef.Types.String.unif_tp
        e.meta.T.em_effect
      in
      let pexp = check_type pexp pexp_tp T.Type.eff_pure in
      let pexp = apply pexp p in
      let pexp = open_effrow pexp e.meta.T.em_effect in
      apply pexp e
    end
