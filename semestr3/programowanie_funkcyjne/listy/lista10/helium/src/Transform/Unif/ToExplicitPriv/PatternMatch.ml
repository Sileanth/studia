open Lang.Node

module S = Lang.Unif
module T = Lang.Explicit
module B = Predef.Types

module ExVal : sig
  type t
  val hole  : t
  val reify : t -> Errors.exval

  val refocus_any    : t -> t
  val dup_hole       : t -> t
  val refocus_ctor   : t -> string -> 'a list -> t
  val refocus_record : t -> string list -> t
end = struct
  type ctx =
  | Top
  | Dup    of ctx
  | Ctor   of string * Errors.exval list * ctx * Errors.exval list
  | Record of (string * Errors.exval) list * string * ctx * string list

  type t =
  | Hole of ctx
  | Done of Errors.exval

  let hole = Hole Top

  let rec reify_ctx ctx exval =
    match ctx with
    | Top     -> exval
    | Dup ctx -> reify_ctx ctx exval
    | Ctor(name, a1, ctx, a2) ->
      reify_ctx ctx (Errors.EVCtor(name, List.rev_append a1 (exval :: a2)))
    | Record(flds, f, ctx, _) ->
      begin match flds, exval with
      | [],   Errors.EVAny -> reify_ctx ctx Errors.EVAny
      | flds, Errors.EVAny -> reify_ctx ctx (Errors.EVRecord flds)
      | flds, exval        ->
        reify_ctx ctx (Errors.EVRecord (List.rev_append flds [ f, exval ]))
      end

  let reify = function
    | Hole ctx   -> reify_ctx ctx Errors.EVAny
    | Done exval -> exval

  let rec refocus ev = function
    | Top     -> Done ev
    | Dup ctx -> Hole ctx
    | Ctor(name, args, ctx, []) ->
      refocus (Errors.EVCtor(name, List.rev_append args [ev])) ctx
    | Ctor(name, args, ctx, _ :: a2) ->
      Hole(Ctor(name, ev :: args, ctx, a2))
    | Record(flds, f, ctx, []) ->
      begin match ev, flds with
      | Errors.EVAny, [] -> refocus Errors.EVAny ctx
      | Errors.EVAny, _  -> refocus (Errors.EVRecord(List.rev flds)) ctx
      | _ -> refocus (Errors.EVRecord(List.rev_append flds [ f, ev ])) ctx
      end
    | Record(flds, f, ctx, f' :: fs) ->
      begin match ev with
      | Errors.EVAny -> Hole(Record(flds, f', ctx, fs))
      | _ -> Hole(Record((f, ev) :: flds, f', ctx, fs))
      end

  let refocus_any = function
    | Hole ctx -> refocus Errors.EVAny ctx
    | Done _   -> failwith "no hole to fill"

  let dup_hole = function
    | Hole ctx -> Hole (Dup ctx)
    | Done _   -> failwith "no hole to duplicate"

  let refocus_ctor ev name args =
    match ev with
    | Hole ctx ->
      begin match args with
      | []        -> refocus (Errors.EVCtor(name, [])) ctx
      | _ :: args -> Hole
        (Ctor(name, [], ctx, List.map (fun _ -> Errors.EVAny) args))
      end
    | Done _ -> failwith "no hole to fill"

  let refocus_record ev fields =
    match ev with
    | Hole ctx ->
      begin match fields with
      | []      -> refocus Errors.EVAny ctx
      | f :: fs -> Hole (Record([], f, ctx, fs))
      end
    | Done _ -> failwith "no hole to fill"

end

type pattern =
| PWildcard of T.ttype
| PVar      of S.var * T.ttype
| PCoerce   of T.val_coercion * pattern * T.ttype
| PCtor     of string * T.k_type T.neutral_type
    * int * S.tconst list * S.pattern list
| PRecord   of string * T.k_type T.neutral_type
    * string list * (int * S.pattern) list

module Pattern = struct
  let rec name = function
    | PWildcard _            -> "_"
    | PVar(x, _)             -> S.Var.name x
    | PCoerce(_, p, _)       -> name p
    | PCtor(nam, _, _, _, _) -> nam
    | PRecord(nam, _, _, _)  -> nam

  let type_of = function
    | PWildcard tp         -> tp
    | PVar(_, tp)          -> tp
    | PCoerce(_, _, tp)    -> tp
    | PCtor(_, l, _, _, _) -> T.Type.of_neutral l
    | PRecord(_, l, _, _)  -> T.Type.of_neutral l
end

let rec tr_pattern env pat =
  match pat.data with
  | S.PWildcard sch -> PWildcard (Type.tr_scheme env sch)
  | S.PVar x        -> PVar(x, Type.tr_scheme env (S.Var.scheme x))
  | S.PCoerce(c, p, tp) ->
    PCoerce(Coercion.tr_val_coercion env c,
      tr_pattern env p,
      Type.tr_ttype env tp)
  | S.PCtor(l, args, n, tcs, ps) ->
    let Type.Params(kind, params) = Type.tr_type_params env args T.KType in
    let x = Type.tr_tconst env l kind in
    PCtor(String.uncapitalize_ascii (S.TConst.name l),
      T.TNeu(T.HVar x, params), n, tcs, ps)
  | S.PRecord(l, args, fpats) ->
    let Type.Params(kind, params) = Type.tr_type_params env args T.KType in
    let x = Type.tr_tconst env l kind in
    let fields = Env.lookup_record env l in
    PRecord(String.uncapitalize_ascii (S.TConst.name l),
      T.TNeu(T.HVar x, params), fields, fpats)

type clause =
  { pats  : pattern list
  ; small : bool
  ; env   : Env.t
  ; gen   : Env.t -> T.expr
  }

type column_type =
| CT_Var
| CT_Coerce of T.val_coercion * pattern * T.ttype
| CT_Ctor   of T.k_type T.neutral_type
| CT_Record of T.k_type T.neutral_type * string list

let rec name_of_pattern pat =
  match pat.data with
  | S.PWildcard _          -> "_"
  | S.PVar x               -> S.Var.name x
  | S.PCoerce(_, p, _)     -> name_of_pattern p
  | S.PCtor(l, _, _, _, _) | S.PRecord(l, _, _) ->
    String.uncapitalize_ascii (S.TConst.name l)

let type_of_pattern pat =
  match pat.data with
  | S.PWildcard sch          -> S.Scheme.to_type sch
  | S.PVar x                 -> S.Var.typ x
  | S.PCoerce(_, _, tp)      -> tp
  | S.PCtor(l, tps, _, _, _) | S.PRecord(l, tps, _) ->
    S.Type.apps (S.Type.tconst l) tps

let rec pat_type pat =
  match pat with
  | PWildcard _ | PVar _ -> CT_Var
  | PCoerce(c, p, tp) ->
    begin match pat_type p with
    | (CT_Ctor _ | CT_Record _) as ctp -> ctp
    | CT_Var | CT_Coerce _ -> CT_Coerce(c, p, tp)
    end
  | PCtor(_, l, _, _, _)     -> CT_Ctor l
  | PRecord(_, l, fields, _) -> CT_Record(l, fields)

let rec column_type clauses =
  match clauses with
  | [] -> CT_Var
  | { pats = [] } :: _ -> assert false (* First column should exist *)
  | { pats = pat :: _ } :: clauses ->
    begin match pat_type pat with
    | CT_Var -> column_type clauses
    | ctp    -> ctp
    end

let open_var_column x clauses =
  List.map (fun cl ->
    match cl.pats with
    | [] -> assert false (* First column should exist *)
    | pat :: pats ->
      begin match pat with
      | PWildcard _ -> { cl with pats = pats }
      | PVar(y, _)  ->
        { cl with
          pats = pats
        ; env  = Env.add_var cl.env y x
        }
      | PCoerce _ | PCtor _ | PRecord _ -> assert false
        (* function should be called on variable columns *)
      end
  ) clauses

let add_dummy_snd_pat tp cl =
  match cl.pats with
  | [] -> assert false (* First column should exist *)
  | pat :: pats ->
    { cl with
      pats = pat :: PWildcard tp :: pats
    }

let rec open_coerce_column tp1 tp2 clauses =
  match clauses with
  | [] -> assert false (* no coercion pattern found *)
  | cl :: clauses ->
    begin match cl.pats with
    | [] -> assert false (* First column should exist *)
    | pat :: pats ->
      begin match pat with
      | PCoerce(_, p, _) ->
        { cl with
          pats = PWildcard tp1 :: p :: pats
        } :: List.map (add_dummy_snd_pat tp2) clauses
      | _ ->
        add_dummy_snd_pat tp2 cl :: open_coerce_column tp1 tp2 clauses
      end
    end

let rec match_with_vars pats vars env =
  match pats, vars with
  | [],          []        -> ([], env)
  | pat :: pats, x :: vars ->
    let (pat, env) =
      match pat with
      | PVar(y, tp) -> (PWildcard tp, Env.add_var env y x)
      | _           -> (pat, env)
    in
    let (pats, env) = match_with_vars pats vars env in
    (pat :: pats, env)
  | _ -> assert false

let vars_of_patterns env pats =
  let rec loop_ps acc env pats =
    match pats with
    | []          -> acc
    | pat :: pats -> loop_ps (loop_p acc env pat) env pats
  and loop_p acc env pat =
    match pat with
    | PWildcard _ -> acc
    | PVar(x, tp) -> (fst acc, (x, tp) :: snd acc)
    | PCoerce(_, p, _) -> loop_p acc env p
    | PCtor(_, _, _, tcs, ps) ->
      let (env, tvs) = Env.add_tconsts env tcs in
      let acc =
        (List.rev_append (Utils.ListExt.zip tcs tvs) (fst acc), snd acc) in
      loop_ps acc env (List.map (tr_pattern env) ps)
    | PRecord(_, _, _, flds) ->
      loop_ps acc env (List.map (fun (_, p) -> tr_pattern env p) flds)
  in
  let (tacc, vacc) = loop_ps ([], []) env pats in
  (List.rev tacc, vacc)

let rename_ctor_type tvs tps =
  let (ren, tvs) =
    Utils.ListExt.fold_map (fun ren (T.TVar.Pack x) ->
      let y = T.TVar.clone x in
      (T.TVar.TVarMap.add x y ren, T.TVar.Pack y)
    ) T.TVar.TVarMap.empty tvs in
  (tvs, List.map (T.Type.rename_m ren) tps)

(* ========================================================================= *)
(* Records *)

let record_types env neu =
  match Env.lookup_adt env neu with
  | [ T.ADTCtor(_, tvs, tps) ] ->
    rename_ctor_type tvs tps
  | _ -> assert false

let rec open_record_column' x nvars cl =
  match cl.pats with
  | [] -> assert false (* First column should exist *)
  | pat :: pats ->
    begin match pat with
    | PWildcard _ ->
      { cl with
        pats = List.map (fun x -> PWildcard (T.Var.type_of x)) nvars 
             @ cl.pats
      }
    | PVar(y, _)  ->
      { cl with
        pats = List.map (fun x -> PWildcard (T.Var.type_of x)) nvars 
             @ cl.pats
      ; env  = Env.add_var cl.env y x
      }
    | PCoerce(c, p, _) ->
      begin match c.data with
      | T.VCId ->
        open_record_column' x nvars { cl with pats = p :: pats }
      | T.VCArrow _ -> assert false
        (* first column matches ADT *)
      end
    | PRecord(_, _, _, fpats) ->
      let fpats = List.mapi (fun n x ->
          match List.assoc_opt n fpats with
          | None     -> PWildcard (T.Var.type_of x)
          | Some pat -> tr_pattern cl.env pat
        ) nvars in
      { cl with
        pats = fpats @ cl.pats
      }
    | PCtor _ -> assert false
      (* function should be called on record columns *)
    end

let open_record_column x nvars clauses =
  List.map (open_record_column' x nvars) clauses

(* ========================================================================= *)
let tr_match_main meta env ~typ ~eff =
  let rec make_clause_tfun env tvs gen =
    match tvs with
    | []                        -> gen env
    | (c, T.TVar.Pack x) :: tvs ->
      let env = Env.add_tconst' env c x in
      { meta
      ; data = T.ETFun(x, make_clause_tfun env tvs gen)
      }
  in
  let rec make_clause_fun env vars gen =
    match vars with
    | [] -> gen env
    | (x, tp) :: vars ->
      let y = T.Var.fresh tp in
      { meta
      ; data = T.EFun(y, make_clause_fun (Env.add_var env x y) vars gen) }
  in
  let make_eff_app e1 e2 =
    { meta; data = T.EApp(
      { meta; data = T.ECoerce(
        { meta; data = T.VCId},
        { meta; data = T.ECLift eff},
        e1)
      },
      { meta; data = T.ECoerce(
        { meta; data = T.VCId},
        { meta; data = T.ECLift eff},
        e2)
      })
    } in
  let rec make_clause_tfun_apply env f tvs =
    match tvs with
    | [] -> f
    | (c, _) :: tvs ->
      let T.TVar.Pack x = Env.lookup_tconst env c in
      make_clause_tfun_apply env
        { meta; data = T.ETApp(f, T.Type.var x) }
        tvs
  in
  let rec make_clause_fun_apply env f args =
    match args with
    | [] -> assert false
    | [ (x, _) ] -> make_eff_app f
      { meta; data = T.EVar(Env.lookup_var env x) }
    | (x, _) :: args ->
      make_clause_fun_apply env
        { meta; data = T.EApp(f,
          { meta; data = T.EVar(Env.lookup_var env x) }) }
        args
  in
  let save_clause_body cl vars cont =
    let (pats, env) = match_with_vars cl.pats vars cl.env in
    match vars_of_patterns env pats with
    | (tvs, []) ->
      let f = T.Var.fresh
        (T.Type.foralls (List.map snd tvs) 
          (T.Type.arrow B.Int.core_tp typ eff))
      in
      let x = T.Var.fresh B.Int.core_tp in
      { meta; data = T.ELetPure(f,
        make_clause_tfun env tvs (fun env ->
          { meta; data = T.EFun(x, cl.gen env) }),
        cont
        { pats  = pats
        ; small = true
        ; env   = env
        ; gen   = (fun _ -> make_eff_app
            { meta; data = T.EVar f }
            { meta; data = T.ENum 0 })
        })
      }
    | (tvs, svars) ->
      let f = T.Var.fresh (T.Type.foralls (List.map snd tvs)
        (T.Type.arrows (List.map snd svars) typ eff)) in
      { meta; data = T.ELetPure(f,
        make_clause_tfun env tvs (fun env ->
          make_clause_fun env svars cl.gen),
        cont
        { pats  = pats
        ; small = true
        ; env   = env
        ; gen   = (fun env ->
          make_clause_fun_apply env
            (make_clause_tfun_apply env { meta; data = T.EVar f } tvs)
            svars)
        })
      }
  in
  let save_var_clause vars cl cont =
    if cl.small then cont cl
    else match cl.pats with
    | (PVar _ | PWildcard _) :: pats ->
      save_clause_body cl vars cont
    | _ -> cont cl
  in
  let rec loop exval vars clauses =
    match vars with
    | [] ->
      begin match clauses with
      | [] ->
        let exval = ExVal.reify exval in
        raise (Errors.non_exhaustive_match meta exval)
      | cl :: _ -> cl.gen cl.env
      end
    | x :: vars ->
      begin match column_type clauses with
      | CT_Var ->
        let exval   = ExVal.refocus_any exval in
        let clauses = open_var_column x clauses in
        loop exval vars clauses
      | CT_Coerce(c, p, tp1) ->
        let exval = ExVal.dup_hole exval in
        let name = Pattern.name p in
        let tp2  = Pattern.type_of p in
        let y = T.Var.fresh ~name:name tp2 in
        let res = loop exval (x :: y :: vars)
          (open_coerce_column tp1 tp2 clauses)
        in
        { meta; data = T.ELetPure(y,
          { meta; data = T.ECoerce(c, { meta; data = T.ECId T.Type.eff_pure },
            { meta; data = T.EVar x })
          }, res)
        }
      | CT_Ctor neu ->
        let ctors = Env.lookup_adt env neu in
        Utils.ListExt.map_cps (save_var_clause (x :: vars)) clauses
        (fun clauses ->
        Utils.ListExt.mapi_cps (tr_ctor_clause exval x vars clauses) ctors
        (fun clauses ->
        { meta; data = T.EMatch(neu,
          { meta; data = T.ECoerce(
            { meta; data = T.VCId},
            { meta; data = T.ECLift eff},
            { meta; data = T.EVar x })
          }, clauses, typ) }))
      | CT_Record(neu, fields) ->
        let exval = ExVal.refocus_record exval fields in
        let (tvs, tps) = record_types env neu in
        let nvars = List.map2 (fun tp name ->
            T.Var.fresh ~name tp
          ) tps fields in
        let clauses = open_record_column x nvars clauses in
        { meta; data = T.EMatch(neu,
          { meta; data = T.ECoerce(
            { meta; data = T.VCId },
            { meta; data = T.ECLift eff},
            { meta; data = T.EVar x })
          }, [ (tvs, nvars, loop exval (nvars @ vars) clauses) ], typ)
        }
      end
  and tr_ctor_clause exval x vars clauses n (T.ADTCtor(name, tvs, tps)) cont =
    let (tvs, tps) = rename_ctor_type tvs tps in
    let exval = ExVal.refocus_ctor exval name tps in
    let nvars = List.map (fun tp -> T.Var.fresh tp) tps in
    Utils.ListExt.map_filter_redo_cps (fun cl cont ->
    match cl.pats with
    | [] -> assert false (* First column should exist *)
    | pat :: pats ->
      begin match pat with
      | PWildcard _ ->
        let pats' = List.map (fun tp -> PWildcard tp) tps in
        cont (Utils.ListExt.Done { cl with pats = pats' @ pats })
      | PVar(y, _) ->
        let pats' = List.map (fun tp -> PWildcard tp) tps in
        cont (Utils.ListExt.Done 
          { cl with pats = pats' @ pats; env = Env.add_var env y x })
      | PCoerce(c, p, _) ->
        begin match c.data with
        | T.VCId ->
          cont (Utils.ListExt.Redo { cl with pats = p :: pats })
        | T.VCArrow _ -> assert false
          (* first column matches ADT *)
        end
      | PCtor(_, _, m, tcs, ps) when n = m ->
        let cl_env = Env.add_tconsts' cl.env tcs tvs in
        cont (Utils.ListExt.Done
          { cl with
            pats = List.map (tr_pattern cl_env) ps @ pats
          ; env  = cl_env
          })
      | PCtor _ -> cont Utils.ListExt.Drop
      | PRecord _ -> assert false
        (* First column matches ADT *)
      end
    ) clauses (fun clauses ->
    cont (tvs, nvars, loop exval (nvars @ vars) clauses))
  in loop

let tr_match meta env x clauses ~typ ~eff =
  tr_match_main meta env ~typ ~eff
    ExVal.hole
    [ x ]
    (List.map (fun (pat, gen) ->
      { pats  = [ tr_pattern env pat ]
      ; small = false
      ; env   = env
      ; gen   = gen
      }) clauses)

let tr_matches meta env name xs clauses ~typ ~eff =
  tr_match_main meta env ~typ ~eff
    (ExVal.refocus_ctor ExVal.hole name xs)
    xs
    (List.map (fun (env, pats, gen) ->
      { pats  = List.map (tr_pattern env) pats
      ; small = false
      ; env   = env
      ; gen   = gen
      }) clauses)
