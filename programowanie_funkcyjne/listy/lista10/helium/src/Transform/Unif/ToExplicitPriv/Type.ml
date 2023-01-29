open Utils.EqDec

module S = Lang.Unif
module T = Lang.Explicit

let internal_kind_error () =
  failwith "Internal kind error during translation (Unif -> Explicit)"

type 'k tr_params_result =
| Params : 'k1 T.kind * ('k1, 'k) Lang.Explicit.type_params ->
    'k tr_params_result

let rec tr_type env tp =
  match S.Type.view tp with
  | S.TEffPure -> T.Type.Pack T.Type.eff_pure
  | S.TEffCons(l, eff) ->
    T.Type.Pack(T.Type.eff_cons (tr_effect env l) (tr_effect env eff))
  | S.TConst c ->
    let T.TVar.Pack x = Env.lookup_tconst env c in
    T.Type.Pack (T.Type.var x)
  | S.TVar(perm, x) ->
    begin match Env.lookup_tvar env x with
    | Some tp -> tr_type env (S.Type.perm (S.TCPerm.rev perm) tp)
    | None ->
      begin match Kind.tr_kind (S.TVar.kind x) with
      | T.Kind.Pack k -> T.Type.Pack(T.Type.any k)
      end
    end
  | S.TArrow(tp1, tp2, eff) ->
    T.Type.Pack(T.Type.arrow
      (tr_ttype env tp1)
      (tr_ttype env tp2)
      (tr_effect env eff))
  | S.TFun(x, tp) ->
    let (env, T.TVar.Pack x) = Env.add_tconst env x in
    let (T.Type.Pack tp) = tr_type env tp in
    T.Type.Pack (T.Type.tfun x tp)
  | S.TApp(tp1, tp2) ->
    let T.Type.Pack tp1 = tr_type env tp1 in
    begin match T.Type.kind tp1 with
    | T.KArrow(k1, _) ->
      T.Type.Pack(T.Type.app tp1 (tr_check_type env k1 tp2))
    | _ -> internal_kind_error ()
    end

and tr_check_type : type k. Env.t -> k T.kind -> S.typ -> k T.typ =
  fun env kind tp ->
  let T.Type.Pack tp = tr_type env tp in
  match T.Kind.equal (T.Type.kind tp) kind with
  | Equal    -> tp
  | NotEqual -> internal_kind_error ()

and tr_ttype env tp =
  tr_check_type env T.KType tp

and tr_effect env tp =
  tr_check_type env T.KEffect tp

let tr_scheme env scheme =
  let rec loop env tcs =
    match tcs with
    | []       -> tr_ttype env (S.Scheme.body scheme)
    | c :: tcs ->
      let (env, T.TVar.Pack x) = Env.add_tconst env c in
      let tp = loop env tcs in
      T.Type.forall x tp
  in loop env (S.Scheme.qvars scheme)

let tr_tconst (type k) env x (k : k T.kind) : k T.tvar =
  let T.TVar.Pack x = Env.lookup_tconst env x in
  match T.Kind.equal (T.TVar.kind x) k with
  | Equal    -> x
  | NotEqual -> internal_kind_error ()

let rec tr_type_params : type k.
    Env.t -> Lang.Unif.typ list -> k T.kind -> k tr_params_result =
    fun env tps kind ->
  match tps with
  | []        -> Params(kind, T.TP_Nil)
  | tp :: tps ->
    let T.Type.Pack tp = tr_type env tp in
    let Params(kind, params) = tr_type_params env tps kind in
    Params(T.KArrow(T.Type.kind tp, kind), T.TP_Cons(tp, params))

(* It may substitute only in tp1 *)
let rec match_type tp1 tp2 =
  match S.Type.view tp1, S.Type.view tp2 with
  | S.TVar(perm1, x), S.TVar(perm2, y) when S.TVar.equal x y ->
    let perm = S.TCPerm.compose (S.TCPerm.rev perm1) perm2 in
    S.TVar.restrict x (S.TCPerm.carrier perm)
  | S.TVar(perm, x), _ ->
    let tp2 = S.Type.perm (S.TCPerm.rev perm) tp2 in
    assert (not (S.Type.contains_tvar x tp2));
    S.TVar.set x tp2
  | S.TEffPure, S.TEffPure -> ()
  | S.TEffPure, _ -> () (* possible, since effect could be moved down *)
  | S.TEffCons(l1, eff1), S.TEffCons(l2, eff2) ->
    match_type l1 l2;
    match_type eff1 eff2
  | S.TEffCons _, _ -> assert false
  | S.TConst c1, S.TConst c2 -> assert (S.TConst.equal c1 c2)
  | S.TConst _, _ -> assert false
  | S.TArrow(ta1, tv1, eff1), S.TArrow(ta2, tv2, eff2) ->
    match_type ta1 ta2;
    match_type tv1 tv2;
    match_type eff1 eff2;
  | S.TArrow _, _ -> assert false
  | S.TFun(x1, tp1),  S.TFun(x2, tp2) ->
    let x = S.TConst.clone x1 in
    let tp1 = S.Type.perm (S.TCPerm.swap x1 x) tp1 in
    let tp2 = S.Type.perm (S.TCPerm.swap x2 x) tp2 in
    match_type tp1 tp2
  | S.TFun _, _ -> assert false
  | S.TApp(tf1, tp1), S.TApp(tf2, tp2) ->
    match_type tf1 tf2;
    match_type tp1 tp2
  | S.TApp _, _ -> assert false 
