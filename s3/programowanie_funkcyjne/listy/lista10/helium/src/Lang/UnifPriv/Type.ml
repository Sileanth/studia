
type tvar =
  {         id    : Utils.UID.t
  ; mutable scope : TConst.Set.t
  ;         kind  : Kind.t
  ; mutable value : tvar_value
  }

and tvar_value =
| TV_None
| TV_Frozen
| TV_Type of t

and 'v typ =
| TDEffPure
| TDEffCons of effect * effect
| TDVar     of 'v
| TDConst   of TConst.t
| TDArrow   of ttype * ttype * effect
| TDFun     of TConst.t * t
| TDApp     of t * t

and type_tvar =
| TTV_Var  of TCPerm.t * tvar
| TTV_Type of t

and t = type_tvar ref typ

and effect = t
and ttype  = t

type head =
| HVar   of TCPerm.t * tvar
| HConst of TConst.t

type nf_view =
| TNF_EffPure
| TNF_EffCons of effect * effect
| TNF_Neu     of head * t list
| TNF_Arrow   of ttype * ttype * effect
| TNF_Fun     of TConst.t * t

type view =
| TEffPure
| TEffCons of effect * effect
| TVar     of TCPerm.t * tvar
| TConst   of TConst.t
| TArrow   of ttype * ttype * effect
| TFun     of TConst.t * t
| TApp     of t * t

type row_view =
| RNil
| RVar  of TCPerm.t * tvar
| RCons of effect * effect

(* ========================================================================= *)
(* constructors *)
let var x    = TDVar (ref (TTV_Var(TCPerm.id, x)))
let tconst x = TDConst x

let arrow tp1 tp2 eff = TDArrow(tp1, tp2, eff)

let rec arrows tps rtp eff =
  match tps with
  | []        -> rtp
  | [tp]      -> TDArrow(tp, rtp, eff)
  | tp :: tps -> TDArrow(tp, arrows tps rtp eff, TDEffPure)

let app tp1 tp2 = TDApp(tp1, tp2)
let apps tp args =
  List.fold_left app tp args

let tfun  x  tp = TDFun(x, tp)
let tfuns xs tp =
  List.fold_right tfun xs tp

let eff_pure = TDEffPure
let eff_cons l eff = TDEffCons(l, eff)

(* ========================================================================= *)
let restrict_tvar x forbidden =
  match x.value with
  | TV_None ->
    x.scope <- TConst.Set.diff x.scope forbidden
  | TV_Frozen -> ()
  | TV_Type _ -> assert false

(* ========================================================================= *)

let of_nf_view tv =
  match tv with
  | TNF_EffPure             -> TDEffPure
  | TNF_EffCons(eff1, eff2) -> TDEffCons(eff1, eff2)
  | TNF_Neu(HVar(perm, x), args) ->
    apps (TDVar(ref (TTV_Var(perm, x)))) args
  | TNF_Neu(HConst c, args) ->
    apps (TDConst c) args
  | TNF_Arrow(tp1, tp2, eff) -> TDArrow(tp1, tp2, eff)
  | TNF_Fun(x, tp)           -> TDFun(x, tp)

let rec simple_view tp =
  match tp with
  | TDEffPure             -> TDEffPure
  | TDEffCons(eff1, eff2) -> TDEffCons(eff1, eff2)
  | TDVar v ->
    begin match !v with
    | TTV_Var(perm, x) ->
      begin match x.value with
      | TV_None | TV_Frozen -> TDVar(perm, x)
      | TV_Type tp ->
        let tp = of_nf_view (nf_view tp) in
        x.value <- TV_Type tp;
        let tp = perm_subst_m perm TConst.Map.empty tp in
        v := TTV_Type tp;
        simple_view tp
      end
    | TTV_Type tp ->
      let tp = of_nf_view (nf_view tp) in
      v := TTV_Type tp;
      simple_view tp
    end
  | TDConst x              -> TDConst x
  | TDArrow(tp1, tp2, eff) -> TDArrow(tp1, tp2, eff)
  | TDFun(x, tp)           -> TDFun(x, tp)
  | TDApp(tp1, tp2)        -> TDApp(tp1, tp2)

and perm_subst_m perm sub tp =
  match nf_view tp with
  | TNF_EffPure -> eff_pure
  | TNF_EffCons(eff1, eff2) ->
    eff_cons (perm_subst_m perm sub eff1) (perm_subst_m perm sub eff2)
  | TNF_Neu(head, args) ->
    let head =
      match head with
      | HVar(perm1, x) ->
        let dom =
          sub
          |> TConst.Map.bindings
          |> List.map fst
          |> TConst.Set.of_list
        in
        let perm = TCPerm.compose perm perm1 in
        restrict_tvar x (TCPerm.image_of (TCPerm.rev perm) dom);
        TDVar(ref (TTV_Var(perm, x)))
      | HConst x ->
        let x = TCPerm.apply perm x in
        begin match TConst.Map.find_opt x sub with
        | None    -> TDConst x
        | Some tp -> tp
        end
    in
    apps head (List.map (perm_subst_m perm sub) args)
  | TNF_Arrow(tp1, tp2, eff) ->
    arrow
      (perm_subst_m perm sub tp1)
      (perm_subst_m perm sub tp2)
      (perm_subst_m perm sub eff)
  | TNF_Fun(x, tp) ->
    let y = TConst.clone x in
    TDFun(y, perm_subst_m (TCPerm.compose perm (TCPerm.swap x y)) sub tp)

and subst x s tp =
  perm_subst_m TCPerm.id (TConst.Map.singleton x s) tp

and normalize tp args =
  match simple_view tp with
  | TDEffPure ->
    assert (args = []);
    TNF_EffPure
  | TDEffCons(eff1, eff2) ->
    assert (args = []);
    TNF_EffCons(eff1, eff2)
  | TDVar(perm, x)         -> TNF_Neu(HVar(perm, x), args)
  | TDConst x              -> TNF_Neu(HConst x, args)
  | TDArrow(tp1, tp2, eff) ->
    assert (args = []);
    TNF_Arrow(tp1, tp2, eff)
  | TDFun(x, tp) ->
    begin match args with
    | []          -> TNF_Fun(x, tp)
    | arg :: args -> normalize (subst x arg tp) args
    end
  | TDApp(tp1, tp2) -> normalize tp1 (tp2 :: args)

and nf_view tp =
  normalize tp []

let view tp =
  let rec unsnoc arg args =
    match args with
    | [] -> ([], arg)
    | x :: args ->
      let (args, y) = unsnoc x args in
      (arg :: args, y)
  in
  match nf_view tp with
  | TNF_EffPure                -> TEffPure
  | TNF_EffCons(eff1, eff2)    -> TEffCons(eff1, eff2)
  | TNF_Neu(HVar(perm, x), []) -> TVar(perm, x)
  | TNF_Neu(HConst c, [])      -> TConst c
  | TNF_Neu(head, arg :: args) ->
    let (args, arg) = unsnoc arg args in
    TApp(of_nf_view(TNF_Neu(head, args)), arg)
  | TNF_Arrow(tp1, tp2, eff)   -> TArrow(tp1, tp2, eff)
  | TNF_Fun(x, tp)             -> TFun(x, tp)

let of_view tv =
  match tv with
  | TEffPure              -> TDEffPure
  | TEffCons(eff1, eff2)  -> TDEffCons(eff1, eff2)
  | TVar(perm, x)         -> TDVar (ref (TTV_Var(perm, x)))
  | TConst c              -> TDConst c
  | TArrow(tp1, tp2, eff) -> TDArrow(tp1, tp2, eff)
  | TFun(x, tp)           -> TDFun(x, tp)
  | TApp(tp1, tp2)        -> TDApp(tp1, tp2)

(* ========================================================================= *)

module TVar = struct
  exception Escapes_scope of TConst.t

  module Core = struct
    type t = tvar
    let compare x y = Utils.UID.compare x.id y.id
  end
  include Core

  let fresh ~scope kind =
    { id    = Utils.UID.fresh ()
    ; scope = scope
    ; kind  = kind
    ; value = TV_None
    }

  let uid  x = x.id
  let kind x = x.kind

  let scope x = x.scope

  let equal x y = Utils.UID.equal x.id y.id

  let rec check_scope scope tp =
    match nf_view tp with
    | TNF_EffPure -> ()
    | TNF_EffCons(eff1, eff2) ->
      check_scope scope eff1;
      check_scope scope eff2
    | TNF_Neu(h, args) ->
      begin match h with
      | HVar(perm, x) ->
        begin match x.value with
        | TV_None ->
          x.scope <-
            TConst.Set.inter x.scope (TCPerm.image_of (TCPerm.rev perm) scope)
        | TV_Frozen -> ()
        | TV_Type _ -> assert false
        end
      | HConst c ->
        if TConst.Set.mem c scope then ()
        else raise (Escapes_scope c)
      end;
      List.iter (check_scope scope) args
    | TNF_Arrow(tp1, tp2, eff) ->
      check_scope scope tp1;
      check_scope scope tp2;
      check_scope scope eff
    | TNF_Fun(x, tp) ->
      check_scope (TConst.Set.add x scope) tp
      
  let set x tp =
    assert (x.value = TV_None);
    check_scope x.scope tp;
    x.scope <- TConst.Set.empty;
    x.value <- TV_Type tp

  let set' x tv = set x (of_view tv)

  let freeze x =
    assert (x.value = TV_None || x.value = TV_Frozen);
    x.value <- TV_Frozen

  let restrict = restrict_tvar

  module Set = Set.Make(Core)
  module Map = Map.Make(Core)
end

let of_row_view rv =
  match rv with
  | RNil -> TDEffPure
  | RVar(perm, x) -> TDVar(ref (TTV_Var(perm, x)))
  | RCons(eff1, eff2) -> TDEffCons(eff1, eff2)

let rec row_view tp =
  match view tp with
  | TEffPure         -> RNil
  | TEffCons(l, eff) ->
    begin match row_view l with
    | RNil   -> row_view eff
    | RVar(perm, x) ->
      begin match row_view eff with
      | RNil -> RVar(perm, x)
      | eff  -> RCons(TDVar(ref (TTV_Var(perm, x))), of_row_view eff)
      end
    | RCons(l, eff') -> RCons(l, TDEffCons(eff', eff))
    end
  | TVar(perm, x) -> RVar(perm, x)
  | TConst _ | TApp _ ->
    RCons(tp, TDEffPure)
  | TArrow _ | TFun _ -> assert false

let perm p tp =
  perm_subst_m p TConst.Map.empty tp

let fresh_tvar ~scope kind =
  var (TVar.fresh ~scope kind)

let rec kind tp =
  match tp with
  | TDVar   v ->
    begin match !v with
    | TTV_Var(_, x) -> TVar.kind x
    | TTV_Type tp   -> kind tp
    end
  | TDConst c -> TConst.kind c
  | TDArrow _ -> Kind.ktype
  | TDEffPure | TDEffCons _ -> Kind.keffect
  | TDFun(x, tp) -> Kind.arrow (TConst.kind x) (kind tp)
  | TDApp(tp, _) ->
    begin match Kind.view(kind tp) with
    | Kind.KArrow(_, k) -> k
    | _ -> assert false
    end

let rec contains_tvar x tp =
  match nf_view tp with
  | TNF_EffPure -> false
  | TNF_EffCons(l, eff) ->
    contains_tvar x l || contains_tvar x eff
  | TNF_Neu(h, args) ->
    begin match h with
    | HVar(_, y) -> TVar.equal x y
    | HConst _   -> false
    end || List.exists (contains_tvar x) args
  | TNF_Arrow(tp1, tp2, eff) ->
    contains_tvar x tp1 || contains_tvar x tp2 || contains_tvar x eff
  | TNF_Fun(_, tp) -> contains_tvar x tp

let contains_tvar' x tv =
  contains_tvar x (of_view tv)

let rec tvars tp =
  match nf_view tp with
  | TNF_EffPure -> TVar.Set.empty
  | TNF_EffCons(l, eff) ->
    TVar.Set.union (tvars l) (tvars eff)
  | TNF_Neu(head, args) ->
    List.fold_left (fun tvs tp -> TVar.Set.union tvs (tvars tp))
      begin match head with
      | HVar(_, x) -> TVar.Set.singleton x
      | HConst _   -> TVar.Set.empty
      end
      args
  | TNF_Arrow(tp1, tp2, eff) -> 
    TVar.Set.union (TVar.Set.union (tvars tp1) (tvars tp2))
      (tvars eff)
  | TNF_Fun(_, tp) -> tvars tp

let open_with sub tp =
  perm_subst_m TCPerm.id sub tp

let opening_subst ~scope ?(vtypes=[]) ?(ctypes=[]) () =
  let sub = TConst.Map.empty in
  let (sub, tvs) = Utils.ListExt.fold_map (fun sub c ->
      let x = TVar.fresh ~scope (TConst.kind c) in
      (TConst.Map.add c (var x) sub, x)
    ) sub vtypes in
  let sub = List.fold_left (fun sub (c, c') ->
      TConst.Map.add c (TDConst c') sub
    ) sub ctypes in
  (sub, tvs)

let rec visible_for ~scope tp =
  match nf_view tp with
  | TNF_EffPure -> true
  | TNF_EffCons(eff1, eff2) ->
    visible_for ~scope eff1 && visible_for ~scope eff2
  | TNF_Neu(head, args) ->
    begin match head with
    | HVar _   -> true
    | HConst c -> TConst.Set.mem c scope
    end
    && List.for_all (visible_for ~scope) args
  | TNF_Arrow(tp1, tp2, eff) ->
    visible_for ~scope tp1
    && visible_for ~scope tp2
    && visible_for ~scope eff
  | TNF_Fun(x, tp) ->
    visible_for ~scope:(TConst.Set.add x scope) tp

let rec is_positive x tp =
  match nf_view tp with
  | TNF_EffPure -> true
  | TNF_EffCons(l, eff) -> is_positive x l && is_positive x eff
  | TNF_Neu(_, args) ->
    not (List.exists (contains_tvar x) args)
  | TNF_Arrow(tp1, tp2, eff) ->
    is_negative x tp1 && is_positive x tp2 && is_positive x eff
  | TNF_Fun(_, tp) -> not (contains_tvar x tp)

and is_negative x tp =
  match nf_view tp with
  | TNF_EffPure -> true
  | TNF_EffCons(l, eff) -> is_negative x l && is_negative x eff
  | TNF_Neu(HVar(_, y), args) ->
    not (TVar.equal x y)
    && not (List.exists (contains_tvar x) args)
  | TNF_Neu(HConst _, args) ->
    not (List.exists (contains_tvar x) args)
  | TNF_Arrow(tp1, tp2, eff) ->
    is_positive x tp1 && is_negative x tp2 && is_negative x eff
  | TNF_Fun(_, tp) -> not (contains_tvar x tp)
