open Kind.Datatypes
open TVar.Datatypes

module Core = struct
  type _ t =
  | TEffPure : k_effect t
  | TEffCons : effect * effect -> k_effect t
  | TAny     : 'k kind -> 'k t
  | TVar     : 'k tvar -> 'k t
  | TArrow   : ttype * ttype * effect -> k_type t
  | TForall  : 'k tvar * ttype -> k_type t
  | TFun     : 'k1 tvar * 'k2 t -> ('k1, 'k2) k_arrow t
  | TApp     : ('k1, 'k2) k_arrow t * 'k1 t -> 'k2 t

  and ttype  = k_type t
  and effect = k_effect t
end

module T = struct
  include Core

  type (_, 'k) params =
  | TP_Nil  : ('k, 'k) params
  | TP_Cons : 'k1 t * ('k0, 'k) params -> (('k1, 'k0) k_arrow, 'k) params

  type 'k head =
  | HVar : 'k tvar -> 'k head
  | HAny : 'k kind -> 'k head

  type 'k neutral =
  | TNeu : 'xk head * ('xk, 'k) params -> 'k neutral

  type at_effect = k_effect neutral

  type _ view =
  | TVEffect  : at_effect list -> k_effect view
  | TVNeutral : 'k neutral -> 'k view
  | TVArrow   : ttype * ttype * effect -> k_type view
  | TVForall  : 'k tvar * ttype -> k_type view
  | TVFun     : 'k1 tvar * 'k2 t -> ('k1, 'k2) k_arrow view

  module Exists = Utils.Exists.Make(Core)
  module TVarMap = TVar.Map.Make(Core)
end

module Datatypes = struct
  type 'k typ = 'k T.t

  type ttype  = k_type typ
  type effect = k_effect typ

  type ('xk, 'k) type_params = ('xk, 'k) T.params =
  | TP_Nil  : ('k, 'k) type_params
  | TP_Cons : 'k1 typ * ('k0, 'k) type_params ->
      (('k1, 'k0) k_arrow, 'k) type_params

  type 'k type_head = 'k T.head =
  | HVar : 'k tvar -> 'k type_head
  | HAny : 'k kind -> 'k type_head

  type 'k neutral_type = 'k T.neutral =
  | TNeu : 'xk type_head * ('xk, 'k) type_params -> 'k neutral_type

  type at_effect = k_effect neutral_type

  type 'k type_view = 'k T.view =
  | TVEffect  : at_effect list -> k_effect type_view
  | TVNeutral : 'k neutral_type -> 'k type_view
  | TVArrow   : ttype * ttype * effect -> k_type type_view
  | TVForall  : 'k tvar * ttype -> k_type type_view
  | TVFun     : 'k1 tvar * 'k2 typ -> ('k1, 'k2) k_arrow type_view
end

module type S = sig
  type 'k t = 'k T.t

  type ttype  = k_type t
  type effect = k_effect t

  type ('xk, 'k) params = ('xk, 'k) T.params =
  | TP_Nil  : ('k, 'k) params
  | TP_Cons : 'k1 t * ('k0, 'k) params -> (('k1, 'k0) k_arrow, 'k) params

  type 'k head = 'k T.head =
  | HVar : 'k tvar -> 'k head
  | HAny : 'k kind -> 'k head

  type 'k neutral = 'k T.neutral =
  | TNeu : 'xk head * ('xk, 'k) params -> 'k neutral

  type at_effect = k_effect neutral

  type 'k view = 'k T.view =
  | TVEffect  : at_effect list -> k_effect view
  | TVNeutral : 'k neutral -> 'k view
  | TVArrow   : ttype * ttype * effect -> k_type view
  | TVForall  : 'k tvar * ttype -> k_type view
  | TVFun     : 'k1 tvar * 'k2 t -> ('k1, 'k2) k_arrow view

  module Exists : Utils.Exists.S
    with type 'k data = 'k t
    and  type t = T.Exists.t
  include module type of Exists.Datatypes

  module TVarMap : TVar.Map.S
    with type 'k v = 'k t
    and  type  t = T.TVarMap.t

  val view : 'k t -> 'k view

  (* constructors *)
  val var      : 'k tvar -> 'k t
  val any      : 'k kind -> 'k t
  val eff_pure : effect
  val eff_cons : effect -> effect -> effect
  val arrow    : ttype -> ttype -> effect -> ttype
  val forall   : 'k tvar -> ttype -> ttype
  val tfun     : 'k1 tvar -> 'k2 t -> ('k1, 'k2) k_arrow t
  val app      : ('k1, 'k2) k_arrow t -> 'k1 t -> 'k2 t

  val eff_conses : effect list -> effect
  val arrows     : ttype list -> ttype -> effect -> ttype
  val foralls    : TVar.ex list -> ttype -> ttype

  val of_neutral     : 'k neutral -> 'k t
  val to_neutral_opt : 'k t -> 'k neutral option

  val of_at_effects : at_effect list -> effect
  val to_at_effects : effect -> at_effect list

  val kind : 'k t -> 'k kind 
  val head_kind : 'k head -> 'k kind

  val rename_m : TVar.TVarMap.t -> 'k t -> 'k t
  val subst_m  : TVarMap.t -> 'k t -> 'k t

  val rename_params_m :
    TVar.TVarMap.t -> ('k1, 'k2) params -> ('k1, 'k2) params

  val rename_neutral_m :
    TVar.TVarMap.t -> 'k neutral -> 'k neutral

  val rename : 'k1 tvar -> 'k1 tvar -> 'k t -> 'k t
  val subst  : 'k1 tvar -> 'k1 t -> 'k t -> 'k t
end
include T
include T.Exists.Datatypes

let var x = TVar x
let any k = TAny k
let eff_pure = TEffPure
let eff_cons eff1 eff2 = TEffCons(eff1, eff2)
let arrow tp1 tp2 eff = TArrow(tp1, tp2, eff)
let forall x tp = TForall(x, tp)
let tfun x tp   = TFun(x, tp)
let app tp1 tp2 = TApp(tp1, tp2)

let rec eff_conses effs =
  match effs with
  | [] -> TEffPure
  | [ eff ] -> eff
  | eff :: effs -> TEffCons(eff, eff_conses effs)

let rec arrows tps tp eff =
  match tps with
  | [] -> failwith "no-arg function"
  | [ tp1 ]    -> TArrow(tp1, tp, eff)
  | tp1 :: tps -> TArrow(tp1, arrows tps tp eff, TEffPure)

let rec foralls tvs tp =
  match tvs with
  | [] -> tp
  | TVar.Pack x :: tvs -> TForall(x, foralls tvs tp)

let rec apply : type k1 k2. k1 t -> (k1, k2) params -> k2 t =
    fun tp params ->
  match params with
  | TP_Nil -> tp
  | TP_Cons(p, params) -> apply (TApp(tp, p)) params

let of_head h =
  match h with
  | HVar x -> TVar x
  | HAny k -> TAny k

let of_neutral (TNeu(h, params)) =
  apply (of_head h) params

let rec of_at_effects ats =
  match ats with
  | []        -> TEffPure
  | [at]      -> of_neutral at
  | at :: ats -> TEffCons(of_neutral at, of_at_effects ats)

let rec kind : type k. k t -> k kind =
  function
  | TEffPure     -> KEffect
  | TEffCons _   -> KEffect
  | TAny k       -> k
  | TVar x       -> TVar.kind x
  | TArrow  _    -> KType
  | TForall _    -> KType
  | TFun(x, tp)  -> KArrow(TVar.kind x, kind tp)
  | TApp(tp1, _) ->
    begin match kind tp1 with
    | KArrow(_, k) -> k
    end

let head_kind : type k. k head -> k kind =
  function
  | HAny k -> k
  | HVar x -> TVar.kind x

let rec rename_m : type k.
    TVar.TVarMap.t -> k t -> k t =
    fun tm tp ->
  match tp with
  | TEffPure -> TEffPure
  | TEffCons (e1, e2) ->
    TEffCons (rename_m tm e1, rename_m tm e2)
  | TAny k  -> TAny k
  | TVar x  -> TVar (TVar.rename_m tm x)
  | TArrow(tp1, tp2, eff) ->
    TArrow(rename_m tm tp1, rename_m tm tp2, rename_m tm eff)
  | TForall(x, tp) ->
    let y = TVar.clone x in
    TForall(y, rename_m (TVar.TVarMap.add x y tm) tp)
  | TFun(x, tp) ->
    let y = TVar.clone x in
    TFun(y, rename_m (TVar.TVarMap.add x y tm) tp)
  | TApp(tp1, tp2) ->
    TApp(rename_m tm tp1, rename_m tm tp2)

let rec subst_m : type k. TVarMap.t -> k t -> k t =
    fun m tp ->
  match tp with
  | TEffPure -> TEffPure
  | TEffCons (e1, e2) ->
    TEffCons (subst_m m e1, subst_m m e2)
  | TAny k  -> TAny k
  | TVar x  ->
    begin match TVarMap.find_opt x m with
    | None    -> TVar x
    | Some tp -> tp
    end
  | TArrow(tp1, tp2, eff) ->
    TArrow(subst_m m tp1, subst_m m tp2, subst_m m eff)
  | TForall(x, tp) ->
    let y = TVar.clone x in
    TForall(y, subst_m (TVarMap.add x (TVar y) m) tp)
  | TFun(x, tp) ->
    let y = TVar.clone x in
    TFun(y, subst_m (TVarMap.add x (TVar y) m) tp)
  | TApp(tp1, tp2) ->
    TApp(subst_m m tp1, subst_m m tp2)

let rec rename_params_m : type k1 k2.
    TVar.TVarMap.t -> (k1, k2) params -> (k1, k2) params =
    fun tm params ->
  match params with
  | TP_Nil -> TP_Nil
  | TP_Cons(tp, params) -> TP_Cons(rename_m tm tp, rename_params_m tm params)

let rename_head_m tm h =
  match h with
  | HVar l -> HVar (TVar.rename_m tm l)
  | HAny k -> HAny k

let rename_neutral_m tm (TNeu(h, params)) =
  TNeu(rename_head_m tm h, rename_params_m tm params)

let rename (type k1 k2) (x : k1 tvar) (y : k1 tvar) (tp : k2 t) : k2 t =
  rename_m (TVar.TVarMap.singleton x y) tp

let subst (type k1 k2) (x : k1 tvar) (s : k1 t) (tp : k2 t) : k2 t =
  subst_m (TVarMap.singleton x s) tp

let rec normalize : type k1 k2. k1 t -> (k1, k2) params -> k2 view =
    fun tp params ->
  match tp with
  | TEffPure ->
    begin match params with
    | TP_Nil -> TVEffect []
    end
  | TEffCons(eff1, eff2) ->
    begin match params with
    | TP_Nil -> TVEffect(to_at_effects eff1 @ to_at_effects eff2)
    end
  | TAny k -> TVNeutral (TNeu (HAny k, params))
  | TVar x -> TVNeutral (TNeu (HVar x, params))
  | TArrow(tp1, tp2, eff) ->
    begin match params with
    | TP_Nil -> TVArrow(tp1, tp2, eff)
    end
  | TForall(x, tp) ->
    begin match params with
    | TP_Nil -> TVForall(x, tp)
    end
  | TFun(x, tp) ->
    begin match params with
    | TP_Nil -> TVFun(x, tp)
    | TP_Cons(p1, params) ->
      normalize (subst x p1 tp) params
    end
  | TApp(tp1, tp2) ->
    normalize tp1 (TP_Cons(tp2, params))

and view : type k. k t -> k view =
    fun tp ->
  match kind tp with
  | KEffect ->
    begin match normalize tp TP_Nil with
    | TVEffect ats -> TVEffect ats
    | TVNeutral at -> TVEffect [ at ]
    end
  | _ -> normalize tp TP_Nil

and to_at_effects (eff : effect) : at_effect list =
  match view eff with
  | TVEffect ats -> ats
  | TVNeutral at -> [ at ]

let to_neutral_opt (type k) (tp : k t) : k neutral option =
  match view tp with
  | TVEffect [ neu ] -> Some neu
  | TVEffect ([] | _ :: _ :: _) -> None
  | TVNeutral neu -> Some neu
  | TVArrow  _ -> None
  | TVForall _ -> None
  | TVFun    _ -> None
