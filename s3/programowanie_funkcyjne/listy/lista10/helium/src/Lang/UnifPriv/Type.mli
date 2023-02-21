type tvar
type t
type ttype  = t
type effect = t

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

module TVar : sig
  type t = tvar

  exception Escapes_scope of TConst.t

  val compare : t -> t -> int

  val fresh : scope:TConst.Set.t -> Kind.t -> t

  val uid  : t -> Utils.UID.t
  val kind : t -> Kind.t

  val scope : t -> TConst.Set.t

  val equal : t -> t -> bool

  val set  : t -> ttype -> unit
  val set' : t -> view  -> unit

  val freeze : t -> unit

  val restrict : t -> TConst.Set.t -> unit

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

val nf_view  : t -> nf_view
val view     : t -> view
val row_view : t -> row_view

val of_nf_view  : nf_view -> t
val of_view     : view -> t
val of_row_view : row_view  -> t

val perm : TCPerm.t -> t -> t

val var    : tvar -> t
val tconst : TConst.t -> t

val arrow  : t -> t -> t -> t
val arrows : t list -> t -> t -> t

val app  : t -> t -> t
val apps : t -> t list -> t

val tfun  : TConst.t -> t -> t
val tfuns : TConst.t list -> t -> t

val eff_pure : t
val eff_cons : t -> t -> t

val fresh_tvar : scope:TConst.Set.t -> Kind.t -> t

val kind : t -> Kind.t

val contains_tvar  : tvar -> t -> bool 
val contains_tvar' : tvar -> view -> bool

val tvars : t -> TVar.Set.t

val open_with : t TConst.Map.t -> t -> t

val opening_subst :
  scope:TConst.Set.t ->
  ?vtypes: TConst.t list ->
  ?ctypes: (TConst.t * TConst.t) list ->
    unit -> t TConst.Map.t * tvar list

(** visible_for scope tp checks if all type constants that occurs in tp
  are visible in the scope. *)
val visible_for : scope:TConst.Set.t -> t -> bool

val is_positive : tvar -> t -> bool
