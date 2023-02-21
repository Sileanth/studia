open Node

type kvar
type kind
type kind_view =
| KType
| KEffect
| KVar   of kvar
| KArrow of kind * kind

type mpath = string list

module TConst : sig
  type info = mpath * kind
  include Utils.Variable.S with type typ := info
  val kind  : t -> kind
  val mpath : t -> mpath
  val full_name : t -> string
end
type tconst = TConst.t

type tcperm
type tvar
type typ
type effect = typ

type type_head =
| HVar   of tcperm * tvar
| HConst of tconst

type type_nf_view =
| TNF_EffPure
| TNF_EffCons of effect * effect
| TNF_Neu     of type_head * typ list
| TNF_Arrow   of typ * typ * effect
| TNF_Fun     of tconst * typ

type type_view =
| TEffPure
| TEffCons of effect * effect
| TVar     of tcperm * tvar
| TConst   of tconst
| TArrow   of typ * typ * effect
| TFun     of tconst * typ
| TApp     of typ * typ

type row_view =
| RNil
| RVar  of tcperm * tvar
| RCons of effect * effect

type scheme

type op_decl = (Utils.Position.t, op_decl_data) node
and op_decl_data =
| OpDecl of string * tconst list * typ list * typ

type adt_ctor = (Utils.Position.t, adt_ctor_data) node
and adt_ctor_data =
| ADTCtor of string * tconst list * typ list

type field_decl = (Utils.Position.t, field_decl_data) node
and field_decl_data =
| FieldDecl of string * tconst list * typ

type typedef =
| TDEffect of tconst * tconst list * op_decl list
| TDData   of tconst * tconst list * adt_ctor list
| TDRecord of tconst * tconst list * field_decl list

type var

type eff_coercion =
| ECId   of effect
| ECOpen of effect
| ECLift of effect * effect
| ECSwap of effect * effect * effect
| ECCons of effect * eff_coercion
| ECComp of eff_coercion * eff_coercion

type val_coercion =
| VCId
| VCArrow of val_coercion * val_coercion * eff_coercion

type pattern = (Utils.Position.t, pattern_data) node
and pattern_data =
| PWildcard of scheme
| PVar      of var
| PCoerce   of val_coercion * pattern * typ
| PCtor     of tconst * typ list * int * tconst list * pattern list
| PRecord   of tconst * typ list * (int * pattern) list

type expr_meta =
  { em_pos    : Utils.Position.t
  ; em_type   : typ
  ; em_effect : effect
  }

type expr = (expr_meta, expr_data) node
and expr_data =
| ENum      of int
| EChar     of char
| EString   of string
| EVar      of var
| ECtor     of tconst * typ list * int * typ list
| ECoerce   of val_coercion * eff_coercion * expr
| EFun      of pattern * expr
| EOp       of tconst * typ list * int * typ list
| EApp      of expr * expr
| ELet      of var * expr * expr
| EFix      of rec_function list * expr
| EMatch    of expr * match_clause list
| EHandle   of tconst * typ list * expr * handler list
| ERecord   of tconst * typ list * (int * field_def) list
| ETypedef  of typedef list * expr
| EAbsType  of tconst * expr
| EExtern   of string * scheme
| EReplExpr of expr * Utils.Seal.t * (unit -> expr)
| ERepl     of Utils.Seal.t * (unit -> expr)

and rec_function = var * expr
and match_clause = pattern * expr

and handler = (Utils.Position.t, handler_data) node
and handler_data =
| HReturn of pattern * expr
| HOp     of int * tconst list * pattern list * var * expr

and field_def = (Utils.Position.t, field_def_data) node
and field_def_data =
| FieldDefMono of expr
| FieldDefPoly of tconst list * expr

module TCPerm : sig
  type t = tcperm

  val id      : t
  val rev     : t -> t
  val compose : t -> t -> t
  val swap    : tconst -> tconst -> t

  val image_of : t -> TConst.Set.t -> TConst.Set.t
  val carrier  : t -> TConst.Set.t
end

module KVar : sig
  type t = kvar

  val equal : t -> t -> bool

  val set  : t -> kind -> unit
  val set' : t -> kind_view -> unit
end

module Kind : sig
  type t = kind

  val ktype   : t
  val keffect : t

  val arrow  : t -> t -> t
  val arrows : t list -> t -> t

  val view : t -> kind_view

  val fresh_kvar : unit -> t

  val contains_kvar  : kvar -> kind -> bool 
  val contains_kvar' : kvar -> kind_view -> bool
end

module TVar : sig
  exception Escapes_scope of tconst

  type t = tvar

  val compare : t -> t -> int

  val fresh : scope:TConst.Set.t -> kind -> t

  val uid  : t -> Utils.UID.t
  val kind : t -> kind

  val scope : t -> TConst.Set.t

  val equal : t -> t -> bool

  val set  : t -> typ -> unit
  val set' : t -> type_view -> unit

  val freeze : t -> unit

  val restrict : t -> TConst.Set.t -> unit

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

module Type : sig
  type t = typ

  val nf_view  : t -> type_nf_view
  val view     : t -> type_view
  val row_view : t -> row_view

  val of_nf_view  : type_nf_view -> t
  val of_view     : type_view    -> t
  val of_row_view : row_view     -> t

  val perm : tcperm -> t -> t

  val var    : tvar -> t
  val tconst : tconst -> t

  val arrow  : t -> t -> t -> t
  val arrows : t list -> t -> t -> t

  val app  : t -> t -> t
  val apps : t -> t list -> t

  val tfun  : tconst -> t -> t
  val tfuns : tconst list -> t -> t

  val eff_pure : t
  val eff_cons : t -> t -> t

  val fresh_tvar : scope:TConst.Set.t -> kind -> t

  val kind : t -> kind

  val contains_tvar  : tvar -> t -> bool 
  val contains_tvar' : tvar -> type_view -> bool

  val tvars : t -> TVar.Set.t

  val open_with : t TConst.Map.t -> t -> t

  val opening_subst :
    scope:TConst.Set.t ->
    ?vtypes: tconst list ->
    ?ctypes: (tconst * tconst) list ->
      unit -> t TConst.Map.t * tvar list

  (** visible_for scope tp checks if all type constants that occurs in tp
    are visible in the scope. *)
  val visible_for : scope:TConst.Set.t -> t -> bool
end

module Scheme : sig
  type t = scheme

  val qvars : t -> tconst list
  val body  : t -> typ
  val subst : t -> (tvar * typ) list

  val apply_subst    : t -> typ -> typ

  val open_s     : scope:TConst.Set.t -> t -> tvar list * typ
  val close_with : ?abs_types: tconst list -> tvar list -> typ -> t

  (** Same as {!close_with}, but sets to {v [] v} all the type variables that:
  - have kind {v effect v};
  - are in s;
  - have only positive occurences
  into {v [] v}. The function is useful, since it make type scheme smaller and
  still equivalent.
  *)
  val close_down_with : ?abs_types: tconst list -> tvar list -> typ -> t

  (** Coerce scheme to type. Fails if scheme quanitfies some variables *)
  val to_type : t -> typ

  val tvars : t -> TVar.Set.t
end

module TypeDef : sig
  type t = typedef

  val tconst : t -> tconst

  val tvars : t -> TVar.Set.t
end

module Var : sig
  type t = var
  val fresh  : ?name: string -> scheme -> t
  val name   : t -> string
  val scheme : t -> scheme
  val typ    : t -> typ

  val update_scheme : t -> scheme -> unit

  module Map : Map.S with type key = t
end

module Expr : sig
  type t = expr

  val is_value : t -> bool
end

module Keys : sig
  val repl_effect    : effect Utils.Seal.key
  val repl_expr_type : typ Utils.Seal.key
end

val flow_node : expr Flow.node
