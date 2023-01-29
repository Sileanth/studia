open Node

module Kind = UnifPriv.Kind
type kvar = Kind.kvar
type kind = Kind.t
type kind_view = Kind.view =
| KType
| KEffect
| KVar   of kvar
| KArrow of kind * kind

module TConst = UnifPriv.TConst
type mpath  = TConst.mpath
type tconst = TConst.t

module TCPerm = UnifPriv.TCPerm
type tcperm = TCPerm.t

module Type = UnifPriv.Type
type tvar   = Type.tvar
type typ    = Type.t
type effect = Type.t

type type_head = Type.head =
| HVar   of tcperm * tvar
| HConst of tconst

type type_nf_view = Type.nf_view =
| TNF_EffPure
| TNF_EffCons of effect * effect
| TNF_Neu     of type_head * typ list
| TNF_Arrow   of typ * typ * effect
| TNF_Fun     of tconst * typ

type type_view = Type.view =
| TEffPure
| TEffCons of effect * effect
| TVar     of tcperm * tvar
| TConst   of tconst
| TArrow   of typ * typ * effect
| TFun     of tconst * typ
| TApp     of typ * typ

type row_view = Type.row_view =
| RNil
| RVar  of tcperm * tvar
| RCons of effect * effect

module Scheme = UnifPriv.Scheme
type scheme = Scheme.t

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

type var =
  {         var_id     : Utils.UID.t
  ;         var_name   : string
  ; mutable var_scheme : scheme
  }

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

module KVar = Kind.KVar
module TVar = Type.TVar

module OpDecl = struct
  type t = op_decl

  let tvars op =
    match op.data with
    | OpDecl(_, ets, inputs, output) ->
      List.fold_left (fun s input ->
          TVar.Set.union s (Type.tvars input)
        ) (Type.tvars output) inputs
end

module ADTCtor = struct
  type t = adt_ctor

  let tvars ctor =
    match ctor.data with
    | ADTCtor(_, _, tps) ->
      List.fold_left (fun s tp ->
          TVar.Set.union s (Type.tvars tp)
        ) TVar.Set.empty tps
end

module FieldDecl = struct
  type t = field_decl

  let tvars fld =
    match fld.data with
    | FieldDecl(_, _, tp) -> Type.tvars tp
end

module TypeDef = struct
  type t = typedef

  let tconst td =
    match td with
    | TDEffect(l, _, _) -> l
    | TDData(l, _, _)   -> l
    | TDRecord(l, _, _) -> l

  let tvars td =
    match td with
    | TDEffect(_, _, ops) ->
      List.fold_left (fun s op ->
          TVar.Set.union s (OpDecl.tvars op)
        ) TVar.Set.empty ops
    | TDData(_, _, ctors) ->
      List.fold_left (fun s ctor ->
          TVar.Set.union s (ADTCtor.tvars ctor)
        ) TVar.Set.empty ctors
    | TDRecord(_, _, fields) ->
      List.fold_left (fun s fld ->
          TVar.Set.union s (FieldDecl.tvars fld)
        ) TVar.Set.empty fields
end

module Var = struct
  module Core = struct
    type t = var
    let compare x y = Utils.UID.compare x.var_id y.var_id
  end
  include Core

  let fresh ?(name="x") sch =
    { var_id     = Utils.UID.fresh ()
    ; var_name   = name
    ; var_scheme = sch
    }

  let name   x = x.var_name
  let scheme x = x.var_scheme
  let typ x = Scheme.to_type (scheme x)

  let update_scheme x sch =
    x.var_scheme <- sch

  module Map = Map.Make(Core)
end

module Expr = struct
  type t = expr

  let rec is_value e =
    match e.data with
    | ENum _ | EChar _ | EString _ | EVar _ | ECtor _ | EFun _ | EOp _
    | EExtern _ -> true
    | ECoerce(_, _, e) -> is_value e
    | ERecord(_, _, flds) ->
      List.for_all (fun (_, fld) ->
        begin match fld.data with
        | FieldDefMono e     -> is_value e
        | FieldDefPoly(_, e) -> is_value e
        end) flds
    | EApp _ | ELet _ | EFix _ | EMatch _ | EHandle _ | ETypedef _
    | EAbsType _ | EReplExpr _ | ERepl _ -> false
end

module Keys = struct
  let repl_effect    = Utils.Seal.gen_key ()
  let repl_expr_type = Utils.Seal.gen_key ()
end

let flow_node = Flow.Node.create "Unif"
