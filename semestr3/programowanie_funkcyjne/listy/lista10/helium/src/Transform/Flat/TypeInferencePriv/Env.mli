
type t

type op_decl =
  { op_effect      : Lang.Unif.tconst
  ; op_effect_args : Lang.Unif.tconst list
  ; op_index       : int
  ; op_decl        : Lang.Unif.op_decl
  }

type adt_ctor =
  { ctor_datatype      : Lang.Unif.tconst
  ; ctor_datatype_args : Lang.Unif.tconst list
  ; ctor_index         : int
  ; ctor_decl          : Lang.Unif.adt_ctor
  }

type field_decl =
  { fld_record      : Lang.Unif.tconst
  ; fld_record_args : Lang.Unif.tconst list
  ; fld_index       : int
  ; fld_decl        : Lang.Unif.field_decl
  }

type tconst =
| TConst of Lang.Unif.tconst
| TAlias of Lang.Unif.typ

type bool_type =
  { bt_type      : Lang.Unif.typ
  ; bt_true_pat  : Lang.Unif.pattern
  ; bt_false_pat : Lang.Unif.pattern
  }

type unit_type =
  { ut_type  : Lang.Unif.typ
  ; ut_value : Lang.Unif.expr
  }

type list_type =
  { lt_nil_name   : string
  ; lt_nil_proxy  : Lang.Unif.var
  ; lt_nil_ctor   : adt_ctor
  ; lt_cons_name  : string
  ; lt_cons_proxy : Lang.Unif.var
  ; lt_cons_ctor  : adt_ctor
  }

val init : unit -> t

val add_var    : t -> Lang.Flat.var -> Lang.Unif.scheme -> t * Lang.Unif.var
val add_op     : t -> Lang.Flat.op -> op_decl -> t
val add_ctor   : t -> Lang.Flat.ctor -> adt_ctor -> t
val add_field  : t -> Lang.Flat.field -> field_decl -> t

val add_op_proxy : t -> Lang.Flat.op -> Lang.Unif.scheme -> t * Lang.Unif.var
val add_ctor_proxy : t ->
  Lang.Flat.ctor -> Lang.Unif.scheme -> t * Lang.Unif.var
val add_field_sel : t ->
  Lang.Flat.field -> Lang.Unif.scheme -> t * Lang.Unif.var

val extend_tconst : t -> ?fresh:bool -> Lang.Unif.tconst -> t

val add_tconst : t -> ?fresh:bool -> ?kind:Lang.Unif.kind ->
  Lang.Flat.tconst -> t * Lang.Unif.tconst

val add_tconst' : t -> ?fresh:bool -> string -> Lang.Unif.kind ->
  t * Lang.Unif.tconst

val add_type_alias : t -> Lang.Flat.tconst -> Lang.Unif.typ -> t

val add_typedef : t -> Lang.Unif.tconst -> SyntaxExt.typedef -> t

val lookup_var    : t -> Lang.Flat.var -> Lang.Unif.var
val lookup_op     : t -> Lang.Flat.op -> op_decl
val lookup_ctor   : t -> Lang.Flat.ctor -> adt_ctor
val lookup_field  : t -> Lang.Flat.field -> field_decl
val lookup_tconst : t -> Lang.Flat.tconst -> tconst

val lookup_op_proxy   : t -> Lang.Flat.op    -> Lang.Unif.var
val lookup_ctor_proxy : t -> Lang.Flat.ctor  -> Lang.Unif.var
val lookup_field_sel  : t -> Lang.Flat.field -> Lang.Unif.var

val lookup_typedef : t -> Lang.Unif.tconst -> SyntaxExt.typedef option

val register_bool_type : t -> bool_type -> t
val register_unit_type : t -> unit_type -> t
val register_list_type : t -> list_type -> t
val register_printer_type : t -> Lang.Unif.tconst -> t

val register_printer_interp  : t -> Lang.Flat.expr -> t
val register_default_printer : t -> Lang.Unif.var -> t
val register_printer         : t -> Lang.Unif.var -> t

val bool_type : pos:Utils.Position.t -> t -> bool_type
val unit_type : pos:Utils.Position.t -> t -> unit_type
val list_type : pos:Utils.Position.t -> t -> list_type

val printer_type    : t -> Lang.Unif.tconst option
val printer_interp  : t -> Lang.Flat.expr option
val default_printer : t -> Lang.Unif.var option
val printers        : t -> Lang.Unif.var list

val can_swap : t -> Lang.Unif.tconst -> Lang.Unif.tconst -> bool

val tvars : t -> Lang.Unif.TVar.Set.t

val type_scope : t -> Lang.Unif.TConst.Set.t
