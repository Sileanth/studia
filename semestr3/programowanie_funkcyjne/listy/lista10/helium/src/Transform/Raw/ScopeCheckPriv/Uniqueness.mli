
val check_op_uniqueness         : Lang.Raw.op_decl list -> unit
val check_ctor_uniqueness       : Lang.Raw.adt_ctor list -> unit
val check_field_decl_uniqueness : Lang.Raw.field_decl list -> unit
val check_field_pat_uniqueness  : Lang.Raw.field_pattern list -> unit
val check_recfun_uniqueness     : Lang.Raw.rec_function list -> unit
val check_field_def_uniqueness  : Lang.Raw.field_def list -> unit
val check_typedef_uniqueness    : Lang.Raw.typedef list -> unit
