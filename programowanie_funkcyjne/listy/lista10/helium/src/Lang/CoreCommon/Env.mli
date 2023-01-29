open Kind.Datatypes
open Var.Datatypes
open TVar.Datatypes
open Type.Datatypes
open TypeDef.Datatypes

type t

val empty : t

val add_var   : t -> var -> t
val add_vars  : t -> var list -> t
val add_tvar  : t -> ?fresh:bool -> 'k tvar -> t
val add_tvars : t -> ?fresh:bool -> TVar.ex list -> t

val add_typedef_formal_params : t -> ('k1, 'k2) typedef_formal_params -> t

val has_var  : t -> var -> bool
val has_tvar : t -> 'k tvar -> bool

val lookup_effect : t -> k_effect neutral_type -> op_decl list
val lookup_op     : t -> k_effect neutral_type -> int -> op_decl

val lookup_adt     : t -> k_type neutral_type -> adt_ctor list
val lookup_adt_opt : t -> k_type neutral_type -> adt_ctor list option
val lookup_ctor    : t -> k_type neutral_type -> int -> adt_ctor

val can_swap : t -> 'k1 tvar -> 'k2 tvar -> bool

val prepare_typedef  : t -> fresh:bool -> typedef -> t
val prepare_typedefs : t -> fresh:bool -> typedef list -> t

val add_typedef  : t -> typedef -> t
val add_typedefs : t -> typedef list -> t 
