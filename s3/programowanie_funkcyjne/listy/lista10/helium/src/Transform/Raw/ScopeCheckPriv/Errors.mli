
type named_obj =
| N_Var
| N_Op
| N_Ctor
| N_Field
| N_Type
| N_Module

type module_kind =
| MKStruct
| MKFunctor

type scope_error =
| OperationRedefinition   of Utils.Position.t * string
| ADTCtorRedefinition     of Utils.Position.t * string
| FieldRedeclaration      of Utils.Position.t * string
| RecFunctionRedefinition of Utils.Position.t * string
| FieldRedefinition       of Utils.Position.t * string
| TypeRedefinition        of Utils.Position.t * string
| TVarNotAllowed          of Utils.Position.t
| PatternExpected         of Utils.Position.t
| PolymorphicComputation  of Utils.Position.t
| IllegalAbstraction      of Utils.Position.t
| MissingRecFuncArg       of Utils.Position.t
| NotInfixCtor            of Utils.Position.t * string
| ValueNotProvided        of Utils.Position.t * Utils.Position.t * string
| TypeNotProvided         of Utils.Position.t * Utils.Position.t * string
| UnknownExtern           of Utils.Position.t * string
| Unbound                 of Utils.Position.t * named_obj * string
| NotModuleMember         of Utils.Position.t * named_obj * string * string
| TypeElemsNotVisible     of Utils.Position.t * string
| ModuleKindMismatch      of Utils.Position.t * module_kind * module_kind
| FunctorWithoutValType   of Utils.Position.t
| UnboundModuleNP         of string
| DependencyLoop          of string * string list

exception Scope_error of scope_error

val operation_redefinition    : Utils.Position.t -> string -> exn
val adt_ctor_redefinition     : Utils.Position.t -> string -> exn
val field_redeclaration       : Utils.Position.t -> string -> exn
val rec_function_redefinition : Utils.Position.t -> string -> exn
val field_redefinition        : Utils.Position.t -> string -> exn
val type_redefinition         : Utils.Position.t -> string -> exn

val tvar_not_allowed        : Utils.Position.t -> exn
val pattern_expected        : Utils.Position.t -> exn
val polymorphic_computation : Utils.Position.t -> exn
val illegal_abstraction     : Utils.Position.t -> exn
val missing_rec_func_arg    : Utils.Position.t -> exn

val not_infix_ctor : Utils.Position.t -> string -> exn

val value_not_provided : Utils.Position.t -> Utils.Position.t -> string -> exn
val type_not_provided  : Utils.Position.t -> Utils.Position.t -> string -> exn

val unknown_extern : Utils.Position.t -> string -> exn

val unbound : Utils.Position.t -> named_obj -> string -> exn

val not_module_member : Utils.Position.t ->
  named_obj -> string -> string -> exn

val type_elems_not_visible : Utils.Position.t -> string -> exn

val module_kind_mismatch : Utils.Position.t ->
  module_kind -> module_kind -> exn

val functor_without_val_type : Utils.Position.t -> exn

val unbound_module_np : string -> exn

val dependency_loop : string -> string list -> exn
