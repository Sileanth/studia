
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

val tr_program : Lang.Raw.file -> Lang.Flat.expr
