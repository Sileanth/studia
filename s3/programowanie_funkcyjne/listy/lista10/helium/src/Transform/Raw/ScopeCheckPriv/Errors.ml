
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

let operation_redefinition pos name =
  Scope_error(OperationRedefinition(pos, name))

let adt_ctor_redefinition pos name =
  Scope_error(ADTCtorRedefinition(pos, name))

let field_redeclaration pos name =
  Scope_error(FieldRedeclaration(pos, name))

let rec_function_redefinition pos name =
  Scope_error(RecFunctionRedefinition(pos, name))

let field_redefinition pos name =
  Scope_error(FieldRedefinition(pos, name))

let type_redefinition pos name =
  Scope_error(TypeRedefinition(pos, name))

let tvar_not_allowed pos =
  Scope_error(TVarNotAllowed pos)

let pattern_expected pos =
  Scope_error(PatternExpected pos)

let polymorphic_computation pos =
  Scope_error(PolymorphicComputation pos)

let illegal_abstraction pos =
  Scope_error(IllegalAbstraction pos)

let missing_rec_func_arg pos =
  Scope_error(MissingRecFuncArg pos)

let not_infix_ctor pos name =
  Scope_error(NotInfixCtor(pos, name))

let value_not_provided pos dpos name =
  Scope_error(ValueNotProvided(pos, dpos, name))

let type_not_provided pos dpos name =
  Scope_error(TypeNotProvided(pos, dpos, name))

let unknown_extern pos name =
  Scope_error(UnknownExtern(pos, name))

let unbound pos no name =
  Scope_error(Unbound(pos, no, name))

let not_module_member pos ob name mname =
  Scope_error(NotModuleMember(pos, ob, name, mname))

let type_elems_not_visible pos name =
  Scope_error(TypeElemsNotVisible(pos, name))

let module_kind_mismatch pos k1 k2 =
  Scope_error(ModuleKindMismatch(pos, k1, k2))

let functor_without_val_type pos =
  Scope_error(FunctorWithoutValType pos)

let unbound_module_np name =
  Scope_error(UnboundModuleNP name)

let dependency_loop path paths =
  Scope_error(DependencyLoop(path, paths))
