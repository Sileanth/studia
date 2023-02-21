
type syn_cat =
| Expression
| ExpressionEffRow
| Pattern
| Handler
| Field

type type_mismatch_reason = Lang.Unif.tconst option

type type_error =
| KindMismatch     of Utils.Position.t * Lang.Unif.kind * Lang.Unif.kind
| TArgKindMismatch of Utils.Position.t * Lang.Unif.kind * Lang.Unif.kind
| TypeMismatch     of Utils.Position.t
    * syn_cat * Lang.Unif.typ * Lang.Unif.typ * type_mismatch_reason
| ValueSigMismatch of Utils.Position.t * Utils.Position.t * string
    * Lang.Unif.typ * Lang.Unif.typ
| TypeSigMismatch  of Utils.Position.t * Utils.Position.t * string
    * Lang.Unif.kind * Lang.Unif.kind
| EmptyHandler   of Utils.Position.t
| CtorArity      of Utils.Position.t * string * int * int
| OpArity        of Utils.Position.t * string * int * int
| CtorTypeArity  of Utils.Position.t * string * int * int
| OpTypeArity    of Utils.Position.t * string * int * int
| EffectAnnotInPure  of Utils.Position.t
| PolymorphicPattern of Utils.Position.t
| NonValuePolyField  of Utils.Position.t * string

exception Type_error of type_error

let kind_mismatch pos k1 k2 =
  Type_error (KindMismatch(pos, k1, k2))

let targ_kind_mismatch pos k1 k2 =
  Type_error (TArgKindMismatch(pos, k1, k2))

let type_mismatch pos sc tp1 tp2 reason =
  Type_error (TypeMismatch(pos, sc, tp1, tp2, reason))

let value_sig_mismatch pos dpos name tp1 tp2 =
  Type_error (ValueSigMismatch(pos, dpos, name, tp1, tp2))

let type_sig_mismatch pos dpos name k1 k2 =
  Type_error (TypeSigMismatch(pos, dpos, name, k1, k2))

let empty_handler pos =
  Type_error (EmptyHandler pos)

let ctor_arity pos name n1 n2 =
  Type_error (CtorArity(pos, name, n1, n2))

let op_arity pos name n1 n2 =
  Type_error (OpArity(pos, name, n1, n2))

let ctor_type_arity pos name n1 n2 =
  Type_error (CtorTypeArity(pos, name, n1, n2))

let op_type_arity pos name n1 n2 =
  Type_error (OpTypeArity(pos, name, n1, n2))

let effect_annot_in_pure pos =
  Type_error (EffectAnnotInPure pos)

let polymorphic_pattern pos =
  Type_error (PolymorphicPattern pos)

let non_value_poly_field pos name =
  Type_error (NonValuePolyField(pos, name))
