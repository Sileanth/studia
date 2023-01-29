
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

val tr_expr : Lang.Flat.expr -> Lang.Unif.expr
