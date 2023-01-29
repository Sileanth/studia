
type closure
type mcont
type eff_value
type coercion

type value =
| VNum      of int
| VChar     of char
| VString   of string
| VClo      of closure
| VCont     of mcont
| VCtor     of int * value list
| VPrim     of (value -> value)
| VEffect   of eff_value
| VCoercion of coercion
| VSeal     of Utils.Seal.seal

val run : Lang.Untyped.expr -> value

val pretty_flow_node : value Flow.node
val flow_node        : value Flow.node

val eval_tag : Flow.tag
