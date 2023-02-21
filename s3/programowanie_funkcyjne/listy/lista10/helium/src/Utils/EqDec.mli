
type (_, _) eq_dec =
| Equal    : ('a, 'a) eq_dec
| NotEqual : ('a, 'b) eq_dec
