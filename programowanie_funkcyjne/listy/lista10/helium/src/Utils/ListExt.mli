include module type of List
open Either

type ('a, 'b) decision =
| Drop
| Done of 'b
| Redo of 'a

val last : 'a -> 'a list -> 'a

val fold_map   : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
val filter_map : ('a -> 'b option) -> 'a list -> 'b list

val partition_map : ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list

val fold_left_i : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val zip : 'a list -> 'b list -> ('a * 'b) list

val map_cps  : ('a -> ('b -> 'c) -> 'c) -> 'a list -> ('b list -> 'c) -> 'c
val mapi_cps : (int -> 'a -> ('b -> 'c) -> 'c) ->
  'a list -> ('b list -> 'c) -> 'c

val fold_left_cps :
  ('st -> 'a -> ('st -> 'ans) -> 'ans) ->
  'st -> 'a list -> ('st -> 'ans) -> 'ans
val fold_left_i_cps :
  (int -> 'st -> 'a -> ('st -> 'ans) -> 'ans) ->
  'st -> 'a list -> ('st -> 'ans) -> 'ans

val map_filter_redo_cps : ('a -> (('a, 'b) decision -> 'c) -> 'c) ->
  'a list -> ('b list -> 'c) -> 'c
