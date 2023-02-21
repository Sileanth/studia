
type 'v value_class =
  { v_unit    : 'v
  ; list_nil  : 'v
  ; list_cons : 'v -> 'v -> 'v
  ; of_int    : int -> 'v
  ; of_bool   : bool -> 'v
  ; of_string : string -> 'v
  ; of_char   : char -> 'v
  ; of_func   : ('v -> 'v) -> 'v
  ; of_seal   : Utils.Seal.seal -> 'v
  ; to_int    : 'v -> int
  ; to_bool   : 'v -> bool
  ; to_string : 'v -> string
  ; to_char   : 'v -> char
  ; to_seal   : 'v -> Utils.Seal.seal
  }

type t =
  { mk_value : 'v. 'v value_class -> 'v }
