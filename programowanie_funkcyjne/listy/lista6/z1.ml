

type ('a, 'b) format =
  | Lit : string -> ('a, 'a) format
  | Int : (int -> 'a ,'a) format
  | Str : (string -> 'a, 'a) format
  