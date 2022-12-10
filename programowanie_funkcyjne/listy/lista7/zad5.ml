

type 'a regexp =
  | Eps
  | Lit of ('a -> bool)
  | Or of 'a regexp * 'a regexp
  | Cat of 'a regexp * 'a regexp
  | Star of 'a regexp * 'a regexp


let ( +% ) r1 r2 = Or(r1, r2)
let ( *% ) r1 r2 = Cat(r1, r2)
