let ctrue f s = f
let cfalse f s = s

let cand b1 b2 t f = b1 (b2 t f) (b1 t f)
let cor b1 b2 t f = b1 (b1 t f) (b2 t f)

let cbool b = if b then ctrue else cfalse
let bool_of_cbool f = f true false


let zero f x = x
let succ p f x = f (p f x)

let add a b f x = a f (b f x)

let mult a b f x = a (b f) x 


let is_zero n = n (fun x -> cfalse) ctrue

let rec cnum_of_int n f x = if n = 0 then x else f (cnum_of_int (n - 1) f x)

let int_of_cnum n = n (fun x -> x + 1) 0