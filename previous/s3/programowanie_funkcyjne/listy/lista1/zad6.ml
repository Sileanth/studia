


let ctrue f s = f
let cfalse f s = s

let cand b1 b2 t f = b1 (b2 t f) (b1 t f)
let cor b1 b2 t f = b1 (b1 t f) (b2 t f)

let cbool b = if b = true then ctrue else cfalse
let bool_of_cbool f = if f true false = true then true else false