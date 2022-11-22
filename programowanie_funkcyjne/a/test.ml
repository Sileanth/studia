open Proof
open Logic
let a = proof [] (Imp ((Var "p"), (Imp ((Imp ((Var "p"), (Var "q"))), Var "q"))))
let b = intro "H1" a
let c = intro "H2" b
let d = apply_assm "H2" c
let cc = next c


let e = apply_assm "H1" d 

let f = qed e




