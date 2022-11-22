open Logic


let a = by_assumption (Var "p")
let b = imp_i (Var "p") a
(*
  ⊢ p -> p   
*)


let c = by_assumption (Var "p")
let d = imp_i (Var "q") c
let e = imp_i (Var "p") d
(*
  ⊢ p -> q -> p   
*)


let x = by_assumption Neg
let y = bot_e (Var "p") x
let z = imp_i Neg y
(*
  ⊢ ⊥ -> p   
*)


let pqrL = (Imp (Var "p", Imp (Var "q", Var "r")))
let pqrT = by_assumption pqrL
let pT = by_assumption (Var "p")
let qrT = imp_e pqrT pT
let pqT = by_assumption (Imp ((Var "p"), (Var "q")))
let qT = imp_e pqT pT
let rT = imp_e qrT qT
let prT = imp_i (Var "p") rT 
let pqprT  = imp_i (Imp (Var "p", Var "q")) prT
let pqrpqprT = imp_i pqrL pqprT
(*
   ⊢ (p -> q -> r) -> (p -> q) -> p -> r 
*)


let _ = (pp_print_theorem Format.std_formatter  b)