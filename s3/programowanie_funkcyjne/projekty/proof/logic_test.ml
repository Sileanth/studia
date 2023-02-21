open Formula
open Logic


module EmptyTheory = struct
        type axiom = 
                | Nax
        let axiom ax = match ax with
        | Nax -> Neg
end
open EmptyTheory

module EmptyLogic = Logic.Make(EmptyTheory) 


(* By assumption test *)
let f1_1 = And (Neg, Top)
let th1_1 = EmptyLogic.by_assumption f1_1
let _ = assert (EmptyLogic.consequnce th1_1 = f1_1)
let _ = assert (EmptyLogic.assumptions th1_1 = [f1_1])



(* implication introduction test *)

let th2_1 = EmptyLogic.imp_i f1_1 th1_1
let _ = assert (EmptyLogic.consequnce th2_1 = Imp (f1_1, f1_1))
let _ = assert (EmptyLogic.assumptions th2_1 = [])


(* top introduction test *)
let th3_1 = EmptyLogic.top_i 
let _ = assert (EmptyLogic.consequnce th3_1 = Top)
let _ = assert (EmptyLogic.assumptions th3_1 = [])



(* Axioms test*)
let th4_1 = EmptyLogic.axiom Nax
let _ = assert (EmptyLogic.consequnce th4_1 = Neg)
let _ = assert (EmptyLogic.assumptions th4_1 = [])


(* bot eliminations tests *)
let f5_1 = (Imp (Top, Top))
let th5_1 = EmptyLogic.bot_e (EmptyLogic.axiom Nax) f5_1

let _ = assert (EmptyLogic.consequnce th5_1 = f5_1)
let _ = assert (EmptyLogic.assumptions th5_1 = [])

let th5_2 = EmptyLogic.by_assumption Neg
let th5_3 = EmptyLogic.bot_e th5_2 f5_1

let _ = assert (EmptyLogic.consequnce th5_3 = f5_1)
let _ = assert (EmptyLogic.assumptions th5_3 = [Neg])


let _ = try let _ = (EmptyLogic.bot_e EmptyLogic.top_i Top) in assert false
        with
        | Failure _ -> ()


(*  imp elimination tests *)
let th6_1 = EmptyLogic.by_assumption (Imp (Top, Neg))
let th6_2 = EmptyLogic.by_assumption Top
let th6_3 = EmptyLogic.top_i
let th6_4 = EmptyLogic.imp_e th6_1 th6_2
let th6_5 = EmptyLogic.imp_e th6_1 th6_3

let _ = assert (EmptyLogic.consequnce th6_4 = Neg) 
let _ = assert (EmptyLogic.consequnce th6_5 = Neg) 
let _ = assert (EmptyLogic.assumptions th6_5 = [Imp (Top, Neg)])
let _ = assert (EmptyLogic.assumptions th6_4 = [Imp (Top, Neg); Top])

let _ = try let _ = (EmptyLogic.imp_e th6_1 th6_4) in assert false
        with
        | Failure _ -> ()

(* Forall introduction *)
let th7_1 = EmptyLogic.by_assumption (Rel ("x", [Var 0; Var 1])) 
let th7_2 = EmptyLogic.all_i th7_1
let _ = assert (EmptyLogic.consequnce th7_2 = (All ("0",  (Rel ("x", [Var 1; Var 2]))))) 
let _ = assert (EmptyLogic.assumptions th7_2 = [(Rel ("x", [Var 0; Var 1]))]) 


(* forall elimination *)

let th8_1 = EmptyLogic.top_i

let _ = try let _ = (EmptyLogic.all_e th8_1 (Var 5)) in assert false
        with
        | Failure _ -> ()


(* to do pozytywny test *)

(* equiv test też by się przydał *)


(* pozostałe nice to have , ta część logiki nie jest używana do udowadniania arytmetyki*)

