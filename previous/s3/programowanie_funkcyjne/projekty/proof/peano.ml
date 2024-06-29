open Formula
open NewVar
module T = struct
        type axiom = 
                | EqRefl
                | EqElim of var * formula
                | PlusZ
                | PlusS
                | Induction of var * formula

        let axiom = function
                | EqRefl ->
                                let x = new_var () in
                                All (x, Rel ("=", [Var 0; Var 0]))
                | EqElim(x, f) ->
                                let y, z = new_var (), new_var () in
                                All (y, All (z, Imp (
                                        Rel ("=", [Var 0; Var 1]),
                                        Imp (
                                                subst_in_formula x (Var 1) f,
                                                subst_in_formula x (Var 0) f))))
                | PlusS ->
                                let n, m = new_var (), new_var () in
                                All (n, All(m, Rel ("=",
                                [ Sym("+", [Sym("s", [Var 0]); Var 1])
                                ; Sym("s", [Sym("+", [Var 0; Var 1])])
                                ])))
                | PlusZ ->
                                let n = new_var () in
                                All (n, Rel ("=",
                                [ Sym("+", [Sym ("z", []); Var 0])
                                ; Var 0
                                ]))
                | Induction (x, f) ->
                                let n = new_var () in
                                Imp (
                                        subst_in_formula x (Sym("z", [])) f,
                                        Imp (
                                                All(n, Imp(
                                                        subst_in_formula x (Var 0) f,
                                                        subst_in_formula x (Sym("s", [Var 0])) f)),
                                                All(n, subst_in_formula x (Var 0) f)))

                                  

end

open T
module P = Proof.Make(T)
open P

let form_l1 = All ("x", All ("y", Imp (Rel ("=", [Var 0; Var 1]), Rel ("=", [Var 1; Var 0])))) 
let l1_1 = proof form_l1 StringMap.empty (StringMap.empty, VarMap.empty)
let l1_2 = forall_intro l1_1 
let th_1 = (P.AxiomLogic.axiom (EqElim (0, (Rel ("=", [Var 0; Var 0])))))
(*
let _ = print_endline (proof_string l1_1)
let _ = print_endline (proof_string l1_2)
let _ = print_endline (string_theorem VarMap.empty th_1)
*)
let form_l2 = All ("n", Rel ("=", [Sym ("+", [Var 0; Sym ("z", [])]); Var 0]))
let l2_1 = proof form_l2 StringMap.empty (StringMap.empty, VarMap.empty)

let th2_1 = P.AxiomLogic.axiom (Induction (0, Rel ("=", [Sym ("+", [Var 0; Sym ("z", [])]); Var 0])))

let f2_1 = P.AxiomLogic.consequnce (th2_1)
let l2_2 = apply_implication f2_1 l2_1 
let l2_3 = apply_theorem th2_1 l2_2
let th2_2 = P.AxiomLogic.axiom (PlusZ )
let l2_4 = forall_exlim (Rel ("=", [Sym ("+", [Var 0; Sym ("z", []) ]  );Var 0 ])) "3" (Sym ("z", [])) l2_3
let l2_5 = apply_theorem th2_2 l2_4
let l2_6 = forall_intro l2_5
let l2_7 = intro "ih" l2_6
let _ = print_endline (proof_string l2_1)
let _ = print_endline (string_theorem VarMap.empty th2_2) 
let _ = print_endline (proof_string l2_2)
let _ = print_endline (proof_string l2_3)
let _ = print_endline (proof_string l2_4)
let _ = print_endline (proof_string l2_5)
let _ = print_endline (proof_string l2_6)
let _ = print_endline (proof_string l2_7)
