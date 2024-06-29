open Formula

module T = struct
        type axiom = 
                | X
                | Magic of formula
        let axiom = function
                | X -> Neg
                | Magic f -> f
end

open T
module P = Proof.Make(T)
open P

(* # ProofTree Testing *)

(* goal_test *)

let g = (Top, P.StringMap.empty, (P.StringMap.empty, VarMap.empty))

let pf1_1 = P.ProofTree.goal g 
let _ = assert (P.ProofTree.view pf1_1 = VGoal g)


(* lemat test *)

let th2_1 = P.AxiomLogic.axiom X
let pf2_1 = P.ProofTree.lemat th2_1

let _ = assert (P.ProofTree.view pf2_1 = VLemat th2_1)

(* impI test *)

let n3_1 = g 
let th3_1 = P.AxiomLogic.top_i
let pf3_1 = P.ProofTree.impI n3_1 (P.ProofTree.lemat th3_1)
let pf3_2 = P.ProofTree.goal g
let pf3_3 = ProofTree.impI n3_1 pf3_2 

let _ = assert (P.ProofTree.view pf3_1 = VLemat (P.AxiomLogic.axiom (Magic (Imp (Top, Top))))) 
let _ = assert (ProofTree.view pf3_3 = VImpI (n3_1, pf3_2))


(* impE test *)

let n4_1 = (Top, StringMap.empty, (P.StringMap.empty, VarMap.empty))
let pf4_1 = (ProofTree.goal (Imp (Top, Top), StringMap.empty, (P.StringMap.empty, VarMap.empty)))
let pf4_2 = (ProofTree.lemat (AxiomLogic.axiom (Magic Top)))
let pf4_3 = (ProofTree.impE n4_1 pf4_1 pf4_2)

let _ = assert (ProofTree.view pf4_3 = VImpE (n4_1, pf4_1, pf4_2))

let pf4_4 = (ProofTree.lemat (AxiomLogic.axiom (Magic (Imp (Top, Top)))))
let pf4_5 = (ProofTree.impE n4_1 pf4_4 pf4_2)
let _ = assert (ProofTree.view pf4_5 = VLemat ((AxiomLogic.axiom (Magic Top))))


(* botE test *)

let n5_1 = (Or (Top, Neg), P.StringMap.empty, (P.StringMap.empty, VarMap.empty))

let pf5_1 = ProofTree.goal (Neg, StringMap.empty, (P.StringMap.empty, VarMap.empty))

let pf5_2 = ProofTree.lemat (AxiomLogic.axiom (Magic Neg))
let pf5_3 = ProofTree.botE n5_1 pf5_1
let pf5_4 = ProofTree.botE n5_1 pf5_2

let _ = assert (ProofTree.view pf5_3 = VBotE (n5_1, pf5_1))
let _ = assert (ProofTree.view pf5_4 = VLemat ((AxiomLogic.axiom (Magic (Or (Top, Neg))))))

(* AllI  test *)

let n6_1 = (Or (Top, Neg), P.StringMap.empty, (P.StringMap.empty, VarMap.empty))
let n6_2 = ((All ("x", Or (Top, Neg))), P.StringMap.empty, (P.StringMap.empty, VarMap.empty))
let pf6_1 = ProofTree.goal n6_1
let pf6_2 = ProofTree.allI n6_2 pf6_1

let pf6_3 = ProofTree.lemat (AxiomLogic.axiom (Magic (Or (Top, Neg))))
let pf6_4 = ProofTree.allI n6_2 pf6_3

let _ = assert (ProofTree.view pf6_2 = VAllI (n6_2, pf6_1))
let _ = assert (ProofTree.view pf6_4 = VLemat ((AxiomLogic.axiom (Magic ((All ("0", Or (Top, Neg))))))))

(* All elimination test *)
let f7_1 = Rel ("=", [Var 5]) 
let n7_1 = (f7_1, P.StringMap.empty, (P.StringMap.empty, VarMap.empty))
let f7_2 = All ("x", Rel ("=", [Var 0])) 
let pf7_1 = ProofTree.goal (f7_2, P.StringMap.empty, (P.StringMap.empty, VarMap.empty))
let pf7_2 = ProofTree.lemat (AxiomLogic.axiom (Magic f7_2))
let t7_1 = (Var 5)
let pf7_3 = ProofTree.allE t7_1 n7_1 pf7_1
let pf7_4 = ProofTree.allE t7_1 n7_1 pf7_2

let _ = assert (ProofTree.view pf7_3 = VAllE (n7_1, pf7_1, t7_1))
let _ = assert (ProofTree.view pf7_4 = VLemat (AxiomLogic.axiom (Magic f7_1)))

(* equiv test *)
let f8_1 = (All ("x", Top))
let f8_2 = (All ("y", Top))
let n8_1 = (f8_1, P.StringMap.empty, (P.StringMap.empty, VarMap.empty))
let pf8_1 = ProofTree.goal (f8_2,  P.StringMap.empty, (P.StringMap.empty, VarMap.empty))
let pf8_2 = ProofTree.lemat (AxiomLogic.axiom (Magic f8_2))

let pf8_3 = ProofTree.equiv_b n8_1 f8_1 pf8_1
let pf8_4 = ProofTree.equiv_b n8_1 f8_1 pf8_2

let _ = assert (ProofTree.view pf8_3 = VEquiv (n8_1, pf8_1, f8_1))
let _ = assert (ProofTree.view pf8_4 = VLemat (AxiomLogic.axiom (Magic f8_1)))


(* # Print theorem test *)
let th9_1 = AxiomLogic.by_assumption (Imp (Neg, Rel ("xd", [Var 0; Var 1; Var 2])))
let map = [(0, "a"); (1, "b"); (2, "c"); (3, "d"); (4, "e"); (5, "f")] |> List.to_seq |> VarMap.of_seq


let str1 = string_theorem map th9_1
(* let _ = print_endline str1 *)


let p9_1 = Complete (th9_1, (StringMap.empty, map))
let str2 = proof_string p9_1
(* let _ = print_endline str2 *)


let assumptions_list = [("ab", Top); ("c", Rel ("xd", [Var 0]))]
let assumptions = assumptions_list |> List.to_seq |> StringMap.of_seq
let p9_2 = proof (Imp (Neg, Rel ("=", [Var 0; Var 1; Var 2]))) assumptions (StringMap.empty, map)
let str3 = proof_string p9_2
(* let _ = print_endline str3 *)


(* rec (find goal) test *)

let pf10_1 = ProofTree.goal ((Imp (Neg, Top)), assumptions , (StringMap.empty, map))
let pf10_2 = ProofTree.goal (Neg, StringMap.empty , (StringMap.empty, map))
let n10_1 = (Top, assumptions, (StringMap.empty, map))
let pf10_3 = ProofTree.impE n10_1 pf10_1 pf10_2
let (pf10_4, ctx10_1) = rek pf10_3 CRoot

let _ = assert (ctx10_1 = CImpeEL (CRoot, n10_1, pf10_2))
let _ = assert (pf10_4 = pf10_1)


let (pf10_5, ctx10_2) = rek pf10_4 ctx10_1

let _ = assert (ctx10_2 = CImpeER (CRoot, n10_1, pf10_1)) 

let (pf10_6, ctx10_3) = rek pf10_5 ctx10_2

let _ = assert (ctx10_3 = CImpeEL (CRoot, n10_1, pf10_2))
let _ = assert (pf10_6 = pf10_1)


(* TO DO testy z innymi nodami w drzewie *)


(* testy next goal *)
let p11_1 = Sketch (((Imp (Neg, Top)), assumptions , (StringMap.empty, map)),  CImpeEL (CRoot, n10_1, pf10_2))

(* let str11_1 = proof_string p11_1
let _ = print_endline str11_1 *)

let p11_2 = next_goal p11_1

(* let str11_2 = proof_string p11_2
let _ = print_endline str11_2 *)

let p11_3 = next_goal p11_2
let _ = assert (p11_1 = p11_3)
(* let str11_3 = proof_string p11_3
let _ = print_endline str11_3 *)

(* intro tests *) 

let p12_1 = proof (Imp (Neg, Top)) StringMap.empty (StringMap.empty, map)
let p12_2 = intro "ab" p12_1
let p12_3 = next_goal p12_2

let p12_4 = intro "h1" p11_3
let p12_5 = next_goal p12_4
let p12_6 = next_goal p12_5
(* 
let _ = print_endline (proof_string p12_1)
let _ = print_endline (proof_string p12_2)
let _ = print_endline (proof_string p12_3)
let _ = print_endline (proof_string p11_3)
let _ = print_endline (proof_string p12_4)
let _ = print_endline (proof_string p12_5)
let _ = print_endline (proof_string p12_6) 
*)

let p13_1 = proof (All ("x", (Imp (Neg, Top)))) StringMap.empty (StringMap.empty, map)
let p13_2 = forall_intro p13_1
(*
let _ = print_endline (proof_string p13_1)
let _ = print_endline (proof_string p13_2)
*)

let p14_1 = proof (Rel ("=", [Var 3; Var 4; Var 5])) StringMap.empty (StringMap.empty, map)
let p14_2 = forall_exlim (Rel ("=", [Var 0; Var 4; Var 5])) "x" (Var 3) p14_1
(*
let _ = print_endline (proof_string p14_1)
let _ = print_endline (proof_string p14_2)
*)

let p15_1 = proof (Imp (Neg, Neg)) StringMap.empty (StringMap.empty, map)
let p15_2 = intro "a" p15_1
let p15_3 = apply_assumption "a" p15_2

(*
let _ = print_endline (proof_string p15_1)
let _ = print_endline (proof_string p15_2)
let _ = print_endline (proof_string p15_3)
*)
let p16_1 = proof (Imp (Rel ("p", []), Imp (Rel ("q", []), Rel ("p", [])))) StringMap.empty (StringMap.empty, map)
let p16_2 = intro "p" p16_1
let p16_3 = intro "_" p16_2
let p16_4 = apply_assumption "p" p16_3
(*
let _ = print_endline (proof_string p16_1)
let _ = print_endline (proof_string p16_2)
let _ = print_endline (proof_string p16_3)
let _ = print_endline (proof_string p16_4)
*)

(* (p → q → r) → (p → q) → p → r, *)

let f17_1 = Imp (Imp ((Rel ("p", [])), Imp (Rel ("q", []), Rel ("r", []))), Imp (Imp (Rel ("p", []), Rel ("q", [])), Imp (Rel ("p", []), Rel ("r", []))))
let p17_1 = proof f17_1 StringMap.empty (StringMap.empty, map)
let p17_2 = intro "h1" p17_1
let p17_3 = intro "h2" p17_2
let p17_4 = intro "p" p17_3

let f17_2 = Imp (Rel ("p", []), Imp (Rel ("q", []), Rel ("r", [])))
let p17_5 = apply_implication f17_2 p17_4
(*
let p17_c = next_goal p17_5
let _ = print_endline (proof_string p17_c)
let p17_c = next_goal p17_c
let _ = print_endline (proof_string p17_c)
let p17_c = next_goal p17_c
let _ = print_endline (proof_string p17_c)
let p17_c = next_goal p17_c
let _ = print_endline (proof_string p17_c)
let p17_c = next_goal p17_c
let _ = print_endline (proof_string p17_c)
let p17_c = next_goal p17_c
let _ = print_endline (proof_string p17_c)
*)


let p17_6 = apply_assumption "h1" p17_5
let p17_7 = apply_assumption "p" p17_6
let p17_8 = apply_implication (Imp (Rel ("p", []), Rel ("q", []))) p17_7
let p17_9 = apply_assumption "h2" p17_8
let p17_10 = apply_assumption "p" p17_9
(*
let _ = print_endline (proof_string p17_1)
let _ = print_endline (proof_string p17_2)
let _ = print_endline (proof_string p17_3)
let _ = print_endline (proof_string p17_4)
let _ = print_endline (proof_string p17_5)
let _ = print_endline (proof_string p17_6)
let _ = print_endline (proof_string p17_7)
let _ = print_endline (proof_string p17_8)
let _ = print_endline (proof_string p17_9)
let _ = print_endline (proof_string p17_10)
*)

(* efalso testy *)


let p18_1 = proof (Rel ("xd", [])) StringMap.empty (StringMap.empty, map)
let p18_2 = exfalso p18_1
let p18_3 = apply_theorem (AxiomLogic.axiom (Magic Neg)) p18_2
(*
let _ = print_endline (proof_string p18_1)
let _ = print_endline (proof_string p18_2)
let _ = print_endline (proof_string p18_3)
*)



