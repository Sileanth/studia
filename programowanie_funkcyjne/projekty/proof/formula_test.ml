open Formula


(* max free in term test *)
let term1 = Sym("abc", [Var 0; Var 3;Var 6; Sym("cda", [Var 2;Var 7;Var 9])])
let _ = assert (max_free_var_in_term term1 = 9) 


let term2 = Sym("abc", [])
let _ = assert (max_free_var_in_term term2 = 0) 


(* max free in form test *)
let form1 = Neg
let _ = assert (max_free_var_in_formula form1 = 0)
let form2 = Top
let _ = assert (max_free_var_in_formula form2 = 0)

let form3 = Imp (Top, Rel ("abcv", [Var 3]))
let _ = assert (max_free_var_in_formula form3 = 3)
let form4 = Imp (Rel ("abcv", [Var 3]), Top)
let _ = assert (max_free_var_in_formula form4 = 3)


let form5 = Rel ("abc", [])
let _ = assert (max_free_var_in_formula form5 = 0)


let form6 = All ("abc", Ex ("abc", Top))
let _ = assert (max_free_var_in_formula form6 = 0)
let form7 = All ("abc", Rel ("xd", [Var 3; Var 6; Var 9]))
let _ = assert (max_free_var_in_formula form7 = 8)


(* free in term test *)

let term2_1 = Var 5
let _ = assert (free_in_term 5 term2_1)
let _ = assert (not (free_in_term 4 term2_1))

let term2_2 = Sym ("abc", [ Var 3; Var 8; Sym ("cda", [Var 1; Var 2])])
let _ = assert (free_in_term 3 term2_2)
let _ = assert (free_in_term 2 term2_2)
let _ = assert (not (free_in_term 4 term2_2))


(* free in formula test *)
let form2_1 = Neg
let _ = assert (not (free_in_formula 0 form2_1))

let form2_2 = All ("abc", Rel ("cda", [Var 0; Var 2]))
let _ = assert (free_in_formula 1 form2_2)
let _ = assert (not (free_in_formula 0 form2_2))

let form2_3 = Rel ("abc", [Var 1; Var 2; Sym ("cda", [Var 0; Var 5])])
let _ = assert (free_in_formula 2 form2_3)
let _ = assert (free_in_formula 5 form2_3)
let _ = assert (not (free_in_formula 3 form2_3))

let form2_4 = Or (Rel ("abc", [Var 6]), And (Top, Rel ("cda", [Var 2])))
let _ = assert (free_in_formula 6 form2_4)
let _ = assert (free_in_formula 2 form2_4)
let _ = assert (not (free_in_formula 3 form2_4))


(* apply inc in term test *)

let term3_1 = (Var 3)
let ans3_1  = (Var 4)
let _ = assert ( apply_inc_in_term 0 term3_1 = term3_1)
let _ = assert (apply_inc_in_term 1 term3_1 = ans3_1)

let term3_2 = Sym ("abc", [Var 2; Var 3; Sym ("cda", [Var 0; Var 1])])
let ans3_2 = Sym ("abc", [Var 3; Var 4; Sym ("cda", [Var 1; Var 2])])
let _ = assert ( apply_inc_in_term 0 term3_2 = term3_2)
let _ = assert (apply_inc_in_term 1 term3_2 = ans3_2)

(* apply inc in formula test *)

let form3_1 = Top 
let _ = assert (apply_inc_in_formula 4 form3_1 = form3_1)

let form3_2 = Imp (Rel ("abc", [Var 3; Sym ("abc", [Var 2]); Var 4]), Or (Top, Rel ("cda", [Var 2])))
let ans3_2 = Imp (Rel ("abc", [Var 5; Sym ("abc", [Var 4]); Var 6]), Or (Top, Rel ("cda", [Var 4])))
let _ = assert (apply_inc_in_formula 0 form3_2 = form3_2)
let _ = assert (apply_inc_in_formula 2 form3_2 = ans3_2)

let form3_3 = All ("abc", Ex ("cda", Rel ("abdsa", [Var 3; Var 0; Var 1])))
let ans3_3 = All ("abc", Ex ("cda", Rel ("abdsa", [Var 4; Var 1; Var 2])))
let _ = assert (apply_inc_in_formula 0 form3_3 = form3_3)
let _ = assert (apply_inc_in_formula 1 form3_3 = ans3_3)


(* Test subst_in_term    funkcje pomocnicze dla tej funkcji nie są testowane *)
let term4_1 = Var 3
let ans4_1  = Sym ("cda", [Var 1; Var 2])
let _ = assert (subst_in_term 0 (Var 3) term4_1 = term4_1)
let _ = assert (subst_in_term 3 (ans4_1) term4_1 = ans4_1) 

let term4_2 = Sym ("abc", [Var 0 ; Var 1; Sym ("cda", [Var 0])])
let anss4_2 = Sym ("abc", [Var 3 ; Var 1; Sym ("cda", [Var 3])])
let _ = assert (subst_in_term 0 (Var 0) term4_2 = term4_2)
let _ = assert (subst_in_term 0 (Var 3) term4_2 = anss4_2)


(* test subst_in_formula funkcje pomocnicze dla tej funkcji nie są testowane *)

let form4_1 = Neg
let _ = assert (subst_in_formula 3 (Var 6) form4_1 = form4_1)
let form4_2 = Top
let _ = assert (subst_in_formula 3 (Var 6) form4_2 = form4_2)

let form4_3 = Imp (And (Top, Rel ("cda", [Var 3])), Or (Rel ("a", []), Rel ("aa", [Var 2; Var 3])))
let anss4_3 = Imp (And (Top, Rel ("cda", [Sym ("cda", [Var 2])])), Or (Rel ("a", []), Rel ("aa", [Var 2;Sym ("cda", [Var 2]) ])))
let _ = assert (subst_in_formula 3 (Sym ("cda", [Var 2])) form4_3 = anss4_3)

let form4_4 = Ex ("a", Or (Rel ("cda", [Var 0; Var 1]), All ("b", Rel ("c", [Var 0; Var 1; Var 2]))))
let anss4_4 = Ex ("a", Or (Rel ("cda", [Var 0; Var 10]), All ("b", Rel ("c", [Var 0; Var 1; Var 11]))))
let _ = assert (subst_in_formula 0 (Var 9) form4_4 = anss4_4)


let form4_5 = Or (Rel ("d", [Var 0]), All ("e", Rel ("f", [Var 0; Var 1])))
let anss4_5 = Or (Rel ("d", [Var 3]), All ("e", Rel ("f", [Var 0; Var 4])))
let _ = assert (subst_in_formula 0 (Var 3) form4_5 = anss4_5)
let _ = assert (subst_in_formula 10 (Var 2) form4_5 = form4_5)


(* eq_term wbudowana równość więc nietestowane *)

(* eq_formula *) 

let form5_1a = All ("a", Top)
let form5_1b = All ("b", Top)
let _ = assert (eq_formula form5_1a form5_1b)

let form5_2a = Ex ("a", Top)
let form5_2b = Ex ("a", Neg)
let _ = assert (not (eq_formula form5_2a form5_2b))


let _ = assert (eq_formula Neg Neg)
let _ = assert (eq_formula Top Top)
let _ = assert (not (eq_formula Neg Top))

let form5_3a = Imp (Top, Or (Neg, And (Top, Top)))
let form5_3b = Imp (Top, Or (Neg, And (Neg, Top)))
let _ = assert (eq_formula form5_3a form5_3a)
let _ = assert (not (eq_formula form5_3a form5_3b))


let form5_4a = Rel ("a", [Var 5; Sym ("d", [Var 4])])
let form5_4b = Rel ("b", [Var 5; Sym ("d", [Var 4])])
let form5_4c = Rel ("a", [Var 7; Sym ("d", [Var 4])])
let form5_4d = Rel ("a", [Var 5; Sym ("c", [Var 4])])
let _ = assert (eq_formula form5_4a form5_4a)
let _ = assert (eq_formula form5_4b form5_4b)
let _ = assert (eq_formula form5_4c form5_4c)
let _ = assert (eq_formula form5_4d form5_4d)
let _ = assert (not (eq_formula form5_4a form5_4b))
let _ = assert (not (eq_formula form5_4a form5_4c))
let _ = assert (not (eq_formula form5_4a form5_4d))
