open Formula
open NewVar
exception Failure of string

module type Theory = sig 
  type axiom 
  val axiom : axiom -> formula 
end

module Make(T : Theory) : sig
  type theorem


  val axiom : T.axiom -> theorem

  val consequnce : theorem  -> formula
  val assumptions : theorem -> formula list
 
  val by_assumption : formula -> theorem

  val imp_i : formula -> theorem -> theorem
  val imp_e : theorem -> theorem -> theorem 

  val bot_e : theorem -> formula -> theorem

  val top_i : theorem

  val all_i : theorem -> theorem
  val all_e : theorem -> term -> theorem

  val ex_i  : theorem -> var -> term -> formula -> theorem
  val ex_e  : theorem -> theorem -> theorem

  val and_i : theorem -> theorem -> theorem
  val and_e1 : theorem -> theorem
  val and_e2 : theorem -> theorem
  
  val or_i1 : theorem -> formula -> theorem
  val or_i2 : theorem -> formula -> theorem
  val or_e : theorem -> theorem -> theorem -> theorem

  val equiv : theorem -> formula -> theorem
  val ren : theorem -> var -> var -> theorem 

end = struct 



type env = formula list
type theorem = env * formula

let axiom ax = [], T.axiom ax

let consequnce (_, cons) = cons
let assumptions (env, _) = env


(* Helper functions *)
let rec rem xs f = 
  match xs with
  | [] -> []
  | (x :: xs) when eq_formula x f -> rem xs f
  | (x :: xs) -> x :: rem xs f


let sum (xs : formula list) (ys : formula list)=
  xs @ List.filter (fun y -> not (List.exists (eq_formula y) xs)) ys




(* Deduction rules *)
let by_assumption f = 
  [f], f

  (* Impliacja *) 
let imp_i f (env, form) =
  rem env f, Imp (f, form)

let imp_e (env1, imp) (env2, pimp) =
  sum env1 env2, match imp with 
    | Imp (a, b) when eq_formula pimp a -> b
    | _  -> print_endline (string_of_formula VarMap.empty imp );raise (Failure "wrong usage of implication elimination")


(* Bot *) 
let bot_e (env, neg) f =
  if eq_formula neg Neg then (env, f)
  else raise(Failure "wrong usage of bottom elimination")

  (* Top *)
let top_i = ([], Top)


(* For all *)
let all_i (env, f) =
  env, (All (new_var (), apply_inc_in_formula 1 f)) 

let all_e (env, all) t =
  env, match all with
    | All (_, f) -> subst_in_formula 0 t f
    | _          -> raise (Failure "wrong usage of for_all elimination")


    (* Exists *)

let ex_i (env, sf) x t f : theorem = 
  if eq_formula sf (subst_in_formula x t f) 
  then env ,Ex ((new_var ()), f)  
  else raise (Failure "wrong usage of exist introduction")

let ex_e (env1, e) (env2, f2) =
  match e with
  | Ex (_, f1) -> sum env1 (rem env2 f1), f2
  | _          -> raise (Failure "wrong usage of exists elimination rule")

    (* And *)
let and_i (e1, f1) (e2, f2) =
  sum e1 e2, And (f1, f2)

let and_e1 (env, f) =
  match f with
  | And (f, _) -> (env, f)
  | _          -> raise (Failure "wrong usage of and elim1")

let and_e2 (env, f) =
  match f with
  | And (_, s) -> (env, s)
  | _          -> raise (Failure "wrong usage of and elim1")


  (* Or *)
let or_i1 (env, a) b = 
  env, Or (a, b)

let or_i2 (env, b) a = 
  env, Or (a, b)

let or_e (env, f_or) (env1, a) (env2, b) =
  if not (eq_formula a b) 
  then 
    raise (Failure "a and b not equal in or elimination") 
  else match f_or with
  | Or (o1, o2) -> sum (sum (rem env1 o1) (rem env2 o2)) env, a
  | _           -> raise (Failure "wrong usage of or elimination")
  
  (* Renaming *)
let equiv (env, form) f =
  if eq_formula form f then (env, f)
  else raise (Failure "formulas in equiv are not equal")

let ren (env, form) x y =
  if not (free_in_formula y form) && not (List.exists (free_in_formula y) env) 
    then (List.map (subst_in_formula x (Var y)) env, subst_in_formula x (Var y) form)
    else raise (Failure "var y is free")

end
