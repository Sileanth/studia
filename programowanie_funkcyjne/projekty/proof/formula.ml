
type var = int 
type sym = string 
type rel = string 
type term =
  | Var of var
  | Sym of sym * term list
and formula =
  | Neg
  | Top
  | Imp of formula * formula
  | And of formula * formula
  | Or  of formula * formula
  | Rel of rel * term list
  | All of string * formula
  | Ex  of string * formula

(* Pytanie otwarte czy chcę w All trzymać nazwę związanej zmiennej? *)

module OrderedVar: sig
  type t = var 
  val compare : t -> t -> int
end = struct
  type t = var
  let compare a b =
    if a = b then 0
    else if a < b then -1
    else 1
end
module VarMap = Map.Make(OrderedVar)
(* Moduł do generowania nowych nazw zmiennych by się nie dublowały. Chyba niepotrzebny *)


let rec max_free_var_in_term (t : term) =
  match t with
  | Var v  -> v
  | Sym (s, st) -> List.fold_left (fun a b -> max a b) (0) (List.map max_free_var_in_term st)

let rec max_free_var_in_formula = function
  | Neg -> 0
  | Top -> 0
  | Imp (a, b)  -> max (max_free_var_in_formula a) (max_free_var_in_formula b)
  | And (a, b)  -> max (max_free_var_in_formula a) (max_free_var_in_formula b)
  | Or  (a, b)  -> max (max_free_var_in_formula a) (max_free_var_in_formula b)
  | Rel (_, st) -> List.fold_left (fun a b -> max a b) (0) (List.map max_free_var_in_term st)
  | All (_, f)  -> max 0 ((max_free_var_in_formula f) - 1) 
  | Ex  (_, f)  -> max 0 ((max_free_var_in_formula f) - 1) 

let gen_free_var_in_term t =
  max_free_var_in_term t + 1

let gen_free_var_in_formula t =
  max_free_var_in_formula t + 1


let rec free_in_term (v : var) (t : term) = 
  match t with
  | Var nv      -> nv = v
  | Sym (_, ts) -> List.exists (free_in_term v) ts
and free_in_formula (v : var) ( f : formula) =
  match f with
  | Neg         -> false
  | Top         -> false
  | All (_,  f) -> free_in_formula (v+1) f
  | Ex  (_,  f) -> free_in_formula (v+1) f
  | Imp (p, s)  -> free_in_formula v p || free_in_formula v s
  | And (p, s)  -> free_in_formula v p || free_in_formula v s
  | Or  (p, s)  -> free_in_formula v p || free_in_formula v s
  | Rel (_, ts) -> List.exists (free_in_term v) ts

let rec apply_inc_in_term (inc : int) (t : term) : term =
  match t with 
  | Var v       -> Var (v+inc)
  | Sym (s, st) -> Sym (s, List.map (apply_inc_in_term inc) st)

let rec apply_inc_in_formula (inc : int) (f : formula) : formula=
  match f with 
  | Neg         -> Neg
  | Top         -> Top 
  | Imp (a, b)  -> Imp (apply_inc_in_formula inc a, apply_inc_in_formula inc b)
  | And (a, b)  -> And (apply_inc_in_formula inc a, apply_inc_in_formula inc b)
  | Or  (a, b)  -> Or  (apply_inc_in_formula inc a, apply_inc_in_formula inc b)
  | All (s, f)  -> All (s, apply_inc_in_formula (inc + 1) f)
  | Ex  (s, f)  -> All (s, apply_inc_in_formula (inc + 1) f)
  | Rel (s, ts) -> Rel (s, List.map (apply_inc_in_term inc) ts)

let rec psubt_in_term_helper (inc : int) (map : term VarMap.t) (t : term) : term =
    match t with
    | Var v -> begin match (VarMap.find_opt v map) with
      | None   -> Var v
      | Some t -> t
    end
    | Sym (s , xt) -> Sym (s, List.map (psubt_in_term_helper inc map) xt)

let psubt_in_term = psubt_in_term_helper 0

let rec psub_in_formula_helper (inc : int) (map : term VarMap.t) (f : formula) : formula =
  match f with
  | Neg         -> Neg
  | Top         -> Top 
  | Imp (a, b)  -> Imp (psub_in_formula_helper inc map a, psub_in_formula_helper inc map b)
  | And (a, b)  -> And (psub_in_formula_helper inc map a, psub_in_formula_helper inc map b)
  | Or  (a, b)  -> Or  (psub_in_formula_helper inc map a, psub_in_formula_helper inc map b)
  | Rel (s, ts) -> Rel (s, List.map (psubt_in_term_helper inc map) ts)
  | All (s, f)  -> All (s, psub_in_formula_helper (inc + 1) map f)
  | Ex  (s, f)  -> All (s, psub_in_formula_helper (inc + 1) map f)

let psub_in_formula = psub_in_formula_helper 0

let subst_in_term x s t = psubt_in_term (VarMap.singleton x s) t
let subst_in_formula x s t = psub_in_formula (VarMap.singleton x s) t



let eq_term a b =
  a = b

let rec eq_formula a b = 
  match (a, b) with
  | All (_, a) , All (_, b)  -> eq_formula a b
  | Ex  (_, a) , All (_, b)  -> eq_formula a b
  | Imp (a,b), Imp (c, d)    -> eq_formula a c && eq_formula b d
  | And (a,b), And (c, d)    -> eq_formula a c && eq_formula b d
  | Or (a,b), Or (c, d)      -> eq_formula a c && eq_formula b d
  | Neg, Neg                 -> true
  | Top, Top                 -> true
  | Rel (x, xt), Rel (y, yt) -> x = y && xt = yt
  | _, _                     -> false
